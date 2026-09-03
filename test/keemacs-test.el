;;; keemacs-test.el --- Tests for keemacs -*- lexical-binding: t -*-

;;; Commentary:

;; Run with:
;;   emacs -Q --batch -l ert -l keemacs.el \
;;         -l keemacs-test.el -f ert-run-tests-batch-and-exit
;;
;; Integration tests build a throwaway KeePassXC database via
;; `keepassxc-cli' and are skipped when the binary is absent.

;;; Code:
(require 'ert)
(require 'keemacs)

(defcustom keemacs-test-program
  (or (executable-find "keepassxc-cli") "")
  "keepassxc-cli executable for the optional integration test."
  :type 'string
  :group 'keemacs)

(defun keemacs-test-run (cmd)
  "Run shell command CMD via /bin/sh, returning its stdout."
  (with-temp-buffer
    (call-process "sh" nil t nil "-c" cmd)
    (buffer-string)))

(defun keemacs-test-add (cli db title user url)
  "Add entry TITLE/USER/URL to DB via keepassxc-cli CLI."
  (keemacs-test-run
   (format "printf 'PASS\\n' | %s add -q %s %s -u %s -g --url %s"
           cli (shell-quote-argument db) (shell-quote-argument title)
           (shell-quote-argument user) (shell-quote-argument url))))

(defun keemacs-test-make-db ()
  "Create a fresh throwaway kdbx with a root entry and a group entry."
  (let* ((dir (make-temp-file "kb-test-" t))
         (db (expand-file-name "t.kdbx" dir))
         (cli (shell-quote-argument keemacs-test-program))
         (qdb (shell-quote-argument db)))
    (keemacs-test-run
     (format "printf 'PASS\\nPASS\\n' | %s db-create -q %s --set-password" cli qdb))
    (keemacs-test-run
     (format "printf 'PASS\\n' | %s mkdir -q %s Work" cli qdb))
    (keemacs-test-add cli qdb "email" "me@x.com" "smtp.x.com:465")
    (keemacs-test-add cli qdb "Work/github" "cbit" "https://github.com")
    db))

(defmacro keemacs-test-with-db (&rest body)
  "Bind a fresh test DB and run BODY with it set as the active database."
  `(when keemacs-test-program
     (let* ((db (keemacs-test-make-db))
            (keemacs-database (keemacs-auth-make-db-spec :file db))
            (password-cache-expiry nil))
       (password-cache-add db "PASS")
       (unwind-protect
           (progn ,@body)
         (delete-file db)))))

;;;; Non-subprocess unit tests

(ert-deftest keemacs-parse-show ()
  "`parse-show' picks out the standard fields."
  (let ((entry (keemacs--parse-show
                "Title: t\nUserName: u@x.com\nPassword: secret\nURL: http://x\nNotes: n\nUuid: {x}\n")))
    (should (equal "t" (cdr (assoc "Title" entry))))
    (should (equal "u@x.com" (cdr (assoc "UserName" entry))))
    (should (equal "secret" (cdr (assoc "Password" entry))))
    (should (equal "http://x" (cdr (assoc "URL" entry))))
    (should (equal "n" (cdr (assoc "Notes" entry))))
    ;; Non-standard fields are ignored.
    (should-not (assoc "Uuid" entry))))

(ert-deftest keemacs-parse-entry-buffer ()
  "`parse-entry' reads Field: value lines, including the optional Group."
  (with-temp-buffer
    (insert "Group: /g/\nTitle: /g/t\nUserName: bob\nPassword: pw\nURL: http://x\n")
    (let ((entry (keemacs--parse-entry)))
      (should (equal "/g/" (cdr (assoc "Group" entry))))
      (should (equal "/g/t" (cdr (assoc "Title" entry))))
      (should (equal "bob" (cdr (assoc "UserName" entry))))
      (should (equal "pw" (cdr (assoc "Password" entry)))))))

(ert-deftest keemacs-parse-entry-group-absent ()
  "A buffer without a Group line still parses; Group is optional."
  (with-temp-buffer
    (insert "Title: t\nUserName: u\nPassword: p\nURL: http://x\n")
    (let ((entry (keemacs--parse-entry)))
      (should-not (assoc "Group" entry))
      (should (equal "t" (cdr (assoc "Title" entry)))))))

(ert-deftest keemacs-edit-includes-group-line ()
  "`keemacs-edit' templates the Group line above Title."
  (let* ((entry '(("Title" . "t") ("UserName" . "u") ("Password" . "p")
                  ("URL" . "x") ("Notes" . "n")))
         (keemacs--entry-parents '(("/g/t" . "/g/")))
         (box (list nil)))
    ;; Stub entry-get and entry-open to capture the template.
    (cl-letf (((symbol-function 'keemacs--entry-get) (lambda (_) entry))
              ((symbol-function 'keemacs--entry-open)
               (lambda (_name _action _path template)
                 (setcar box template))))
      (keemacs-edit "/g/t"))
    (should (string-match-p "^Group: /g/\n" (car box)))
    (should (string-match-p "\nTitle: t\n" (car box)))))

(ert-deftest keemacs-parse-entry-ignores-hint-line ()
  "The `;; Keys:' hint line is not parsed into an entry field."
  (with-temp-buffer
    (insert ";; Keys: C-c C-c commit | C-c C-p select group | C-c C-r regenerate\n"
            ";; C-c C-k cancel\n"
            "Group: /Work/\nTitle: t\nUserName: u\nPassword: p\n")
    (let ((entry (keemacs--parse-entry)))
      (should-not (assoc "Keys" entry))
      (should (equal "/Work/" (cdr (assoc "Group" entry))))
      (should (equal "t" (cdr (assoc "Title" entry))))
      (should (equal "u" (cdr (assoc "UserName" entry)))))))

(ert-deftest keemacs-spec-label ()
  "A database spec's label is its :name, defaulting to the file name."
  (should (equal "mydb" (keemacs--spec-label '(:name "mydb" :file "/p/db.kdbx"))))
  (should (equal "db.kdbx" (keemacs--spec-label '(:file "/path/db.kdbx")))))

(ert-deftest keemacs-select-database-by-label ()
  "Selecting a database completes over labels and sets a plist spec."
  (let* ((keemacs-databases
          '((:name "work" :file "/a/work.kdbx")
            (:file "/b/personal.kdbx")))
         (keemacs-database nil))
    (let ((result
           (cl-letf (((symbol-function 'completing-read)
                      (lambda (_prompt coll &rest _) (car coll))))
             (keemacs-select-database))))
      ;; The first label is "work"; the matching entry is the whole plist.
      (should (equal '(:name "work" :file "/a/work.kdbx") result))
      (should (equal result keemacs-database)))))

(ert-deftest keemacs-select-database-by-key ()
  "The by-key menu offers (KEY LABEL FILE) choices and stores the whole
plist spec of the picked database.  Every documented `:key' form is
honoured -- a character, a one-character string, a function -- and
keyless databases get an auto-assigned mnemonic key."
  (let ((keemacs-databases
         '((:name "work" :key "w" :file "/a/work.kdbx")
           (:file "/b/personal.kdbx")))
        (keemacs-database nil))
    ;; The string "w" is coerced to ?w; the keyless database is assigned
    ;; the first free character of its label, ?p.
    (let ((choices-box (list nil)))
      (cl-letf (((symbol-function 'read-multiple-choice)
                 (lambda (_prompt choices &rest _)
                   (setcar choices-box choices)
                   (assq ?p choices))))
        (should (equal '(:file "/b/personal.kdbx")
                       (keemacs-select-database-by-key))))
      (should (equal '((?w "work" "/a/work.kdbx")
                       (?p "personal.kdbx" "/b/personal.kdbx"))
                     (car choices-box)))
      (should (equal '(:file "/b/personal.kdbx") keemacs-database))))
  ;; Picking the explicitly keyed database stores that whole plist.
  (let ((keemacs-databases
         '((:name "work" :key ?w :file "/a/work.kdbx")
           (:file "/b/personal.kdbx")))
        (keemacs-database nil))
    (cl-letf (((symbol-function 'read-multiple-choice)
               (lambda (_prompt choices &rest _) (assq ?w choices))))
      (should (equal '(:name "work" :key ?w :file "/a/work.kdbx")
                     (keemacs-select-database-by-key))))
    (should (equal '(:name "work" :key ?w :file "/a/work.kdbx")
                   keemacs-database)))
  ;; A function :key is called and its result used.
  (let ((keemacs-databases
         '((:name "f" :key (lambda () ?f) :file "/f.kdbx")))
        (keemacs-database nil))
    (cl-letf (((symbol-function 'read-multiple-choice)
               (lambda (_prompt choices &rest _) (assq ?f choices))))
      (should (equal '(:name "f" :key (lambda () ?f) :file "/f.kdbx")
                     (keemacs-select-database-by-key))))
    (should (equal keemacs-databases (list keemacs-database)))))

(ert-deftest keemacs-ensure-database-prompt-frequency ()
  "`keemacs-always-select-database' decides whether an already active
database is kept (nil, the default) or the selector is asked again
(non-nil).  A single configured database is always picked silently."
  (let* ((dbs '((:name "work" :file "/a/work.kdbx")
                (:file "/b/personal.kdbx")))
         (keemacs-databases dbs)
         (keemacs-database (car dbs)))
    ;; Default: the active database is kept; nothing prompts.
    (let ((keemacs-always-select-database nil)
          (prompts 0))
      (cl-letf (((symbol-function 'keemacs-select-database)
                 (lambda () (setq prompts (1+ prompts)))))
        (should (equal (car dbs) (keemacs--ensure-database)))
        (should (= 0 prompts))))
    ;; Non-nil: the selector runs and its choice becomes the active db.
    (let ((keemacs-always-select-database t))
      (cl-letf (((symbol-function 'keemacs-select-database)
                 (lambda () (setq keemacs-database (cadr dbs)))))
        (should (equal (cadr dbs) (keemacs--ensure-database)))
        (should (equal (cadr dbs) keemacs-database)))))
  ;; One database: auto-selected even with always-select on.
  (let* ((keemacs-databases '((:name "solo" :file "/s.kdbx")))
         (keemacs-always-select-database t)
         (keemacs-database nil))
    (should (equal '(:name "solo" :file "/s.kdbx")
                   (keemacs--ensure-database)))))

(ert-deftest keemacs-favorites-by-key-selects-database-by-key ()
  "`favorites-by-key' picks its database with the by-key hotkey menu,
not `keemacs-select-database'."
  (let* ((keemacs-favorites-default '((:key ?p :title "Pika")))
         (keemacs-databases '((:name "work" :key "w" :file "/a/work.kdbx")
                              (:file "/b/personal.kdbx")))
         (keemacs-database nil)
         (by-key-calls 0)
         (label-calls 0)
         (acted (list nil))
         (entries '(("/Backups/Pika" . (("Group" . "/Backups/")
                                        ("Title" . "Pika"))))))
    (cl-letf (((symbol-function 'keemacs-select-database-by-key)
               (lambda ()
                 (setq by-key-calls (1+ by-key-calls))
                 (setq keemacs-database (car keemacs-databases))))
              ((symbol-function 'keemacs-select-database)
               (lambda () (setq label-calls (1+ label-calls))))
              ((symbol-function 'keemacs--load-entries) (lambda () entries))
              ((symbol-function 'read-multiple-choice)
               (lambda (_prompt choices &rest _) (car choices)))
              ((symbol-function 'embark-act) (lambda () (setcar acted t))))
      (keemacs-favorites-by-key))
    (should (= 1 by-key-calls))
    (should (= 0 label-calls))
    (should (car acted))))

(ert-deftest keemacs-entry-mode-map-bindings ()
  "The entry-mode keymap binds group-choosing to `C-c C-p'.
Not `C-c C-g': a C-g after a prefix key is treated by Emacs as \"cancel
the prefix\" and can never be dispatched to a binding."
  (should (eq #'keemacs--entry-choose-group
              (lookup-key keemacs-entry-mode-map (kbd "C-c C-p"))))
  (should (eq #'keemacs--entry-commit
              (lookup-key keemacs-entry-mode-map (kbd "C-c C-c"))))
  ;; Guard against reintroducing the C-g trap.
  (should-not (lookup-key keemacs-entry-mode-map (kbd "C-c C-g"))))

(ert-deftest keemacs-entry-open-unmodified ()
  "A freshly opened entry buffer is not marked modified until edited."
  (let ((buf (cl-letf (((symbol-function 'switch-to-buffer) #'ignore))
               (keemacs--entry-open "*kb-open-test*" "add" nil))))
    (should-not (buffer-modified-p buf))
    (with-current-buffer buf
      (insert "x"))
    (should (buffer-modified-p buf))
    (kill-buffer buf)))

(ert-deftest keemacs-view-shows-group ()
  "The view buffer shows a Group line (the entry's folder) above Title."
  (with-temp-buffer
    (setq-local keemacs-view-path "/Internet/Google/mail")
    (cl-letf (((symbol-function 'keemacs--entry-get)
               (lambda (_) '(("Title" . "mail") ("UserName" . "u")
                             ("Password" . "p") ("URL" . "x")
                             ("Notes" . "n")))))
      (keemacs-view-update nil))
    (goto-char (point-min))
    (should (string-match-p "^Group\\s-+/Internet/Google/\n" (buffer-string)))
    (should (string-match-p "^Group\\s-+.+\nTitle\\s-+mail\n" (buffer-string)))))

(ert-deftest keemacs-generate-args ()
  "The option labels expand into full keepassxc-cli commands.
The `:length' placeholder is replaced by the requested length as a string
(since `call-process' takes only strings)."
  (should (equal '("generate" "--upper" "--length" "16")
                 (keemacs--generate-args "upper case" 16)))
  (should (equal '("generate" "--lower" "--upper" "--numeric" "--length" "12")
                 (keemacs--generate-args "with numeric" 12)))
  ;; The all-printable set is the full !-~ range via --custom.
  (let ((ascii (keemacs--generate-args "all printable (!-~)" 8)))
    (should (equal '("generate" "--custom") (seq-take ascii 2)))
    (let* ((set (nth 2 ascii)))
      (should (= 94 (length set)))                 ; ! (0x21) .. ~ (0x7e)
      (should (string-match-p "!" set))
      (should (string-match-p "~" set))
      (should-not (string-match-p " " set))))      ; 0x20 is outside the range
  ;; The diceware option uses --words, not --length.
  (should (equal '("diceware" "--words" "6")
                 (keemacs--generate-args "passphrase" 6)))
  ;; A label not in the alist is an error.
  (should-error (keemacs--generate-args "bogus" 8)))

(ert-deftest keemacs-generate-failure-message ()
  "The keepassxc 'Invalid password generator' error advises a longer length."
  (let ((msg (keemacs--generate-failure-message
              "with special"
              "Invalid password generator after applying all options.")))
    (should (string-match-p "longer length" msg))
    (should (string-match-p "with special" msg)))
  ;; Other failures keep a generic message.
  (should (string-match-p "failed"
                          (keemacs--generate-failure-message
                           "with special" "some other error"))))

(ert-deftest keemacs-read-charset-remembers ()
  "`read-charset' offers the last choice as default, else the first option."
  (let ((keemacs--last-generated-charset "mixed case"))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_p _c _x _r _h _i def)
                 (unless (string= def "mixed case")
                   (ert-fail (format "expected default label, got %S" def)))
                 "upper case")))
      (should (equal "upper case" (keemacs--read-charset)))
      (should (equal "upper case"
                     keemacs--last-generated-charset))))
  ;; Nothing chosen yet -> the first entry's label is the default.
  (let ((keemacs--last-generated-charset nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_p _c _x _r _h _i def)
                 (unless (string= def "all printable (!-~)")
                   (ert-fail (format "expected default label, got %S" def)))
                 def)))
      (should (equal "all printable (!-~)"
                     (keemacs--read-charset))))))

(ert-deftest keemacs-parse-entry-multiline-notes ()
  "Notes extends to the end of the buffer, preserving multiple lines."
  (with-temp-buffer
    (insert "Title: t\nUserName: u\nPassword: p\nURL: http://x\n"
            "Notes: first line\nsecond line\n\nthird line\n")
    (let ((entry (keemacs--parse-entry)))
      (should (equal "first line\nsecond line\n\nthird line"
                     (cdr (assoc "Notes" entry))))
      ;; Other fields still parsed correctly.
      (should (equal "t" (cdr (assoc "Title" entry))))
      (should (equal "u" (cdr (assoc "UserName" entry))))
      (should (equal "p" (cdr (assoc "Password" entry)))))))

(ert-deftest keemacs-format-candidate-tags-path ()
  "`format-candidate' tags the line with the entry path."
  (let* ((path "/g/t")
         (entry '(("Title" . "t") ("UserName" . "u") ("URL" . "http://x")))
         (keemacs-fields '("Title" "UserName" "URL"))
         (cand (keemacs--format-candidate path entry)))
    (should (equal path (keemacs--path-of cand)))))

(ert-deftest keemacs-format-candidate-icon-prefix ()
  "`format-candidate' prefixes the glyph for the entry's IconID."
  (let* ((keemacs-fields '("Title"))
         (cand (keemacs--format-candidate
                "/mail" '(("IconID" . "19") ("Title" . "mail")))))
    ;; 19 is the envelope.
    (should (string-prefix-p "✉️" cand))
    (should (string-match-p "mail" cand))
    ;; The kb-path tag still covers the glyph, so Embark/vertico resolve it.
    (should (equal "/mail" (keemacs--path-of cand))))
  ;; Out-of-range and missing ids get no glyph.
  (should (string-prefix-p "T"
                           (keemacs--format-candidate
                            "/x" '(("IconID" . "999") ("Title" . "T")))))
  (should (string-prefix-p "T"
                           (keemacs--format-candidate "/x" '(("Title" . "T"))))))

(ert-deftest keemacs-custom-icons-parse-and-prefix ()
  "Custom icon blobs and entry references come out of the export XML."
  (skip-unless (fboundp 'libxml-parse-xml-region))
  (let* ((xml "<KeePassFile><Meta><CustomIcons><Icon>\
<UUID>abc+/==</UUID><Data>iVBORw0KGgo=</Data></Icon></CustomIcons></Meta>\
<Root><Group><Entry><UUID>u1</UUID><IconID>0</IconID>\
<CustomIconUUID>abc+/==</CustomIconUUID>\
<String><Key>Title</Key><Value>mail</Value></String></Entry>\
<Entry><UUID>u2</UUID><IconID>19</IconID>\
<String><Key>Title</Key><Value>plain</Value></String></Entry>\
</Group></Root></KeePassFile>")
         (tree (with-temp-buffer
                 (insert xml)
                 (libxml-parse-xml-region (point-min) (point-max))))
         (keemacs--custom-icons nil)
         (keemacs--entry-custom-icons nil)
         (entries (progn
                    (setq keemacs--custom-icons
                          (keemacs--custom-icons-from tree))
                    (keemacs--collect
                     (car (keemacs--xml-children-tag
                           (car (keemacs--xml-children-tag tree 'Root))
                           'Group))
                     ""))))
    ;; The blob is decoded (base64 of a PNG header).
    (should (equal "abc+/==" (caar keemacs--custom-icons)))
    (should (equal "\211PNG" (substring (cdar keemacs--custom-icons) 0 4)))
    ;; The entry referencing the icon is mapped by path.
    (should (equal '(("/mail" . "abc+/=="))
                   keemacs--entry-custom-icons))
    ;; Candidates still tag paths, and the plain entry keeps its glyph.
    (let ((keemacs-fields '("Title")))
      (should (equal "/mail"
                     (keemacs--path-of
                      (keemacs--format-candidate
                       "/mail" (cdr (assoc "/mail" entries))))))
      (should (string-prefix-p "✉️"
                               (keemacs--format-candidate
                                "/plain" (cdr (assoc "/plain" entries)))))))
  ;; The custom icon becomes a real image on a graphic display, shown as a
  ;; propertized space prefix; unknown UUIDs fall back to the glyph.
  (let ((keemacs--custom-icons '(("abc+/==" . "\211PNGxxxx")))
        (keemacs--entry-custom-icons '(("/mail" . "abc+/==")))
        (keemacs-fields '("Title")))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda () t)))
      (let ((cand (keemacs--format-candidate
                   "/mail" '(("IconID" . "0") ("Title" . "mail")))))
        (should (equal ?\s (aref cand 0)))
        (should (eq 'image (car-safe (get-text-property 0 'display cand))))))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda () nil)))
      (should (string-prefix-p "🔑"
                               (keemacs--format-candidate
                                "/mail" '(("IconID" . "0") ("Title" . "mail"))))))))

(ert-deftest keemacs-valid-field-p ()
  "Only the five standard fields are recognised."
  (should (keemacs--valid-field-p "Title"))
  (should (keemacs--valid-field-p "Notes"))
  (should-not (keemacs--valid-field-p "Uuid"))
  (should-not (keemacs--valid-field-p "host")))

;;;; Group navigation helpers

(ert-deftest keemacs-entry-directory-basename ()
  "`entry-directory'/'entry-basename' split KeePass paths without TRAMP.
A title like \"Apple:foo:bar\" gives path \"/Apple:foo:bar\", which any
`file-name-*' function would hand to TRAMP (\"Method `Apple' is not
known\").  The pure string helpers must never touch those."
  (should (equal "/" (keemacs--entry-directory "/b")))
  (should (equal "/a/" (keemacs--entry-directory "/a/b")))
  (should (equal "/" (keemacs--entry-directory "/")))
  (should (equal "" (keemacs--entry-directory "")))
  (should (equal "/" (keemacs--entry-directory "/Apple:foo:bar")))
  (should (equal "Apple:foo:bar" (keemacs--entry-basename "/Apple:foo:bar")))
  (should (equal "b" (keemacs--entry-basename "/a/b")))
  (should (equal "" (keemacs--entry-basename "/"))))

(ert-deftest keemacs-group-contents-tramp-safe ()
  "`group-contents' handles entry titles that look like TRAMP remote names."
  (let ((keemacs--group-icons nil) ; hermetic: no tree groups
        (keemacs--entry-parents nil) ; and no recorded parents
        (entries '(("/Apple:foo:bar" . nil)
                   ("/Internet/Apple:foo:bar" . nil)
                   ("/Work/g" . nil))))
    (pcase-let* ((`(,groups . ,subs)
                  (keemacs--group-contents entries "/")))
      ;; '/Apple:foo:bar' is a direct root entry, not a spurious subgroup.
      (should (equal '("/Internet/" "/Work/") groups))
      (should (equal '("/Apple:foo:bar") (mapcar #'car subs))))))

(ert-deftest keemacs-group-contents ()
  "`group-contents' splits entries into child groups and child entries."
  (let ((keemacs--group-icons nil) ; hermetic: no tree groups
        (keemacs--entry-parents nil) ; and no recorded parents
        (entries '(("/Internet/Google/a" . nil)
                   ("/Internet/Google/b" . nil)
                   ("/Internet/Yahoo/c" . nil)
                   ("/Work/g" . nil)
                   ("/Root" . nil))))
    ;; Root: subgroups /Internet/ + /Work/, direct entry /Root.
    (pcase-let* ((`(,groups . ,subs)
                  (keemacs--group-contents entries "/")))
      (should (equal '("/Internet/" "/Work/") groups))
      (should (equal '("/Root") (mapcar #'car subs))))
    ;; /Internet: subgroups /Internet/Google/ + /Internet/Yahoo/, no direct
    ;; entries.
    (pcase-let* ((`(,groups . ,subs)
                  (keemacs--group-contents entries "/Internet/")))
      (should (equal '("/Internet/Google/" "/Internet/Yahoo/") groups))
      (should (null subs)))
    ;; Deepest group: two direct entries, no subgroups; group name without a
    ;; trailing slash is normalized the same way.
    (pcase-let* ((`(,groups . ,subs)
                  (keemacs--group-contents entries "/Internet/Google")))
      (should (null groups))
      (should (equal '("/Internet/Google/a" "/Internet/Google/b")
                     (mapcar #'car subs))))))

(ert-deftest keemacs-group-choose-drills-down ()
  "`group-choose' descends through subgroups to reach an entry, recursing."
  (let* ((entries '(("/Internet/Google/a" . (("Title" . "a")))))
         (queue (list (keemacs--format-group "/Internet/")
                      (keemacs--format-candidate
                       "/Internet/Google/a" (cdar entries)))))
    (cl-letf (((symbol-function 'consult--read)
               (lambda (&rest _) (pop queue))))
      (should (equal "/Internet/Google/a"
                     (keemacs--group-choose entries "/"))))
    (should (null queue))))

(ert-deftest keemacs-group-command-picks-entry ()
  "`keemacs-group' drills down and runs the default action on the entry."
  (let* ((entries '(("/A/b" . nil)))
         (queue (list (keemacs--format-group "/A/")
                      (keemacs--format-candidate "/A/b" nil)))
         (keemacs-database (keemacs-auth-make-db-spec :file "db.kdbx"))
         (box (list nil))
         (keemacs-default-action (lambda (p) (setcar box p))))
    (cl-letf (((symbol-function 'keemacs--load-entries)
               (lambda () entries))
              ((symbol-function 'consult--read)
               (lambda (&rest _) (pop queue))))
      (should (equal "/A/b" (keemacs-group))))
    (should (equal "/A/b" (car box)))))

(ert-deftest keemacs-format-group-tags-path ()
  "`format-group' prefixes the icon glyph and tags the full path.
With no icon info recorded, a group shows the keepassxc default (48, a
folder)."
  (let ((cand (keemacs--format-group "/Internet/")))
    (should (string-prefix-p "📁 Internet/" cand))
    (should (equal "/Internet/" (keemacs--path-of cand))))
  ;; A nested group keeps its full path in the tag.
  (should (equal "/Internet/Google/"
                 (keemacs--path-of
                  (keemacs--format-group "/Internet/Google/")))))

(ert-deftest keemacs-group-icons-from-export ()
  "Group paths and icons are recorded from the export tree.
The root group's own name names no path; the Recycle Bin subtree is
skipped."
  (skip-unless (fboundp 'libxml-parse-xml-region))
  (let ((xml "<KeePassFile><Root><Group><Name>Passwords</Name>\
<Group><Name>Internet</Name><IconID>1</IconID>\
<CustomIconUUID>cu-uuid</CustomIconUUID>\
<Group><Name>Empty</Name><IconID>48</IconID></Group></Group>\
<Group><Name>Recycle Bin</Name><Group><Name>x</Name></Group></Group>\
</Group></Root></KeePassFile>")
        (keemacs--group-icons nil))
    (with-temp-buffer
      (insert xml)
      ;; Like `keemacs--load-entries': start below the root group,
      ;; whose own name names no path.
      (keemacs--collect-groups
       (car (keemacs--xml-children-tag
             (car (keemacs--xml-children-tag
                   (libxml-parse-xml-region (point-min) (point-max)) 'Root))
             'Group))
       "")
      ;; Mirror the Recycle Bin exclusion `keemacs--load-entries`
      ;; applies after collecting.
      (setq keemacs--group-icons
            (seq-filter (lambda (g)
                          (not (string-prefix-p "/Recycle Bin" (car g))))
                        keemacs--group-icons)))
    ;; The root group's name is not recorded...
    (should-not (assoc "/Passwords" keemacs--group-icons))
    ;; ...but nested groups are, with both standard and custom icons.
    (should (equal '("1" . "cu-uuid")
                   (cdr (assoc "/Internet" keemacs--group-icons))))
    (should (equal '("48" . nil)
                   (cdr (assoc "/Internet/Empty" keemacs--group-icons))))
    ;; The Recycle Bin subtree is not recorded.
    (should-not (assoc "/Recycle Bin" keemacs--group-icons))
    (should-not (assoc "/Recycle Bin/x" keemacs--group-icons))
    ;; Glyphs come from the recorded ids; the custom icon falls back to the
    ;; glyph on a non-graphic display.
    (should (equal "🌍" (keemacs--group-prefix "/Internet/")))
    (should (equal "📁" (keemacs--group-prefix "/Internet/Empty/")))))

(ert-deftest keemacs-group-contents-parents ()
  "Entries are classified by their recorded parent group, so a title
containing \"/\" cannot carve itself into phantom subgroups."
  (let* ((slashy "/Backups/Odd / Title – with /slashes")
         (keemacs--group-icons '(("/Backups" . ("48" . nil))))
         (entries `((,slashy . (("Group" . "/Backups/")))
                    ("/Backups/Normal" . (("Group" . "/Backups/"))))))
    ;; Root: /Backups is a real subgroup; the slashy title does not leak a
    ;; phantom "Odd / Title –" segment.
    (pcase-let* ((`(,groups . ,subs)
                  (keemacs--group-contents entries "/")))
      (should (equal '("/Backups/") groups))
      (should (null subs)))
    ;; /Backups: the slashy entry is a direct child, with its full title.
    (pcase-let* ((`(,groups . ,subs)
                  (keemacs--group-contents entries "/Backups/")))
      (should (null groups))
      (should (equal (sort (mapcar #'car entries) #'string<)
                     (mapcar #'car subs))))))

(ert-deftest keemacs-edit-slashy-title-templates-real-values ()
  "The edit screen templates the real Title and Group for a slashy title.
Deriving them from the path would truncate the title (renaming the entry
on commit) and invent a phantom group."
  (let* ((slashy "/Backups/Odd / Title – with /slashes")
         (keemacs--entry-parents `((,slashy . "/Backups/")))
         (box (list nil)))
    (cl-letf (((symbol-function 'keemacs--entry-get)
               (lambda (_) '(("Title" . "Odd / Title – with /slashes")
                             ("UserName" . "chris"))))
              ((symbol-function 'keemacs--entry-open)
               (lambda (_name _action _path template)
                 (setcar box template))))
      (keemacs-edit slashy))
    (should (string-match-p "^Group: /Backups/\n" (car box)))
    (should (string-match-p
             "\nTitle: Odd / Title – with /slashes\n" (car box)))))

(ert-deftest keemacs-command-map-bindings ()
  "The command keymap binds every command under C-:."
  (dolist (bind '(("t" keemacs)
                  ("g" keemacs-group)
                  ("d" keemacs-select-database)
                  ("f" keemacs-favorites)
                  ("k" keemacs-favorites-by-key)
                  ("K" keemacs-select-database-by-key)
                  ("c" keemacs-auth-forget-cached)
                  ("a" keemacs-add)))
    (let ((resolved (lookup-key keemacs-command-map (kbd (car bind)))))
      (should (eq resolved (cadr bind))))))

(ert-deftest keemacs-command-map-group-keys ()
  "The command keymap binds the group-maintenance commands."
  (should (eq (lookup-key keemacs-command-map (kbd "A"))
              #'keemacs-add-group))
  (should (eq (lookup-key keemacs-command-map (kbd "D"))
              #'keemacs-delete-group)))

(ert-deftest keemacs-add-group-name-validation ()
  "add-group rejects empty names and names containing a slash."
  (let ((db (keemacs-test-make-db))
        (keemacs-database nil)
        (password-cache-expiry nil))
    (unwind-protect
        (progn
          (setq keemacs-database (keemacs-auth-make-db-spec :file db))
          (password-cache-add db "PASS")
          ;; The name is prompted; "/" in it is rejected before any run.
          (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "bad/name")))
            (should-error (keemacs-add-group "/Work/")
                          :type 'user-error))
          (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "")))
            (should-error (keemacs-add-group "/Work/")
                          :type 'user-error))
          ;; Nothing was created by the failed attempts.
          (let ((paths (keemacs--group-paths)))
            (should-not (member "/Work/bad" paths))
            (should-not (member "/Work/bad/name" paths))))
      (delete-file db))))

(ert-deftest keemacs-format-candidate-title-width ()
  "The Title column uses `keemacs-title-width'; other columns use
`keemacs-field-width'."
  (let ((keemacs-fields '("Title" "UserName"))
        (keemacs-title-width 34)
        (keemacs-field-width 24))
    (let* ((cand (keemacs--format-candidate
                  "/m" '(("Title" . "abc") ("UserName" . "u"))))
           (cols (split-string cand "\t")))
      ;; Title padded to 34, UserName to 24.
      (should (= 34 (string-width (car cols))))
      (should (= 24 (string-width (cadr cols)))))))

(ert-deftest keemacs-prompt-shows-db-name ()
  "Prompts are tagged with the database name -- but only when more
than one database is configured, matching the view screen."
  ;; Two databases: tagged with the active one's name.
  (let ((keemacs-databases '((:name "mydb" :file "/x.kdbx")
                             (:name "work" :file "/y.kdbx")))
        (keemacs-database '(:name "work" :file "/y.kdbx")))
    (should (equal "KeePass entry (work): "
                   (keemacs--prompt "KeePass entry: "))))
  ;; One database: no tag.
  (let ((keemacs-databases '((:name "mydb" :file "/x.kdbx")))
        (keemacs-database '(:name "mydb" :file "/x.kdbx")))
    (should (equal "KeePass entry: " (keemacs--prompt "KeePass entry: "))))
  ;; None selected: no tag.
  (let ((keemacs-databases '((:name "mydb" :file "/x.kdbx")))
        (keemacs-database nil))
    (should (equal "KeePass entry: " (keemacs--prompt "KeePass entry: ")))))

(ert-deftest keemacs-embark-title-shows-db-name ()
  "The embark menu title gains a (dbname) prefix -- only with multiple
databases -- and the target string actions receive is untouched."
  (let ((keemacs-databases '((:name "mydb" :file "/x.kdbx")
                             (:name "work" :file "/y.kdbx")))
        (keemacs-database '(:name "work" :file "/y.kdbx")))
    (should (equal "Act on keemacs-select (work) ‘/Mail/gmail’"
                   (keemacs--embark-format-targets
                    (lambda (&rest _) "Act on keemacs-select ‘/Mail/gmail’")
                    (list :type 'keemacs-select :target "/Mail/gmail")))))
  (let ((keemacs-databases '((:name "mydb" :file "/x.kdbx")))
        (keemacs-database '(:name "mydb" :file "/x.kdbx")))
    (should (equal "Act on keemacs-select ‘/Mail/gmail’"
                   (keemacs--embark-format-targets
                    (lambda (&rest _) "Act on keemacs-select ‘/Mail/gmail’")
                    (list :type 'keemacs-select :target "/Mail/gmail"))))))

(ert-deftest keemacs-choose-group-offers-root ()
  "The group chooser offers the root / and defaults to it."
  (cl-letf (((symbol-function 'keemacs--group-paths)
             (lambda () '("/Bank/" "/Work/")))
            ((symbol-function 'completing-read)
             (lambda (_prompt coll &rest _)
               (should (member "/" coll))
               (car coll))))  ; RET: the default, root
    (should (equal "/" (keemacs--choose-group)))))

(ert-deftest keemacs-favorites-parse ()
  "`favorites--parse' keeps usable items and drops broken ones with a
message, never an error.  Keyless items are kept -- the keyed menu
assigns their keys later."
  (should (equal '((?b "Pika" "^/Backups/")
                   (?m nil "str-key")
                   (nil "keyless" nil)
                   (nil nil "Pika"))
                 (keemacs-favorites--parse
                  '((:key ?b :group "^/Backups/" :title "Pika")
                    (:key "m" :group "str-key")
                    (:title "keyless")
                    (:group "Pika")
                    (:key ?c)
                    (:key ?b :title "dup")
                    (:key ?x :title 42 :group "^/G")
                    (:key ?z :group "^/G" :title 42))))))

(ert-deftest keemacs-favorites-key-for ()
  "`key-for' prefers a mnemonic from the label, falling back to the pool."
  ;; Phase 1: the first unused character of the label itself.
  (should (equal ?g (keemacs-favorites--key-for "github" nil)))
  (should (equal ?i (keemacs-favorites--key-for "github" '(?g))))
  ;; Phase 1 skips non-regular characters: punctuation is never picked.
  (should (equal ?B (keemacs-favorites--key-for "@Bank/" nil)))
  (should (equal ?M (keemacs-favorites--key-for "\302\253Mail\302\273" nil)))
  ;; Uppercase is part of the regular set.
  (should (equal ?W (keemacs-favorites--key-for "Wise" '(?b))))
  ;; Phase 2: every regular label character taken -> first unused pool char.
  (should (equal ?1 (keemacs-favorites--key-for "github" '(?g ?i ?t ?h ?u ?b))))
  ;; The pool runs through digits, a-z, then A-Z.
  (should (equal ?A (keemacs-favorites--key-for ""
                     (string-to-list "123456789abcdefghijklmnopqrstuvwxyz"))))
  ;; Only ?A taken: digits and a-z are all still free, so ?1.
  (should (equal ?1 (keemacs-favorites--key-for "" '(?A))))
  ;; Digits+a-z taken: the pool reaches ?A.
  (should (equal ?A (keemacs-favorites--key-for ""
                     (string-to-list "123456789abcdefghijklmnopqrstuvwxyz"))))
  ;; Digits+a-z+A taken: next is ?B.
  (should (equal ?B (keemacs-favorites--key-for ""
                     (append (string-to-list "123456789abcdefghijklmnopqrstuvwxyz")
                             '(?A)))))
  ;; Nothing taken, no label: the pool starts at ?1.
  (should (equal ?1 (keemacs-favorites--key-for "" nil)))
  ;; Everything taken -> user-error.
  (should-error (keemacs-favorites--key-for ""
                 (append (string-to-list "123456789abcdefghijklmnopqrstuvwxyz")
                         (string-to-list "ABCDEFGHIJKLMNOPQRSTUVWXYZ")))
                :type 'user-error))

(ert-deftest keemacs-favorites-assign-keys ()
  "`assign-keys' gives keyless items mnemonic keys, pool fallback."
  (should (equal '((?a "a" "/X") (?b "b" "/Y") (?m "m" "/Z"))
                 (keemacs-favorites--assign-keys
                  '((nil "a" "/X") (nil "b" "/Y") (?m "m" "/Z")))))
  ;; Explicit keys are honoured; keyless items avoid them (phase 1 fails
  ;; on "a" since ?a is taken, so the pool supplies ?1).
  (should (equal '((?a "a") (?1 "a"))
                 (keemacs-favorites--assign-keys
                  '((?a "a") (nil "a"))))))

(ert-deftest keemacs-favorites-match ()
  "`favorites--match' ANDs the given regexps, matches the group with
its trailing slash, and deduplicates on (group . title)."
  (let* ((entries '(("/Mail/gmail" . (("Group" . "/Mail/")
                                      ("Title" . "gmail")))
                    ("/Backups/Pika" . (("Group" . "/Backups/")
                                        ("Title" . "Pika")))
                    ("/Backups/Pika-old" . (("Group" . "/Backups/")
                                            ("Title" . "Pika-old")))))
         (spec '((?m nil "^/Mail/")
                 (?p "Pika" nil)
                 (?a "Pika" "^/Backups/")))
         (matched (keemacs-favorites--match spec entries)))
    ;; ?p and ?a both match /Backups/Pika: offered once (3 results total).
    (should (= 3 (length matched)))
    (should (equal '("/Backups/Pika" "/Backups/Pika-old" "/Mail/gmail")
                   (sort (mapcar #'car matched) #'string<)))
    ;; AND semantics: "gmail" in the /Backups group matches nothing.
    (should (null (keemacs-favorites--match
                   '((?x "gmail" "^/Backups/")) entries)))
    ;; Anchors: an exact group path matches only that group.
    (should (equal '("/Mail/gmail")
                   (mapcar #'car (keemacs-favorites--match
                                  '((?x nil "\\`/Mail/\\'")) entries))))))

(ert-deftest keemacs-favorites-choice ()
  "`favorites--choice' prefers the title for the name, group as
description; group alone is the name."
  (should (equal '(?b "Pika" "/Backups/")
                 (keemacs-favorites--choice '(?b "Pika" "/Backups/"))))
  (should (equal '(?s "Secret")
                 (keemacs-favorites--choice '(?s nil "Secret")))))

(ert-deftest keemacs-favorites-selects-matches ()
  "`keemacs-favorites' offers exactly the matching entries."
  (keemacs-test-with-db
    (let* ((keemacs-favorites-default
            '((:key ?g :title "github") (:key ?e :title "email")))
           (keemacs--selecting t)
           (entry-box (list nil))
           (keemacs-default-action
            (lambda (p) (setcar entry-box p)))
           (path (cl-letf (((symbol-function 'consult--read)
                            (lambda (candidates &rest _)
                              (should (= 2 (length candidates)))
                              (car candidates))))
               (keemacs-favorites))))
      (should (member path '("/email" "/Work/github")))
      (should (equal path (car entry-box)))
      (let ((keemacs-favorites-default '((:key ?x))))
        (should-error (keemacs-favorites) :type 'user-error)))))

(ert-deftest keemacs-favorites-by-key-flows ()
  "`favorites-by-key': one match goes to the embark action menu; several
matches go to a keyed menu of the matches first, then the picked entry
goes to the same action menu."
  (keemacs-test-with-db
    (let* ((menu-box (list nil))
           (target-box (list nil))
           (entry-box (list nil))
           (keemacs-default-action (lambda (p) (setcar entry-box p))))
      ;; Single match: straight to the embark action menu on the entry.
      (let ((keemacs-favorites-default '((:key ?g :title "github"))))
        (cl-letf (((symbol-function 'read-multiple-choice)
                   (lambda (_prompt choices &optional _help _show)
                     (assq ?g choices)))
                  ((symbol-function 'embark-act)
                   (lambda ()
                     (setcar menu-box t)
                     (setcar target-box
                             (funcall (car embark-target-finders))))))
          (keemacs-favorites-by-key))
        (should (car menu-box))
        (should (equal target-box
                       (list '(keemacs-select . "/Work/github")))))
      ;; Several matches: favorites menu, then keyed menu of the matches,
      ;; then the action menu on the picked entry.
      (let ((keemacs-favorites-default '((:key ?a :title "."))))
        (cl-letf (((symbol-function 'read-multiple-choice)
                   (lambda (_prompt choices &optional _help _show)
                     (car choices)))
                  ((symbol-function 'embark-act)
                   (lambda ()
                     (setcar target-box
                             (funcall (car embark-target-finders))))))
          (keemacs-favorites-by-key))
        ;; The entries menu's first choice is the github entry; the picked
        ;; entry goes to the action menu, not the default action.
        (should (equal target-box
                       (list '(keemacs-select . "/Work/github")))))
      (should (null (car entry-box))))))

(ert-deftest keemacs-group-contents-includes-empty ()
  "`group-contents' lists empty groups recorded from the export tree."
  (let* ((keemacs--group-icons
          '(("/A" . ("48" . nil)) ("/A/Empty" . ("48" . nil))))
         (entries '(("/A/x" . nil))))
    ;; Root: /A is a subgroup; the only entry lives under /A, not at root.
    (pcase-let* ((`(,groups . ,subs)
                  (keemacs--group-contents entries "/")))
      (should (equal '("/A/") groups))
      (should (null subs)))
    ;; /A: the empty subgroup appears even though no entry lives there.
    (pcase-let* ((`(,groups . ,subs)
                  (keemacs--group-contents entries "/A/")))
      (should (equal '("/A/Empty/") groups))
      (should (equal '("/A/x") (mapcar #'car subs))))))

(ert-deftest keemacs-group-custom-icon-thumbnail ()
  "A group with a custom icon shows an image prefix on a graphic display."
  (let ((keemacs--custom-icons '(("cu" . "\211PNGxx")))
        (keemacs--group-icons '(("/G" . ("48" . "cu")))))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda () t)))
      (let ((cand (keemacs--format-group "/G/")))
        (should (equal ?\s (aref cand 0)))
        (should (eq 'image (car-safe (get-text-property 0 'display cand))))
        (should (equal "/G/" (keemacs--path-of cand)))))
    ;; Non-graphic: the standard-icon glyph.
    (cl-letf (((symbol-function 'display-graphic-p) (lambda () nil)))
      (should (string-prefix-p "📁"
                               (keemacs--format-group "/G/"))))))

;;;; Integration tests (need keepassxc-cli)

(ert-deftest keemacs-list-paths ()
  "Entry and group paths are listed with normalized leading slashes."
  (keemacs-test-with-db
    (let ((paths (keemacs--entry-paths))
          (groups (keemacs--group-paths)))
      (should (member "/email" paths))
      (should (member "/Work/github" paths))
      (should (member "/Work/" groups)))))

(ert-deftest keemacs-group-contents-real ()
  "`group-contents' on a real database walks the group tree."
  (keemacs-test-with-db
    (let ((entries (keemacs--load-entries)))
      ;; Root: the /Work group plus the top-level /email entry.
      (pcase-let* ((`(,groups . ,subs)
                    (keemacs--group-contents entries "/")))
        (should (equal '("/Work/") groups))
        (should (equal '("/email") (mapcar #'car subs))))
      ;; /Work: only the nested github entry.
      (pcase-let* ((`(,groups . ,subs)
                    (keemacs--group-contents entries "/Work/")))
        (should (null groups))
        (should (equal '("/Work/github") (mapcar #'car subs)))))))

(ert-deftest keemacs-entry-get-fields ()
  "`entry-get' returns the real fields for an entry."
  (keemacs-test-with-db
    (let ((entry (keemacs--entry-get "/email")))
      (should (equal "me@x.com" (cdr (assoc "UserName" entry))))
      (should (equal "smtp.x.com:465" (cdr (assoc "URL" entry)))))))

(ert-deftest keemacs-copy-password-to-kill-ring ()
  "Copying puts the real password on the kill ring."
  (keemacs-test-with-db
    (keemacs-copy-password "/email")
    (let ((entry (keemacs--entry-get "/email")))
      (should (equal (cdr (assoc "Password" entry)) (car kill-ring))))))

(ert-deftest keemacs-candidates-tagged ()
  "Candidates carry kb-path so Embark can act on the entry under point."
  (keemacs-test-with-db
    (let* ((keemacs-fields '("Title" "UserName" "URL"))
           (cands (keemacs--candidates)))
      (should (= 2 (length cands)))
      (dolist (c cands)
        (should (keemacs--path-of c))))))

(ert-deftest keemacs-entry-commit-adds ()
  "Committing an add buffer creates the entry in the database."
  (keemacs-test-with-db
    (with-temp-buffer
      (insert "Title: /Work/new2\nUserName: carol\nPassword: pw2\nURL: http://x\n")
      (keemacs-entry-mode)
      (setq-local keemacs--entry-action "add")
      (setq-local keemacs--entry-original nil)
      (keemacs--entry-commit))
    (let ((entry (keemacs--entry-get "/Work/new2")))
      (should (equal "carol" (cdr (assoc "UserName" entry)))))))

(ert-deftest keemacs-entry-commit-edits ()
  "Committing an edit buffer updates the entry's fields."
  (keemacs-test-with-db
    (with-temp-buffer
      (insert "Title: email\nUserName: newuser\nPassword: newpw\nURL: smtp.x.com:465\n")
      (keemacs-entry-mode)
      (setq-local keemacs--entry-action "edit")
      (setq-local keemacs--entry-original "/email")
      (keemacs--entry-commit))
    (let ((entry (keemacs--entry-get "/email")))
      (should (equal "newuser" (cdr (assoc "UserName" entry))))
      (should (equal "newpw" (cdr (assoc "Password" entry)))))))

(ert-deftest keemacs-entry-commit-rename-keeps-one ()
  "Renaming via edit -t updates the title in place, keeping one entry."
  (keemacs-test-with-db
    (with-temp-buffer
      (insert "Title: github-new\nUserName: cbit\nPassword: x\nURL: https://github.com\n")
      (keemacs-entry-mode)
      (setq-local keemacs--entry-action "edit")
      (setq-local keemacs--entry-original "/Work/github")
      (keemacs--entry-commit))
    (let ((paths (keemacs--entry-paths)))
      (should (member "/Work/github-new" paths))
      (should-not (member "/Work/github" paths)))))

(ert-deftest keemacs-entry-commit-moves-group ()
  "Changing the Group field moves the entry into another group (mv).
The move must not delete+re-add: the entry survives in its new group and
the old path is gone."
  (keemacs-test-with-db
    (with-temp-buffer
      (insert "Group: /Work/\nTitle: email\nUserName: me@x.com\nPassword: PASS\nURL: smtp.x.com:465\n")
      (keemacs-entry-mode)
      (setq-local keemacs--entry-action "edit")
      (setq-local keemacs--entry-original "/email")
      (keemacs--entry-commit))
    (let ((paths (keemacs--entry-paths)))
      (should (member "/Work/email" paths))
      (should-not (member "/email" paths)))
    ;; The entry's fields survive the move.
    (let ((entry (keemacs--entry-get "/Work/email")))
      (should (equal "me@x.com" (cdr (assoc "UserName" entry)))))))

(ert-deftest keemacs-delete-removes ()
  "Deleting an entry removes it from the list."
  (keemacs-test-with-db
    (keemacs--delete-entry "/email")
    (let ((paths (keemacs--entry-paths)))
      (should-not (member "/email" paths)))))

(ert-deftest keemacs-wrong-password-errors ()
  "A wrong master password raises an error, not a silent empty result."
  (keemacs-test-with-db
    ;; The password cache is keyed by the database *path*, so override the
    ;; good entry with a wrong password under that key.
    (let ((password-cache-expiry nil))
      (password-cache-add (keemacs--database-path) "WRONG"))
    (should-error (keemacs--load-entries) :type 'error)))

(ert-deftest keemacs-copy-totp-absent ()
  "Copying TOTP for an entry without one is a clean user-error, not a crash."
  (keemacs-test-with-db
    (should-error (keemacs-copy-totp "/email") :type 'user-error)))

(provide 'keemacs-test)
;;; keemacs-test.el ends here