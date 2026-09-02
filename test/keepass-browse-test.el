;;; keepass-browse-test.el --- Tests for keepass-browse -*- lexical-binding: t -*-

;;; Commentary:

;; Run with:
;;   emacs -Q --batch -l ert -l keepass-browse.el \
;;         -l keepass-browse-test.el -f ert-run-tests-batch-and-exit
;;
;; Integration tests build a throwaway KeePassXC database via
;; `keepassxc-cli' and are skipped when the binary is absent.

;;; Code:
(require 'ert)
(require 'keepass-browse)

(defcustom keepass-browse-test-program
  (or (executable-find "keepassxc-cli") "")
  "keepassxc-cli executable for the optional integration test."
  :type 'string
  :group 'keepass-browse)

(defun keepass-browse-test-run (cmd)
  "Run shell command CMD via /bin/sh, returning its stdout."
  (with-temp-buffer
    (call-process "sh" nil t nil "-c" cmd)
    (buffer-string)))

(defun keepass-browse-test-add (cli db title user url)
  "Add entry TITLE/USER/URL to DB via keepassxc-cli CLI."
  (keepass-browse-test-run
   (format "printf 'PASS\\n' | %s add -q %s %s -u %s -g --url %s"
           cli (shell-quote-argument db) (shell-quote-argument title)
           (shell-quote-argument user) (shell-quote-argument url))))

(defun keepass-browse-test-make-db ()
  "Create a fresh throwaway kdbx with a root entry and a group entry."
  (let* ((dir (make-temp-file "kb-test-" t))
         (db (expand-file-name "t.kdbx" dir))
         (cli (shell-quote-argument keepass-browse-test-program))
         (qdb (shell-quote-argument db)))
    (keepass-browse-test-run
     (format "printf 'PASS\\nPASS\\n' | %s db-create -q %s --set-password" cli qdb))
    (keepass-browse-test-run
     (format "printf 'PASS\\n' | %s mkdir -q %s Work" cli qdb))
    (keepass-browse-test-add cli qdb "email" "me@x.com" "smtp.x.com:465")
    (keepass-browse-test-add cli qdb "Work/github" "cbit" "https://github.com")
    db))

(defmacro keepass-browse-test-with-db (&rest body)
  "Bind a fresh test DB and run BODY with it set as the active database."
  `(when keepass-browse-test-program
     (let* ((db (keepass-browse-test-make-db))
            (keepass-browse-database (keepass-make-db-spec :file db))
            (keepass-browse-cache-expiry nil)
            (password-cache-expiry nil))
       (password-cache-add db "PASS")
       (unwind-protect
           (progn ,@body)
         (delete-file db)))))

;;;; Non-subprocess unit tests

(ert-deftest keepass-browse-parse-show ()
  "`parse-show' picks out the standard fields."
  (let ((entry (keepass-browse--parse-show
                "Title: t\nUserName: u@x.com\nPassword: secret\nURL: http://x\nNotes: n\nUuid: {x}\n")))
    (should (equal "t" (cdr (assoc "Title" entry))))
    (should (equal "u@x.com" (cdr (assoc "UserName" entry))))
    (should (equal "secret" (cdr (assoc "Password" entry))))
    (should (equal "http://x" (cdr (assoc "URL" entry))))
    (should (equal "n" (cdr (assoc "Notes" entry))))
    ;; Non-standard fields are ignored.
    (should-not (assoc "Uuid" entry))))

(ert-deftest keepass-browse-parse-entry-buffer ()
  "`parse-entry' reads Field: value lines, including the optional Group."
  (with-temp-buffer
    (insert "Group: /g/\nTitle: /g/t\nUserName: bob\nPassword: pw\nURL: http://x\n")
    (let ((entry (keepass-browse--parse-entry)))
      (should (equal "/g/" (cdr (assoc "Group" entry))))
      (should (equal "/g/t" (cdr (assoc "Title" entry))))
      (should (equal "bob" (cdr (assoc "UserName" entry))))
      (should (equal "pw" (cdr (assoc "Password" entry)))))))

(ert-deftest keepass-browse-parse-entry-group-absent ()
  "A buffer without a Group line still parses; Group is optional."
  (with-temp-buffer
    (insert "Title: t\nUserName: u\nPassword: p\nURL: http://x\n")
    (let ((entry (keepass-browse--parse-entry)))
      (should-not (assoc "Group" entry))
      (should (equal "t" (cdr (assoc "Title" entry)))))))

(ert-deftest keepass-browse-edit-includes-group-line ()
  "`keepass-browse-edit' templates the Group line above Title."
  (let* ((entry '(("Title" . "t") ("UserName" . "u") ("Password" . "p")
                  ("URL" . "x") ("Notes" . "n")))
         (keepass-browse--entry-parents '(("/g/t" . "/g/")))
         (box (list nil)))
    ;; Stub entry-get and entry-open to capture the template.
    (cl-letf (((symbol-function 'keepass-browse--entry-get) (lambda (_) entry))
              ((symbol-function 'keepass-browse--entry-open)
               (lambda (_name _action _path template)
                 (setcar box template))))
      (keepass-browse-edit "/g/t"))
    (should (string-match-p "^Group: /g/\n" (car box)))
    (should (string-match-p "\nTitle: t\n" (car box)))))

(ert-deftest keepass-browse-parse-entry-ignores-hint-line ()
  "The `;; Keys:' hint line is not parsed into an entry field."
  (with-temp-buffer
    (insert ";; Keys: C-c C-c commit | C-c C-p select group | C-c C-r regenerate\n"
            ";; C-c C-k cancel\n"
            "Group: /Work/\nTitle: t\nUserName: u\nPassword: p\n")
    (let ((entry (keepass-browse--parse-entry)))
      (should-not (assoc "Keys" entry))
      (should (equal "/Work/" (cdr (assoc "Group" entry))))
      (should (equal "t" (cdr (assoc "Title" entry))))
      (should (equal "u" (cdr (assoc "UserName" entry)))))))

(ert-deftest keepass-browse-spec-label ()
  "A database spec's label is its :name, defaulting to the file name."
  (should (equal "mydb" (keepass-browse--spec-label '(:name "mydb" :file "/p/db.kdbx"))))
  (should (equal "db.kdbx" (keepass-browse--spec-label '(:file "/path/db.kdbx")))))

(ert-deftest keepass-browse-select-database-by-label ()
  "Selecting a database completes over labels and sets a plist spec."
  (let* ((keepass-browse-databases
          '((:name "work" :file "/a/work.kdbx")
            (:file "/b/personal.kdbx")))
         (keepass-browse-database nil))
    (let ((result
           (cl-letf (((symbol-function 'completing-read)
                      (lambda (_prompt coll &rest _) (car coll))))
             (keepass-browse-select-database))))
      ;; The first label is "work"; the matching entry is the whole plist.
      (should (equal '(:name "work" :file "/a/work.kdbx") result))
      (should (equal result keepass-browse-database)))))

(ert-deftest keepass-browse-entry-mode-map-bindings ()
  "The entry-mode keymap binds group-choosing to `C-c C-p'.
Not `C-c C-g': a C-g after a prefix key is treated by Emacs as \"cancel
the prefix\" and can never be dispatched to a binding."
  (should (eq #'keepass-browse--entry-choose-group
              (lookup-key keepass-browse-entry-mode-map (kbd "C-c C-p"))))
  (should (eq #'keepass-browse--entry-commit
              (lookup-key keepass-browse-entry-mode-map (kbd "C-c C-c"))))
  ;; Guard against reintroducing the C-g trap.
  (should-not (lookup-key keepass-browse-entry-mode-map (kbd "C-c C-g"))))

(ert-deftest keepass-browse-entry-open-unmodified ()
  "A freshly opened entry buffer is not marked modified until edited."
  (let ((buf (cl-letf (((symbol-function 'switch-to-buffer) #'ignore))
               (keepass-browse--entry-open "*kb-open-test*" "add" nil))))
    (should-not (buffer-modified-p buf))
    (with-current-buffer buf
      (insert "x"))
    (should (buffer-modified-p buf))
    (kill-buffer buf)))

(ert-deftest keepass-browse-view-shows-group ()
  "The view buffer shows a Group line (the entry's folder) above Title."
  (with-temp-buffer
    (setq-local keepass-browse-view-path "/Internet/Google/mail")
    (cl-letf (((symbol-function 'keepass-browse--entry-get)
               (lambda (_) '(("Title" . "mail") ("UserName" . "u")
                             ("Password" . "p") ("URL" . "x")
                             ("Notes" . "n")))))
      (keepass-browse-view-update nil))
    (goto-char (point-min))
    (should (string-match-p "^Group\\s-+/Internet/Google/\n" (buffer-string)))
    (should (string-match-p "^Group\\s-+.+\nTitle\\s-+mail\n" (buffer-string)))))

(ert-deftest keepass-browse-generate-args ()
  "The option labels expand into full keepassxc-cli commands.
The `:length' placeholder is replaced by the requested length as a string
(since `call-process' takes only strings)."
  (should (equal '("generate" "--upper" "--length" "16")
                 (keepass-browse--generate-args "upper case" 16)))
  (should (equal '("generate" "--lower" "--upper" "--numeric" "--length" "12")
                 (keepass-browse--generate-args "with numeric" 12)))
  ;; The all-printable set is the full !-~ range via --custom.
  (let ((ascii (keepass-browse--generate-args "all printable (!-~)" 8)))
    (should (equal '("generate" "--custom") (seq-take ascii 2)))
    (let* ((set (nth 2 ascii)))
      (should (= 94 (length set)))                 ; ! (0x21) .. ~ (0x7e)
      (should (string-match-p "!" set))
      (should (string-match-p "~" set))
      (should-not (string-match-p " " set))))      ; 0x20 is outside the range
  ;; The diceware option uses --words, not --length.
  (should (equal '("diceware" "--words" "6")
                 (keepass-browse--generate-args "passphrase" 6)))
  ;; A label not in the alist is an error.
  (should-error (keepass-browse--generate-args "bogus" 8)))

(ert-deftest keepass-browse-generate-failure-message ()
  "The keepassxc 'Invalid password generator' error advises a longer length."
  (let ((msg (keepass-browse--generate-failure-message
              "with special"
              "Invalid password generator after applying all options.")))
    (should (string-match-p "longer length" msg))
    (should (string-match-p "with special" msg)))
  ;; Other failures keep a generic message.
  (should (string-match-p "failed"
                          (keepass-browse--generate-failure-message
                           "with special" "some other error"))))

(ert-deftest keepass-browse-read-charset-remembers ()
  "`read-charset' offers the last choice as default, else the first option."
  (let ((keepass-browse--last-generated-charset "mixed case"))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_p _c _x _r _h _i def)
                 (unless (string= def "mixed case")
                   (ert-fail (format "expected default label, got %S" def)))
                 "upper case")))
      (should (equal "upper case" (keepass-browse--read-charset)))
      (should (equal "upper case"
                     keepass-browse--last-generated-charset))))
  ;; Nothing chosen yet -> the first entry's label is the default.
  (let ((keepass-browse--last-generated-charset nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_p _c _x _r _h _i def)
                 (unless (string= def "all printable (!-~)")
                   (ert-fail (format "expected default label, got %S" def)))
                 def)))
      (should (equal "all printable (!-~)"
                     (keepass-browse--read-charset))))))

(ert-deftest keepass-browse-parse-entry-multiline-notes ()
  "Notes extends to the end of the buffer, preserving multiple lines."
  (with-temp-buffer
    (insert "Title: t\nUserName: u\nPassword: p\nURL: http://x\n"
            "Notes: first line\nsecond line\n\nthird line\n")
    (let ((entry (keepass-browse--parse-entry)))
      (should (equal "first line\nsecond line\n\nthird line"
                     (cdr (assoc "Notes" entry))))
      ;; Other fields still parsed correctly.
      (should (equal "t" (cdr (assoc "Title" entry))))
      (should (equal "u" (cdr (assoc "UserName" entry))))
      (should (equal "p" (cdr (assoc "Password" entry)))))))

(ert-deftest keepass-browse-format-candidate-tags-path ()
  "`format-candidate' tags the line with the entry path."
  (let* ((path "/g/t")
         (entry '(("Title" . "t") ("UserName" . "u") ("URL" . "http://x")))
         (keepass-browse-fields '("Title" "UserName" "URL"))
         (cand (keepass-browse--format-candidate path entry)))
    (should (equal path (keepass-browse--path-of cand)))))

(ert-deftest keepass-browse-format-candidate-icon-prefix ()
  "`format-candidate' prefixes the glyph for the entry's IconID."
  (let* ((keepass-browse-fields '("Title"))
         (cand (keepass-browse--format-candidate
                "/mail" '(("IconID" . "19") ("Title" . "mail")))))
    ;; 19 is the envelope.
    (should (string-prefix-p "✉️" cand))
    (should (string-match-p "mail" cand))
    ;; The kb-path tag still covers the glyph, so Embark/vertico resolve it.
    (should (equal "/mail" (keepass-browse--path-of cand))))
  ;; Out-of-range and missing ids get no glyph.
  (should (string-prefix-p "T"
                           (keepass-browse--format-candidate
                            "/x" '(("IconID" . "999") ("Title" . "T")))))
  (should (string-prefix-p "T"
                           (keepass-browse--format-candidate "/x" '(("Title" . "T"))))))

(ert-deftest keepass-browse-custom-icons-parse-and-prefix ()
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
         (keepass-browse--custom-icons nil)
         (keepass-browse--entry-custom-icons nil)
         (entries (progn
                    (setq keepass-browse--custom-icons
                          (keepass-browse--custom-icons-from tree))
                    (keepass-browse--collect
                     (car (keepass-browse--xml-children-tag
                           (car (keepass-browse--xml-children-tag tree 'Root))
                           'Group))
                     ""))))
    ;; The blob is decoded (base64 of a PNG header).
    (should (equal "abc+/==" (caar keepass-browse--custom-icons)))
    (should (equal "\211PNG" (substring (cdar keepass-browse--custom-icons) 0 4)))
    ;; The entry referencing the icon is mapped by path.
    (should (equal '(("/mail" . "abc+/=="))
                   keepass-browse--entry-custom-icons))
    ;; Candidates still tag paths, and the plain entry keeps its glyph.
    (let ((keepass-browse-fields '("Title")))
      (should (equal "/mail"
                     (keepass-browse--path-of
                      (keepass-browse--format-candidate
                       "/mail" (cdr (assoc "/mail" entries))))))
      (should (string-prefix-p "✉️"
                               (keepass-browse--format-candidate
                                "/plain" (cdr (assoc "/plain" entries)))))))
  ;; The custom icon becomes a real image on a graphic display, shown as a
  ;; propertized space prefix; unknown UUIDs fall back to the glyph.
  (let ((keepass-browse--custom-icons '(("abc+/==" . "\211PNGxxxx")))
        (keepass-browse--entry-custom-icons '(("/mail" . "abc+/==")))
        (keepass-browse-fields '("Title")))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda () t)))
      (let ((cand (keepass-browse--format-candidate
                   "/mail" '(("IconID" . "0") ("Title" . "mail")))))
        (should (equal ?\s (aref cand 0)))
        (should (eq 'image (car-safe (get-text-property 0 'display cand))))))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda () nil)))
      (should (string-prefix-p "🔑"
                               (keepass-browse--format-candidate
                                "/mail" '(("IconID" . "0") ("Title" . "mail"))))))))

(ert-deftest keepass-browse-valid-field-p ()
  "Only the five standard fields are recognised."
  (should (keepass-browse--valid-field-p "Title"))
  (should (keepass-browse--valid-field-p "Notes"))
  (should-not (keepass-browse--valid-field-p "Uuid"))
  (should-not (keepass-browse--valid-field-p "host")))

;;;; Group navigation helpers

(ert-deftest keepass-browse-entry-directory-basename ()
  "`entry-directory'/'entry-basename' split KeePass paths without TRAMP.
A title like \"Apple:foo:bar\" gives path \"/Apple:foo:bar\", which any
`file-name-*' function would hand to TRAMP (\"Method `Apple' is not
known\").  The pure string helpers must never touch those."
  (should (equal "/" (keepass-browse--entry-directory "/b")))
  (should (equal "/a/" (keepass-browse--entry-directory "/a/b")))
  (should (equal "/" (keepass-browse--entry-directory "/")))
  (should (equal "" (keepass-browse--entry-directory "")))
  (should (equal "/" (keepass-browse--entry-directory "/Apple:foo:bar")))
  (should (equal "Apple:foo:bar" (keepass-browse--entry-basename "/Apple:foo:bar")))
  (should (equal "b" (keepass-browse--entry-basename "/a/b")))
  (should (equal "" (keepass-browse--entry-basename "/"))))

(ert-deftest keepass-browse-group-contents-tramp-safe ()
  "`group-contents' handles entry titles that look like TRAMP remote names."
  (let ((keepass-browse--group-icons nil) ; hermetic: no tree groups
        (keepass-browse--entry-parents nil) ; and no recorded parents
        (entries '(("/Apple:foo:bar" . nil)
                   ("/Internet/Apple:foo:bar" . nil)
                   ("/Work/g" . nil))))
    (pcase-let* ((`(,groups . ,subs)
                  (keepass-browse--group-contents entries "/")))
      ;; '/Apple:foo:bar' is a direct root entry, not a spurious subgroup.
      (should (equal '("/Internet/" "/Work/") groups))
      (should (equal '("/Apple:foo:bar") (mapcar #'car subs))))))

(ert-deftest keepass-browse-group-contents ()
  "`group-contents' splits entries into child groups and child entries."
  (let ((keepass-browse--group-icons nil) ; hermetic: no tree groups
        (keepass-browse--entry-parents nil) ; and no recorded parents
        (entries '(("/Internet/Google/a" . nil)
                   ("/Internet/Google/b" . nil)
                   ("/Internet/Yahoo/c" . nil)
                   ("/Work/g" . nil)
                   ("/Root" . nil))))
    ;; Root: subgroups /Internet/ + /Work/, direct entry /Root.
    (pcase-let* ((`(,groups . ,subs)
                  (keepass-browse--group-contents entries "/")))
      (should (equal '("/Internet/" "/Work/") groups))
      (should (equal '("/Root") (mapcar #'car subs))))
    ;; /Internet: subgroups /Internet/Google/ + /Internet/Yahoo/, no direct
    ;; entries.
    (pcase-let* ((`(,groups . ,subs)
                  (keepass-browse--group-contents entries "/Internet/")))
      (should (equal '("/Internet/Google/" "/Internet/Yahoo/") groups))
      (should (null subs)))
    ;; Deepest group: two direct entries, no subgroups; group name without a
    ;; trailing slash is normalized the same way.
    (pcase-let* ((`(,groups . ,subs)
                  (keepass-browse--group-contents entries "/Internet/Google")))
      (should (null groups))
      (should (equal '("/Internet/Google/a" "/Internet/Google/b")
                     (mapcar #'car subs))))))

(ert-deftest keepass-browse-group-choose-drills-down ()
  "`group-choose' descends through subgroups to reach an entry, recursing."
  (let* ((entries '(("/Internet/Google/a" . (("Title" . "a")))))
         (queue (list (keepass-browse--format-group "/Internet/")
                      (keepass-browse--format-candidate
                       "/Internet/Google/a" (cdar entries)))))
    (cl-letf (((symbol-function 'consult--read)
               (lambda (&rest _) (pop queue))))
      (should (equal "/Internet/Google/a"
                     (keepass-browse--group-choose entries "/"))))
    (should (null queue))))

(ert-deftest keepass-browse-group-command-picks-entry ()
  "`keepass-browse-group' drills down and runs the default action on the entry."
  (let* ((entries '(("/A/b" . nil)))
         (queue (list (keepass-browse--format-group "/A/")
                      (keepass-browse--format-candidate "/A/b" nil)))
         (keepass-browse-database (keepass-make-db-spec :file "db.kdbx"))
         (box (list nil))
         (keepass-browse-default-action (lambda (p) (setcar box p))))
    (cl-letf (((symbol-function 'keepass-browse--load-entries)
               (lambda () entries))
              ((symbol-function 'consult--read)
               (lambda (&rest _) (pop queue))))
      (should (equal "/A/b" (keepass-browse-group))))
    (should (equal "/A/b" (car box)))))

(ert-deftest keepass-browse-format-group-tags-path ()
  "`format-group' prefixes the icon glyph and tags the full path.
With no icon info recorded, a group shows the keepassxc default (48, a
folder)."
  (let ((cand (keepass-browse--format-group "/Internet/")))
    (should (string-prefix-p "📁 Internet/" cand))
    (should (equal "/Internet/" (keepass-browse--path-of cand))))
  ;; A nested group keeps its full path in the tag.
  (should (equal "/Internet/Google/"
                 (keepass-browse--path-of
                  (keepass-browse--format-group "/Internet/Google/")))))

(ert-deftest keepass-browse-group-icons-from-export ()
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
        (keepass-browse--group-icons nil))
    (with-temp-buffer
      (insert xml)
      ;; Like `keepass-browse--load-entries': start below the root group,
      ;; whose own name names no path.
      (keepass-browse--collect-groups
       (car (keepass-browse--xml-children-tag
             (car (keepass-browse--xml-children-tag
                   (libxml-parse-xml-region (point-min) (point-max)) 'Root))
             'Group))
       "")
      ;; Mirror the Recycle Bin exclusion `keepass-browse--load-entries`
      ;; applies after collecting.
      (setq keepass-browse--group-icons
            (seq-filter (lambda (g)
                          (not (string-prefix-p "/Recycle Bin" (car g))))
                        keepass-browse--group-icons)))
    ;; The root group's name is not recorded...
    (should-not (assoc "/Passwords" keepass-browse--group-icons))
    ;; ...but nested groups are, with both standard and custom icons.
    (should (equal '("1" . "cu-uuid")
                   (cdr (assoc "/Internet" keepass-browse--group-icons))))
    (should (equal '("48" . nil)
                   (cdr (assoc "/Internet/Empty" keepass-browse--group-icons))))
    ;; The Recycle Bin subtree is not recorded.
    (should-not (assoc "/Recycle Bin" keepass-browse--group-icons))
    (should-not (assoc "/Recycle Bin/x" keepass-browse--group-icons))
    ;; Glyphs come from the recorded ids; the custom icon falls back to the
    ;; glyph on a non-graphic display.
    (should (equal "🌍" (keepass-browse--group-prefix "/Internet/")))
    (should (equal "📁" (keepass-browse--group-prefix "/Internet/Empty/")))))

(ert-deftest keepass-browse-group-contents-parents ()
  "Entries are classified by their recorded parent group, so a title
containing \"/\" cannot carve itself into phantom subgroups."
  (let* ((slashy "/Backups/Odd / Title – with /slashes")
         (keepass-browse--group-icons '(("/Backups" . ("48" . nil))))
         (entries `((,slashy . (("Group" . "/Backups/")))
                    ("/Backups/Normal" . (("Group" . "/Backups/"))))))
    ;; Root: /Backups is a real subgroup; the slashy title does not leak a
    ;; phantom "Odd / Title –" segment.
    (pcase-let* ((`(,groups . ,subs)
                  (keepass-browse--group-contents entries "/")))
      (should (equal '("/Backups/") groups))
      (should (null subs)))
    ;; /Backups: the slashy entry is a direct child, with its full title.
    (pcase-let* ((`(,groups . ,subs)
                  (keepass-browse--group-contents entries "/Backups/")))
      (should (null groups))
      (should (equal (sort (mapcar #'car entries) #'string<)
                     (mapcar #'car subs))))))

(ert-deftest keepass-browse-edit-slashy-title-templates-real-values ()
  "The edit screen templates the real Title and Group for a slashy title.
Deriving them from the path would truncate the title (renaming the entry
on commit) and invent a phantom group."
  (let* ((slashy "/Backups/Odd / Title – with /slashes")
         (keepass-browse--entry-parents `((,slashy . "/Backups/")))
         (box (list nil)))
    (cl-letf (((symbol-function 'keepass-browse--entry-get)
               (lambda (_) '(("Title" . "Odd / Title – with /slashes")
                             ("UserName" . "chris"))))
              ((symbol-function 'keepass-browse--entry-open)
               (lambda (_name _action _path template)
                 (setcar box template))))
      (keepass-browse-edit slashy))
    (should (string-match-p "^Group: /Backups/\n" (car box)))
    (should (string-match-p
             "\nTitle: Odd / Title – with /slashes\n" (car box)))))

(ert-deftest keepass-browse-command-map-bindings ()
  "The command keymap binds every command under C-:."
  (dolist (bind '(("t" keepass-browse)
                  ("g" keepass-browse-group)
                  ("d" keepass-browse-select-database)
                  ("f" keepass-browse-favorites)
                  ("k" keepass-browse-favorites-embark)
                  ("c" keepass-auth-source-forget-cached)
                  ("a" keepass-browse-add)))
    (let ((resolved (lookup-key keepass-browse-command-map (kbd (car bind)))))
      (should (eq resolved (cadr bind))))))

(ert-deftest keepass-browse-command-map-group-keys ()
  "The command keymap binds the group-maintenance commands."
  (should (eq (lookup-key keepass-browse-command-map (kbd "A"))
              #'keepass-browse-add-group))
  (should (eq (lookup-key keepass-browse-command-map (kbd "D"))
              #'keepass-browse-delete-group)))

(ert-deftest keepass-browse-add-group-name-validation ()
  "add-group rejects empty names and names containing a slash."
  (let ((db (keepass-browse-test-make-db))
        (keepass-browse-database nil)
        (keepass-browse-cache-expiry nil)
        (password-cache-expiry nil))
    (unwind-protect
        (progn
          (setq keepass-browse-database (keepass-make-db-spec :file db))
          (password-cache-add db "PASS")
          ;; The name is prompted; "/" in it is rejected before any run.
          (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "bad/name")))
            (should-error (keepass-browse-add-group "/Work/")
                          :type 'user-error))
          (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "")))
            (should-error (keepass-browse-add-group "/Work/")
                          :type 'user-error))
          ;; Nothing was created by the failed attempts.
          (let ((paths (keepass-browse--group-paths)))
            (should-not (member "/Work/bad" paths))
            (should-not (member "/Work/bad/name" paths))))
      (delete-file db))))

(ert-deftest keepass-browse-choose-group-offers-root ()
  "The group chooser offers the root / and defaults to it."
  (cl-letf (((symbol-function 'keepass-browse--group-paths)
             (lambda () '("/Bank/" "/Work/")))
            ((symbol-function 'completing-read)
             (lambda (_prompt coll &rest _)
               (should (member "/" coll))
               (car coll))))  ; RET: the default, root
    (should (equal "/" (keepass-browse--choose-group)))))

(ert-deftest keepass-browse-favorites-parse ()
  "`favorites--parse' keeps usable items and drops broken ones with a
message, never an error.  Keyless items are kept -- the keyed menu
assigns their keys later."
  (should (equal '((?b "Pika" "^/Backups/")
                   (?m nil "str-key")
                   (nil "keyless" nil)
                   (nil nil "Pika"))
                 (keepass-browse-favorites--parse
                  '((:key ?b :group "^/Backups/" :title "Pika")
                    (:key "m" :group "str-key")
                    (:title "keyless")
                    (:group "Pika")
                    (:key ?c)
                    (:key ?b :title "dup")
                    (:key ?x :title 42 :group "^/G")
                    (:key ?z :group "^/G" :title 42))))))

(ert-deftest keepass-browse-favorites-key-for ()
  "`key-for' prefers a mnemonic from the label, falling back to the pool."
  ;; Phase 1: the first unused character of the label itself.
  (should (equal ?g (keepass-browse-favorites--key-for "github" nil)))
  (should (equal ?i (keepass-browse-favorites--key-for "github" '(?g))))
  ;; Phase 1 skips non-regular characters: punctuation is never picked.
  (should (equal ?B (keepass-browse-favorites--key-for "@Bank/" nil)))
  (should (equal ?M (keepass-browse-favorites--key-for "\302\253Mail\302\273" nil)))
  ;; Uppercase is part of the regular set.
  (should (equal ?W (keepass-browse-favorites--key-for "Wise" '(?b))))
  ;; Phase 2: every regular label character taken -> first unused pool char.
  (should (equal ?1 (keepass-browse-favorites--key-for "github" '(?g ?i ?t ?h ?u ?b))))
  ;; The pool runs through digits, a-z, then A-Z.
  (should (equal ?A (keepass-browse-favorites--key-for ""
                     (string-to-list "123456789abcdefghijklmnopqrstuvwxyz"))))
  ;; Only ?A taken: digits and a-z are all still free, so ?1.
  (should (equal ?1 (keepass-browse-favorites--key-for "" '(?A))))
  ;; Digits+a-z taken: the pool reaches ?A.
  (should (equal ?A (keepass-browse-favorites--key-for ""
                     (string-to-list "123456789abcdefghijklmnopqrstuvwxyz"))))
  ;; Digits+a-z+A taken: next is ?B.
  (should (equal ?B (keepass-browse-favorites--key-for ""
                     (append (string-to-list "123456789abcdefghijklmnopqrstuvwxyz")
                             '(?A)))))
  ;; Nothing taken, no label: the pool starts at ?1.
  (should (equal ?1 (keepass-browse-favorites--key-for "" nil)))
  ;; Everything taken -> user-error.
  (should-error (keepass-browse-favorites--key-for ""
                 (append (string-to-list "123456789abcdefghijklmnopqrstuvwxyz")
                         (string-to-list "ABCDEFGHIJKLMNOPQRSTUVWXYZ")))
                :type 'user-error))

(ert-deftest keepass-browse-favorites-assign-keys ()
  "`assign-keys' gives keyless items mnemonic keys, pool fallback."
  (should (equal '((?a "a" "/X") (?b "b" "/Y") (?m "m" "/Z"))
                 (keepass-browse-favorites--assign-keys
                  '((nil "a" "/X") (nil "b" "/Y") (?m "m" "/Z")))))
  ;; Explicit keys are honoured; keyless items avoid them (phase 1 fails
  ;; on "a" since ?a is taken, so the pool supplies ?1).
  (should (equal '((?a "a") (?1 "a"))
                 (keepass-browse-favorites--assign-keys
                  '((?a "a") (nil "a"))))))

(ert-deftest keepass-browse-favorites-match ()
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
         (matched (keepass-browse-favorites--match spec entries)))
    ;; ?p and ?a both match /Backups/Pika: offered once (3 results total).
    (should (= 3 (length matched)))
    (should (equal '("/Backups/Pika" "/Backups/Pika-old" "/Mail/gmail")
                   (sort (mapcar #'car matched) #'string<)))
    ;; AND semantics: "gmail" in the /Backups group matches nothing.
    (should (null (keepass-browse-favorites--match
                   '((?x "gmail" "^/Backups/")) entries)))
    ;; Anchors: an exact group path matches only that group.
    (should (equal '("/Mail/gmail")
                   (mapcar #'car (keepass-browse-favorites--match
                                  '((?x nil "\\`/Mail/\\'")) entries))))))

(ert-deftest keepass-browse-favorites-choice ()
  "`favorites--choice' prefers the title for the name, group as
description; group alone is the name."
  (should (equal '(?b "Pika" "/Backups/")
                 (keepass-browse-favorites--choice '(?b "Pika" "/Backups/"))))
  (should (equal '(?s "Secret")
                 (keepass-browse-favorites--choice '(?s nil "Secret")))))

(ert-deftest keepass-browse-favorites-selects-matches ()
  "`keepass-browse-favorites' offers exactly the matching entries."
  (keepass-browse-test-with-db
    (let* ((keepass-browse-favorites-default
            '((:key ?g :title "github") (:key ?e :title "email")))
           (keepass-browse--selecting t)
           (entry-box (list nil))
           (keepass-browse-default-action
            (lambda (p) (setcar entry-box p)))
           (path (cl-letf (((symbol-function 'consult--read)
                            (lambda (candidates &rest _)
                              (should (= 2 (length candidates)))
                              (car candidates))))
               (keepass-browse-favorites))))
      (should (member path '("/email" "/Work/github")))
      (should (equal path (car entry-box)))
      (let ((keepass-browse-favorites-default '((:key ?x))))
        (should-error (keepass-browse-favorites) :type 'user-error)))))

(ert-deftest keepass-browse-favorites-embark-flows ()
  "`favorites-embark': one match goes to the embark action menu; several
matches go to a keyed menu of the matches first, then the picked entry
goes to the same action menu."
  (keepass-browse-test-with-db
    (let* ((menu-box (list nil))
           (target-box (list nil))
           (entry-box (list nil))
           (keepass-browse-default-action (lambda (p) (setcar entry-box p))))
      ;; Single match: straight to the embark action menu on the entry.
      (let ((keepass-browse-favorites-default '((:key ?g :title "github"))))
        (cl-letf (((symbol-function 'read-multiple-choice)
                   (lambda (_prompt choices &optional _help _show)
                     (assq ?g choices)))
                  ((symbol-function 'embark-act)
                   (lambda ()
                     (setcar menu-box t)
                     (setcar target-box
                             (funcall (car embark-target-finders))))))
          (keepass-browse-favorites-embark))
        (should (car menu-box))
        (should (equal target-box
                       (list '(keepass-browse-select . "/Work/github")))))
      ;; Several matches: favorites menu, then keyed menu of the matches,
      ;; then the action menu on the picked entry.
      (let ((keepass-browse-favorites-default '((:key ?a :title "."))))
        (cl-letf (((symbol-function 'read-multiple-choice)
                   (lambda (_prompt choices &optional _help _show)
                     (car choices)))
                  ((symbol-function 'embark-act)
                   (lambda ()
                     (setcar target-box
                             (funcall (car embark-target-finders))))))
          (keepass-browse-favorites-embark))
        ;; The entries menu's first choice is the github entry; the picked
        ;; entry goes to the action menu, not the default action.
        (should (equal target-box
                       (list '(keepass-browse-select . "/Work/github")))))
      (should (null (car entry-box))))))

(ert-deftest keepass-browse-group-contents-includes-empty ()
  "`group-contents' lists empty groups recorded from the export tree."
  (let* ((keepass-browse--group-icons
          '(("/A" . ("48" . nil)) ("/A/Empty" . ("48" . nil))))
         (entries '(("/A/x" . nil))))
    ;; Root: /A is a subgroup; the only entry lives under /A, not at root.
    (pcase-let* ((`(,groups . ,subs)
                  (keepass-browse--group-contents entries "/")))
      (should (equal '("/A/") groups))
      (should (null subs)))
    ;; /A: the empty subgroup appears even though no entry lives there.
    (pcase-let* ((`(,groups . ,subs)
                  (keepass-browse--group-contents entries "/A/")))
      (should (equal '("/A/Empty/") groups))
      (should (equal '("/A/x") (mapcar #'car subs))))))

(ert-deftest keepass-browse-group-custom-icon-thumbnail ()
  "A group with a custom icon shows an image prefix on a graphic display."
  (let ((keepass-browse--custom-icons '(("cu" . "\211PNGxx")))
        (keepass-browse--group-icons '(("/G" . ("48" . "cu")))))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda () t)))
      (let ((cand (keepass-browse--format-group "/G/")))
        (should (equal ?\s (aref cand 0)))
        (should (eq 'image (car-safe (get-text-property 0 'display cand))))
        (should (equal "/G/" (keepass-browse--path-of cand)))))
    ;; Non-graphic: the standard-icon glyph.
    (cl-letf (((symbol-function 'display-graphic-p) (lambda () nil)))
      (should (string-prefix-p "📁"
                               (keepass-browse--format-group "/G/"))))))

;;;; Integration tests (need keepassxc-cli)

(ert-deftest keepass-browse-list-paths ()
  "Entry and group paths are listed with normalized leading slashes."
  (keepass-browse-test-with-db
    (let ((paths (keepass-browse--entry-paths))
          (groups (keepass-browse--group-paths)))
      (should (member "/email" paths))
      (should (member "/Work/github" paths))
      (should (member "/Work/" groups)))))

(ert-deftest keepass-browse-group-contents-real ()
  "`group-contents' on a real database walks the group tree."
  (keepass-browse-test-with-db
    (let ((entries (keepass-browse--load-entries)))
      ;; Root: the /Work group plus the top-level /email entry.
      (pcase-let* ((`(,groups . ,subs)
                    (keepass-browse--group-contents entries "/")))
        (should (equal '("/Work/") groups))
        (should (equal '("/email") (mapcar #'car subs))))
      ;; /Work: only the nested github entry.
      (pcase-let* ((`(,groups . ,subs)
                    (keepass-browse--group-contents entries "/Work/")))
        (should (null groups))
        (should (equal '("/Work/github") (mapcar #'car subs)))))))

(ert-deftest keepass-browse-entry-get-fields ()
  "`entry-get' returns the real fields for an entry."
  (keepass-browse-test-with-db
    (let ((entry (keepass-browse--entry-get "/email")))
      (should (equal "me@x.com" (cdr (assoc "UserName" entry))))
      (should (equal "smtp.x.com:465" (cdr (assoc "URL" entry)))))))

(ert-deftest keepass-browse-copy-password-to-kill-ring ()
  "Copying puts the real password on the kill ring."
  (keepass-browse-test-with-db
    (keepass-browse-copy-password "/email")
    (let ((entry (keepass-browse--entry-get "/email")))
      (should (equal (cdr (assoc "Password" entry)) (car kill-ring))))))

(ert-deftest keepass-browse-candidates-tagged ()
  "Candidates carry kb-path so Embark can act on the entry under point."
  (keepass-browse-test-with-db
    (let* ((keepass-browse-fields '("Title" "UserName" "URL"))
           (cands (keepass-browse--candidates)))
      (should (= 2 (length cands)))
      (dolist (c cands)
        (should (keepass-browse--path-of c))))))

(ert-deftest keepass-browse-entry-commit-adds ()
  "Committing an add buffer creates the entry in the database."
  (keepass-browse-test-with-db
    (with-temp-buffer
      (insert "Title: /Work/new2\nUserName: carol\nPassword: pw2\nURL: http://x\n")
      (keepass-browse-entry-mode)
      (setq-local keepass-browse--entry-action "add")
      (setq-local keepass-browse--entry-original nil)
      (keepass-browse--entry-commit))
    (let ((entry (keepass-browse--entry-get "/Work/new2")))
      (should (equal "carol" (cdr (assoc "UserName" entry)))))))

(ert-deftest keepass-browse-entry-commit-edits ()
  "Committing an edit buffer updates the entry's fields."
  (keepass-browse-test-with-db
    (with-temp-buffer
      (insert "Title: email\nUserName: newuser\nPassword: newpw\nURL: smtp.x.com:465\n")
      (keepass-browse-entry-mode)
      (setq-local keepass-browse--entry-action "edit")
      (setq-local keepass-browse--entry-original "/email")
      (keepass-browse--entry-commit))
    (let ((entry (keepass-browse--entry-get "/email")))
      (should (equal "newuser" (cdr (assoc "UserName" entry))))
      (should (equal "newpw" (cdr (assoc "Password" entry)))))))

(ert-deftest keepass-browse-entry-commit-rename-keeps-one ()
  "Renaming via edit -t updates the title in place, keeping one entry."
  (keepass-browse-test-with-db
    (with-temp-buffer
      (insert "Title: github-new\nUserName: cbit\nPassword: x\nURL: https://github.com\n")
      (keepass-browse-entry-mode)
      (setq-local keepass-browse--entry-action "edit")
      (setq-local keepass-browse--entry-original "/Work/github")
      (keepass-browse--entry-commit))
    (let ((paths (keepass-browse--entry-paths)))
      (should (member "/Work/github-new" paths))
      (should-not (member "/Work/github" paths)))))

(ert-deftest keepass-browse-entry-commit-moves-group ()
  "Changing the Group field moves the entry into another group (mv).
The move must not delete+re-add: the entry survives in its new group and
the old path is gone."
  (keepass-browse-test-with-db
    (with-temp-buffer
      (insert "Group: /Work/\nTitle: email\nUserName: me@x.com\nPassword: PASS\nURL: smtp.x.com:465\n")
      (keepass-browse-entry-mode)
      (setq-local keepass-browse--entry-action "edit")
      (setq-local keepass-browse--entry-original "/email")
      (keepass-browse--entry-commit))
    (let ((paths (keepass-browse--entry-paths)))
      (should (member "/Work/email" paths))
      (should-not (member "/email" paths)))
    ;; The entry's fields survive the move.
    (let ((entry (keepass-browse--entry-get "/Work/email")))
      (should (equal "me@x.com" (cdr (assoc "UserName" entry)))))))

(ert-deftest keepass-browse-delete-removes ()
  "Deleting an entry removes it from the list."
  (keepass-browse-test-with-db
    (keepass-browse--delete-entry "/email")
    (let ((paths (keepass-browse--entry-paths)))
      (should-not (member "/email" paths)))))

(ert-deftest keepass-browse-wrong-password-errors ()
  "A wrong master password raises an error, not a silent empty result."
  (keepass-browse-test-with-db
    ;; The password cache is keyed by the database *path*, so override the
    ;; good entry with a wrong password under that key.
    (let ((password-cache-expiry nil))
      (password-cache-add (keepass-browse--database-path) "WRONG"))
    (should-error (keepass-browse--load-entries) :type 'error)))

(ert-deftest keepass-browse-copy-totp-absent ()
  "Copying TOTP for an entry without one is a clean user-error, not a crash."
  (keepass-browse-test-with-db
    (should-error (keepass-browse-copy-totp "/email") :type 'user-error)))

(provide 'keepass-browse-test)
;;; keepass-browse-test.el ends here