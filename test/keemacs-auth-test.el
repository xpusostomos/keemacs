;;; keemacs-auth-test.el --- Tests for keemacs-auth -*- lexical-binding: t -*-

;;; Commentary:

;; Run with:
;;   emacs -Q --batch -l ert -l keemacs-auth.el \
;;         -l keemacs-auth-test.el -f ert-run-tests-batch-and-exit

;;; Code:
(require 'ert)
(require 'keemacs-auth)

(defmacro keemacs-auth--with-cli (cli &rest body)
  "Bind the active backend to CLI while running BODY."
  `(let ((keemacs-auth-cache-expiry nil)
         keemacs-auth--active-cli)
     (setq keemacs-auth--active-cli ,cli)
     ,@body))

;;;; Parse helpers

(ert-deftest keemacs-auth-parse-auth-maps-fields ()
  "KPScript `S: key = value' lines map to the auth-source plist shape."
  (let ((a (keemacs-auth--parse-auth
            "S: UserName = foo\nS: Password = secret\nS: Title = My\nS: URL = https://x"
            587)))
    (should (equal 587 (plist-get a :port)))
    (should (equal "foo" (plist-get a :user)))
    (should (equal "My" (plist-get a :title)))
    (should (equal "https://x" (plist-get a :host)))
    ;; secret is wrapped in a thunk
    (should (equal "secret" (funcall (plist-get a :secret))))))

(ert-deftest keemacs-auth-parse-handles-empty ()
  (let ((r (keemacs-auth--parse "Status line\n\n" nil)))
    (should (listp (car r)))
    (should (stringp (nth 1 r)))))

;;;; keepassxc-cli parsing (pure, mocked input)

(ert-deftest keemacs-auth-keepassxc-parse-summary ()
  "A `keepassxc-cli show' summary should parse into the S: shape and a plist."
  (let ((plist (keemacs-auth--keepassxc-parse
                "Title: aws\nUserName: c@e.com\nPassword: real\nURL: https://host/a\nNotes: \nTags: \n"
                443)))
    (should (equal 443 (plist-get plist :port)))
    (should (equal "aws" (plist-get plist :title)))
    (should (equal "c@e.com" (plist-get plist :user)))
    (should (equal "real" (funcall (plist-get plist :secret))))
    (should (equal "https://host/a" (plist-get plist :host)))
    (should (eq nil (plist-get plist :url)))
    ;; A value containing a colon must not be swallowed by a greedy key match.
    (should (equal "https://host/a" (plist-get plist :host)))))

(ert-deftest keemacs-auth-keepassxc-parse-protected-password ()
  "A Password: PROTECTED line means the secret was not revealed, so no
`:secret' is produced rather than storing the literal \"PROTECTED\"."
  (let ((plist (keemacs-auth--keepassxc-parse
                "Title: x\nUserName: u\nPassword: PROTECTED\n" nil)))
    (should (null (plist-get plist :secret)))))

;;;; Command builders

(ert-deftest keemacs-auth-kpscript-command-uses-custom-program ()
  (let ((keemacs-auth-kpscript-program "my-kpscript"))
    (should (string-match-p
             (regexp-quote "my-kpscript -C:ListEntries")
             (keemacs-auth--kpscript-command
              "/tmp/db.kdbx" "u" "x.y/p" "pw")))))

(ert-deftest keemacs-auth-kpscript-command-includes-refs ()
  (let ((cmd (keemacs-auth--kpscript-command
              "/tmp/db.kdbx" "user1" "host.example.com" "pass")))
    (should (string-match-p "-ref-Username:\"user1\"" cmd))
    (should (string-match-p "-ref-URL:\"//host.example.com//\"" cmd))
    (should (string-match-p "-pw:\"pass\"" cmd))))

;;;; Backend resolution

(ert-deftest keemacs-auth-resolve-forced ()
  (let ((keemacs-auth-cli 'keepassxc))
    (should (eq 'keepassxc (keemacs-auth--resolve-cli))))
  (let ((keemacs-auth-cli 'kpscript))
    (should (eq 'kpscript (keemacs-auth--resolve-cli)))))

(ert-deftest keemacs-auth-resolve-auto-prefers-keepassxc ()
  (let ((keemacs-auth-cli 'auto)
        (keemacs-auth-keepassxc-cli-program "keepassxc-cli"))
    ;; On a machine with keepassxc-cli on PATH this resolves to keepassxc.
    (if (executable-find "keepassxc-cli")
        (should (eq 'keepassxc (keemacs-auth--resolve-cli)))
      (should (memq (keemacs-auth--resolve-cli) '(nil kpscript))))))

;;;; Locked detection

(ert-deftest keemacs-auth-locked-p ()
  (should (keemacs-auth--keepassxc-locked-p :locked))
  (should (keemacs-auth--keepassxc-locked-p
           "Error while reading the database: Invalid credentials were provided, please try again.
If this reoccurs, then your database file may be corrupt."))
  (should-not (keemacs-auth--keepassxc-locked-p ""))
  (should-not (keemacs-auth--keepassxc-locked-p nil)))

;;;; Integration: full search against a real keepassxc-cli (optional)

(defcustom keemacs-auth-test-program
  (or (executable-find "keepassxc-cli") "")
  "keepassxc-cli executable for the optional integration test."
  :type 'string
  :group 'keemacs)

(defun keemacs-auth-test-run (cmd)
  "Run shell command CMD via /bin/sh, returning its stdout."
  (with-temp-buffer
    (call-process "sh" nil t nil "-c" cmd)
    (buffer-string)))

(defun keemacs-auth-test-add (cli db title user url)
  "Add an entry TITLE with USER and URL to DB via keepassxc-cli CLI."
  (keemacs-auth-test-run
   (format "printf 'PASS\\n' | %s add -q %s %s -u %s -g --url %s"
           cli (shell-quote-argument db) (shell-quote-argument title)
           (shell-quote-argument user) (shell-quote-argument url))))

(defun keemacs-auth-test-make-db ()
  "Create a fresh throwaway kdbx with the target entry plus decoys.
The real SMTP-style entry is `target' (host x.example.com, user alice, port
443 in the URL).  The decoys deliberately mismatch one credential each, so a
correct search must reject all of them."
  (let* ((dir (make-temp-file "kpa-test-" t))
         (db (expand-file-name "t.kdbx" dir))
         (cli (shell-quote-argument keemacs-auth-test-program))
         (qdb (shell-quote-argument db)))
    (keemacs-auth-test-run
     (format "printf 'PASS\\nPASS\\n' | %s db-create -q %s --set-password" cli qdb))
    ;; The target entry: right host:port, right user.
    (keemacs-auth-test-add cli qdb "target" "alice" "x.example.com:443")
    ;; Decoys -- each is close but wrong in one way.
    (keemacs-auth-test-add cli qdb "wrong-user"   "bob"     "x.example.com:443")
    (keemacs-auth-test-add cli qdb "wrong-host"   "alice"   "y.example.com:443")
    (keemacs-auth-test-add cli qdb "wrong-port"   "alice"   "x.example.com:465")
    (keemacs-auth-test-add cli qdb "same-user-only" "alice" "https://z.example.com")
    (keemacs-auth-test-add cli qdb "same-host-only" "carol" "https://x.example.com/path")
    (keemacs-auth-test-add cli qdb "bare-host"    "alice"   "x.example.com")
    db))

(defun keemacs-auth-test-search (db spec)
  "Search DB with SPEC (a plist), returning the matching entries.
Runs inside the keepassxc backend with PASS as the master password."
  (save-window-excursion
    (keemacs-auth--with-cli 'keepassxc
      (let ((password-cache-expiry nil))
        (password-cache-add db "PASS"))
      (let ((backend (auth-source-backend
                      :type 'keepass
                      :source db
                      :data (keemacs-auth-make-db-spec :file db)
                      :search-function #'keemacs-auth-source-search)))
        (apply #'keemacs-auth-source-search :backend backend :max 5 (append spec nil))))))

(ert-deftest keemacs-auth-integration-via-keepassxc ()
  (skip-unless keemacs-auth-test-program)
  (let* ((db (keemacs-auth-test-make-db))
         (backend (auth-source-backend
                   :type 'keepass
                   :source db
                   :data (keemacs-auth-make-db-spec :file db)
                   :search-function #'keemacs-auth-source-search)))
    (unwind-protect
        (progn
          (keemacs-auth--with-cli 'keepassxc
            (let ((password-cache-expiry nil))
              (password-cache-add db "PASS"))
            (let ((res (keemacs-auth-source-search
                        :backend backend :host "x.example.com" :user "alice" :port 443 :max 1)))
              (should (= 1 (length res)))
              (let* ((en (car res))
                     (pw (funcall (plist-get en :secret))))
                (should (stringp pw))
                (should (= 443 (plist-get en :port)))))))
      (delete-file db))))

(ert-deftest keemacs-auth-integration-host-user-port ()
  "Host+user+port search selects the target plus the portless record
(bare-host), rejecting wrong-user, wrong-host and wrong-port decoys."
  (skip-unless keemacs-auth-test-program)
  (let ((db (keemacs-auth-test-make-db)))
    (unwind-protect
        (let ((res (keemacs-auth-test-search
                    db '(:host "x.example.com" :user "alice" :port 443))))
          ;; Lenient port rule: an entry whose URL spells no port still
          ;; matches a port-requesting search, so both target (:443) and
          ;; bare-host (no port) are returned; wrong-port (:465) is not.
          (let ((titles (mapcar (lambda (e) (plist-get e :title)) res)))
            (should (member "target" titles))
            (should (member "bare-host" titles))
            (should-not (member "wrong-port" titles))
            (should-not (member "wrong-user" titles))
            (should-not (member "wrong-host" titles))))
      (delete-file db))))

(ert-deftest keemacs-auth-integration-host-user ()
  "Host+user search (no port) matches both a bare-host and host:port entry."
  (skip-unless keemacs-auth-test-program)
  (let ((db (keemacs-auth-test-make-db)))
    (unwind-protect
        (let ((res (keemacs-auth-test-search
                    db '(:host "x.example.com" :user "alice"))))
          ;; Substring narrow: url:x.example.com matches both 'bare-host'
          ;; (URL "x.example.com") and 'target' (URL "x.example.com:443").
          ;; Wrong user/host must be rejected.  'same-host-only' has user carol.
          (let ((titles (mapcar (lambda (e) (plist-get e :title)) res)))
            (should (member "target" titles))
            (should (member "bare-host" titles))
            (should-not (member "wrong-user" titles))
            (should-not (member "wrong-host" titles))
            (should-not (member "same-host-only" titles))))
      (delete-file db))))

(ert-deftest keemacs-auth-integration-title-finds-entry ()
  "Searching by title alone returns exactly that entry."
  (skip-unless keemacs-auth-test-program)
  (let ((db (keemacs-auth-test-make-db)))
    (unwind-protect
        (let ((res (keemacs-auth-test-search
                    db '(:title "target"))))
          (should (= 1 (length res)))
          (should (string-equal "target" (plist-get (car res) :title))))
      (delete-file db))))

(defun keemacs-auth-test-titles (db spec)
  "Return the sorted entry titles returned by searching DB with SPEC."
  (sort (mapcar (lambda (e) (plist-get e :title))
                (keemacs-auth-test-search db spec))
        #'string<))

(defun keemacs-auth-test-assert (db spec &rest expected)
  "Assert searching DB with SPEC returns exactly the EXPECTED title set.
Each entry in the decoy DB takes one canonical role:
  target         x.example.com:443 / alice
  wrong-user     x.example.com:443 / bob
  wrong-host     y.example.com:443 / alice
  wrong-port     x.example.com:465 / alice
  same-user-only https://z.example.com / alice
  same-host-only https://x.example.com/path / carol
  bare-host      x.example.com / alice
A search must return all entries that match *every* key given, and only them."
  (let ((got (keemacs-auth-test-titles db `(,@spec :max 5))))
    (should (equal (sort (copy-sequence (append expected nil)) #'string<) got))))

(ert-deftest keemacs-auth-integration-matrix ()
  "A broad host/user/port matrix returns the exact right entries."
  (skip-unless keemacs-auth-test-program)
  (let ((db (keemacs-auth-test-make-db)))
    (unwind-protect
        (progn
          ;; Host alone: everything whose URL contains the host (incl. host:port
          ;; and full-URL entries with a different user -- the user is a
          ;; separate key).
          (keemacs-auth-test-assert
           db '(:host "x.example.com")
           "target" "wrong-user" "wrong-port" "bare-host" "same-host-only")
          (keemacs-auth-test-assert
           db '(:host "y.example.com") "wrong-host")
          ;; Host + port: entries whose URL embeds host:port match, and
          ;; portless-URL entries are accepted for any requested port.
          (keemacs-auth-test-assert
           db '(:host "x.example.com" :port 443)
           "target" "wrong-user" "bare-host" "same-host-only")
          (keemacs-auth-test-assert
           db '(:host "x.example.com" :port 465)
           "wrong-port" "bare-host" "same-host-only")
          ;; Host + user.
          (keemacs-auth-test-assert
           db '(:host "x.example.com" :user "alice")
           "target" "wrong-port" "bare-host")
          (keemacs-auth-test-assert
           db '(:host "y.example.com" :user "alice")
           "wrong-host")
          (keemacs-auth-test-assert
           db '(:host "x.example.com" :user "bob")
           "wrong-user")
          ;; Host + user + port.
          (keemacs-auth-test-assert
           db '(:host "x.example.com" :user "alice" :port 443)
           "target" "bare-host")
          (keemacs-auth-test-assert
           db '(:host "x.example.com" :user "bob" :port 443)
           "wrong-user")
          (keemacs-auth-test-assert
           db '(:host "x.example.com" :user "alice" :port 465)
           "wrong-port" "bare-host")
          ;; User alone.
          (keemacs-auth-test-assert
           db '(:user "alice")
           "target" "wrong-host" "wrong-port" "same-user-only" "bare-host")
          ;; User + title.
          (keemacs-auth-test-assert
           db '(:user "alice" :title "wrong")
           "wrong-host" "wrong-port")
          ;; Title alone: substring match on the title field.
          (keemacs-auth-test-assert
           db '(:title "host") "bare-host" "wrong-host" "same-host-only")
          ;; A port alone (no host) is not a realistic auth-source shape;
          ;; it matches nothing.
          (keemacs-auth-test-assert db '(:port 443)))
      (delete-file db))))

(ert-deftest keemacs-auth-integration-wrong-password-signals ()
  "A wrong master password is reported and the database skipped --
nil returned, the bad cached password evicted -- so an auth-source
walk over several databases continues to the next one."
  (skip-unless keemacs-auth-test-program)
  (let* ((db (keemacs-auth-test-make-db))
         (backend (auth-source-backend
                   :type 'keepass
                   :source db
                   :data (keemacs-auth-make-db-spec :file db)
                   :search-function #'keemacs-auth-source-search)))
    (unwind-protect
        (keemacs-auth--with-cli 'keepassxc
          (let ((password-cache-expiry nil))
            (password-cache-add db "WRONG"))
          (should-not (keemacs-auth-source-search
                       :backend backend :host "x.example.com" :user "alice" :port 443 :max 1))
          ;; The bad password was evicted.
          (should-not (password-in-cache-p db)))
      (delete-file db))))

;;;; Search-term builder: aliases map onto the 5 canonical fields

(ert-deftest keemacs-auth-term-aliases ()
  "The search term maps host/user/port onto canonical field prefixes, ANDed."
  (let ((keemacs-auth-cache-expiry nil))
    (should (equal "url:h.example.com"
                   (keemacs-auth-keepassxc-term
                    '(:host "h.example.com"))))
    ;; The port is deliberately NOT folded into the url term: entries often
    ;; store a bare host, and a "host:port" substring would exclude them
    ;; before the matcher can apply its port rules.
    (should (equal "url:h.example.com"
                   (keemacs-auth-keepassxc-term
                    '(:host "h.example.com" :port 443))))
    (should (equal "user:alice@example.com"
                   (keemacs-auth-keepassxc-term
                    '(:user "alice@example.com"))))
    (should (equal "title:My Title"
                   (keemacs-auth-keepassxc-term
                    '(:title "My Title"))))
    (should (equal "password:s3cret"
                   (keemacs-auth-keepassxc-term
                    '(:password "s3cret"))))
    (should (equal "notes:meeting"
                   (keemacs-auth-keepassxc-term
                    '(:notes "meeting"))))
    ;; A port without a host yields no term (port-only lookups are not a
    ;; realistic auth-source pattern and keepassxc can't scope a bare port).
    (should-not (keemacs-auth-keepassxc-term '(:port 443)))
    ;; Terms for all present keys are ANDed with a space, in a fixed order.
    (should (equal "url:h.example.com user:u@x.com title:T"
                   (keemacs-auth-keepassxc-term
                    '(:host "h.example.com" :user "u@x.com" :title "T"))))
    (should-not (keemacs-auth-keepassxc-term '()))))

;;;; Pure spec-matcher tests (no subprocess, all 5 canonical attributes)

(ert-deftest keemacs-auth-matcher-canonical-attributes ()
  "The matcher reads every canonical KeePass attribute from an entry plist."
  (let* ((spec '(:host "h.example.com" :user "u@e.com" :port 443
                        :title "T" :password "pw" :notes "N"))
         (entry '(:host "h.example.com:443" :user "u@e.com" :title "T"
                         :secret (lambda () "pw") :notes "N"))
         (m (keemacs-auth-keepassxc-spec-matcher spec)))
    (should (funcall m entry))
    ;; Each attribute on its own satisfied; vary one and it fails.
    (should-not (funcall (keemacs-auth-keepassxc-spec-matcher
                          (plist-put (copy-sequence spec) :host "other.example.com"))
                         entry))
    (should-not (funcall (keemacs-auth-keepassxc-spec-matcher
                          (plist-put (copy-sequence spec) :user "other@e.com"))
                         entry))
    (should-not (funcall (keemacs-auth-keepassxc-spec-matcher
                          (plist-put (copy-sequence spec) :title "Other"))
                         entry))
    (should-not (funcall (keemacs-auth-keepassxc-spec-matcher
                          (plist-put (copy-sequence spec) :password "nope"))
                         entry))
    (should-not (funcall (keemacs-auth-keepassxc-spec-matcher
                          (plist-put (copy-sequence spec) :notes "Nope"))
                         entry))))

(ert-deftest keemacs-auth-matcher-port-and-scheme ()
  "Host/port matching is general across bare host, host:port and scheme URLs."
  ;; Requesting host+port: an entry with host:port matches; a differing
  ;; explicit port is rejected; a bare host (no port) still matches.
  (let ((m (keemacs-auth-keepassxc-spec-matcher
            '(:host "smtp.gmail.com" :user "x" :port "465"))))
    (should (funcall m '(:host "smtp.gmail.com:465" :user "x")))
    (should-not (funcall m '(:host "smtp.gmail.com:995" :user "x")))
    (should (funcall m '(:host "smtp.gmail.com" :user "x")))
    (should (funcall m '(:host "https://smtp.gmail.com:465/" :user "x"))))
  ;; Requesting host only: a host:port or a scheme://host... URL both match.
  (let ((m (keemacs-auth-keepassxc-spec-matcher
            '(:host "smtp.gmail.com" :user "x"))))
    (should (funcall m '(:host "smtp.gmail.com" :user "x")))
    (should (funcall m '(:host "smtp.gmail.com:465" :user "x")))
    (should (funcall m '(:host "https://smtp.gmail.com/some/path" :user "x")))
    (should-not (funcall m '(:host "other.example" :user "x")))))

;;;; DB spec: constructor, accessors, predicate, normalization

(ert-deftest keemacs-auth-make-db-spec-constructor ()
  "`keemacs-auth-make-db-spec' builds a canonical keyword plist.
An omitted `:password' is NOT an explicit nil: the latter means a
database with no master password, while omission means `:prompt'."
  ;; All fields given; canonical key order is :name :file :keyfile :password :yubi :key.
  (should (equal '(:name "mydb" :file "db.kdbx" :keyfile "k.txt" :password "pw" :yubi "1:7370001" :key nil)
                 (keemacs-auth-make-db-spec :password "pw" :yubi "1:7370001"
                                       :name "mydb" :file "db.kdbx"
                                       :keyfile "k.txt")))
  ;; A :key is retained as given (character or one-character string).
  (should (eq ?p (keemacs-auth-db-spec-key (keemacs-auth-make-db-spec :file "d.kdbx" :key ?p))))
  (should (equal "w" (keemacs-auth-db-spec-key (keemacs-auth-make-db-spec :file "d.kdbx" :key "w"))))
  ;; Password omitted -> :prompt.
  (should (equal :prompt
                 (plist-get (keemacs-auth-make-db-spec :file "db.kdbx") :password)))
  ;; Password explicitly nil -> nil (genuinely no master password).
  (should (eq nil (plist-get (keemacs-auth-make-db-spec :file "db.kdbx" :password nil)
                             :password)))
  ;; Keyfile, yubi and name default to nil.
  (should-not (plist-get (keemacs-auth-make-db-spec :file "db.kdbx") :keyfile))
  (should-not (plist-get (keemacs-auth-make-db-spec :file "db.kdbx") :yubi))
  (should-not (plist-get (keemacs-auth-make-db-spec :file "db.kdbx") :name))
  ;; Unknown keywords are rejected up front.
  (should-error (keemacs-auth-make-db-spec :file "db" :bogus 1))
  ;; A spec must spell out a :file.
  (should-error (keemacs-auth-make-db-spec :password "pw")))

(ert-deftest keemacs-auth-db-spec-predicate ()
  "`keemacs-auth-db-spec-p' recognizes spec plists, not strings/lists/garbage."
  (should (keemacs-auth-db-spec-p
           (keemacs-auth-make-db-spec :file "db.kdbx" :password nil)))
  (should (keemacs-auth-db-spec-p '(:file "db.kdbx")))
  (should (keemacs-auth-db-spec-p '(:name "mydb" :file "db.kdbx")))
  ;; A positional list, a string, and a plist with an unknown keyword aren't.
  (should-not (keemacs-auth-db-spec-p "db.kdbx"))
  (should-not (keemacs-auth-db-spec-p '("db.kdbx" "k.txt")))
  (should-not (keemacs-auth-db-spec-p '(:file "db.kdbx" :bogus 1)))
  ;; A spec without :file is incomplete.
  (should-not (keemacs-auth-db-spec-p '(:keyfile "k.txt"))))

(ert-deftest keemacs-auth-db-spec-accessors ()
  "The `keemacs-auth-db-spec-*' accessors read the canonical fields."
  (let* ((kf (lambda () "k.txt"))
         (ps (lambda () "pw"))
         (spec (keemacs-auth-make-db-spec :file "db.kdbx" :keyfile kf
                                     :password ps :yubi "1:7" :name "mydb")))
    (should (equal "mydb" (keemacs-auth-db-spec-name spec)))
    (should (equal "db.kdbx" (keemacs-auth-db-spec-file spec)))
    (should (eq kf (keemacs-auth-db-spec-keyfile spec)))
    (should (eq ps (keemacs-auth-db-spec-password spec)))
    (should (equal "1:7" (keemacs-auth-db-spec-yubi spec)))))

(ert-deftest keemacs-auth-db-spec-normalize ()
  "A spec plist normalizes to the canonical plist; a bare string is rejected."
  ;; A bare file name is no longer a valid spec -- use `keemacs-auth-make-db-spec'.
  (should-error (keemacs-auth-db-spec-normalize "db.kdbx"))
  ;; A spec plist carries its fields through, re-canonicalized.
  (should (equal '(:name nil :file "d.kdbx" :keyfile nil :password nil :yubi "1:7" :key nil)
                 (keemacs-auth-db-spec-normalize (keemacs-auth-make-db-spec
                                             :file "d.kdbx" :password nil
                                             :yubi "1:7"))))
  ;; A :key survives normalization.
  (should (eq ?w (keemacs-auth-db-spec-key
                  (keemacs-auth-db-spec-normalize
                   (keemacs-auth-make-db-spec :file "d.kdbx" :key ?w)))))
  ;; A :name survives normalization.
  (should (equal "mydb"
                 (keemacs-auth-db-spec-name
                  (keemacs-auth-db-spec-normalize (keemacs-auth-make-db-spec
                                              :file "db.kdbx" :name "mydb")))))
  ;; Key file, password and yubi may be functions, retained as-is.
  (let ((kf (lambda () "k.txt")) (ps (lambda () "pw")) (ys (lambda () "1:7")))
    (should (equal (list :name nil :file "db.kdbx" :keyfile kf :password ps :yubi ys :key nil)
                   (keemacs-auth-db-spec-normalize (keemacs-auth-make-db-spec
                                               :file "db.kdbx" :keyfile kf
                                               :password ps :yubi ys)))))
  ;; The old positional list form is gone: a list that is not a spec plist
  ;; (nor a string) is rejected, not mistranslated.
  (should-error (keemacs-auth-db-spec-normalize '("db.kdbx" "k.txt" "pw")))
  (should-error (keemacs-auth-db-spec-normalize '("db.kdbx" "k.txt")))
  (should-error (keemacs-auth-db-spec-normalize '("db.kdbx")))
  ;; Garbage is rejected.
  (should-error (keemacs-auth-db-spec-normalize 42)))

(ert-deftest keemacs-auth-resolve-keyfile-password ()
  "Key file and password specs accept strings, functions, :prompt and nil."
  ;; Key file: string stays, function is called, nil is nil.
  (should (equal "k.txt" (keemacs-auth--resolve-keyfile "k.txt")))
  (should (equal "k.txt" (keemacs-auth--resolve-keyfile (lambda () "k.txt"))))
  (should-not (keemacs-auth--resolve-keyfile nil))
  ;; Password: string stays, function is called.
  (should (equal "pw" (keemacs-auth--resolve-password "pw" "db")))
  (should (equal "pw" (keemacs-auth--resolve-password (lambda () "pw") "db")))
  ;; nil means NO password, reported as the `:no-password' sentinel.
  (should (eq :no-password
              (keemacs-auth--resolve-password nil "nopwdb"))))

;;;; YubiKey argument generation

(ert-deftest keemacs-auth-yubi-args ()
  "`--yubikey' arguments are built from a string, a function, or nil."
  (should (equal '("--yubikey" "1:7370001")
                 (keemacs-auth--yubi-args "1:7370001")))
  (should (equal '("--yubikey" "2")
                 (keemacs-auth--yubi-args (lambda () "2"))))
  (should-not (keemacs-auth--yubi-args nil))
  (should-not (keemacs-auth--yubi-args 42))
  ;; The shared string-or-function resolver backs keyfile and yubi on the same
  ;; rules.
  (should (equal "s" (keemacs-auth--resolve-string "s")))
  (should (equal "s" (keemacs-auth--resolve-string (lambda () "s"))))
  (should-not (keemacs-auth--resolve-string nil)))

;;;; Negative-cache suppression

(ert-deftest keemacs-auth-suppress-negative-cache ()
  "`auth-source-remember' is a no-op for empty results when suppression is on."
  (let ((keemacs-auth-suppress-negative-cache t)
        (called nil))
    (unwind-protect
        (progn
          (advice-add 'auth-source-remember :around
                      #'keemacs-auth--remember-advice)
          ;; An empty FOUND must not be remembered.
          (keemacs-auth--remember-advice
           (lambda (_ _) (setq called t)) '(:host "x") nil)
          (should-not called)
          ;; A non-empty FOUND passes through.
          (let ((result))
            (setq result
                  (keemacs-auth--remember-advice
                   (lambda (_ found) found) '(:host "x") '(:secret "s")))
            (should (equal '(:secret "s") result))))
      (advice-remove 'auth-source-remember
                     #'keemacs-auth--remember-advice))))

(ert-deftest keemacs-auth-enable-idempotent ()
  "Calling `keemacs-auth-enable' twice installs the advice once and
does not error.  The second call used to crash with
\"wrong-type-argument listp\": `advice-member-p' returns the installed
advice's flist, not a list, and the old idempotence check fed it to
`memq'."
  (skip-unless (executable-find "keepassxc-cli"))
  (unwind-protect
      (progn
        (keemacs-auth-enable)
        (should (advice-member-p #'keemacs-auth--remember-advice
                                 'auth-source-remember))
        (keemacs-auth-enable)
        (should (advice-member-p #'keemacs-auth--remember-advice
                                 'auth-source-remember)))
    (advice-remove 'auth-source-remember
                   #'keemacs-auth--remember-advice)))

(ert-deftest keemacs-auth-parser-declines-non-keepass ()
  "The backend parser declines non-keepass `auth-sources' entries
silently.  Stock entries such as the \"~/.authinfo\" string are normal
in `auth-sources'; once this parser was registered it used to signal
`Invalid keepass database spec' on them, which broke every
auth-source search -- including mu4e/smtpmail password lookups."
  (let ((keemacs-auth--active-cli 'keepassxc)
        (keemacs-auth-cache-expiry nil))
    ;; The stock string entry, and assorted non-keepass shapes: all nil.
    (should-not (keemacs-auth-source-backend-parser "~/.authinfo"))
    (should-not (keemacs-auth-source-backend-parser '(:host "smtp.gmail.com")))
    (should-not (keemacs-auth-source-backend-parser '(mac . apple)))
    (should-not (keemacs-auth-source-backend-parser nil))
    ;; A non-kdbx keepass spec is still declined, without error.
    (should-not (keemacs-auth-source-backend-parser
                 (keemacs-auth-make-db-spec :file "/x.txt")))
    ;; A kdbx spec yields the backend, as always.
    (should (auth-source-backend-p
             (keemacs-auth-source-backend-parser
              (keemacs-auth-make-db-spec :file "/x.kdbx"))))))

(ert-deftest keemacs-auth-open-failure-errors ()
  "A missing database file is skipped silently; a present-but-locked
database is skipped with a message (so the next configured database
still gets asked) and the bad cached password is evicted."
  (let* ((spec (keemacs-auth-make-db-spec :file "/nope.kdbx" :password "pw"))
         (backend (auth-source-backend
                   :type 'keepass :source "/nope.kdbx"
                   :search-function #'keemacs-auth-source-search
                   :data spec))
         (missing-count 0))
    ;; File missing: skipped silently, no password prompt.
    (cl-letf (((symbol-function 'file-exists-p)
               (lambda (_) (setq missing-count (1+ missing-count)) nil)))
      (should-not (keemacs-auth-source-search
                   :backend backend :host "smtp.gmail.com")))
    (should (= 1 missing-count))
    ;; File present but locked: skipped, cache evicted.
    (password-cache-add "/nope.kdbx" "stale")
    (cl-letf (((symbol-function 'keemacs-auth--list-entries)
               (lambda (_entity _spec _password) '(nil :locked)))
              ((symbol-function 'file-exists-p) (lambda (_) t)))
      (should-not (keemacs-auth-source-search
                   :backend backend :host "smtp.gmail.com")))
    (should-not (password-in-cache-p "/nope.kdbx"))
    (password-cache-remove "/nope.kdbx")))

(ert-deftest keemacs-auth-empty-password-skips ()
  "An empty entry at the master-password prompt means the user chose
not to unlock: the database is skipped, returning nil -- never the
message string, which auth-source would mistake for a search result."
  (let* ((spec (keemacs-auth-make-db-spec :file "/nope.kdbx"))
         (backend (auth-source-backend
                   :type 'keepass :source "/nope.kdbx"
                   :search-function #'keemacs-auth-source-search
                   :data spec)))
    (cl-letf (((symbol-function 'file-exists-p) (lambda (_) t))
              ((symbol-function 'keemacs-auth--resolve-password)
               (lambda (_password-spec _entity _expiry) ""))
              ((symbol-function 'keemacs-auth--list-entries)
               (lambda (&rest _) (error "must not be reached"))))
      (should-not (keemacs-auth-source-search
                   :backend backend :host "smtp.gmail.com")))))

(ert-deftest keemacs-auth-remember-rejects-non-list ()
  "A non-list search result is never remembered, and the remember
cache is purged when one is seen.  A bug once returned a message
string as a result; auth-source remembered it and served it for every
subsequent search, breaking callers with
`let*: Wrong type argument: listp'."
  (let ((keemacs-auth-suppress-negative-cache t))
    (unwind-protect
        (progn
          (auth-source-forget-all-cached)
          (let ((remembered nil))
            (cl-letf (((symbol-function 'auth-source-remember)
                       (lambda (_spec found)
                         (setq remembered (cons found remembered)))))
              ;; Empty result: suppressed.
              (keemacs-auth--remember-advice
               #'auth-source-remember '(:host "x") nil)
              ;; A string result: rejected and never remembered.
              (keemacs-auth--remember-advice
               #'auth-source-remember '(:host "x")
               "keemacs: /x.kdbx skipped (no password entered)")
              ;; A well-formed list result passes through.
              (keemacs-auth--remember-advice
               #'auth-source-remember '(:host "x")
               (list (list :host "smtp.gmail.com" :secret (lambda () "s")))))
            ;; Only the well-formed list was remembered.
            (should (= 1 (length remembered)))
            (should (listp (car remembered)))))
      (auth-source-forget-all-cached))))

(ert-deftest keemacs-auth-multi-db-separate-caches ()
  "Multiple databases each answer their own queries, each master
password is cached separately, and entries with portless URLs are found
by searches that request a port."
  (let* ((keemacs-auth-cache-expiry nil)
        (keemacs-auth--active-cli 'keepassxc)
        (db1 (concat (file-name-as-directory (make-temp-file "one-" t)) "one.kdbx"))
        (db2 (concat (file-name-as-directory (make-temp-file "two-" t)) "two.kdbx"))
        (auth-sources (list (keemacs-auth-make-db-spec :file db1)
                                       (keemacs-auth-make-db-spec :file db2))))
    ;; Register the backend parser, as `keemacs-auth-enable' would:
    ;; without it auth-source parses the .kdbx entries with its default
    ;; (netrc) parser and the search finds no usable backend.
    (add-hook 'auth-source-backend-parser-functions
              #'keemacs-auth-source-backend-parser)
    (unless (executable-find "keepassxc-cli")
      (ert-skip "keepassxc-cli not available"))
    (unwind-protect
        (progn
          (with-temp-buffer
            (call-process "sh" nil t nil "-c"
                          (concat "printf 'PASSONE\\nPASSONE\\n' | keepassxc-cli db-create -q "
                                  (shell-quote-argument db1) " --set-password"))
            (call-process "sh" nil t nil "-c"
                          (concat "printf 'PASSONE\\n' | keepassxc-cli add -q "
                                  (shell-quote-argument db1)
                                  " smtp -u me@one --url smtp.one.example"))
            (call-process "sh" nil t nil "-c"
                          (concat "printf 'PASSTWO\\nPASSTWO\\n' | keepassxc-cli db-create -q "
                                  (shell-quote-argument db2) " --set-password"))
            (call-process "sh" nil t nil "-c"
                          (concat "printf 'PASSTWO\\n' | keepassxc-cli add -q "
                                  (shell-quote-argument db2)
                                  " smtp -u me@two --url smtp.two.example")))
          (should (file-exists-p db1))
          (should (file-exists-p db2))
          (let ((password-cache-expiry nil))
            (password-cache-add db1 "PASSONE")
            (password-cache-add db2 "PASSTWO")
            (let* ((r1 (auth-source-search :host "smtp.one.example" :user "me@one"
                                           :port "465" :require '(:secret)))
                   (r2 (auth-source-search :host "smtp.two.example" :user "me@two"
                                           :port "465" :require '(:secret))))
              (should (= 1 (length r1)))
              (should (equal "me@one" (plist-get (car r1) :user)))
              (should (= 1 (length r2)))
              (should (equal "me@two" (plist-get (car r2) :user)))))
          (should (equal "PASSONE" (password-read-from-cache db1)))
          (should (equal "PASSTWO" (password-read-from-cache db2))))
      (ignore-errors (delete-file db1))
      (ignore-errors (delete-file db2)))))

(ert-deftest keemacs-auth-keyfile-honoured ()
  "A database secured by both a master password and a key file is found
when the auth-sources entry lists the key file.  The password may come
from a string or from a function in the spec."
  (let* ((keemacs-auth-cache-expiry nil)
        (keemacs-auth--active-cli 'keepassxc)
        (dir (make-temp-file "kpa-kf-" t))
        (db (concat (file-name-as-directory (make-temp-file "kf-db-" t)) "db.kdbx"))
        (keyfile (concat (file-name-as-directory dir) "key.txt")))
    (unless (executable-find "keepassxc-cli")
      (ert-skip "keepassxc-cli not available"))
    (with-temp-file keyfile (insert "STANDARD-KEY-FILE-SECRET"))
    (add-hook 'auth-source-backend-parser-functions
              #'keemacs-auth-source-backend-parser)
    (unwind-protect
        (progn
          (with-temp-buffer
            ;; Create a DB that requires both the key file and the password.
            (call-process "sh" nil t nil "-c"
                          (concat "printf 'KEYPW\\nKEYPW\\n' | keepassxc-cli db-create -q "
                                  (shell-quote-argument db)
                                  " --set-key-file " (shell-quote-argument keyfile)
                                  " --set-password"))
            (call-process "sh" nil t nil "-c"
                          (concat "printf 'KEYPW\\n' | keepassxc-cli add -q "
                                  "--key-file " (shell-quote-argument keyfile) " "
                                  (shell-quote-argument db)
                                  " kf -u me@kf --url kf.example")))
          (should (file-exists-p db))
          (let ((password-cache-expiry nil))
            ;; Password as a string in the spec: no user prompt, key file
            ;; passed on every CLI call.
            (let ((auth-sources (list (keemacs-auth-make-db-spec
                                       :file db :keyfile keyfile :password "KEYPW"))))
              (let ((res (auth-source-search :host "kf.example" :user "me@kf"
                                             :port "465" :require '(:secret))))
                (should (= 1 (length res)))
                (should (equal "me@kf" (plist-get (car res) :user)))
                (should (stringp (funcall (plist-get (car res) :secret))))))
            ;; Password as a function; same search still works.
            (let ((auth-sources (list (keemacs-auth-make-db-spec
                                       :file db :keyfile keyfile
                                       :password (lambda () "KEYPW")))))
              (let ((res (auth-source-search :host "kf.example" :user "me@kf"
                                             :port "465" :require '(:secret))))
                (should (= 1 (length res))))))
          ;; A wrong key file means keepassxc-cli cannot open the DB: the
          ;; failed open is reported as a wrong credential and the lookup
          ;; skips the database (nil), so a walk over several databases
          ;; continues.  (A distinct host avoids the auth-source success
          ;; cache from the earlier searches masking the failure.)
          (let ((auth-sources (list (keemacs-auth-make-db-spec
                                     :file db :keyfile "/nonexistent-key.txt"
                                     :password "KEYPW"))))
            (let ((password-cache-expiry nil))
              (should-not (auth-source-search :host "other.example" :user "me@kf"
                                              :port "465" :require '(:secret))))))
      (ignore-errors (delete-file db))
      (ignore-errors (delete-file keyfile))
      (ignore-errors (delete-directory dir)))))

(ert-deftest keemacs-auth-plist-spec-integration ()
  "A keyword spec plist in `auth-sources' drives a working search.
Exercises `keemacs-auth-make-db-spec' through the backend parser and the full
search path (password supplied as a string in the spec)."
  (let* ((keemacs-auth-cache-expiry nil)
         (keemacs-auth--active-cli 'keepassxc)
         (db (concat (file-name-as-directory (make-temp-file "spec-db-" t)) "db.kdbx")))
    (unless (executable-find "keepassxc-cli")
      (ert-skip "keepassxc-cli not available"))
    (with-temp-buffer
      (call-process "sh" nil t nil "-c"
                    (concat "printf 'SPECPW\\nSPECPW\\n' | keepassxc-cli db-create -q "
                            (shell-quote-argument db) " --set-password"))
      (call-process "sh" nil t nil "-c"
                    (concat "printf 'SPECPW\\n' | keepassxc-cli add -q "
                            (shell-quote-argument db)
                            " spec -u me@spec --url spec.example")))
    (add-hook 'auth-source-backend-parser-functions
              #'keemacs-auth-source-backend-parser)
    (unwind-protect
        (let ((password-cache-expiry nil)
              (auth-sources (list (keemacs-auth-make-db-spec :file db :password "SPECPW"))))
          (let ((res (auth-source-search :host "spec.example" :user "me@spec"
                                         :port "465" :require '(:secret))))
            (should (= 1 (length res)))
            (should (equal "me@spec" (plist-get (car res) :user)))
            (should (stringp (funcall (plist-get (car res) :secret))))))
      (ignore-errors (delete-file db)))))

(provide 'keemacs-auth-test)
;;; keemacs-auth-test.el ends here