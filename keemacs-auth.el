;;; keemacs-auth-auth.el --- KeePass auth-source backend for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2026 Chris Bitmead

;; Author: Chris Bitmead <xpusostomos@gmail.com>
;; Maintainer: Chris Bitmead <xpusostomos@gmail.com>
;; Package-Requires: ((emacs "27.1") (consult "0.1") (embark "0.1") (embark-consult "0.1"))
;; Keywords: keepass auth-source passwords
;; URL: https://github.com/xpusostomos/keemacs
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Adds KeePass support to auth-source.

;; This package can talk to a KeePass database using one of two command
;; line backends:
;;
;;   - `kpscript'     KeePass's KPScript.exe.  This is the *Windows-only*
;;                    scripting plugin that drives an installed KeePass.
;;   - `keepassxc'    keepassxc-cli, KeePassXC's cross-platform command
;;                    line client.  Works on GNU/Linux, macOS and Windows.
;;
;; The backend is chosen automatically by `keemacs-auth-enable',
;; preferring keepassxc-cli when it is available.  The active backend can
;; be forced via `keemacs-auth-cli', and the path to each external
;; program can be customized with `keemacs-auth-*-program'.

;;; Code:
(require 'auth-source)
(require 'cl-lib)
(require 'password-cache)
(require 'seq)
(require 'simple)
(require 'url-parse)

;;; Portability helpers (previously provided by dash.el / s.el, kept local
;;; so this package can run with zero external dependencies).

(defun keemacs-auth-s-contains-p (needle haystack &optional ignore-case)
  "Return t if NEEDLE is contained in HAYSTACK, else nil.
When IGNORE-CASE is non-nil, comparison is case-insensitive."
  (let ((case-fold-search (if ignore-case t case-fold-search)))
    (and (string-match-p (regexp-quote needle) haystack) t)))

;;;###autoload
(defcustom keemacs-auth-cache-expiry 7200
  "How many seconds the KeePass database password is cached,
or nil to disable expiry."
  :type '(choice (const :tag "Never" nil)
          (const :tag "All Day" 86400)
          (const :tag "2 Hours" 7200)
          (const :tag "30 Minutes" 1800)
          (integer :tag "Seconds"))
  :group 'keemacs)

(defcustom keemacs-auth-match-title t
  "If the `title' argument passed to `auth-source-search' should select an entry.
Entries matching `title' are selected if one and only one entry matches on
`url'.  If no entries match but several are found, prompt the user to pick."
  :type 'boolean
  :group 'keemacs)

(defcustom keemacs-auth-cli 'auto
  "Which external KeePass client backend to use.

`auto'     Pick a backend based on which executables are available
           (preferring `keepassxc' over `kpscript').
`keepassxc' Use keepassxc-cli (cross-platform).
`kpscript' Use KeePass's KPScript.exe (Windows-only)."
  :type '(choice (const :tag "Auto-detect" auto)
                 (const :tag "keepassxc-cli" keepassxc)
                 (const :tag "KPScript (KeePass, Windows)" kpscript))
  :group 'keemacs)

(defcustom keemacs-auth-keepassxc-cli-program "keepassxc-cli"
  "The keepassxc-cli executable, or path to it.
If a bare name it is looked up on `exec-path'."
  :type 'string
  :group 'keemacs)

(defcustom keemacs-auth-kpscript-program "kpscript"
  "The KPScript executable (KeePass scripting plugin, Windows).
If a bare name it is looked up on `exec-path'."
  :type 'string
  :group 'keemacs)

(defcustom keemacs-auth-keepass-program "keepass"
  "The KeePass executable (Windows, required by KPScript).
If a bare name it is looked up on `exec-path'."
  :type 'string
  :group 'keemacs)

(defcustom keemacs-auth-verbose nil
  "If non-nil, log each keepassxc-cli invocation to the *Messages* buffer.
Useful for debugging why a lookup narrows the way it does."
  :type 'boolean
  :group 'keemacs)

(defcustom keemacs-auth-suppress-negative-cache t
  "Whether to keep `auth-source' from caching failed lookups.

`auth-source' normally remembers every search result -- including an empty
one -- for `auth-source-cache-expiry' seconds.  A single failure (the
database is locked, the password was wrong, keepassxc-cli was momentarily
unavailable) therefore keeps every later lookup returning nil long after
the cause is gone, and makes configuration changes look as if they had no
effect.

When non-nil (the default), only non-empty results are cached; a miss is
never remembered, so the next lookup always really queries the database."
  :type 'boolean
  :group 'keemacs)

;; The backend actually in use (resolved from `keemacs-auth-cli').
(defvar keemacs-auth--active-cli nil)

(defun keemacs-auth--parse-auth (auth-string port)
  (save-match-data
    (with-temp-buffer
      (insert auth-string)
      (goto-char (point-min))
      (let ((result `(:port ,port))
            (mappings '(:url :host
                        :username :user
                        :password :secret)))
        (while (search-forward-regexp "^S: \\(.*\\) = \\(.*\\)$" nil t)
          (let* ((key (intern (concat ":" (downcase (match-string 1)))))
                 (key (or (plist-get mappings key) key))
                 (value (match-string 2))
                 (value (if (eq :secret key) `(lambda () ,value) value)))
            (setq result (plist-put result key value))))
        result))))

(defun keemacs-auth--parse (output port)
  (let* ((results (split-string output "\n\n"))
         (status (car results))
         (auths (mapcar (lambda (it) (keemacs-auth--parse-auth it port))
                        (butlast results))))
    `(,auths ,status)))

(defun keemacs-auth--resolve-cli ()
  "Resolve the backend to use.
Returns `kpscript' or `keepassxc' by honoring `keemacs-auth-cli'.
For the `auto' value, keepassxc-cli is preferred when available, falling
back to KPScript.  Returns nil if no suitable backend is found."
  (pcase keemacs-auth-cli
    ('auto
     (cond ((executable-find keemacs-auth-keepassxc-cli-program) 'keepassxc)
           ((and (executable-find keemacs-auth-keepass-program)
                 (executable-find keemacs-auth-kpscript-program)) 'kpscript)
           (t nil)))
    (other other)))

;;; KPScript backend (Windows)

(defun keemacs-auth--kpscript-command (entity user url password)
  "Return the KPScript list-entries command for ENTITY filtered by USER and URL."
  (mapconcat #'identity
             (list
              (shell-quote-argument keemacs-auth-kpscript-program)
              "-C:ListEntries"
              (format "\"%s\"" (expand-file-name entity))
              (format "-ref-Username:\"%s\"" (or user ""))
              (format "-ref-URL:\"//%s//\"" url)
              (format "-pw:\"%s\"" password))
             " "))

;;; keepassxc-cli backend (cross-platform)

(defun keemacs-auth--keepassxc-executable ()
  "Return the resolved path to keepassxc-cli, else its configured name."
  (or (executable-find keemacs-auth-keepassxc-cli-program)
      keemacs-auth-keepassxc-cli-program))

(defun keemacs-auth--keepassxc-parse (show-output port)
  "Parse a single `keepassxc-cli show' summary into an auth plist.
The `Key: value' lines are turned into the same `S: Key = value' shape
produced by KPScript so `keemacs-auth--parse-auth' can map them
identically (URL -> :host, UserName -> :user, Password -> :secret, ...).
A password line reading literally \"PROTECTED\" means keepassxc-cli did
not reveal the secret, so it is dropped rather than stored as the actual
password."
  (keemacs-auth--parse-auth
   (mapconcat #'identity
              (mapcar
               (lambda (line)
                 (when (and (string-match "^\\([^:]+\\)[[:space:]]*:[[:space:]]*\\(.*\\)$" line)
                            (not (and (string-equal "Password" (string-trim (match-string 1 line)))
                                      (string-equal "PROTECTED" (string-trim (match-string 2 line))))))
                   (format "S: %s = %s" (string-trim (match-string 1 line))
                           (match-string 2 line))))
               (split-string show-output "\n"))
              "\n")
   port))

;;;; Database specification
;;
;; Databases are described by a keyword *spec* plist built with
;; `keemacs-auth-make-db-spec', read with the `keemacs-auth-db-spec-*' accessors, and
;; stored in canonical key order.  Because the spec is a plain plist (not a
;; `cl-defstruct'), `equal' comparisons, printing and Customize's `plist'
;; widget all work on it directly; the accessors hide the key order, so the
;; representation could be swapped for a struct later without changing any
;; caller.

(defconst keemacs-auth-db-spec-keys '(:name :file :keyfile :password :yubi :key)
  "The keywords of a keepass database spec, in canonical order.")

(define-widget 'keemacs-auth-db-spec 'plist
  "Customize widget for a keepass database specification keyword plist.
See `keemacs-auth-make-db-spec' for the meaning of each keyword.  The value
type is a lenient catch-all (a value may be a string, a function, the
symbol `:prompt', or nil) because which shape is valid depends on the
keyword."
  :key-type '(choice (const :name)
                     (const :file)
                     (const :keyfile)
                     (const :password)
                     (const :yubi)
                     (const :key))
  :value-type '(choice (const :prompt)
                       (const :tag "none" nil)
                       (string :tag "text")
                       function))

(defun keemacs-auth-make-db-spec (&rest spec-plist)
  "Build a keepass database spec keyword plist from SPEC-PLIST.

Accepted keywords (see `keemacs-auth-db-spec-keys'):
  :name      a short, user-visible label for the database.  Used by
             `keemacs' to name the database; omitted means the
             file name is used where a name is shown.
  :file      the kdbx file path (string).  The only required keyword.
  :keyfile   the key file: a file name, a no-argument function returning
             one, or nil for none.
  :password  the master password: a string, a no-argument function
             returning one, `:prompt' to ask the user, or nil for a
             database with no master password.
             Omitted means `:prompt'.
  :yubi      a YubiKey: a keepassxc-cli \"slot[:serial]\" string (e.g.
             \"1:7370001\"), a no-argument function returning one, or nil
             for none.
  :key       a hotkey for the database, used by
             `keemacs-select-database-by-key': a character, a one-character
             string, or a no-argument function returning one.  Omitted
             means the key is assigned automatically in that menu.

An absent `:password' is NOT the same as an explicit nil -- nil means the
database genuinely has no master password.  The result is returned in
canonical key order so `equal' comparisons are order-independent."
  (let* ((name (plist-get spec-plist :name))
         (file (plist-get spec-plist :file))
         (keyfile (plist-get spec-plist :keyfile))
         (password (if (plist-member spec-plist :password)
                       (plist-get spec-plist :password)
                     :prompt))
         (yubi (plist-get spec-plist :yubi))
         (key (plist-get spec-plist :key)))
    ;; Reject unknown keywords, walking only the key positions: values may
    ;; themselves be keywords (e.g. `:password :prompt') and must not be
    ;; mistaken for keys.
    (let ((tail spec-plist))
      (while tail
        (let ((key (car tail)))
          (when (and (keywordp key)
                     (not (memq key keemacs-auth-db-spec-keys)))
            (user-error "Unknown keepass database spec keyword: %S" key)))
        (setq tail (cdr tail))
        (when tail (setq tail (cdr tail)))))
    (unless (stringp file)
      (user-error "keepass database spec requires a `:file' keyword"))
    (list :name name :file file :keyfile keyfile :password password :yubi yubi :key key)))

(defun keemacs-auth-db-spec-p (spec)
  "Return non-nil if SPEC is a keepass database spec keyword plist.
A spec is a plist whose own keywords all belong to
`keemacs-auth-db-spec-keys' and which spells out a `:file'.  Values may be any
Lisp object."
  ;; A dotted cons such as `(mac . apple)' is a cons but not a plist;
  ;; `plist-member' would signal on it.
  (and (consp spec)
       (proper-list-p spec)
       (plist-member spec :file)
       (let ((tail spec) (ok t))
         ;; Walk only the key (odd) positions.
         (while (and ok tail)
           (unless (memq (car tail) keemacs-auth-db-spec-keys)
             (setq ok nil))
           (setq tail (cdr tail))
           (when tail (setq tail (cdr tail))))
         ok)))

(defun keemacs-auth-db-spec-name (spec)
  "Return the user-visible name of database SPEC, or nil if absent.
When nil, callers fall back to the database file name."
  (plist-get spec :name))

(defun keemacs-auth-db-spec-file (spec)
  "Return the kdbx file path of database spec SPEC."
  (plist-get spec :file))

(defun keemacs-auth-db-spec-keyfile (spec)
  "Return the key file spec of database SPEC, or nil.
A file-name string, a no-argument function, or nil."
  (plist-get spec :keyfile))

(defun keemacs-auth-db-spec-password (spec)
  "Return the password spec of database SPEC.
A string, a no-argument function, `:prompt' (ask the user), or nil (no
master password)."
  (plist-get spec :password))

(defun keemacs-auth-db-spec-yubi (spec)
  "Return the YubiKey spec of database SPEC, or nil.
A keepassxc-cli \"slot[:serial]\" string, a no-argument function, or nil."
  (plist-get spec :yubi))

(defun keemacs-auth-db-spec-key (spec)
  "Return the hotkey spec of database SPEC, or nil.
A character, a one-character string, a no-argument function returning
one, or nil -- nil meaning the key is assigned automatically in
`keemacs-select-database-by-key'."
  (plist-get spec :key))

(defun keemacs-auth-db-spec-key-char (spec)
  "Return the hotkey of database SPEC as a character, or nil.
Resolves every form `keemacs-auth-db-spec-key' documents: a character
passes through, a one-character string is coerced, a no-argument
function is called.  nil means the key is assigned automatically in
`keemacs-select-database-by-key'."
  (let ((raw (keemacs-auth-db-spec-key spec)))
    (cond ((characterp raw) raw)
          ((and (stringp raw) (= (length raw) 1)) (aref raw 0))
          ((functionp raw) (funcall raw)))))

(defun keemacs-auth-db-spec-normalize (spec)
  "Coerce SPEC into a canonical keepass database spec plist.

SPEC is one database entry as it may appear in `auth-sources' or
`keemacs-databases': a keyword plist built with
`keemacs-auth-make-db-spec'.

Returns a plist with the keys of `keemacs-auth-db-spec-keys'.  Read it with
the `keemacs-auth-db-spec-*' accessors."
  (if (keemacs-auth-db-spec-p spec)
      (apply #'keemacs-auth-make-db-spec spec)
    (user-error "Invalid keepass database spec: %S -- use `keemacs-auth-make-db-spec' to build a spec plist" spec)))

(defun keemacs-auth--no-password (db)
  "Return whether DB has no master password, honouring a cached answer.
A database created with `--no-password' has no master password; the
password cache keyed by DB records a yes-or-no answer."
  (let ((password-cache-expiry (or keemacs-auth-cache-expiry nil)))
    (and (password-in-cache-p db)            ; cached at all
         (not (car (password-read-from-cache db))))))

(defun keemacs-auth--resolve-password (password-spec db &optional expiry)
  "Return the master password for DB from PASSWORD-SPEC.
Returns a string (the password), or the symbol `:no-password' meaning the
database has no master password and `--no-password' must be passed to
keepassxc-cli.  The cases:
  `:prompt'  -> ask the user (and cache) as usual, returning the typed
      string;
  a string     -> used as-is;
  a function   -> called to obtain its result;
  nil          -> the database has no master password; return `:no-password'."
  (pcase password-spec
    ((pred stringp) password-spec)
    ((pred functionp) (let ((v (funcall password-spec)))
                        (if v v :no-password)))
    (:prompt (keemacs-auth--read-password db expiry))
    (_ :no-password)))

(defun keemacs-auth--resolve-string (value)
  "Resolve VALUE to a string, or nil.
A string is returned as-is, a no-argument function is called for its
result, and anything else (including nil) means nil."
  (pcase value
    ((pred stringp) value)
    ((pred functionp) (funcall value))
    (_ nil)))

(defun keemacs-auth--resolve-keyfile (keyfile)
  "Resolve a key file specification KEYFILE to a file name, or nil.
A string is the file name; a function is called to obtain it; anything
else (including nil) means no key file."
  (keemacs-auth--resolve-string keyfile))

(defun keemacs-auth--read-password (db &optional expiry)
  "Read the master password for database DB, caching it for reuse.
The cache entry is keyed by DB, so multiple databases each keep their own
master password.  EXPIRY defaults to `keemacs-auth-cache-expiry'.
Returns the password."
  (let* ((prompt (format "Keepass password (%s): " db))
         (password-cache-expiry (or expiry keemacs-auth-cache-expiry))
         (password (cond
                    ((password-read-from-cache db))
                    ((password-read prompt db)))))
    ;; An empty entry means the user declined to unlock: don't cache
    ;; that, or the database could never be unlocked this session.
    (unless (string-empty-p password)
      (password-cache-add db password))
    password))

(defun keemacs-auth--keyfile-args (keyfile)
  "Return the keepassxc-cli arguments for key file KEYFILE.
KEYFILE is a file-name string, a no-argument function returning one, or
nil for no key file.  Returns (\"--key-file\" FILE) or nil, with FILE
expanded so a leading \"~\" works."
  (when-let* ((file (keemacs-auth--resolve-keyfile keyfile)))
    (list "--key-file" (expand-file-name file))))

(defun keemacs-auth--yubi-args (yubi)
  "Return the keepassxc-cli arguments for YubiKey YUBI, or nil.
YUBI is a keepassxc-cli \"slot[:serial]\" string (e.g. \"1:7370001\"), a
no-argument function returning one, or nil for no YubiKey.  Returns
\(\"--yubikey\" VALUE), ready to splice into a keepassxc-cli invocation."
  (when-let* ((y (keemacs-auth--resolve-string yubi)))
    (list "--yubikey" y)))

(defun keemacs-auth--log (format-string &rest args)
  "Append a line to the *keemacs-auth-log* buffer (read-only).
The buffer is never displayed automatically and nothing is echoed to
*Messages*; the user can open it with \\[switch-to-buffer] when curious."
  (let ((line (apply #'format format-string args)))
    (with-current-buffer (get-buffer-create "*keemacs-auth-log*")
      (setq buffer-read-only t)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert line "\n")))))

(defun keemacs-auth--no-password-flag (password)
  "Return the keepassxc-cli global option for a password-less database, or nil.
PASSWORD is the resolved master password; the symbol `:no-password' means
the database has no master password and keepassxc-cli must be told so with
its `--no-password' option instead of reading stdin."
  (if (eq password :no-password) (list "--no-password") nil))

(defun keemacs-auth--keepassxc-run (password &rest args)
  (when keemacs-auth-verbose
    (keemacs-auth--log "keepassxc-cli %s"
                              (mapconcat #'identity args " ")))
  (let* ((prog (keemacs-auth--keepassxc-executable))
         (out-buf (generate-new-buffer " *keemacs-auth-out*"))
         (err-file (make-temp-file "keemacs-auth-err")))
    (unwind-protect
        (with-temp-buffer
          ;; Feed PASSWORD (plus a terminating newline, as interactive
          ;; keepassxc-cli reads input line-by-line) to the child's stdin,
          ;; unless the database has no password at all.
          (unless (eq password :no-password)
            (insert (or password "") "\n"))
          (let ((exit (apply #'call-process-region
                             (point-min) (point-max)
                             prog t (list out-buf err-file) nil
                             args)))
            ;; Output was appended into OUT-BUF and stderr into ERR-FILE;
            ;; return them merged with the exit code.  stderr carries the
            ;; error messages -- notably the ones --quiet suppresses on
            ;; stdout, and everything on a wrong master password.
            (cons (concat (with-current-buffer out-buf (buffer-string))
                          (condition-case nil
                              (with-temp-buffer
                                (insert-file-contents err-file)
                                (buffer-string))
                            (error nil)))
                  exit)))
      (ignore-errors (delete-file err-file))
      (kill-buffer out-buf))))

(defun keemacs-auth--keepassxc-run-stdin (stdin &rest args)
  "Run keepassxc-cli ARGS feeding STDIN (a string) on standard input.
Like `keemacs-auth--keepassxc-run', for commands that need more on
standard input than the database password alone (e.g. `add'/`edit' with
the new entry's password).  Returns (OUTPUT . EXIT)."
  (let* ((prog (keemacs-auth--keepassxc-executable))
         (out-buf (generate-new-buffer " *keemacs-auth-out*")))
    (unwind-protect
        (with-temp-buffer
          (when keemacs-auth-verbose
            (keemacs-auth--log "keepassxc-cli %s"
                                      (mapconcat #'identity args " ")))
          (insert stdin)
          (let ((exit (apply #'call-process-region
                             (point-min) (point-max)
                             prog t (list out-buf) nil
                             args)))
            (cons (with-current-buffer out-buf (buffer-string)) exit)))
      (kill-buffer out-buf))))

(defun keemacs-auth--error (output &optional db exit-code)
  "Signal an error describing a failed keepassxc-cli run (OUTPUT).
DB, when given, is the database whose cached master password should be
dropped when the credentials were wrong; EXIT-CODE is the run's exit
status, which distinguishes an empty-output unlock failure from other
failures."
  (let ((msg (string-trim output)))
    (cond
     ((string-match-p "Invalid credentials were provided" msg)
      (when db (password-cache-remove db))
      (user-error "Incorrect master password"))
     ;; --quiet makes keepassxc-cli print nothing at all on a failed
     ;; unlock -- no prompt, no error -- so empty output with a nonzero
     ;; exit is a wrong master password (or an unreadable database) in
     ;; practice.  Treat it as such: the stale cached password is evicted,
     ;; so the next lookup re-prompts instead of looping forever.
     ((and db (/= 0 exit-code) (string-empty-p msg))
      (when db (password-cache-remove db))
      (user-error "Could not unlock the database (wrong master password?) -- the cached password was cleared, try again"))
     (t (user-error "keepassxc-cli failed: %s"
                    (if (> (length msg) 0) msg "unknown error"))))))

(defun keemacs-auth-keepassxc-term (spec)
  "Return the keepassxc-cli `search' query for SPEC, or nil.

keepassxc-cli's `search' accepts multiple space-separated terms ANDed
together, each scoped to one of the five canonical fields (title, user,
password, url, notes) with a \\='field:keyword\\=' prefix, matched as a
substring.  This is a deliberate lossless pre-filter: it returns a small
candidate set without assuming which combination of keys the caller passed;
the candidate set is then filtered precisely against every present key.

Each auth-source key maps to one canonical field:
  host     -> url:HOST
  user     -> user:USER
  title    -> title:TITLE
  password -> password:PASSWORD
  notes    -> notes:NOTES

The port is deliberately NOT folded into the url term: many entries store a
bare host or a scheme://host URL, and a \"host:port\" substring would
exclude them before the precise matcher ever runs.  The matcher enforces
port semantics instead.

All present keys are joined with spaces, so a search for \"host=A, user=B\"
runs the single command: A-AND-B in one keepassxc-cli call."
  (let* ((host (plist-get spec :host))
         (user (plist-get spec :user))
         (title (plist-get spec :title))
         (password (plist-get spec :password))
         (notes (plist-get spec :notes))
         (terms
          (delq nil
                (list
                 (and host (not (string-blank-p host))
                      (format "url:%s" host))
                 (and user (not (string-blank-p user))
                      (format "user:%s" user))
                 (and title (not (string-blank-p title))
                      (format "title:%s" title))
                 (and password (not (string-blank-p password))
                      (format "password:%s" password))
                 (and notes (not (string-blank-p notes))
                      (format "notes:%s" notes))))))
    (when terms
      (mapconcat #'identity terms " "))))

(defun keemacs-auth--strip-scheme (url)
  "Return URL without a leading \\='scheme://\\=' (or \\='scheme:\\='), lowercased.
Only a full scheme (ending in \"://\") is stripped; a bare \"host:port\" is
left intact, since \"smtp.gmail.com\" is not a scheme."
  (let ((u (downcase (or url ""))))
    (if (string-match "\\`[a-z][a-z0-9+.-]*://" u)
        (substring u (match-end 0))
      u)))

(defun keemacs-auth-keepassxc-spec-matcher (spec)
  "Return a predicate matching an entry plist against SPEC.

Applies host/user/port to the canonical KeePass fields:
  host        -> the URL's host (scheme/path stripped) contains \"host\"
  host+port   -> additionally, an explicit \"host:port\" in the URL must
                 match the requested port (a URL that spells no port is
                 also accepted)
  user        -> UserName equals \"user\"
  title, password, notes -> their canonical KeePass fields."
  (let ((host (plist-get spec :host))
        (port (plist-get spec :port))
        (user (plist-get spec :user))
        (title (plist-get spec :title))
        (password (plist-get spec :password))
        (notes (plist-get spec :notes)))
    (lambda (entry)
      (let* ((e-raw (or (plist-get entry :host) ""))
             (e-host (keemacs-auth--strip-scheme e-raw))
             (e-user (or (plist-get entry :user) ""))
             (e-password (or (plist-get entry :secret) ""))
             (e-title (or (plist-get entry :title) ""))
             (e-notes (or (plist-get entry :notes) "")))
        (and
         (or (string-blank-p (or host ""))
             ;; The entry's URL must contain the requested host (covers bare
             ;; "host", "host:port" and full "scheme://host.../path" URLs).
             (let ((h (keemacs-auth--strip-scheme host)))
               (and (keemacs-auth-s-contains-p h e-host t)
                    (or (null port)
                        (string-blank-p (format "%s" port))
                        ;; A requested port must be honored.
                        (keemacs-auth-s-contains-p
                         (format "%s:%s" h port) e-host t)
                        ;; ...or the URL spells no explicit port at all (a
                        ;; bare host), which we accept for a portless record.
                        (not (string-match-p ":" e-host))))))
         (or (string-blank-p (or user ""))     ; user matches UserName
             (string-equal user e-user))
         (or (string-blank-p (or password "")) ; password matches
             (and (functionp e-password)
                  (string-equal password (funcall e-password))))
         (or (string-blank-p (or title ""))    ; title matches
             (keemacs-auth-s-contains-p title e-title t))
         (or (string-blank-p (or notes ""))    ; notes matches
             (keemacs-auth-s-contains-p notes e-notes t)))))))

(defun keemacs-auth--keepassxc-narrow (entity password term &optional keyfile yubi)
  "Return the entry paths in ENTITY whose any field contains TERM.
Uses the server-side `search' command so only a handful of candidates
are returned, instead of every entry in the database.  KEYFILE, when
non-nil, is the database's key file (see
`keemacs-auth--keyfile-args'); YUBI, likewise, is its YubiKey spec
\(see `keemacs-auth--yubi-args')."
  (let* ((run (apply #'keemacs-auth--keepassxc-run
                     password
                     (append (list "search" "--quiet")
                             (keemacs-auth--no-password-flag password)
                             (keemacs-auth--keyfile-args keyfile)
                             (keemacs-auth--yubi-args yubi)
                             (list entity term))))
         (output (car run))
         (exit (cdr run)))
    (when (eq exit 0)
      (seq-filter (lambda (s) (not (string-blank-p s)))
        (split-string output "\n" t)))))

(defun keemacs-auth--keepassxc-locked-p (status)
  "Return non-nil if STATUS indicates a wrong master password.
STATUS is either the sentinel `:locked' or a raw output string."
  (or (eq status :locked)
      (and (stringp status)
           (string-match-p
            "Invalid credentials were provided\\|Error while reading the database\\|Failed to open"
            status))))

(defun keemacs-auth--keepassxc-list-entries (spec password)
  "Collect entries matching SPEC, using keepassxc-cli.
SPEC is the raw `auth-source-search' plist (host/user/port/title/...).
Uses the server-side `search' command (aliased: host->URL, host+port->
URL \"host:port\", user->UserName) to narrow down the database to a small
candidate set, then `show's only those candidates and filters them against
the full SPEC.  Returns (ENTRIES . STATUS)."
  (let* ((status nil)
         (db (plist-get spec :db))
         (keyfile (plist-get spec :keyfile))
         (yubi (plist-get spec :yubi))
         (term (keemacs-auth-keepassxc-term spec))
         (open (apply #'keemacs-auth--keepassxc-run
                      password
                      (append (list "ls" "--quiet")
                              (keemacs-auth--no-password-flag password)
                              (keemacs-auth--keyfile-args keyfile)
                              (keemacs-auth--yubi-args yubi)
                              (list db))))
         (locked-p (not (eq (cdr open) 0)))
         (paths (and (not locked-p) term
                     (keemacs-auth--keepassxc-narrow
                      db password term keyfile yubi)))
         (matcher (keemacs-auth-keepassxc-spec-matcher spec))
         (entries
         (and paths
               (let* ((shows (mapcar
                              (lambda (path)
                                (car (apply #'keemacs-auth--keepassxc-run
                                            password
                                            (append
                                             (list "show" "--quiet" "--show-protected")
                                             (keemacs-auth--no-password-flag password)
                                             (keemacs-auth--keyfile-args keyfile)
                                             (keemacs-auth--yubi-args yubi)
                                             (list db path)))))
                              paths))
                      (entries (mapcar (lambda (show) (keemacs-auth--keepassxc-parse
                                                       show (plist-get spec :port)))
                                       shows)))
                 (seq-filter matcher entries)))))
    `(,entries ,(if locked-p :locked status))))

(defun keemacs-auth--list-entries (entity spec password)
  "Return (ENTRIES . STATUS) for ENTITY matching the auth-source SPEC.
Dispatches to the active backend.  ENTRES is a list of auth plists (each
carrying PORT); STATUS is raw backend output for error reporting and is
nil for backends that do not emit one."
  (pcase keemacs-auth--active-cli
    ('kpscript
     ;; KPScript refs match the canonical fields directly (as the original
     ;; package did): Username -> -ref-Username, host+path -> -ref-URL.
     (let* ((url (concat (plist-get spec :host)
                         (plist-get spec :path)))
            (cmd (keemacs-auth--kpscript-command
                  entity
                  (plist-get spec :user)
                  url
                  password))
            (output (shell-command-to-string cmd)))
       (keemacs-auth--parse output (plist-get spec :port))))
    ('keepassxc
     (keemacs-auth--keepassxc-list-entries spec password))
    (_ (user-error "No usable keepass backend (keemacs-auth-cli = %S)"
                   keemacs-auth-cli))))

(cl-defun keemacs-auth-source-search (&rest spec
                                      &key backend host user port max title
                                        &allow-other-keys)
  "Find the password for a request.
If several passwords are available, prompt the user to select an entry.
A database that fails to unlock -- wrong password, or a cancelled
prompt -- is skipped with a message, so the next configured database
still gets asked; `auth-source' walks one backend per database."
  ;; The backend's `source' slot is the database path (its type is string);
  ;; the full spec (key file, password, YubiKey) lives in the `data' slot
  ;; when the entry was a spec, and defaults apply otherwise.  The search
  ;; spec is reconstructed from whatever the backend parser stashed.
  (let* ((data (slot-value backend 'data))
         (db-spec (if (keemacs-auth-db-spec-p data)
                      data
                    ;; A plain-string :source (no spec) -> a spec with just
                    ;; the file, i.e. prompt-for-password.
                    (keemacs-auth-make-db-spec :file (slot-value backend 'source))))
         ;; keepass-cli cannot open a leading-~ path (call-process does
         ;; no shell expansion), so the database argument is always
         ;; absolute.  The expanded path is also the cache key that
         ;; `M-x keemacs' uses, so one unlock serves both the tree and
         ;; auth-source searches.
         (entity (expand-file-name (keemacs-auth-db-spec-file db-spec)))
         (keyfile (keemacs-auth-db-spec-keyfile db-spec))
         (password-spec (keemacs-auth-db-spec-password db-spec))
         (yubi (keemacs-auth-db-spec-yubi db-spec)))
    (when (file-exists-p entity)
      ;; A failed unlock (wrong password, cancelled prompt) skips this
      ;; database instead of aborting the search -- the caller may have
      ;; several configured, and the next one may well unlock.
      (condition-case err
          (let* ((password (keemacs-auth--resolve-password
                            password-spec entity keemacs-auth-cache-expiry))
                 (url (url-generic-parse-url host))
                 (url (if (url-fullness url)
                          url
                        (url-generic-parse-url (concat "//" host))))
                 (host (or (url-host url) ""))
                 (max (or max 1))
                 (path (or (car (url-path-and-query url)) ""))
                 (spec `(:host ,host :user ,user :port ,port :title ,title
                            :path ,path :db ,entity
                            :keyfile ,keyfile :yubi ,yubi))
                 ;; An empty password means the user declined to
                 ;; unlock: nothing is listed for this database.
                 (parsed (if (and (stringp password) (string-empty-p password))
                             nil
                           (keemacs-auth--list-entries entity spec password)))
                 (result (nth 0 parsed))
                 (status (nth 1 parsed)))
            (cond
             ;; The user hit enter on an empty prompt: they chose not
             ;; to unlock this database -- skip it without an error.
             ;; (Return nil explicitly: `message' returns the string,
             ;; which auth-source would take for a search result.)
             ((and (stringp password) (string-empty-p password))
              (message "keemacs: %s skipped (no password entered)" entity)
              nil)
             ;; Wrong master password (backend-specific marker).
             ((and (eq keemacs-auth--active-cli 'keepassxc)
                   (keemacs-auth--keepassxc-locked-p status))
              (password-cache-remove entity)
              (user-error "Incorrect password for %s" entity))
             ((and (eq keemacs-auth--active-cli 'kpscript)
                   (with-temp-buffer
                     (insert status)
                     (goto-char 0)
                     (search-forward-regexp "^Unhandled Exception:" nil t)))
              (password-cache-remove entity)
              (user-error
               "An exception was thrown by KeePass.exe (your KPScript is likely out of date)\n %s"
               status))
             ((and (eq keemacs-auth--active-cli 'kpscript)
                   (with-temp-buffer
                     (insert status)
                     (goto-char 0)
                     (search-forward-regexp "^E:" nil t)))
              (cond
               ((string-match-p "The master key is invalid" status)
                (password-cache-remove entity)
                (user-error "Incorrect password for %s" entity))
               (t (user-error "Something went wrong in keepass: %s" status))))
             (t (let* ((rc (when (and keemacs-auth-match-title
                                      title
                                      (not (string-blank-p title)))
                                (seq-filter
                                 (lambda (it)
                                   (keemacs-auth-s-contains-p
                                    title (plist-get it :title) t))
                                 result)))
                        (used (if (= 1 (length rc)) rc result)))
                   (cond
                    ((= 0 (length used)) nil)
                    (t
                     (when (and keemacs-auth-verbose
                                (> (length used) 1))
                       (message (concat "keemacs: %d matching entries "
                                        "for %S; returning up to %d")
                                (length used) host max))
                     (seq-take used max)))))))
        (user-error
         (message "keemacs: %s -- trying the next database"
                  (error-message-string err))
         nil)))))

(defun keemacs-auth-source-backend-parser (entry)
  "Provide a keepass backend for ENTRY when it is a kdbx database spec.
ENTRY is one `auth-sources' element; anything that is not a database
spec plist -- stock entries such as the \"~/.authinfo\" string are
perfectly normal there -- yields nil, which tells auth-source to try
the next parser.  (Signalling on those entries used to break *every*
auth-source search once this parser was registered.)  The key file,
password and YubiKey specifications are carried on the backend's
`data' slot so the search can honour them."
  (when (and (listp entry)         ; a dotted pair is no plist either
             (keemacs-auth-db-spec-p entry))
    (let* ((db (keemacs-auth-db-spec-normalize entry))
           (path (keemacs-auth-db-spec-file db)))
      (when (and (stringp path)
                 (string-equal "kdbx" (file-name-extension path)))
        (auth-source-backend :type 'keepass
                             :source path
                             :search-function #'keemacs-auth-source-search
                             ;; Stash the whole spec (key file, password,
                             ;; YubiKey, name) for the search function, which
                             ;; reads it back from `data'.  The `source' slot
                             ;; stays the bare path string because that is its
                             ;; declared type.
                             :data db)))))

(defun keemacs-auth--remember-advice (fn spec found)
  "Call auth-source-remember FN only for well-formed FOUND.
Suppresses negative caching: a lookup that finds nothing is not
remembered, so a transient failure does not mask later queries.  See
`keemacs-auth-suppress-negative-cache'.

A non-list FOUND is never remembered, and the remember cache is
purged when one is seen: an early bug returned a message STRING as a
search result, which auth-source then remembered and served for every
subsequent search -- a truthy string that broke callers with
`let*: Wrong type argument: listp' until the cache was flushed."
  (cond
   ((and keemacs-auth-suppress-negative-cache (null found))
    nil)
   ((and found (not (listp found)))
    ;; Poison guard: purge the remember cache so the garbage is not
    ;; served again, and do not re-remember it.
    (auth-source-forget-all-cached)
    nil)
   (t (funcall fn spec found))))

;;;###autoload
(defun keemacs-auth-enable ()
  "Enable keepass auth source.
Chooses a backend from `keemacs-auth-cli'; by default
keepassxc-cli is used when available, otherwise KeePass/KPScript.
Also installs advice suppressing `auth-source' negative caching when
`keemacs-auth-suppress-negative-cache' is non-nil."
  (interactive)
  (let ((cli (keemacs-auth--resolve-cli)))
    (if cli
        (progn
          (setq keemacs-auth--active-cli cli)
          (auth-source-forget-all-cached)
          ;; Make `auth-source-remember' skip empty results, unless already
          ;; installed (idempotent across repeated calls to `enable').
          ;; `advice-member-p' returns the installed advice's flist --
          ;; not a list -- so it is used directly as the predicate.
          (unless (advice-member-p #'keemacs-auth--remember-advice
                                   'auth-source-remember)
            (advice-add 'auth-source-remember :around
                        #'keemacs-auth--remember-advice))
          (if (boundp 'auth-source-backend-parser-functions)
              (add-hook 'auth-source-backend-parser-functions #'keemacs-auth-source-backend-parser)
            (advice-add 'auth-source-backend-parse :before-until #'keemacs-auth-source-backend-parser)))
      (error "No usable keepass backend found. Install keepassxc-cli, or KeePass with KPScript, and add them to `exec-path'."))))

;;;###autoload
(defun keemacs-auth-forget-cached ()
  "Forget the cached KeePass database master password.

The master password is otherwise reused for
`keemacs-auth-cache-expiry' seconds, so this makes the next lookup
re-prompt for it.  Run this after changing the master password.

This only touches keemacs's own cache; it does not clear
`auth-source' search results (use `auth-source-forget-all-cached' for
those)."
  (interactive)
  (maphash (lambda (key _pwd)
             (when (and (stringp key)
                        (string-suffix-p ".kdbx" key))
               (password-cache-remove key)))
           password-data)
  (message "keemacs master-password cache cleared."))

(provide 'keemacs-auth)
;;; keemacs-auth-auth.el ends here
