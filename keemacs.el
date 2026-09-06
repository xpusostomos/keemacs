;;; keemacs.el --- Browse and edit KeePass entries (consult + embark) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Chris Bitmead

;; Author: Chris Bitmead <xpusostomos@gmail.com>
;; Maintainer: Chris Bitmead <xpusostomos@gmail.com>
;; Assisted-by: Claude
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (consult "0.1") (embark "0.1") (embark-consult "0.1") (magit-section "4.0"))
;; Keywords: comm, tools, passwords, keepassxc
;; URL: https://github.com/xpusostomos/keemacs
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software: you can redistribute it and/or modify
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

;; A richer front-end for KeePass databases on top of `keepassxc-cli',
;; picked with `consult' (works with `vertico') and acted on with `embark'
;; (`embark-consult' provides preview and `embark-act' provides the action
;; menu for the entry under point).
;;
;; This is deliberately separate from `keemacs-auth', which feeds
;; credentials to `auth-source'.  This package is for *using and editing* a
;; database interactively: list entries with the fields you care about,
;; select one, copy/insert its username, password, URL, notes or TOTP, and
;; view, add, clone, edit or delete entries.
;;
;; Only the five standard KeePass fields (Title, UserName, Password, URL,
;; Notes) plus TOTP are handled, matching what `keepassxc-cli' reliably
;; exposes.  Custom attributes are not yet supported (keepassxc-cli cannot
;; yet create them).
;;
;; Entry points:
;;   - `keemacs'            the main screen: a tree view of every
;;     configured database (groups and entries; RET or a double click
;;     opens an entry, TAB expands -- groups, fields and passwords,
;;     `C-.' opens the action menu)
;;   - `keemacs-titles'     pick an entry through the minibuffer
;;     (consult/vertico), then act on it (RET and `C-.' both lead to the
;;     action menu)
;;   - `keemacs-group'      drill down group by group in the minibuffer
;;   - `keemacs-buffer'     a columned listing buffer (Embark works
;;     on the entry at point)
;;
;; The same actions are available from the Embark action maps
;; (`keemacs-action-map' and `keemacs-select-action-map') and
;; as interactive commands.
;;
;; This package builds on `keemacs-auth' (same package): the shared
;; keepassxc-cli execution, master-password prompting/caching, error
;; reporting and the `keemacs-auth-verbose' flag live there.

;;; Code:

(require 'cl-lib)
(require 'consult)
(require 'embark)
(require 'embark-consult)
(require 'image)
(require 'keemacs-auth)
(require 'magit-section)
(require 'password-cache)
(require 'subr-x)

(defgroup keemacs nil
  "Browse and edit KeePass entries with consult and embark."
  :group 'tools
  :prefix "keemacs-")

(defface keemacs-title
  '((t :inherit default))
  "Face for the Title column in candidate lines.
Inherits the default face, so the title follows the user's theme
(white-on-dark, black-on-light); customize it to give titles their
own color."
  :group 'keemacs)

(defface keemacs-field-label
  '((t :foreground "green"))
  "Face for field labels (Group, Title, ...) in the view buffer."
  :group 'keemacs)

(defface keemacs-key-bracket
  '((t :foreground "green"))
  "Face for the brackets around keys in the view buffer's key menu.
The key itself stays uncolored."
  :group 'keemacs)

(defface keemacs-username
  '((t :foreground "light blue"))
  "Face for the UserName column in candidate lines."
  :group 'keemacs)

(defface keemacs-url
  '((t :foreground "orange"))
  "Face for the URL column in candidate lines.
\"orange\" rather than \"light orange\", which is not a valid color
name on some displays and rendered uncolored."
  :group 'keemacs)



(defcustom keemacs-databases nil
  "List of KeePass databases available for browsing.
Each element is a database spec plist as in `keemacs-auth-make-db-spec', with
an optional `:name' label (defaulting to the file name when omitted).
For example:

  (setq keemacs-databases
        \\='((:name \"personal\" :file \"~/passwords.kdbx\")
            (:file \"~/work.kdbx\" :keyfile \"~/work.keyx\")))"
  :type '(repeat keepass-db-spec)
  :group 'keemacs)

(defcustom keemacs-database nil
  "The currently active KeePass database spec (a `keemacs-auth-make-db-spec'
plist).  Set interactively with `keemacs-select-database'."
  :type '(choice (const :tag "None" nil)
                 keepass-db-spec)
  :group 'keemacs)

(defcustom keemacs-always-select-database nil
  "Whether to ask which database to use on every command.
Nil (the default) means the user is prompted only when no database is
active yet; once one is selected it stays active until switched with
`keemacs-select-database' or `keemacs-select-database-by-key'.  Non-nil
means every command that needs a database asks first, through the menu
it uses -- `keemacs-select-database' normally, the hotkey menu in
`keemacs-favorites-by-key'.  With exactly one configured database there
is nothing to choose, so it is picked automatically either way."
  :type 'boolean
  :group 'keemacs)

;; If the database *list* is re-set, any previously selected database may
;; point at something stale (a fixed/removed entry), and
;; `keemacs--ensure-database' would keep using it because it only
;; acts when `keemacs-database' is nil.  Reset it so the user is
;; prompted to re-select.
(add-variable-watcher 'keemacs-databases
                      (lambda (_sym _newval _op _where)
                        (setq keemacs-database nil)))

(defcustom keemacs-auth-cache-expiry 7200
  "How many seconds to cache the database master password.  Nil disables."
  :type '(choice (const :tag "Never" nil)
                 (const :tag "All Day" 86400)
                 (const :tag "2 Hours" 7200)
                 (const :tag "30 Minutes" 1800)
                 (integer :tag "Seconds"))
  :group 'keemacs)

(defcustom keemacs-fields '("Title" "UserName" "URL")
  "Fields shown in each candidate line, in order.
Each must be a standard KeePass field name: \"Title\", \"UserName\",
\"Password\", \"URL\" or \"Notes\"."
  :type '(repeat (choice (const "Title") (const "UserName")
                         (const "Password") (const "URL") (const "Notes")))
  :group 'keemacs)

(defcustom keemacs-field-width 24
  "Width each field is truncated or padded to in candidate lines.
Raise this to see more of long values -- titles containing \"/\"
(particularly) are easier to tell apart when not clipped hard."
  :type 'integer
  :group 'keemacs)

(defcustom keemacs-title-width 34
  "Width of the Title column in candidate lines.
The Title is the first column of `keemacs-fields'; this overrides
`keemacs-field-width' for it alone (34 by default, 10 more than the
other columns, so long titles are easier to tell apart)."
  :type 'integer
  :group 'keemacs)

(defcustom keemacs-clear-clipboard-seconds 0
  "If non-zero, clear the clipboard this many seconds after a copy."
  :type 'integer
  :group 'keemacs)

(defcustom keemacs-generate-length 16
  "Default length for passwords generated with `keemacs--entry-regenerate'.
The prompted default each time; the last-entered length is remembered for
the session."
  :type 'integer
  :group 'keemacs)

(defcustom keemacs-generate-options
  '(("all printable (!-~)"
     ("generate" "--custom"
      "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
      "--length" :length))
    ("upper case"     ("generate" "--upper" "--length" :length))
    ("lower case"     ("generate" "--lower" "--length" :length))
    ("mixed case"     ("generate" "--lower" "--upper" "--length" :length))
    ("with numeric"   ("generate" "--lower" "--upper" "--numeric" "--length" :length))
    ("with special"   ("generate" "--lower" "--upper" "--numeric" "--special" "--length" :length))
    ("with extended"  ("generate" "--lower" "--upper" "--numeric" "--special" "--extended" "--length" :length))
    ("passphrase"     ("diceware" "--words" :length)))
  "Generation options offered when regenerating a password.
Each entry is (LABEL ARGS).  LABEL is what the user picks by completion.
ARGS is the whole keepassxc-cli command -- subcommand first (\"generate\"
or \"diceware\"), then its flags -- with the symbol `:length' as a
placeholder for the requested length (generators take \"--length\", the
diceware passphrase takes \"--words\").  The first entry is the default
until a choice is remembered.  Add, remove or reorder your own sets here."
  :type '(repeat (cons (string :tag "Label")
                       (repeat (choice (string :tag "Arg")
                                       (const :tag ":length" :length)))))
  :group 'keemacs)

(defcustom keemacs-default-action #'keemacs-view
  "Function run on the selected entry when a selector command returns.
Called with the entry path.  The default, `keemacs-view', shows the
entry; it can be changed to e.g. `keemacs-copy-password' to copy the
password directly on RET."
  :type '(choice (function :tag "View entry" keemacs-view)
                 (function :tag "Copy password" keemacs-copy-password)
                 (function :tag "None (just return the path)" ignore))
  :group 'keemacs)

;; The single source of truth for the entry actions.  Both the Embark action
;; keymaps and the visible key-menu on the view screen are generated from
;; this list, so the two menus can never drift apart.  Each element is
;; (KEY LABEL FUNCTION), where FUNCTION takes an entry path.
(defconst keemacs--actions
  '(("t" "copy title"    keemacs-copy-title)
    ("u" "copy username" keemacs-copy-username)
    ("p" "copy password" keemacs-copy-password)
    ("l" "copy url"      keemacs-copy-url)
    ("n" "copy notes"    keemacs-copy-notes)
    ("o" "copy totp"     keemacs-copy-totp)
    ("v" "view"          keemacs-view)
    ("e" "edit"          keemacs-edit)
    ("m" "move to group" keemacs-move)
    ("c" "clone"         keemacs-clone)
    ("a" "add"           keemacs-add)
    ("d" "delete"        keemacs-delete))
  "Actions for a keemacs entry, in canonical field order.
See `keemacs--action-map'.  (TOTP is the entry's time-based one-time
password, i.e. a stored two-factor code; see `keemacs-copy-totp'.)")

;;; Internal state

(defvar keemacs--last-killed nil
  "The last string copied, so clearing only happens if it is unchanged.")

(defvar keemacs--clear-timer nil
  "Timer to clear the clipboard.")

(defvar keemacs-history nil
  "History for `keemacs-select'.")

(defvar keemacs--last-generated-length nil
  "Length last used by `keemacs--entry-regenerate'.
Nil until the first generation; thereafter the default offered.")

(defvar keemacs--last-generated-charset nil
  "Label of the character set last used by `keemacs--entry-regenerate'.
A label from `keemacs-generate-options'.  Nil until the first
generation; thereafter the default offered.")

(defvar keemacs--custom-icons nil
  "((UUID . BYTES)) decoded custom icon images from the last export.")

(defvar keemacs--entry-custom-icons nil
  "((PATH . UUID)) custom icon per entry path from the last export.")

(defvar keemacs--entry-parents nil
  "((ENTRY-PATH . GROUP-PATH)) real parent group per entry.
Recorded from the export tree while collecting; the group and the title
arrive separately and are kept separately.  Titles may contain
\"/\", so an entry's path string alone cannot be split into group and
title -- this map is the only reliable record of where an entry lives,
for call sites that only have the path.")

(defvar keemacs--group-icons nil
  "((PATH . (ICON-ID . CUSTOM-UUID))) per group from the last export.
ICON-ID is the standard icon id as a string (48 is the keepassxc default
for groups); CUSTOM-UUID is the group's custom icon image, or nil.
Unlike the entry-derived paths in `keemacs--group-contents', this
includes empty groups.  The Recycle Bin subtree is not recorded.")

(defvar keemacs--icon-image-cache nil
  "Created custom icon images, keyed by (UUID . MAX-PIXELS).")

;; vertico is an optional completion framework (consult works without it);
;; these variables only exist once vertico is loaded, hence the `defvar'
;; declarations so the byte-compiler does not warn about them, and the
;; `bound-and-true-p' guards at the call sites.
(defvar vertico--index)
(defvar vertico--candidates)

;;; Subprocess plumbing
;;
;; All keepassxc-cli execution and master-password prompting is shared with
;; `keemacs-auth' (which this package requires); the wrappers below
;; adapt it to the database configured for browsing.  See
;; `keemacs-auth--keepassxc-run' et al.

(defun keemacs--db-spec ()
  "Return the active database spec, signalling an error if none is set."
  (unless keemacs-database
    (user-error "No KeePass database selected; run `keemacs-select-database' first"))
  keemacs-database)

(defun keemacs--database-path ()
  "Return the active database's expanded file path.
The `:file' of the active database's spec, expanded so a leading \"~\"
works; `file-exists-p' accepts \"~\" but keepassxc-cli does not."
  (let ((spec (keemacs--db-spec)))
    (expand-file-name
     (keemacs-auth-db-spec-file (keemacs-auth-db-spec-normalize spec)))))

(defun keemacs--db-keyfile ()
  "Return the active database's key file argument list, or nil.
A list (\"--key-file\" FILE), ready to splice into a keepassxc-cli
invocation."
  (let ((spec (keemacs--db-spec)))
    (keemacs-auth--keyfile-args
     (keemacs-auth-db-spec-keyfile (keemacs-auth-db-spec-normalize spec)))))

(defun keemacs--db-yubi ()
  "Return the active database's YubiKey argument list, or nil.
A list (\"--yubikey\" VALUE), ready to splice into a keepassxc-cli
invocation."
  (let ((spec (keemacs--db-spec)))
    (keemacs-auth--yubi-args
     (keemacs-auth-db-spec-yubi (keemacs-auth-db-spec-normalize spec)))))

(defun keemacs--db-password ()
  "Return the active database's master password, per its spec.
A string or function in the spec is used as-is; `:prompt' (or an
unspecified password) asks the user via `password-cache' (keyed by the
database path), honoring `keemacs-auth-cache-expiry'; nil means no
password and resolves to `:no-password'."
  (let ((db (keemacs--database-path))
        (password-spec (keemacs-auth-db-spec-password
                        (keemacs-auth-db-spec-normalize keemacs-database))))
    (keemacs-auth--resolve-password
     password-spec db keemacs-auth-cache-expiry)))

(defun keemacs--run (password &rest args)
  "Run keepassxc-cli with ARGS on the active database.
PASSWORD is the resolved master password, or `:no-password' for a
passwordless database (which gets keepassxc-cli's --no-password global
option and no stdin).  Looks up the key file and YubiKey from the active
spec."
  (apply #'keemacs-auth--keepassxc-run
         password
         (append (keemacs-auth--no-password-flag password)
                 (keemacs--db-keyfile)
                 (keemacs--db-yubi)
                 args)))

(defun keemacs--run-stdin (password stdin &rest args)
  "Run keepassxc-cli with ARGS and STDIN, e.g. an entry edit or add.
STDIN already contains the database password (or nothing for a
passwordless DB) plus the entry's password, as required by
`keemacs-auth--keepassxc-run-stdin'.  PASSWORD is the database's
resolved master password (possibly `:no-password'), used to build the
global options; the actual passwords travel in STDIN."
  (apply #'keemacs-auth--keepassxc-run-stdin
         stdin
         (append args
                 (keemacs-auth--no-password-flag password)
                 (keemacs--db-keyfile)
                 (keemacs--db-yubi))))

(defun keemacs--require-db (&optional selector)
  "Signal an error unless a database is configured.
Also applies the default-to-sole-database rule.  SELECTOR, when given,
is the function that prompts for a database; it is passed to
`keemacs--ensure-database'."
  (keemacs--ensure-database selector))

;;; Listing and parsing

(defun keemacs--valid-field-p (field)
  "Return non-nil if FIELD names a standard KeePass entry field."
  (member field '("Title" "UserName" "Password" "URL" "Notes")))

(defun keemacs--parse-show (output)
  "Parse `keepassxc-cli show' OUTPUT into an alist of FIELD . VALUE."
  (let ((result '()))
    (dolist (line (split-string output "\n"))
      (when (string-match "^\\([^:]+\\):[[:space:]]*\\(.*\\)$" line)
        (let ((key (string-trim (match-string 1 line))))
          (when (keemacs--valid-field-p key)
            (setq result (cons (cons key (string-trim (match-string 2 line)))
                               result))))))
    result))

(defun keemacs--entry-get (path)
  "Return the FIELD . VALUE alist for the entry at PATH.
PATH is normally the clean entry path.  If it is a padded display string
(which an Embark action may hand over), it is resolved through
`keemacs--path-of' first.  Fetches directly from keepassxc-cli with
no caching, so database changes made elsewhere (e.g. Google Drive sync) are
always seen."
  (setq path (or (keemacs--path-of path) path))
  (let* ((db (keemacs--database-path))
         (pw (keemacs--db-password))
         (run (apply #'keemacs--run pw
                     (list "show" "--quiet" "--show-protected" db path))))
    (if (eq (cdr run) 0)
        (keemacs--parse-show (car run))
      (keemacs-auth--error (car run) (keemacs--database-path) (cdr run)))))

(defun keemacs--field (entry field)
  "Return the value of FIELD in parsed alist ENTRY, or \"\"."
  (or (cdr (assoc field entry)) ""))

(defun keemacs--entry-paths ()
  "Return the list of entry paths (excluding group rows) in the database.
Reads freshly from keepassxc-cli, with no caching."
  (let* ((db (keemacs--database-path))
         (run (apply #'keemacs--run
                     (keemacs--db-password)
                     (list "ls" "--quiet" "--recursive" "--flatten" db))))
    (unless (eq (cdr run) 0)
      (keemacs-auth--error (car run) (keemacs--database-path) (cdr run)))
    (let ((paths (seq-filter (lambda (s)
                               (and (not (string-blank-p s))
                                    (not (string-suffix-p "/" s))))
                             (split-string (car run) "\n" t))))
      (mapcar (lambda (p) (if (string-prefix-p "/" p) p (concat "/" p)))
              paths))))

(defun keemacs--group-paths ()
  "Return the list of group paths (each ending in /) in the database.
Reads freshly from keepassxc-cli, with no caching."
  (let* ((db (keemacs--database-path))
         (run (apply #'keemacs--run
                     (keemacs--db-password)
                     (list "ls" "--quiet" "--recursive" "--flatten" db))))
    (unless (eq (cdr run) 0)
      (keemacs-auth--error (car run) (keemacs--database-path) (cdr run)))
    (mapcar (lambda (g) (if (string-prefix-p "/" g) g (concat "/" g)))
            (seq-filter (lambda (s) (string-suffix-p "/" s))
                        (split-string (car run) "\n" t)))))

(defun keemacs--export ()
  "Return the export XML node tree for the database.
Does ONE `keepassxc-cli export' call so the whole database (all entries,
all fields, passwords included) is fetched up front, freshly each time."
  (let ((run (apply #'keemacs--run
                    (keemacs--db-password)
                    (list "export" "--quiet" (keemacs--database-path)))))
    (unless (eq (cdr run) 0)
      (keemacs-auth--error (car run) (keemacs--database-path) (cdr run)))
    (with-temp-buffer
      (insert (car run))
      (goto-char (point-min))
      (libxml-parse-xml-region (point-min) (point-max)))))

(defun keemacs--xml-children-tag (node tag)
  "Return the XML child nodes of NODE whose tag is TAG."
  (seq-filter (lambda (c) (and (consp c) (eq (car c) tag)))
              (cdr node)))

(defun keemacs--xml-tag-text (node tag)
  "Return the text of the first child of NODE with tag TAG, or nil.
libxml nodes are (TAG ATTRIBUTES &rest CHILDREN); the text is the first
string among the children, after the attributes slot."
  (let ((n (car (keemacs--xml-children-tag node tag))))
    (when n
      (catch 'found
        (dolist (el (cdr n))
          (when (stringp el) (throw 'found el)))
        ""))))

(defun keemacs--entry-fields (entry-node)
  "Return the FIELD . VALUE alist for export XML ENTRY-NODE."
  (let (fields)
    (dolist (str (keemacs--xml-children-tag entry-node 'String))
      (let ((key (keemacs--xml-tag-text str 'Key)))
        (when (keemacs--valid-field-p key)
          (setq fields (cons (cons key (keemacs--xml-tag-text str 'Value))
                             fields)))))
    (nreverse fields)))

(defun keemacs--collect (node group-path)
  "Return ((PATH . FIELDS) ...) for export XML NODE under GROUP-PATH.
The root group's own name is not part of entries' paths, but nested
groups' names are.  Each entry's FIELDS also carry its \"Group\" (the
GROUP-PATH it was collected under, trailing slash included) and its
\"IconID\": group and title are known separately here and must stay that
way -- a title containing \"/\" makes the PATH string ambiguous, so
nothing downstream may re-derive group or title by splitting it."
  (let ((acc '()))
    ;; Add each entry in this node.
    (dolist (entry (keemacs--xml-children-tag node 'Entry))
      (let* ((fields (keemacs--entry-fields entry))
             ;; The entry's standard icon id, for the picker glyph.
             (icon-id (keemacs--xml-tag-text entry 'IconID))
             (fields (if (and icon-id (not (string-empty-p icon-id)))
                         (cons (cons "IconID" icon-id) fields)
                       fields))
             (fields (cons (cons "Group" (concat group-path "/"))
                           fields))
             ;; The entry's custom icon, when it has one.
             (custom-id (keemacs--xml-tag-text entry 'CustomIconUUID))
             (title (cdr (assoc "Title" fields)))
             (path (concat group-path "/" title)))
        (when (and custom-id (not (string-empty-p custom-id)))
          (push (cons path (string-trim custom-id))
                keemacs--entry-custom-icons))
        (push (cons path (concat group-path "/"))
              keemacs--entry-parents)
        (setq acc (cons (cons path fields) acc))))
    ;; Recurse into child groups, extending the path with the group name.
    (dolist (subgroup (keemacs--xml-children-tag node 'Group))
      (let ((name (keemacs--xml-tag-text subgroup 'Name)))
        (setq acc (nconc (keemacs--collect
                          subgroup (concat group-path "/" name))
                         acc))))
    acc))

(defun keemacs--collect-groups (node group-path)
  "Record icon info for the child groups of NODE at GROUP-PATH.
Each child group is recorded in `keemacs--group-icons' as
PATH -> (ICON-ID . CUSTOM-UUID), then recursed into.  NODE should be a
group element whose own name is GROUP-PATH's business -- `--load-entries'
starts below the root group, so the root group's own name names no path,
like entry paths.  The Recycle Bin subtree is filtered out afterwards."
  (dolist (subgroup (keemacs--xml-children-tag node 'Group))
    (let* ((name (keemacs--xml-tag-text subgroup 'Name))
           (path (concat group-path "/" name))
           (icon-id (keemacs--xml-tag-text subgroup 'IconID))
           (custom (keemacs--xml-tag-text subgroup 'CustomIconUUID)))
      (push (cons path
                  (cons icon-id
                        (and custom (not (string-empty-p custom))
                             (string-trim custom))))
            keemacs--group-icons)
      (keemacs--collect-groups subgroup path))))

(defun keemacs--load-entries ()
  "Return ((PATH . FIELDS) ...) for the whole database, freshly.
Does ONE export call and discards the result, so no entries are cached
behind the scenes; database changes made elsewhere are always visible.
Also captures the database's custom icon images, which entries use them
(see `keemacs--custom-icons'), and the group tree's icons (see
`keemacs--group-icons')."
  (let* ((tree (keemacs--export))
         (root (car (keemacs--xml-children-tag tree 'Root)))
         (entries '()))
    (setq keemacs--custom-icons
          (keemacs--custom-icons-from tree)
          keemacs--entry-custom-icons nil
          keemacs--entry-parents nil
          keemacs--group-icons nil)
    (dolist (g (keemacs--xml-children-tag root 'Group))
      ;; Start below the root group: its own name names no path, exactly
      ;; like `keemacs--collect' for entries.
      (setq entries (append entries (keemacs--collect g "")))
      (keemacs--collect-groups g ""))
    ;; The Recycle Bin subtree is excluded from the group map, matching the
    ;; entry filter below.
    (setq keemacs--group-icons
          (seq-filter (lambda (g)
                        (not (string-prefix-p "/Recycle Bin" (car g))))
                      keemacs--group-icons))
    ;; `keepassxc-cli rm' moves deleted entries to the Recycle Bin; exclude
    ;; them so a rename (add-new + delete-old) does not show a duplicate.
    (seq-filter (lambda (e)
                  (not (string-prefix-p "/Recycle Bin/" (car e))))
                entries)))

(defun keemacs--entry-directory (path)
  "Return the directory part of KeePass entry PATH, trailing slash included.
Examples: \"/a/b\" -> \"/a/\", \"/b\" -> \"/\", \"/\" -> \"/\", \"\" -> \"\".
Pure string arithmetic: entry paths are record paths that just happen to
look like file names, so they must never reach the `file-name-*' functions,
which route through `file-name-handler-alist' -- and therefore through
TRAMP -- hence a title such as \"Apple:foo:bar\" (path \"/Apple:foo:bar\")
would raise \"Method `Apple' is not known\"."
  (if (string-empty-p path)
      ""
    (let ((pos (string-match "/[^/]*\\'" path)))
      (if pos (substring path 0 (1+ pos)) "/"))))

(defun keemacs--entry-basename (path)
  "Return the last path segment of KeePass entry PATH.
Examples: \"/a/b\" -> \"b\", \"/b\" -> \"b\", \"/\" -> \"\".  Pure string
arithmetic -- see `keemacs--entry-directory'."
  (if (string-match "/[^/]*\\'" path)
      (substring path (1+ (match-beginning 0)))
    path))

(defun keemacs--entry-group (path)
  "Return the real group of the entry at PATH, trailing slash included.
The parent recorded from the export tree when known (a title containing
\"/\" mis-splits the path string), else derived from the path."
  (or (cdr (assoc path keemacs--entry-parents))
      (keemacs--entry-directory path)))

(defun keemacs--group-contents (entries group)
  "Return the immediate children of GROUP in ENTRIES.
ENTRIES is a list of (PATH . FIELDS) pairs as returned by
`keemacs--load-entries'.  GROUP names a group: a path ending in
\"/\" (e.g. \"/Internet/\"), or \"/\" for the root group; a missing
trailing slash is added.  Returns (GROUPS . ENTRIES), where GROUPS is the
list of child group paths (each ending in \"/\") and ENTRIES the child
entry (PATH . FIELDS) pairs, both sorted by path.  Subgroups come both
from the entry paths and from the group tree recorded in
`keemacs--group-icons', so empty groups are included."
  (let* ((group (if (string-suffix-p "/" group) group (concat group "/")))
         (in-group nil)
         (subgroups nil))
    (dolist (entry entries)
      (let* ((path (car entry))
             (fields (cdr entry))
             (entry-group (cdr (assoc "Group" fields))))
        (if entry-group
            ;; The entry carries its real group: it belongs exactly there,
            ;; whatever its title contains (a title with "/" must not
            ;; become phantom subgroups).
            (when (string-equal entry-group group)
              (push entry in-group))
          ;; No recorded group (synthetic data): fall back to the path.
          (when (string-equal (keemacs--entry-directory path) group)
            (push entry in-group))
          ;; A path strictly deeper than GROUP contributes its next segment as a
          ;; subgroup; a direct child entry (rest has no "/") is just that.
          (when (and (string-prefix-p group path)
                     (string-match-p "/" (substring path (length group))))
            (let* ((rest (substring path (length group)))
                   (seg (car (split-string rest "/" t))))
              (when (and seg (not (string-empty-p seg)))
                (push (concat group seg "/") subgroups)))))))
    ;; Groups recorded from the export tree: unlike the entry-derived
    ;; segments above, these include empty groups.  A recorded path is the
    ;; group itself, so even a direct child (rest without "/") contributes.
    (dolist (g keemacs--group-icons)
      (let ((p (car g)))
        (when (string-prefix-p group p)
          (let* ((rest (substring p (length group)))
                 (seg (car (split-string rest "/" t))))
            (when (and seg (not (string-empty-p seg)))
              (push (concat group seg "/") subgroups))))))
    (cons (sort (delete-dups subgroups) #'string<)
          (sort in-group
                (lambda (a b) (string< (car a) (car b)))))))

;;; Candidates

;; KeePassXC's standard entry icons (ID 0..68), each mapped to a unicode
;; character approximating the artwork, so candidates can be prefixed with a
;; glyph instead of shipping or rendering the SVG set.  Where the artwork has
;; no direct unicode equivalent the closest imaginative stand-in is used.
(defconst keemacs--icon-chars
  ["🔑"  ;  0 password (key)
   "🌍"  ;  1 network (world)
   "⚠️"  ;  2 warning
   "🗄️"  ;  3 server (stacked)
   "📋"  ;  4 clipboard
   "👤"  ;  5 user
   "⚙"  ;  6 parts (puzzle)
   "📝"  ;  7 notepad
   "📤"  ;  8 upload arrow
   "🪪"  ;  9 identity
   "📧"  ; 10 contact (@-mail)
   "📷"  ; 11 camera
   "🕹️️"  ; 12 IR Remote
   "🗝️"  ; 13 multi keys
   "🔌️"  ; 14 plug 
   "📻"  ; 15 scanner
   "🔖"  ; 16 bookmark
   "💿"  ; 17 CDROM
   "🖥️"  ; 18 display
   "✉️"  ; 19 mail
   "⚙️"  ; 20 configuration (gear)
   "🗹"  ; 21 organiser (tick/clipboard)
   "📄"  ; 22 paper
   "🔣"  ; 23 icons
   "⚡"  ; 24 connection (lightning)
   "🪎"  ; 25 safe/vault
   "💾"  ; 26 save (floppy)
   "⏏"  ; 27 nfs unmount
   "📽️️"  ; 28 quicktime (film)
   "🔏"  ; 29 PGP (locked terminal)
   "$_"  ; 30 terminal
   "🖨️"  ; 31 printer
   "🎛️"  ; 32 FS view (buttons)
   "🧱"  ; 33 run (bricks/grid)
   "🔧"  ; 34 configure (wrench)
   "🖵"  ; 35 screen share 
   "🗜️"  ; 36 archive and compression
   "％"  ; 37 percent/symbols
   "🪟"  ; 38 samba unmount (windows desktop)
   "🕐"  ; 39 history (clock)
   "🔍"  ; 40 find (magnifier)
   "⛰️"  ; 41 vector graphics (mountain)
   "📟"  ; 42 memory (chip)
   "🗑️"  ; 43 trash
   "📝️"  ; 44 notes
   "❌"  ; 45 cancel
   "❓"  ; 46 question
   "📦"  ; 47 package
   "📁"  ; 48 folder
   "📂"  ; 49 folder open
   "🗃️"  ; 50 tar
   "🔓️"  ; 51 decrypted
   "🔒"  ; 52 encrypted
   "✅"  ; 53 apply (tick)
   "✏️"  ; 54 pencil
   "🖼️"  ; 55 thumbnail
   "👥"  ; 56 address book
   "📊"  ; 57 spreadsheet
   "🛡️"  ; 58 PGP (locked terminal)
   "🛠️"  ; 59 tools
   "🏠"  ; 60 home
   "⭐"  ; 61 star
   "🐧"  ; 62 Linux
   "🤖"  ; 63 Android
   "🍎"  ; 64 Apple
   "🔗"  ; 65 wiki
   "💵"  ; 66 money
   "📜"  ; 67 certificate
   "📱"  ; 68 mobile
   ]
  "Unicode glyph for each standard KeePass icon ID (0..68).
Indexed by IconID; see `keemacs--icon-char'.")

(defun keemacs--icon-char (entry)
  "Return the unicode glyph for ENTRY's standard icon, or \"\".
ENTRY is a (FIELD . VALUE) alist carrying an \"IconID\" when it came from
a database export; entries without one (e.g. from `show') get no glyph."
  (let* ((id (cdr (assoc "IconID" entry)))
         (n (and id (string-to-number id)))
         (chars keemacs--icon-chars))
    (if (and n (>= n 0) (< n (length chars)))
        (aref chars n)
      "")))

;;;; Custom icons
;;
;; Entries may use an imported image instead of a standard icon.  The kdbx
;; stores those images once in the database's Meta as base64 blobs keyed by
;; UUID, and each entry points at one with a CustomIconUUID.  `export'
;; hands us both, so the images can be decoded and shown as real pictures.

(defun keemacs--custom-icons-from (tree)
  "Return ((UUID . BYTES)) for the custom icons in export XML TREE.
Each base64 Data blob is decoded; UUID is the entry-level
CustomIconUUID spelling."
  (let* ((meta (car (keemacs--xml-children-tag tree 'Meta)))
         (icons (car (keemacs--xml-children-tag meta 'CustomIcons))))
    (mapcar (lambda (icon)
              (cons (string-trim (keemacs--xml-tag-text icon 'UUID))
                    (base64-decode-string
                     (replace-regexp-in-string
                      "[\n\r\t ]" ""
                      (keemacs--xml-tag-text icon 'Data)))))
            (keemacs--xml-children-tag icons 'Icon))))

(defcustom keemacs-icon-scale 0.8
  "Size multiplier for custom icon images, relative to the line height.
1.0 is exactly the height of a line of text; emoji glyphs tend to render
a touch larger than that (and icon PNGs often carry transparent padding),
so the default is slightly above 1 to visually match the unicode glyphs
used for standard icons."
  :type 'number
  :group 'keemacs)

(defun keemacs--icon-pixels (&optional scale)
  "Return the pixel size for a custom icon at SCALE.
The picker matches the height of a unicode glyph, which renders at about
the default frame character height; the view buffer uses a multiple."
  (let ((h (condition-case nil (frame-char-height) (error 16))))
    (max 8 (round (* keemacs-icon-scale (or scale 1.0) h)))))

(defun keemacs--custom-icon-image (uuid &optional max)
  "Return an image for custom icon UUID scaled to MAX pixels tall, or nil.
MAX defaults to one glyph height (see `keemacs--icon-pixels').
The image is scaled to exactly MAX via `:height' -- unlike `:max-width'
and `:max-height', which only shrink oversized images and would leave a
small icon (e.g. a 16x16 favicon) at its natural size no matter what
scale was asked for.  Images are created once and cached.  Returns nil
when UUID is unknown or the bytes are not a renderable image."
  (let ((max (or max (keemacs--icon-pixels))))
    (when-let* ((bytes (cdr (assoc uuid keemacs--custom-icons)))
                (key (cons uuid max)))
      (or (cdr (assoc key keemacs--icon-image-cache))
          (let ((img (condition-case nil
                         (create-image bytes 'png t
                                       :height max
                                       :ascent 'center)
                       (error nil))))
            (when img
              (push (cons key img) keemacs--icon-image-cache))
            img)))))

(defun keemacs--candidate-prefix (path entry)
  "Return the display prefix for the entry at PATH with fields ENTRY.
A real thumbnail of the entry's custom icon on graphic displays,
otherwise the unicode glyph for its standard icon."
  (if-let* ((uuid (cdr (assoc path keemacs--entry-custom-icons)))
            (img (and (display-graphic-p)
                      (keemacs--custom-icon-image uuid))))
      (propertize " " 'display img)
    (keemacs--icon-char entry)))

(defun keemacs--format-candidate (path entry)
  "Return a display string for ENTRY at PATH, tagged with `kb-path'.
The line is prefixed with a picture of the entry's custom icon when it
has one, else a unicode glyph approximating its standard icon (see
`keemacs--icon-chars')."
  (let* ((prefix (keemacs--candidate-prefix path entry))
         ;; Per-column face: title inherits the default face (theme colors),
         ;; username and url are tinted.  The text is padded first, then
         ;; propertized, so the whole column (padding included) takes the face.
         (col (lambda (f)
                (let* ((text (truncate-string-to-width
                              (keemacs--field entry f)
                              (if (equal f "Title")
                                  keemacs-title-width
                                keemacs-field-width)
                              0 ?\s))
                       (face (pcase f
                               ("Title" 'keemacs-title)
                               ("UserName" 'keemacs-username)
                               ("URL" 'keemacs-url))))
                  (propertize text 'face face))))
         (str (concat prefix
                      (when (not (string-empty-p prefix)) " ")
                      (mapconcat col keemacs-fields "\t"))))
    (put-text-property 0 (length str) 'kb-path path str)
    str))

(defun keemacs--group-prefix (path)
  "Return the display prefix for the group at PATH.
A real thumbnail of the group's custom icon on graphic displays,
otherwise the unicode glyph for its standard icon (48, a folder, is the
keepassxc default for groups)."
  (let* ((trimmed (if (string-suffix-p "/" path) (substring path 0 -1) path))
         (info (cdr (assoc trimmed keemacs--group-icons)))
         (custom (cdr info)))
    (if-let* ((img (and custom
                        (display-graphic-p)
                        (keemacs--custom-icon-image custom))))
        (propertize " " 'display img)
      (keemacs--icon-char
       `(("IconID" . ,(or (car info) "48")))))))

(defun keemacs--format-group (path)
  "Return a display string for group PATH, tagged with `kb-path'.
The line is prefixed with the group's icon: a custom thumbnail when the
group has one, else the glyph for its standard icon."
  (let* ((trimmed (if (string-suffix-p "/" path) (substring path 0 -1) path))
         (prefix (keemacs--group-prefix path))
         (str (concat prefix
                      " "
                      (keemacs--entry-basename trimmed) "/")))
    (put-text-property 0 (length str) 'kb-path path str)
    str))

(defun keemacs--candidates ()
  "Return the candidate strings for the current database."
  (mapcar (lambda (c)
            (keemacs--format-candidate (car c) (cdr c)))
          (keemacs--load-entries)))

(defun keemacs--path-of (candidate)
  "Return the entry path stored in CANDIDATE, or nil.
Reads the path purely from CANDIDATE's `kb-path' text property.  The
display text is never parsed back for the path, so a title/username column
that merely looks similar cannot resolve to the wrong entry."
  (get-text-property 0 'kb-path candidate))

;;; Clip and copy

(defun keemacs--kill (value &optional msg)
  "Copy VALUE to the kill ring; optionally rearm the clear timer."
  (kill-new value)
  (when (> keemacs-clear-clipboard-seconds 0)
    (setq keemacs--last-killed value)
    (when keemacs--clear-timer
      (cancel-timer keemacs--clear-timer))
    (setq keemacs--clear-timer
          (run-with-timer keemacs-clear-clipboard-seconds nil
                          #'keemacs--clear-clipboard)))
  (message "%s" msg))

(defun keemacs--clear-clipboard ()
  "Clear the clipboard if it still holds the last value we copied."
  (when (and keemacs--last-killed
             (string-equal keemacs--last-killed (car kill-ring)))
    (kill-new ""))
  (setq keemacs--last-killed nil))

(defun keemacs--totp (path)
  "Return the current TOTP for the entry at PATH, or nil."
  (setq path (or (keemacs--path-of path) path)) ; resolve padded target
  (let ((run (apply #'keemacs--run
                    (keemacs--db-password)
                    (list "show" "--quiet" "--totp"
                          (keemacs--database-path) path))))
    (when (eq (cdr run) 0)
      (string-trim (car run)))))

;;; Actions (each takes an entry path)

(defun keemacs-copy-title (path)
  "Copy the title of the entry at PATH."
  (interactive "sEntry path: ")
  (keemacs--kill (keemacs--field (keemacs--entry-get path) "Title")
                        (format "Title of %s copied" path)))

(defun keemacs-copy-username (path)
  "Copy the username of the entry at PATH."
  (interactive "sEntry path: ")
  (keemacs--kill (keemacs--field (keemacs--entry-get path) "UserName")
                        (format "Username of %s copied" path)))

(defun keemacs-copy-password (path)
  "Copy the password of the entry at PATH."
  (interactive "sEntry path: ")
  (keemacs--kill (keemacs--field (keemacs--entry-get path) "Password")
                        "Password copied"))

(defun keemacs-copy-url (path)
  "Copy the URL of the entry at PATH."
  (interactive "sEntry path: ")
  (keemacs--kill (keemacs--field (keemacs--entry-get path) "URL")
                        "URL copied"))

(defun keemacs-copy-notes (path)
  "Copy the notes of the entry at PATH."
  (interactive "sEntry path: ")
  (keemacs--kill (keemacs--field (keemacs--entry-get path) "Notes")
                        "Notes copied"))

(defun keemacs-copy-totp (path)
  "Copy the current TOTP of the entry at PATH."
  (interactive "sEntry path: ")
  (let ((totp (keemacs--totp path)))
    (if totp
        (keemacs--kill totp "TOTP copied")
      (user-error "No TOTP available for %s" path))))

(defun keemacs-insert-password (path)
  "Insert the password of the entry at PATH at point."
  (interactive "sEntry path: ")
  (insert (keemacs--field (keemacs--entry-get path) "Password")))

(defun keemacs-insert-username (path)
  "Insert the username of the entry at PATH at point."
  (interactive "sEntry path: ")
  (insert (keemacs--field (keemacs--entry-get path) "UserName")))

(defun keemacs-reveal-password (path)
  "Copy the password of the entry at PATH to the kill ring, revealing it."
  (interactive "sEntry path: ")
  (when (button-at (point)) (forward-button 1)) ; move off the revealed text
  (let ((entry (keemacs--entry-get path)))
    (keemacs--kill (keemacs--field entry "Password")
                          "Password copied to clipboard")))

(defvar-local keemacs-view--revealed nil
  "Non-nil while the password is shown in the current view buffer.")

(defvar-local keemacs-view-path nil
  "The entry path shown in `keemacs-view-mode'.")

(defconst keemacs-view-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    ;; Each action operates on the entry being viewed.
    (pcase-dolist (`(,key ,_label ,fn) keemacs--actions)
      (define-key map (kbd key)
        (lambda ()
          (interactive)
          (funcall fn keemacs-view-path))))
    (define-key map (kbd "r") #'keemacs-view-reveal)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "C-.") #'embark-act)
    map)
  "Keymap for `keemacs-view-mode', generated from
`keemacs--actions'; each action operates on the viewed entry.  The
password reveal (r) and quit (q) are view-only.")

(define-derived-mode keemacs-view-mode special-mode "kb-view"
  "Major mode for viewing a KeePass entry.  Password is hidden until
\\[keemacs-view-reveal].")

(defun keemacs--view-menu ()
  "Return the key menu shown at the bottom of the view buffer.
Generated from `keemacs--actions' plus the view-only reveal and
quit, laid out in two columns.  The view action itself is omitted: this
menu is only shown when already viewing an entry."
  (let* ((items (append (seq-filter
                         (lambda (i) (not (equal (car i) "v")))
                         keemacs--actions)
                        '(("r" "reveal password")
                          ("q" "quit"))))
         (half (ceiling (length items) 2))
         (width (apply #'max 0 (mapcar (lambda (i) (length (cadr i))) items)))
         (fmt (format "%%s %%-%ds   %%s" width))
         (rows '()))
    (cl-flet ((menu-item (key)
                ;; "[k]" in `keemacs-key-bracket', the key left uncolored.
                (concat (propertize "[" 'face 'keemacs-key-bracket)
                        (char-to-string key)
                        (propertize "]" 'face 'keemacs-key-bracket))))
      (dotimes (i half)
        (let ((l (nth i items))
              (r (nth (+ half i) items)))
          (push (format fmt (menu-item (aref (car l) 0)) (cadr l)
                        (if r (concat (menu-item (aref (car r) 0))
                                      (format " %s" (cadr r)))
                          ""))
                rows))))
    (concat "\n\n" (string-join (nreverse rows) "\n"))))

(defun keemacs--spec-label (spec)
  "Return a user-visible label for database spec SPEC.
The spec's `:name', or the file name for a spec without one."
  (let ((spec (keemacs-auth-db-spec-normalize spec)))
    (or (keemacs-auth-db-spec-name spec)
        (file-name-nondirectory (keemacs-auth-db-spec-file spec)))))

(defun keemacs--database-name ()
  "Return the user-visible name of the active database, or nil."
  (when keemacs-database
    (keemacs--spec-label keemacs-database)))

(defun keemacs--prompt (label)
  "Return completion prompt LABEL tagged with the active database's name.
\"KeePass entry: \" becomes \"KeePass entry (mydb): \" -- but only when
more than one database is configured (with a single database the tag
would be noise); LABEL is returned unchanged otherwise.  LABEL should
end in \": \" or \":\"."
  (if (and (> (length keemacs-databases) 1)
           (keemacs--database-name))
      (concat (string-trim-right label ": ")
              (format " (%s): " (keemacs--database-name)))
    label))

(defun keemacs-view-update (reveal)
  "Redraw the current view buffer, revealing the password when REVEAL.
The password appears once, on its own line after the username.  It is hidden
until toggled with `keemacs-view-reveal', unless it is empty, in
which case there is nothing to hide."
  (let* ((entry (keemacs--entry-get keemacs-view-path))
         (pw-field (keemacs--field entry "Password"))
         (pw (if (or reveal (string-blank-p pw-field))
                 pw-field
               "[hidden - press r]"))
         (icon (when-let* ((uuid (cdr (assoc keemacs-view-path
                                             keemacs--entry-custom-icons)))
                           (img (and (display-graphic-p)
                                     (keemacs--custom-icon-image
                                      uuid (keemacs--icon-pixels 2)))))
                  img)))
    (let ((inhibit-read-only t)
          (label (lambda (name)
                   ;; Field label in `keemacs-field-label' face.
                   (propertize (format "%-10s " name)
                               'face 'keemacs-field-label))))
      (erase-buffer)
      ;; The entry's own picture, when it has a custom icon.
      (when icon
        (insert-image icon)
        (insert "\n\n"))
      ;; Show which database this entry came from when several are configured.
      (when (> (length keemacs-databases) 1)
        (insert (funcall label "Database")
                (or (keemacs--database-name) "(unknown)") "\n"))
      (insert (funcall label "Group")
              (keemacs--entry-group keemacs-view-path) "\n")
      (dolist (f '("Title" "UserName"))
        (insert (funcall label f) (keemacs--field entry f) "\n"))
      (insert (funcall label "Password") pw "\n")
      (dolist (f '("URL" "Notes"))
        (insert (funcall label f) (keemacs--field entry f) "\n"))
      (insert (keemacs--view-menu)))
    (goto-char (point-min)))
  (setq buffer-read-only t))

(defun keemacs-view-reveal ()
  "Toggle showing the password in the view buffer.
A press reveals the password; a second press hides it again.  Revealing
does not copy -- copying is the `p' action.  Does nothing for an entry
with no password."
  (interactive)
  (if (string-blank-p (keemacs--field
                       (keemacs--entry-get keemacs-view-path)
                       "Password"))
      (message "No password for this entry")
    (setq-local keemacs-view--revealed
                (not keemacs-view--revealed))
    (keemacs-view-update keemacs-view--revealed)))

(defun keemacs-view-copy-username ()
  "Copy the username of the entry in the view buffer."
  (interactive)
  (keemacs--kill (keemacs--field
                         (keemacs--entry-get keemacs-view-path)
                         "UserName")
                        "Username copied"))

(defun keemacs-view-copy-password ()
  "Copy the password of the entry in the view buffer."
  (interactive)
  (keemacs-reveal-password keemacs-view-path))

(defun keemacs-view-edit ()
  "Edit the entry shown in the view buffer."
  (interactive)
  (keemacs-edit keemacs-view-path))

(defun keemacs-view (path)
  "View the entry at PATH, hiding its password until a key reveals it."
  (interactive "sEntry path: ")
  ;; An entry's custom icon is only known from an export; if this view was
  ;; not reached through a browse listing, load once so the icon is there.
  (unless (assoc path keemacs--entry-custom-icons)
    (keemacs--load-entries))
  (let ((buf (get-buffer-create "*keemacs-view*")))
    (with-current-buffer buf
      (keemacs-view-mode)
      (setq-local keemacs-view-path path)
      (keemacs-view-update nil))
    (switch-to-buffer buf)))

(defun keemacs-view-refresh (&optional new-path)
  "Redraw the open view buffer from the (reloaded) database.
NEW-PATH, when given, re-points the view at the entry's new path (after a
rename).  Does nothing if no view buffer is open."
  (let ((buf (get-buffer "*keemacs-view*")))
    (when buf
      (with-current-buffer buf
        (when new-path
          (setq-local keemacs-view-path new-path))
        (keemacs-view-update keemacs-view--revealed)))))


;;; Entry buffer (add / clone / edit)

(defvar-local keemacs--entry-action nil
  "For the entry buffer: the action being performed (add/edit).")
(defvar-local keemacs--entry-original nil
  "For the entry buffer: the original path being edited, if any.")

(defconst keemacs-entry-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'keemacs--entry-commit)
    (define-key map (kbd "C-c C-k") #'kill-buffer-and-window)
    (define-key map (kbd "C-c C-r") #'keemacs--entry-regenerate)
    ;; Not C-c C-g: a C-g after a prefix key is specially handled by Emacs
    ;; as "cancel the prefix" and can never be dispatched to a binding.
    (define-key map (kbd "C-c C-p") #'keemacs--entry-choose-group)
    (define-key map (kbd "TAB") #'keemacs--entry-next-field)
    map)
  "Keymap for `keemacs-entry-mode'.")

(define-minor-mode keemacs-entry-mode
  "Minor mode for editing a KeePass entry as a text buffer.

Keys:
\\<keemacs-entry-mode-map>
\\[keemacs--entry-commit]  commit this entry
\\[keemacs--entry-choose-group]  choose the entry's group by completion
\\[keemacs--entry-regenerate]  regenerate the password
\\[kill-buffer-and-window]  cancel and close
\\[keemacs--entry-next-field]  next field"
  :lighter " Kb-Entry"
  :keymap keemacs-entry-mode-map)

(defconst keemacs--entry-hint
  ";; Keys: C-c C-c commit | C-c C-p select group
;; Keys: C-c C-r regenerate password | C-c C-k cancel"
  "Comment lines shown at the top of entry buffers.
They are ignored by `keemacs--parse-entry' (which only reads
recognized `Field: value' lines), so they never reach the database.")

(defun keemacs--entry-next-field ()
  "Move to the next \"Field: value\" line."
  (interactive)
  (if (re-search-forward "^[A-Za-z]+: " nil t)
      (goto-char (match-beginning 0))
    (goto-char (point-min))))

(defun keemacs--generate-args (label length)
  "Return the keepassxc-cli command for generation option LABEL.
LABEL is the label of an entry (LABEL ARGS) in
`keemacs-generate-options'; the returned list is ARGS with every
`:length' placeholder replaced by LENGTH.  ARGS begins with the
subcommand (\"generate\" or \"diceware\")."
  (let ((args (cadr (assoc label keemacs-generate-options))))
    (unless args
      (user-error "Unknown password generation option: %S" label))
    ;; `call-process' takes only strings, so the numeric length becomes a
    ;; string in place of the `:length' placeholder.
    (mapcar (lambda (a) (if (eq a :length) (number-to-string length) a))
            args)))

(defun keemacs--read-charset ()
  "Prompt for a password character set, with a nice descriptive label.
Completes over the labels of `keemacs-generate-options',
defaulting to `keemacs--last-generated-charset' when set, else to
the first entry; remembers the choice for next time.  Returns the label."
  (let* ((labels (mapcar #'car keemacs-generate-options))
         (default-label (or keemacs--last-generated-charset
                            (car labels)))
         (chosen (completing-read (keemacs--prompt "Password character set: ")
                                  labels nil t nil nil default-label)))
    (setq keemacs--last-generated-charset chosen)
    chosen))

(defun keemacs--generate-failure-message (charset output)
  "Return a helpful message for a failed generation with CHARSET and OUTPUT.
keepassxc-cli reports \"Invalid password generator after applying all
options\" when the requested length is too short for the chosen character
classes; this advises a longer length instead of showing the raw error."
  (if (string-match-p "Invalid password generator" output)
      (format "Password length is too short for the %S option -- generate again with a longer length"
              charset)
    "Could not generate password (keepassxc-cli failed)"))

(defun keemacs--entry-regenerate ()
  "Insert a generated password into the Password line.
Asks for a character set (with a descriptive title, remembering the last
one chosen) and a length (defaulting to `keemacs-generate-length'
or the last length used)."
  (interactive)
  (when (re-search-forward "^Password: " nil t)
    (let* ((charset (keemacs--read-charset))
           (len (read-number "Password length: "
                             (or keemacs--last-generated-length
                                 keemacs-generate-length)))
           (run (apply #'keemacs-auth--keepassxc-run ""
                       (keemacs--generate-args charset len))))
      (setq keemacs--last-generated-length len)
      (if (eq (cdr run) 0)
          (progn
            (delete-region (point) (line-end-position))
            (insert (string-trim (car run))))
        (message "%s" (keemacs--generate-failure-message
                       charset (car run))))))
  (goto-char (point-min)))

(defun keemacs--entry-open (name action path &optional template)
  "Open an entry buffer NAME for ACTION on PATH (or nil to add).
TEMPLATE is the initial text; defaults to blank standard fields.
The freshly-inserted template is marked unmodified, so the buffer does
not look dirty until you actually change something.  Returns the buffer."
  (let ((buf (generate-new-buffer name)))
    (with-current-buffer buf
      (insert keemacs--entry-hint "\n")
      (insert (or template
                  (mapconcat (lambda (f) (format "%s: " f))
                             '("Group" "Title" "UserName" "Password" "URL" "Notes") "\n")))
      (goto-char (point-min))
      (keemacs-entry-mode)
      (setq-local keemacs--entry-action action)
      (setq-local keemacs--entry-original path)
      (set-buffer-modified-p nil))
    (switch-to-buffer buf)
    buf))

(defun keemacs--parse-entry ()
  "Parse the current entry buffer into a FIELD . VALUE alist.
Each field is a single line, except Notes, which extends from after
\"Notes: \" to the end of the buffer, so multi-line notes are preserved.
The `Group' field, when present, is the entry's location -- a KeePass
group path ending in \"/\" -- shown above Title."
  (goto-char (point-min))
  (let ((result '())
        (keys '("Group" "Title" "UserName" "Password" "URL")))
    (dolist (key keys)
      (when (re-search-forward (concat "^" key ": ?\\(.*\\)$") nil t)
        (setq result (cons (cons key (match-string 1)) result))))
    ;; Notes is last: everything from after "Notes: " to the end of the
    ;; buffer belongs to it, so multi-line notes survive intact.
    (goto-char (point-min))
    (when (re-search-forward "^Notes: ?" nil t)
      (let ((notes (buffer-substring-no-properties (point) (point-max))))
        (setq result (cons (cons "Notes" (string-trim notes)) result))))
    (nreverse result)))

;;; Add / edit / clone / delete

(defun keemacs-add (&optional target)
  "Add a new entry, in group TARGET's group or a chosen one.
When invoked as an Embark action, TARGET is the selected entry's path and
the new entry is created in the same group as that entry.  When called
directly (M-x or from the command keymap), the group is chosen by
completion -- there is no entry-path prompt.  Fill in the Title and the
other fields in the buffer that opens, then commit with
`keemacs--entry-commit' (C-c C-c)."
  (interactive)
  (keemacs--require-db)
  (let* ((group (if (and target (not (string-blank-p target)))
                    ;; Same group as the selected entry; pure string ops
                    ;; only (see `keemacs--entry-group').
                    (keemacs--entry-group
                     (concat "/" (string-trim-left target "/")))
                  (keemacs--choose-group))))
    (keemacs--entry-open "*keemacs-add*" "add" nil
                                (format "Group: %s\nTitle: \nUserName: \nPassword: \nURL: \nNotes: \n" group))))

(defun keemacs--choose-group ()
  "Choose a KeePass group path by completion (ends in /).
The root group \"/\" is always offered, and is the default -- RET alone
puts the new entry at the top level of the database."
  (let ((groups (cons "/" (keemacs--group-paths))))
    (completing-read (keemacs--prompt "Group (RET for root): ")
                     groups nil nil nil nil "/")))

;;; Group maintenance

(defun keemacs--run-group-cmd (subcommand group)
  "Run keepassxc-cli SUBCOMMAND (mkdir or rmdir) on GROUP.
Signals an error on failure."
  (let ((run (apply #'keemacs--run
                    (keemacs--db-password)
                    (list subcommand "--quiet"
                          (keemacs--database-path) group))))
    (unless (eq (cdr run) 0)
      (keemacs-auth--error (car run) (keemacs--database-path) (cdr run)))))

(defun keemacs-add-group (&optional parent)
  "Create a new group inside group PARENT.
When called as a command (M-x or the command keymap), PARENT is chosen
by completion over the existing groups, the root \"/\" being the
default; then the new group's name is prompted for.  When invoked from
 Lisp, PARENT may be a group path (\"/\" or ending in \"/\")."
  (interactive)
  (keemacs--require-db)
  (let* ((parent (or parent (keemacs--choose-group)))
         (name (read-string (keemacs--prompt
                             (format "New group under %s: " parent)))))
    (when (string-blank-p name)
      (user-error "Group name may not be empty"))
    (when (string-match-p "/" name)
      (user-error "Group name may not contain \"/\" -- it is created under the chosen group"))
    (let ((path (concat (if (string-suffix-p "/" parent)
                            (substring parent 0 -1)
                          parent)
                        "/" name)))
      (keemacs--run-group-cmd "mkdir" path)
      (message "Created group %s" path))))

(defun keemacs-delete-group (&optional group)
  "Delete the KeePass group GROUP.
GROUP is chosen by completion (the root \"/\" cannot be deleted); it
may also be a list of group paths (the Embark multi-target form) --
one confirmation covers all of them.  keepassxc-cli recycles the
groups: the whole subtree, entries included, moves to the Recycle Bin
and can be restored from the GUI."
  (interactive)
  (keemacs--require-db)
  (let* ((groups (mapcar #'directory-file-name
                         (if (listp group)
                             group
                           (list (or group
                                     ;; Offer only real groups for
                                     ;; deletion -- the root cannot be
                                     ;; deleted.
                                     (completing-read
                                      (keemacs--prompt "Delete group: ")
                                      (keemacs--group-paths)
                                      nil t))))))
         (groups (cl-remove-if (lambda (g)
                                 (or (string-empty-p g) (string-equal g "/")))
                               groups)))
    (when (null groups)
      (user-error "Cannot delete the root group"))
    (dolist (g groups)
      (when (string-prefix-p "/Recycle Bin" g)
        (user-error "Cannot delete the Recycle Bin")))
    (when (yes-or-no-p
           (format "Delete %s (entries go to the Recycle Bin)? "
                   (string-join groups ", ")))
      (dolist (g groups)
        (keemacs--run-group-cmd "rmdir" g)
        (message "Deleted group %s (recycled)" g)))))

(defun keemacs--entry-choose-group ()
  "Choose the entry's group by completion, replacing the `Group' line.
Putting an entry in a group is a rare and error-prone edit, so the safe
way is to complete over the groups that already exist rather than typing
the path by hand (a typo would silently aim at a group that does not
exist).  Bound to `C-c C-p' in `keemacs-entry-mode-map'; with
point in the entry buffer, this replaces the `Group:' line."
  (interactive)
  (let ((group (keemacs--choose-group)))
    (goto-char (point-min))
    (if (re-search-forward "^Group: " nil t)
        (progn
          (delete-region (point) (line-end-position))
          (insert group))
      (insert (format "Group: %s\n" group)))))

(defun keemacs-edit (path)
  "Edit the entry at PATH in an entry buffer.
The Group line holds the entry's group (the folder it sits in); the
Title line the entry's title.  Both come from the database rather than
from PATH: a title containing \"/\" cannot be recovered by splitting the
path (the last segment would silently rename the entry on commit).  The
full path is tracked in `keemacs--entry-original', and the commit
rebuilds the path from it, so editing does not become a spurious
add/delete."
  (interactive "sEntry path: ")
  ;; The recorded parent group comes from an export; if this edit was not
  ;; reached through a browse listing, load once so it is available.
  (unless (assoc path keemacs--entry-parents)
    (keemacs--load-entries))
  (let* ((entry (keemacs--entry-get path))
         (title (or (keemacs--field entry "Title")
                    (keemacs--entry-basename path)))
         (group (keemacs--entry-group path)))
    (keemacs--entry-open "*keemacs-edit*" "edit" path
                                (format "Group: %s\nTitle: %s\nUserName: %s\nPassword: %s\nURL: %s\nNotes: %s\n"
                                        group
                                        title
                                        (keemacs--field entry "UserName")
                                        (keemacs--field entry "Password")
                                        (keemacs--field entry "URL")
                                        (keemacs--field entry "Notes")))))

(defun keemacs-clone (path)
  "Clone the entry at PATH into an entry buffer."
  (interactive "sEntry path: ")
  (unless (assoc path keemacs--entry-parents)
    (keemacs--load-entries))
  (let ((entry (keemacs--entry-get path)))
    (keemacs--entry-open
     "*keemacs-clone*" "add" nil
     (concat (format "Group: %s\n" (keemacs--entry-group path))
             (mapconcat (lambda (f)
                          (format "%s: %s" f (keemacs--field entry f)))
                        '("Title" "UserName" "Password" "URL" "Notes")
                        "\n")))))

(defun keemacs--delete-entry (path)
  "Delete the entry at PATH without confirmation."
  ;; Resolve a padded Embark target to a real path; clean paths (leading /)
  ;; are used as-is.
  (unless (string-prefix-p "/" (or path ""))
    (setq path (keemacs--path-of path)))
  (let ((run (apply #'keemacs--run
                    (keemacs--db-password)
                    (list "rm" "--quiet" (keemacs--database-path) path))))
    (if (eq (cdr run) 0)
        (progn
          ;; If the deleted entry is displayed in the view buffer, close it --
          ;; its path no longer exists, so it could only show stale data.
          (let ((view (get-buffer "*keemacs-view*")))
            (when (and view
                       (with-current-buffer view
                         (equal keemacs-view-path path)))
              (kill-buffer view)))
          (message "Deleted %s" path))
      (keemacs-auth--error (car run) (keemacs--database-path) (cdr run)))))

(defun keemacs-delete (path)
  "Delete the entry at PATH, with confirmation.
PATH may be a list of entry paths (the Embark multi-target form) --
one confirmation covers all of them."
  (interactive "sEntry path: ")
  (if (listp path)
      (when (and path
                 (yes-or-no-p (format "Delete %d entries? " (length path))))
        (dolist (p path) (keemacs--delete-entry p)))
    (when (yes-or-no-p (format "Delete entry %s? " path))
      (keemacs--delete-entry path))))

(defun keemacs-move (target)
  "Move the entry at TARGET into a chosen group, keeping its title.
TARGET is an entry path, or a list of entry paths (the Embark
multi-target form) -- all of them move into the same group, the group
choice being the only confirmation.  The move is a `keepassxc-cli mv',
so entries are never deleted and re-added, and a title containing \"/\"
moves with the entry intact.  Entries already in the chosen group are
skipped."
  (interactive "sEntry path: ")
  (keemacs--require-db)
  (let* ((paths (mapcar (lambda (p)
                          (if (string-prefix-p "/" p) p
                            (or (keemacs--path-of p) p)))
                        (if (listp target) target (list target))))
         (group (keemacs--choose-group))
         (moves (cl-remove-if
                 (lambda (move)
                   (string-equal (keemacs--entry-group (car move)) group))
                 (mapcar (lambda (p)
                           ;; The title as it exists now cannot be
                           ;; derived from the path when it contains
                           ;; "/" -- fetch it.
                           (cons p (concat group
                                           (keemacs--field
                                            (keemacs--entry-get p)
                                            "Title"))))
                         paths))))
    (when (null moves)
      (user-error "Already in %s" group))
    ;; keepassxc-cli reads the database password from stdin for `mv'
    ;; -- exactly like the entry commit's move.  The passwords travel
    ;; in STDIN; the global options are command arguments.
    (let* ((dbpw (keemacs--db-password))
           (stdin (if (eq dbpw :no-password) "" (concat dbpw "\n")))
           (db (keemacs--database-path))
           (global (append (keemacs-auth--no-password-flag dbpw)
                           (keemacs--db-keyfile)
                           (keemacs--db-yubi))))
      (dolist (move moves)
        (let ((run (apply #'keemacs-auth--keepassxc-run-stdin
                          stdin
                          (append (list "mv") global
                                  (list db (car move) group)))))
          (unless (eq (cdr run) 0)
            (keemacs-auth--error (car run) db (cdr run))))))
    ;; Re-point the view buffer at a moved entry it is showing.
    (let ((view (get-buffer "*keemacs-view*")))
      (when view
        (with-current-buffer view
          (when-let* ((move (assoc keemacs-view-path moves)))
            (setq keemacs-view-path (cdr move))
            (keemacs-view-update nil)))))
    (message "Moved %s to %s"
             (mapconcat (lambda (move) (keemacs--entry-basename (car move)))
                        moves ", ")
             group)))

(defun keemacs--entry-commit ()
  "Commit the add/clone/edit in the current entry buffer.
A new entry (or a clone) uses `keepassxc-cli add'.  An edit edits the
entry in place with `keepassxc-cli edit' (passing `-t' when the title
changed, which renames within the current group); if the `Group' field
was changed to a different group, the entry is first moved there with
`keepassxc-cli mv'.  Either way a rename or move never becomes a
delete+add and never creates a Recycle-Bin duplicate."
  (interactive)
  (let* ((entry (keemacs--parse-entry))
         (action (buffer-local-value 'keemacs--entry-action (current-buffer)))
         (original (buffer-local-value 'keemacs--entry-original (current-buffer)))
         (title (string-trim (keemacs--field entry "Title")))
         (password (keemacs--field entry "Password"))
         (group (string-trim (keemacs--field entry "Group")))
         (group (if (string-empty-p group)
                    ;; No Group given: keep the entry where it already is
                    ;; (edit), or root for a new entry.
                    (if original (keemacs--entry-group original) "")
                  (if (string-suffix-p "/" group) group (concat group "/"))))
         ;; Where the entry ends up after this commit.
         (new-path (if (string-prefix-p "/" title) title (concat group title))))
    (when (string-empty-p title)
      (user-error "Title may not be empty"))
    (let* ((dbpw (keemacs--db-password))
           ;; keepassxc-cli reads the database password then (with -p) the
           ;; entry password from stdin.  For a passwordless DB there is no
           ;; database password line; --no-password tells keepassxc-cli so.
           (stdin (if (eq dbpw :no-password)
                      (concat password "\n")
                    (concat dbpw "\n" password "\n")))
           ;; Global options come before the positional database argument.
           (db (keemacs--database-path))
           (global (append (keemacs-auth--no-password-flag dbpw)
                           (keemacs--db-keyfile)))
           (common (append (list "-u" (keemacs--field entry "UserName")
                                 "--url" (keemacs--field entry "URL")
                                 "--notes" (keemacs--field entry "Notes")
                                 "-p")))
           (run
            (cond
             ((string= action "edit")
              (let* ((orig-group (keemacs--entry-group
                                  (or original "")))
                     (moved (not (string-equal orig-group group))))
                (if (not moved)
                    ;; Same group: `edit -t' renames in place.
                    (apply #'keemacs-auth--keepassxc-run-stdin stdin
                           (append (list "edit") global (list db original "-t" title) common))
                  ;; Different group: move first, then edit fields/title at the
                  ;; new site, so the entry is never deleted and re-added.
                  ;; The entry's title as it exists NOW cannot be derived
                  ;; from ORIGINAL when the title contains "/" -- fetch it.
                  (let* ((old-title (or (ignore-errors
                                          (keemacs--field
                                           (keemacs--entry-get original)
                                           "Title"))
                                        (keemacs--entry-basename original)))
                         (tmp (concat group old-title))
                         (mv (apply #'keemacs-auth--keepassxc-run-stdin stdin
                                    (append (list "mv") global (list db original group)))))
                    (unless (eq (cdr mv) 0)
                      (keemacs-auth--error (car mv) (keemacs--database-path) (cdr mv)))
                    (apply #'keemacs-auth--keepassxc-run-stdin stdin
                           (append (list "edit") global (list db tmp "-t" title) common))))))
             (t ; add/clone: create under the chosen group.
              (apply #'keemacs-auth--keepassxc-run-stdin stdin
                     (append (list "add") global (list db new-path) common))))))
      (if (eq (cdr run) 0)
          (progn
            ;; Re-point and redraw the view buffer at the entry as it now
            ;; exists (a rename, a move, or a fresh clone).
            (keemacs-view-refresh new-path)
            (kill-buffer (current-buffer))
            (message "keepassxc-cli %s entry \"%s\""
                     (if (string= action "edit") "edit" "add") title))
        (keemacs-auth--error (car run) (keemacs--database-path) (cdr run))))))

;;; Embark integration

(defvar keemacs--selecting nil
  "Non-nil while `keemacs-select' is active.")

(defun keemacs--path-from-minibuffer ()
  "Return the entry path from the currently-selected minibuffer candidate.
Reads the `kb-path' text property off the candidate vertico has selected
(never the typed text), so the record identity comes straight from the data
structure rather than by parsing the display."
  (or (and (bound-and-true-p vertico--index)
           (>= vertico--index 0)
           (get-text-property 0 'kb-path
                              (nth vertico--index vertico--candidates)))
      (get-text-property 0 'kb-path (minibuffer-contents))))

(defun keemacs-tree--line-path ()
  "Return the `kb-path' anywhere on the current line, or nil.
Point may sit past a line's text -- a click on a short row lands
there -- so the whole line is searched, not just the character at
point."
  (or (get-text-property (point) 'kb-path)
      (save-excursion
        (goto-char (line-beginning-position))
        (let ((eol (line-end-position)))
          (while (and (< (point) eol)
                      (not (get-text-property (point) 'kb-path)))
            (goto-char (next-single-property-change
                        (point) 'kb-path nil eol)))
          (and (< (point) eol)
               (get-text-property (point) 'kb-path))))))

(defun keemacs--embark-target ()
  "Embark target for the entry under point or in the selection minibuffer.
The target type depends on the context, so Embark shows the right menu:
`keemacs-view' in the view buffer (whose menu omits the redundant
view action), `keemacs' in the listing and tree buffers for entries,
`keemacs-tree-group' for a tree group, `keemacs-tree-db' for a tree
database, and `keemacs-select' in the selection minibuffer (whose
menu adds the insert actions, which only make sense while the
originating buffer's point is preserved)."
  (let (path type)
    (cond
     ;; In the tree view: the entry or group at point, or a database
     ;; row.  Acting on an entry or group first selects its own
     ;; database -- the tree never made one active, and an action
     ;; against the wrong (or no) database errors out, which leaves the
     ;; embark menu open.  A database target carries only its display
     ;; label; its menu's actions work on the section at point.
     ((derived-mode-p 'keemacs-tree-mode)
      (let ((p (keemacs-tree--line-path)))
        (cond
         (p
          (let ((db (keemacs-tree--db-section (magit-current-section))))
            (when db (keemacs-tree--use-db db)))
          (if (string-suffix-p "/" p)
              (setq type 'keemacs-tree-group path p)
            (setq type 'keemacs path p)))
         ((eq (oref (magit-current-section) type) 'keemacs-tree-db)
          (setq type 'keemacs-tree-db
                path (keemacs--spec-label
                      (oref (magit-current-section) value)))))))
     ;; In the listing buffer: the entry at point.
     ((derived-mode-p 'keemacs-mode)
      (setq type 'keemacs
            path (get-text-property (point) 'kb-path)))
     ;; In the view buffer: the entry being viewed.
     ((derived-mode-p 'keemacs-view-mode)
      (setq type 'keemacs-view
            path keemacs-view-path))
     ;; In the selection minibuffer: the current candidate.
     ((and (active-minibuffer-window) keemacs--selecting)
      (setq type 'keemacs-select
            path (keemacs--path-from-minibuffer))))
    (when path (cons type path))))

(add-to-list 'embark-target-finders #'keemacs--embark-target)

;; With more than one database configured, tag the embark action-menu title
;; with the active database -- "Act on keemacs (mydb) ‘/Mail/gmail’" -- so
;; it is always clear which database the actions will hit.  Display only:
;; the advice touches the formatted title, never the target string that
;; actions receive.
(defun keemacs--embark-format-targets (fn target &rest args)
  "Prefix embark's menu title with the active database's name.
Around-advice on `embark--format-targets': with several databases
configured, the formatted title gains a \"(mydb) \" prefix."
  (let ((title (apply fn target args)))
    (if (and (> (length keemacs-databases) 1)
             (keemacs--database-name)
             (symbolp (plist-get target :type))
             (string-prefix-p "keemacs" (symbol-name (plist-get target :type))))
        (let ((pos (string-match-p "‘" title)))
          (if pos
              (concat (substring title 0 pos)
                      (format "(%s) " (keemacs--database-name))
                      (substring title pos))
            title))
      title)))

(advice-add 'embark--format-targets :around #'keemacs--embark-format-targets)

(defun keemacs--build-action-map (&optional excluded)
  "Build an Embark action keymap from `keemacs--actions'.
EXCLUDED is a list of keys (e.g. \"v\") to leave out.  define-key prepends,
so the list is walked in reverse to make the menu *display* the actions in
canonical field order; RET (the default action) is defined first so it
displays last."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'keemacs-run-default-action)
    (dolist (entry (reverse keemacs--actions))
      (pcase-let ((`(,key ,_label ,fn) entry))
        (unless (member key excluded)
          (define-key map (kbd key) fn))))
    map))

(defconst keemacs-action-map
  (keemacs--build-action-map)
  "Embark actions for a keemacs entry target.
Used in the listing buffer, where point is not in an editing context and
the insert actions do not apply.  Copy actions are listed in the canonical
field order: Title, UserName, Password, URL, Notes.")

(defconst keemacs-view-action-map
  (keemacs--build-action-map '("v"))
  "Embark actions for the entry shown in the view buffer.
Same as `keemacs-action-map' minus `v' (view): the view buffer
already shows the entry, so re-viewing it is meaningless.")

(defvar-keymap keemacs-select-action-map
  :doc "Embark actions for a keemacs entry selected from the
selection minibuffer.  Inherits the base actions and adds the insert
actions, which make sense because the point of the originating buffer is
preserved while the minibuffer is active."
  :parent keemacs-action-map
  "P" #'keemacs-insert-username
  "U" #'keemacs-insert-password)
;; The default Embark action for our target types is `keemacs-view'
;; (via the wrapper), not -- as Embark would otherwise fall back to for
;; minibuffer targets -- the command that opened the minibuffer
;; (`keemacs-titles' itself), which would run the selector recursively
;; and error.
(mapc (lambda (type)
        (add-to-list 'embark-default-action-overrides
                     (cons type #'keemacs-run-default-action)))
      '(keemacs keemacs-view keemacs-select))

(add-to-list 'embark-keymap-alist '(keemacs . keemacs-action-map))
(add-to-list 'embark-keymap-alist
             '(keemacs-view . keemacs-view-action-map))
(add-to-list 'embark-keymap-alist
             '(keemacs-select . keemacs-select-action-map))

(defconst keemacs-tree-group-actions
  '(("d" "delete group" keemacs-delete-group)
    ("a" "add entry here" keemacs-add)
    ("A" "add subgroup here" keemacs-add-group))
  "The single source of truth for the group actions offered in the
tree's embark menu.  Each element is (KEY LABEL FUNCTION), where
FUNCTION takes the group path (trailing slash included).")

(defun keemacs-tree-group-toggle (_group)
  "Toggle the tree group at point; the embark group menu's default.
GROUP is the target's path -- the group acted on is the one at point,
whose expansion toggles."
  (interactive "sGroup: ")
  (keemacs-tree-toggle))

(defconst keemacs-tree-group-action-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'keemacs-tree-group-toggle)
    (dolist (entry (reverse keemacs-tree-group-actions))
      (pcase-let ((`(,key ,_label ,fn) entry))
        (define-key map (kbd key) fn)))
    map)
  "Embark actions for a keemacs tree group target.
The group acted on is the one at point; the actions run against the
group's own database, selected when the target was found.")

(add-to-list 'embark-keymap-alist
             '(keemacs-tree-group . keemacs-tree-group-action-map))
(add-to-list 'embark-default-action-overrides
             '(keemacs-tree-group . keemacs-tree-group-toggle))

(defconst keemacs-tree-db-actions
  '(("u" "make active" keemacs-tree-db-use)
    ("l" "unlock" keemacs-tree-db-unlock)
    ("f" "forget cached password" keemacs-tree-db-forget-password))
  "The single source of truth for the database actions offered in the
tree's embark menu.  Each element is (KEY LABEL FUNCTION); the function
acts on the database section at point, in its own database's terms --
the target carries only the display label.")

(defun keemacs-tree--db-at-point ()
  "Return the database section at point, signalling an error if none."
  (or (keemacs-tree--db-section (magit-current-section))
      (user-error "No database at point")))

(defun keemacs-tree-db-toggle (_db)
  "Toggle the tree database section at point; the menu's default.
DB is the target's label; the section acted on is the one at point."
  (interactive "sDatabase: ")
  (keemacs-tree-toggle))

(defun keemacs-tree-db-use (_db)
  "Make the database section at point the active one.
Following commands browse that database."
  (interactive "sDatabase: ")
  (keemacs-tree--use-db (keemacs-tree--db-at-point)))

(defun keemacs-tree-db-unlock (_db)
  "Load the database section at point, prompting for its password.
On success the tree rebuilds with the database's groups; a password
typed now is cached, so the database stays readable."
  (interactive "sDatabase: ")
  (keemacs-tree--unlock (oref (keemacs-tree--db-at-point) value)))

(defun keemacs-tree-db-forget-password (_db)
  "Forget the cached master password of the database at point.
The next read of that database prompts again; the tree keeps showing
it until the next `keemacs-tree-refresh' locks it."
  (interactive "sDatabase: ")
  (let* ((spec (oref (keemacs-tree--db-at-point) value))
         (label (keemacs--spec-label spec)))
    (password-cache-remove
     (expand-file-name
      (keemacs-auth-db-spec-file (keemacs-auth-db-spec-normalize spec))))
    (message "Forgot the password for %s" label)))

(defconst keemacs-tree-db-action-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'keemacs-tree-db-toggle)
    (dolist (entry (reverse keemacs-tree-db-actions))
      (pcase-let ((`(,key ,_label ,fn) entry))
        (define-key map (kbd key) fn)))
    map)
  "Embark actions for a keemacs tree database target.
The database acted on is the one at point; making it active and
unlocking are the interesting ones -- the tree itself loaded the
databases it could read without prompting.")

(add-to-list 'embark-keymap-alist
             '(keemacs-tree-db . keemacs-tree-db-action-map))
(add-to-list 'embark-default-action-overrides
             '(keemacs-tree-db . keemacs-tree-db-toggle))

;;; Region selection
;;
;; C-SPC, then move: magit-section highlights the selected sections, and
;; `embark-act-all' acts on all of them at once.  Deleting and moving
;; are multi-target actions, so one prompt covers the whole selection.

(defun keemacs-tree-region-candidates ()
  "Return the tree's region-selected rows as embark candidates.
With an active region that is a valid section selection (C-SPC, then
move) the selection is returned as a `keemacs-multi' target, so
`embark-act-all' can act on all of it at once -- the same way dired
offers its marked files.  The selection must be all entries or all
groups, from a single database: the one the actions will run against,
made active here."
  (when (derived-mode-p 'keemacs-tree-mode)
    (let* ((sections (or (magit-region-sections 'keemacs-tree-entry)
                         (magit-region-sections 'keemacs-tree-group)))
           (dbsecs (and sections
                        (mapcar #'keemacs-tree--db-section sections)))
           (specs (mapcar (lambda (s) (oref s value)) dbsecs)))
      (when (and dbsecs (cl-every #'identity dbsecs)
                 (= 1 (length (cl-remove-duplicates specs :test #'equal))))
        (keemacs-tree--use-db (car dbsecs))
        (cons 'keemacs-multi
              (mapcar (lambda (s) (oref s value)) sections))))))

(add-to-list 'embark-candidate-collectors #'keemacs-tree-region-candidates)

;; Deleting and moving work on the whole selection in one go, when the
;; action is invoked on several candidates at once (`embark-act-all').
;; The region menu is its own type with only the actions that make
;; sense on a set of rows -- the per-entry menu's copies, view and
;; edit would be meaningless there.
(defconst keemacs-multi-actions
  '(("m" "move to group"   keemacs-move)
    ("d" "delete selection" keemacs-multi-delete)
    ("u" "copy usernames"   keemacs-multi-copy-username)
    ("t" "copy titles"      keemacs-multi-copy-title))
  "The single source of truth for the actions offered on a tree
region selection.  Each element is (KEY LABEL FUNCTION); the function
receives the whole selection -- a list of paths -- and acts on it in
one go.")

(defun keemacs-tree--multi-entries (paths)
  "Return the entry paths (no trailing slash) among PATHS."
  (cl-remove-if (lambda (p) (string-suffix-p "/" p)) paths))

(defun keemacs-multi-delete (paths)
  "Delete the selected rows, with one confirmation for all of them.
PATHS is the list of selected paths (the Embark multi-target form):
entries go through `keemacs-delete', groups through
`keemacs-delete-group', each recycling to the Recycle Bin."
  (interactive "sPaths: ")
  (let ((paths (if (listp paths) paths (list paths))))
    (when-let* ((entries (keemacs-tree--multi-entries paths)))
      (keemacs-delete entries))
    (when-let* ((groups (cl-remove-if-not
                         (lambda (p) (string-suffix-p "/" p)) paths)))
      (keemacs-delete-group groups))))

(defun keemacs--multi-copy (field paths)
  "Copy every selected entry's FIELD to the kill ring, one per line.
PATHS is the selection (the Embark multi-target form); group paths
have no fields and are skipped."
  (let ((values (mapcan (lambda (p)
                          (unless (string-suffix-p "/" p)
                            (list (keemacs--field (keemacs--entry-get p)
                                                  field))))
                        (if (listp paths) paths (list paths)))))
    (if values
        (progn (kill-new (string-join values "\n"))
               (message "Copied %d %ss" (length values) field))
      (user-error "No entries in the selection"))))

(defun keemacs-multi-copy-username (paths)
  "Copy every selected entry's username, one per line, to the kill ring."
  (interactive "sPaths: ")
  (keemacs--multi-copy "UserName" paths))

(defun keemacs-multi-copy-title (paths)
  "Copy every selected entry's title, one per line, to the kill ring."
  (interactive "sPaths: ")
  (keemacs--multi-copy "Title" paths))

(defconst keemacs-multi-action-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'keemacs-move)
    (dolist (entry (reverse keemacs-multi-actions))
      (pcase-let ((`(,key ,_label ,fn) entry))
        (define-key map (kbd key) fn)))
    map)
  "Embark actions for a tree region selection (`keemacs-multi').
Only actions that make sense on a set of rows -- the per-entry menu's
copies, view and edit would be meaningless here.  The actions receive
the whole selection and act on it in one go, against the selection's
database, which the candidate collector made active.")

(add-to-list 'embark-keymap-alist
             '(keemacs-multi . keemacs-multi-action-map))
(add-to-list 'embark-default-action-overrides
             '(keemacs-multi . keemacs-move))
(mapc (lambda (command)
        (add-to-list 'embark-multitarget-actions command))
      '(keemacs-move keemacs-multi-delete keemacs-multi-copy-username
                     keemacs-multi-copy-title))

(defun keemacs-tree-act ()
  "Open an embark menu for the tree row at point.
With a valid region selection (C-SPC, then move), open
`embark-act-all' instead, acting on every selected row at once."
  (interactive)
  (if (and (region-active-p) (magit-region-sections))
      (embark-act-all)
    (embark-act)))

(defun keemacs-run-default-action (path)
  "Run `keemacs-default-action' on the entry at PATH.
A command wrapper so RET in the action map can invoke whatever function
`keemacs-default-action' names."
  (interactive "sEntry path: ")
  (funcall keemacs-default-action path))

;;; Listing buffer

(defvar keemacs-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "C-.") #'embark-act)
    (define-key map (kbd "g") #'keemacs-refresh)
    map)
  "Keymap for `keemacs-mode'.")

(define-derived-mode keemacs-mode special-mode "keemacs"
  "Major mode for the keemacs listing buffer."
  (setq buffer-read-only nil
        truncate-lines t
        revert-buffer-function #'keemacs--revert))

(defun keemacs--revert (&rest _)
  "Refresh the listing buffer from the database."
  (keemacs--insert-list))

(defun keemacs--insert-list ()
  "Insert the current entries into the current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (dolist (candidate (keemacs--candidates))
      (insert candidate "\n"))
    (goto-char (point-min))))

(defun keemacs-refresh ()
  "Reload the entry list from the database and redisplay."
  (interactive)
  (let ((buf (current-buffer)))
    (keemacs--load-entries)
    (with-current-buffer buf
      (keemacs--insert-list))))

;;;###autoload
(defun keemacs-buffer ()
  "Open a columned listing buffer of all entries.
Press `embark-act' (`C-.') on a row to reach the action menu."
  (interactive)
  (keemacs--require-db)
  (let ((buf (get-buffer-create "*keemacs*")))
    (switch-to-buffer buf)
    (unless (eq major-mode 'keemacs-mode)
      (keemacs-mode))
    (keemacs--insert-list)))

;;;; Tree view
;;
;; The main screen: the whole active database as a magit-section tree of
;; groups and entries in one full-window buffer.

;; magit-section matches sections by their class.  Plain symbols as the
;; class only work for magit's own registered types (per its docstring,
;; an "undocumented kludge" not available to other packages), so the
;; tree declares real subclasses.
(defclass keemacs-tree-db (magit-section)
  ((data :initarg :data :initform nil)))
(defclass keemacs-tree-group (magit-section) ())
(defclass keemacs-tree-entry (magit-section) ())

(defcustom keemacs-tree-expand-databases 'none
  "Which databases in the tree start with their groups expanded.
`all' opens the groups of every database that can be read without
prompting, `current' only the active database's, `none' (the default)
starts every group closed -- TAB opens one.  Locked databases have
nothing to show until they are unlocked.  Either way an entry's
fields start closed: TAB on an entry reveals them."
  :type '(choice (const :tag "All queryable databases" all)
                 (const :tag "The active database" current)
                 (const :tag "None" none))
  :group 'keemacs)

(defvar keemacs-tree-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "RET") #'keemacs-tree-activate)
    (define-key map (kbd "TAB") #'keemacs-tree-toggle)
    ;; With a region selected this offers `embark-act-all' on every
    ;; selected row -- move or delete the selection in one go.
    (define-key map (kbd "C-.") #'keemacs-tree-act)
    (define-key map (kbd "g") #'keemacs-tree-refresh)
    (define-key map (kbd "q") #'quit-window)
    (define-key map [double-mouse-1] #'keemacs-tree-click)
    map)
  "Keymap for `keemacs-tree-mode'.
Movement and expansion keys (n, p, M-n, M-p, ^, level keys) are
inherited from `magit-section-mode-map'.  A single click only moves
point; a double click acts on the item (open an entry, toggle a group
or database) -- group and database headings also toggle via magit's
own heading bindings.")

(define-derived-mode keemacs-tree-mode magit-section-mode "keemacs-tree"
  "Major mode for the keemacs tree view buffer.
The buffer lists every configured database as a tree: one section per
database, one collapsible section per group, one line per entry.
\\[keemacs-tree-activate] or a double click on an entry runs
`keemacs-default-action'; on a group or database it toggles the
section.  \\[keemacs-tree-toggle] expands and collapses -- on an entry
it reveals the fields, on the masked password line the password.
\\[keemacs-tree-act] opens the action menu for the entry at point -- or,
with a region selected (C-SPC, then move), offers acting on every
selected row at once.  \\[keemacs-tree-refresh] reloads the loaded
databases."
  (setq-local revert-buffer-function #'keemacs-tree-refresh))

(defun keemacs-tree--format-entry (path entry)
  "Return the title-only display line for ENTRY at PATH.
Tagged with `kb-path'; prefixed with the entry's icon (a real
thumbnail on graphic displays, else the standard icon glyph); the
title carries `keemacs-title'."
  (let* ((prefix (keemacs--candidate-prefix path entry))
         (str (concat prefix
                      (if (string-empty-p prefix) "" " ")
                      (propertize (keemacs--field entry "Title")
                                  'face 'keemacs-title))))
    (put-text-property 0 (length str) 'kb-path path str)
    str))

(defun keemacs-tree--queryable-p (spec)
  "Return non-nil if database SPEC can be read without prompting.
That is the case when it has no master password, or its password is a
known string or function, or the `:prompt' password it would ask for is
already in the password cache -- keyed by the expanded database file,
exactly like `keemacs-auth--read-password'."
  (let* ((spec (keemacs-auth-db-spec-normalize spec))
         (password (keemacs-auth-db-spec-password spec)))
    (cond ((null password) t)
          ((or (stringp password) (functionp password)) t)
          (t (password-in-cache-p
              (expand-file-name (keemacs-auth-db-spec-file spec)))))))

(defun keemacs-tree--load (spec)
  "Load database SPEC, returning the data its tree section needs.
This may prompt for the master password through the normal machinery;
check `keemacs-tree--queryable-p' first when building the default view.
The result is a plist carrying the entries plus the export globals
`keemacs--load-entries' publishes, so every database renders from its
own snapshot no matter how many were loaded since."
  (let ((keemacs-database spec))
    (list :spec spec
          :label (keemacs--spec-label spec)
          :entries (keemacs--load-entries)
          :groups keemacs--group-icons
          :parents keemacs--entry-parents
          :custom-icons keemacs--entry-custom-icons
          :images keemacs--custom-icons)))

(defun keemacs-tree--db-open-p (spec)
  "Return non-nil if database SPEC's groups start expanded.
Per `keemacs-tree-expand-databases': `all' opens every loaded
database, `current' only the active one, `none' none."
  (pcase keemacs-tree-expand-databases
    ('all t)
    ('current
     (and keemacs-database
          (equal (expand-file-name
                  (keemacs-auth-db-spec-file
                   (keemacs-auth-db-spec-normalize spec)))
                 (keemacs--database-path))))
    (_ nil)))

(defun keemacs-tree--format-db (spec &optional locked)
  "Return the heading string for database SPEC.
LOCKED marks a database whose tree has not been loaded (or could not
be)."
  (concat (keemacs--spec-label spec)
          (when locked
            (propertize " (locked)" 'face 'shadow))))

(defun keemacs-tree--build-db (data depth)
  "Insert the tree of database DATA (from `keemacs-tree--load') at DEPTH.
Each database renders from its own snapshot of the export globals, so
the databases never see each other's icons or group maps.  Its groups
start expanded per `keemacs-tree-expand-databases'."
  (let ((keemacs--group-icons (plist-get data :groups))
        (keemacs--entry-parents (plist-get data :parents))
        (keemacs--entry-custom-icons (plist-get data :custom-icons))
        (keemacs--custom-icons (plist-get data :images)))
    (keemacs-tree--build (plist-get data :entries) "/" depth
                         (keemacs-tree--db-open-p (plist-get data :spec)))))

(defun keemacs-tree--build (entries group depth open)
  "Insert tree sections for GROUP (path with trailing \"/\") at DEPTH.
ENTRIES is one database's ((PATH . FIELDS) ...).  Groups come before
their entries, in the same order `keemacs--group-choose' offers them.
magit-section does not indent, so the tree indents each level by two
spaces.  OPEN says whether the groups start expanded; entries always
start closed, TAB revealing their fields."
  (let ((inhibit-read-only t))
    (pcase-let* ((`(,groups . ,subentries)
                  (keemacs--group-contents entries group))
                 (indent (make-string (* 2 depth) ?\s)))
      (dolist (g groups)
        (magit-insert-section (keemacs-tree-group g (not open))
          (magit-insert-heading (concat indent (keemacs--format-group g)))
          (keemacs-tree--build entries g (1+ depth) open)))
      (dolist (e subentries)
        (let ((sec (magit-insert-section (keemacs-tree-entry (car e) t)
                     (magit-insert-heading (concat indent
                                                   (keemacs-tree--format-entry
                                                    (car e) (cdr e))))
                     (keemacs-tree--insert-fields (car e) (cdr e)
                                                  (concat indent "  ")))))
          ;; A double click on an entry means "open it", not "toggle
          ;; its fields" -- drop magit's heading keymap (which would
          ;; toggle) so the mode's binding applies.  Group and database
          ;; headings keep it; there a toggle is exactly what a click
          ;; means.
          (remove-text-properties (oref sec start) (oref sec content)
                                  '(keymap nil)))))))

(defun keemacs-tree--insert-fields (path entry indent)
  "Insert ENTRY at PATH's non-empty fields as lines at INDENT.
UserName, Password, URL and Notes -- the title is the entry's own
line, so it is not repeated as a field.  The password shows masked as
\"******\" -- TAB on it reveals the real password as a sub-line, TAB
again conceals it.  Each line is tagged with the entry's `kb-path',
so the action menu works from a field line too.  A multi-line Notes
value shows its first line only."
  (dolist (field '("UserName" "Password" "URL" "Notes"))
    (let ((value (keemacs--field entry field)))
      (unless (string-empty-p value)
        (let* ((masked (equal field "Password"))
               (str (concat indent
                            (propertize
                             (truncate-string-to-width field 10 nil ?\s)
                             'face 'keemacs-field-label)
                            " "
                            (if masked "******"
                              (car (split-string value "\n"))))))
          (put-text-property 0 (length str) 'kb-path path str)
          (when masked
            (put-text-property 0 (length str) 'kb-pw t str)
            (put-text-property 0 (length str) 'kb-indent indent str))
          (insert str "\n"))))))

(defvar-local keemacs-tree--built-databases nil
  "The `keemacs-databases' the tree buffer was last built from.")

(defvar-local keemacs-tree--db-mtimes nil
  "Alist of (FILE . MTIME) for the configured databases, as of the
last tree build.  Used to detect, cheaply, whether the tree is stale:
a rebuild only re-exports when one of the files changed on disk.")

(defun keemacs-tree--current-mtimes ()
  "Return (FILE . MTIME) for every configured database.
A missing file gets an MTIME of nil."
  (mapcar (lambda (spec)
            (let* ((file (expand-file-name
                          (keemacs-auth-db-spec-file
                           (keemacs-auth-db-spec-normalize spec))))
                   (attr (file-attributes file)))
              (cons file (and attr
                              (file-attribute-modification-time attr)))))
          keemacs-databases))

(defun keemacs-tree--stale-p ()
  "Return non-nil when the tree needs rebuilding.
That is the case when `keemacs-databases' changed since the tree was
built, or one of the database files changed on disk -- including by
another program, e.g. the KeePass GUI or a sync."
  (or (not (equal keemacs-databases keemacs-tree--built-databases))
      (cl-some (lambda (entry)
                 (let ((attr (file-attributes (car entry))))
                   (not (equal (and attr
                                    (file-attribute-modification-time attr))
                               (cdr entry)))))
               keemacs-tree--db-mtimes)))

(defun keemacs-tree--insert ()
  "Build the tree sections in the current buffer from the databases.
The root of the tree is one section per database in
`keemacs-databases'.  A database that can be read without prompting --
no master password, or the password known or already cached -- is
expanded; the others show only their name, marked \"(locked)\", until
activated, which asks for the password.  Groups start closed and
entries start with their fields closed; see
`keemacs-tree-expand-databases'."
  (let ((inhibit-read-only t))
    (erase-buffer)
    ;; `magit-insert-section' would make the first top-level section
    ;; the root; point it at a fresh invisible root instead, so the
    ;; top level holds real sections only.  The old root is kept so
    ;; every rebuilt section inherits its open/closed state.
    (setq-local magit-root-section (make-instance 'magit-section :type 'root))
    (setq-local magit-insert-section--current nil)
    (setq-local magit-insert-section--parent magit-root-section)
    (setq-local magit-insert-section--oldroot nil)
    (keemacs--check-databases)
    (if (null keemacs-databases)
        (insert "(no databases configured)\n")
      (dolist (spec keemacs-databases)
      ;; Load before inserting anything: a failed read (e.g. a stale
      ;; cached password) leaves no partial section behind, and the
      ;; database shows as locked instead.
      (let ((data (and (keemacs-tree--queryable-p spec)
                       (condition-case err
                           (keemacs-tree--load spec)
                         (error
                          (message "keemacs: %s: %s"
                                   (keemacs--spec-label spec)
                                   (error-message-string err))
                          nil)))))
        (magit-insert-section (keemacs-tree-db spec nil :data data)
          (magit-insert-heading (keemacs-tree--format-db spec (null data)))
          (when data
            (keemacs-tree--build-db data 1))))))
    ;; Hidden sections only get their overlay when shown is applied
    ;; from the root, which recurses into every hidden child.
    (magit-section-show magit-root-section)
    (goto-char (point-min)))
  (setq-local keemacs-tree--built-databases keemacs-databases
              keemacs-tree--db-mtimes (keemacs-tree--current-mtimes)))

(defun keemacs-tree-refresh (&rest _)
  "Reload the loaded databases and rebuild the tree.
Point stays on the entry it was on, or -- when that entry moved to a
different group -- on an entry of the same name, or moves to the top
when it no longer exists.  Every section keeps its open/closed state
across the rebuild."
  (interactive)
  (let* ((path (get-text-property (point) 'kb-path))
         (base (and path (keemacs--entry-basename path))))
    (keemacs-tree--insert)
    (goto-char
     (or (and path
              (let ((found (text-property-search-forward
                            'kb-path path #'equal)))
                (and found (prop-match-beginning found))))
         ;; The row may have moved to another group: fall back to the
         ;; first entry of the same name.
         (and base
              (let ((found (text-property-search-forward
                            'kb-path base
                            (lambda (_ v)
                              (and (stringp v)
                                   (string-suffix-p (concat "/" base) v))))))
                (and found (prop-match-beginning found))))
         (point-min)))))

(defun keemacs-tree--toggle-password (section)
  "Reveal or conceal the password below the masked field line at point.
SECTION is the entry section the line belongs to; the password is
fetched fresh from the entry's database."
  (keemacs-tree--use-db (keemacs-tree--db-section section))
  (let ((inhibit-read-only t)
        (pw-line (cond ((get-text-property (point) 'kb-pw)
                        (line-beginning-position))
                       ((get-text-property (point) 'kb-pw-reveal)
                        (line-beginning-position 0)))))
    (when pw-line
      (save-excursion
        (goto-char pw-line)
        (let ((next (line-beginning-position 2)))
          (if (get-text-property next 'kb-pw-reveal)
              ;; Conceal: drop the revealed sub-line.
              (delete-region next (line-beginning-position 3))
            ;; Reveal: insert the real password as a sub-line.
            (let* ((path (get-text-property (point) 'kb-path))
                   (password (cdr (assoc "Password"
                                         (keemacs--entry-get path))))
                   (indent (concat (get-text-property (point) 'kb-indent)
                                   "  ")))
              (goto-char next)
              (insert (propertize (concat indent password "\n")
                                  'kb-path path
                                  'kb-pw-reveal t)))))))))

(defun keemacs-tree--conceal (section)
  "Remove any revealed password lines inside SECTION.
Runs before a section is hidden, so a revealed password never
survives a collapse."
  (save-excursion
    (let ((inhibit-read-only t)
          (end (oref section end))
          m)
      (goto-char (oref section start))
      (setq m (text-property-search-forward 'kb-pw-reveal t #'eq))
      (while (and m (< (prop-match-beginning m) end))
        (goto-char (prop-match-beginning m))
        (delete-region (line-beginning-position)
                       (line-beginning-position 2))
        (setq end (oref section end))
        (goto-char (oref section start))
        (setq m (text-property-search-forward 'kb-pw-reveal t #'eq))))))

(defun keemacs-tree--show-ancestors (section)
  "Open any collapsed ancestors of SECTION, topmost first.
Toggling a section inside a collapsed group would rip out the
group's hide overlay -- magit's `remove-overlays' clears any
overlapping overlay -- leaving the group inconsistent (hidden yet
showing its text)."
  (let ((hidden nil)
        (up (oref section parent)))
    (while up
      (when (ignore-errors (oref up hidden))
        (push up hidden))
      (setq up (oref up parent)))
    (dolist (a hidden)
      (magit-section-show a))))

(defun keemacs-tree-toggle ()
  "Expand or collapse the section at point.
A group reveals its entries, an entry its fields, a database its
groups.  TAB on the masked password line reveals the real password as
a sub-line; TAB again conceals it.  On a locked database this loads
the database first, prompting for the master password.  Collapsed
ancestors open first, so what you toggle is always visible."
  (interactive)
  (let ((section (magit-current-section)))
    (keemacs-tree--show-ancestors section)
    (cond
     ((or (get-text-property (point) 'kb-pw)
          (get-text-property (point) 'kb-pw-reveal))
      (keemacs-tree--toggle-password section))
     ((and (eq (oref section type) 'keemacs-tree-db)
           (null (oref section children)))
      (keemacs-tree--unlock (oref section value)))
     (t
      (keemacs-tree--conceal section)
      (magit-section-toggle section)))))

(defun keemacs-tree--db-section (section)
  "Return the database section SECTION belongs to, or nil.
Walks up the section's ancestors."
  (let ((up section))
    (while (and up (not (eq (oref up type) 'keemacs-tree-db)))
      (setq up (oref up parent)))
    (and (eq (oref up type) 'keemacs-tree-db) up)))

(defun keemacs-tree--use-db (section)
  "Make the database of db SECTION the active one.
The entry actions all run against the active database, so activating a
tree entry first selects its own database -- and reinstates that
database's export state, since the tree loaded each database
separately."
  (let* ((spec (oref section value))
         (switched (not (equal spec keemacs-database)))
         (data (oref section data)))
    (setq keemacs-database spec)
    (setq keemacs--group-icons (plist-get data :groups)
          keemacs--entry-parents (plist-get data :parents)
          keemacs--entry-custom-icons (plist-get data :custom-icons)
          keemacs--custom-icons (plist-get data :images))
    (when switched
      (message "Using KeePass database %s" (keemacs--spec-label spec)))))

(defun keemacs-tree--unlock (spec)
  "Load locked database SPEC, prompting for its master password.
On success the tree is rebuilt -- the password is cached then, so SPEC
appears with its groups -- and point moves to its section."
  (condition-case err
      (keemacs-tree--load spec)
    (error (user-error "keemacs: %s: %s" (keemacs--spec-label spec)
                       (error-message-string err))))
  (keemacs-tree-refresh)
  (let ((section (seq-find (lambda (s)
                             (and (eq (oref s type) 'keemacs-tree-db)
                                  (equal (oref s value) spec)))
                           (oref magit-root-section children))))
    (when section
      (goto-char (oref section start))
      (magit-section-show section))))

(defun keemacs-tree-activate ()
  "Act on the tree section at point.
On an entry: run `keemacs-default-action' -- by default `keemacs-view',
which replaces this window with the view buffer -- on the entry's own
database, making it the active one.  On a group: toggle its expansion.
On a database: toggle it too, unless it is locked, in which case it is
loaded first (prompting for the master password).  A revealed password
is concealed before anything is toggled or opened."
  (interactive)
  (let ((section (magit-current-section)))
    (keemacs-tree--show-ancestors section)
    (pcase (oref section type)
      ('keemacs-tree-entry
       (keemacs-tree--conceal section)
       (keemacs-tree--use-db (keemacs-tree--db-section section))
       (keemacs-run-default-action (oref section value)))
      ('keemacs-tree-group
       (keemacs-tree--conceal section)
       (magit-section-toggle section))
      ('keemacs-tree-db
       (keemacs-tree--conceal section)
       (if (oref section children)
           (magit-section-toggle section)
         (keemacs-tree--unlock (oref section value))))
      (_ (user-error "Nothing at point")))))

(defun keemacs-tree-click (event)
  "Act on the tree line at the double click EVENT's position.
Single clicks only move point -- acting on them proved too sensitive."
  (interactive "@e")
  (let ((pos (posn-point (event-start event))))
    (when (numberp pos)
      (goto-char pos)
      (keemacs-tree-activate))))

;;;###autoload
(defun keemacs ()
  "Open the keemacs main screen: every configured database as a tree.
Displays the `keemacs-tree-mode' buffer in the current window, with
one section per database in `keemacs-databases' and its groups beneath
it.  A database that can be read without prompting -- no master
password, or the password known or already cached -- is loaded; the
rest are marked \"(locked)\" and load, prompting, when expanded.  The
groups start closed, opened, or only the active database's do, per
`keemacs-tree-expand-databases'.  RET or a double click on an entry
runs `keemacs-default-action' (by default `keemacs-view') on the
entry's own database, making it the active one; on a group or database
it toggles expansion.  TAB toggles too -- on an entry it reveals the
fields, on the masked password line the password itself.  `C-.' opens
the embark action menu for the entry at point; `g' reloads the loaded
databases."
  (interactive)
  (keemacs--check-databases)
  (unless keemacs-databases
    (user-error "`keemacs-databases' is empty -- add your databases first"))
  (let ((buf (get-buffer-create "*keemacs-tree*")))
    (with-current-buffer buf
      (unless (eq major-mode 'keemacs-tree-mode)
        (keemacs-tree-mode))
      (keemacs-tree--insert))
    (switch-to-buffer buf)))

(defun keemacs-tree--refresh-on-show (&optional frame)
  "Rebuild the tree when it becomes visible, if a database changed.
This keeps the tree honest after changes made anywhere -- by keemacs
itself or by another program writing the kdbx -- without paying for a
re-export on every visit: the rebuild only happens when a configured
database file's modification time changed on disk, or the database
list itself did."
  (dolist (window (window-list frame))
    (with-current-buffer (window-buffer window)
      (when (and (derived-mode-p 'keemacs-tree-mode)
                 (keemacs-tree--stale-p))
        (keemacs-tree-refresh)))))

(add-hook 'window-buffer-change-functions #'keemacs-tree--refresh-on-show)
(add-hook 'window-selection-change-functions #'keemacs-tree--refresh-on-show)

;;;###autoload
(defun keemacs-titles ()
  "Select a KeePass entry via consult/vertico minibuffer completion.
RET runs `keemacs-default-action' (by default `keemacs-view')
on the selected entry; `C-.' opens `keemacs-action-map' for further
actions (copy username/password, edit, ...).  Returns the chosen path."
  (interactive)
  (keemacs--require-db)
  (keemacs--load-entries)
  (let ((keemacs--selecting t))
    (let* ((chosen (consult--read (keemacs--candidates)
                                  :prompt (keemacs--prompt "KeePass entry: ")
                                  :history keemacs-history
                                  :category 'keemacs
                                  :require-match t
                                  :sort nil
                                  :lookup #'consult--lookup-member)))
      (let ((path (keemacs--path-of chosen)))
        (if (null path)
            (user-error "no path on candidate")
          (when keemacs-default-action
            (funcall keemacs-default-action path))
          path)))))

(defun keemacs--group-choose (entries group)
  "Drill down from GROUP, returning the chosen entry path or nil.
ENTRIES is one database's ((PATH . FIELDS) ...) (already exported, so
only a single keepassxc-cli call is made for the whole walk).  At each
level the completion candidates are the current group's subgroups and
entries; choosing a subgroup descends into it (recursively), choosing an
entry returns its path.  Returns nil if a group turns out empty."
  (let* ((contents (keemacs--group-contents entries group))
         (groups (car contents))
         (subentries (cdr contents)))
    (if (and (null groups) (null subentries))
        (progn (message "No entries under %s" group) nil)
      (let* ((cands (append (mapcar #'keemacs--format-group groups)
                            (mapcar (lambda (e)
                                      (keemacs--format-candidate
                                       (car e) (cdr e)))
                                    subentries)))
             (chosen (consult--read cands
                                    :prompt (keemacs--prompt
                                             (format "KeePass (%s): " group))
                                    :history keemacs-history
                                    :category 'keemacs
                                    :require-match t
                                    :sort nil
                                    :lookup #'consult--lookup-member))
             (path (keemacs--path-of chosen)))
        (cond ((null path) (user-error "no path on candidate"))
              ;; A trailing slash marks a group: descend into it.
              ((string-suffix-p "/" path)
               (keemacs--group-choose entries path))
              (t path))))))

;;;###autoload
(defun keemacs-group (&optional path)
  "Select a KeePass entry by navigating its group hierarchy.
Start from GROUP (default \"/\", the root) and complete over each
group's children one level at a time -- subgroups and entries -- until
an entry is chosen; choosing a subgroup descends into it.  When an entry
is picked, `keemacs-default-action' (by default
`keemacs-view') runs on it, exactly as with `keemacs-titles'.
Returns the chosen entry path."
  (interactive)
  (keemacs--require-db)
  (let ((keemacs--selecting t))
    (let* ((path (or path "/"))
           (entries (keemacs--load-entries))
           (result (keemacs--group-choose entries path)))
      (when (and result keemacs-default-action)
        (funcall keemacs-default-action result))
      result)))

;;;; Favorites

(defcustom keemacs-favorites-default nil
  "Favorites offered by `keemacs-favorites' and
`keemacs-favorites-by-key'.
A list of favorite spec plists.  Each spec item describes one favorite:

  (:key   ?b                 ; key in the favorites menu
   :title \"Pika\"            ; regexp against the entry's Title
   :group \"^/Backups/\")     ; regexp against the entry's group path

:title and :group are regular expressions; both may be given, and an
entry matches when every given regexp matches.  The group regexp runs
against the entry's full group path as keepassxc stores it, with a
trailing slash -- \"^/Backups/\" matches the Backups group and
everything in it, while \"/Sales/\" matches a Sales group at any
depth.  The title regexp runs against the entry's Title verbatim.

At least one of :title and :group must be given: items with neither are
ignored with a message when the favorites are used.  :key must be a
character (a one-character string works too) and unique across the
spec; broken or duplicate items are ignored with a message.  :key is
only used by `keemacs-favorites-by-key'; `keemacs-favorites'
narrows by typing instead."
  :type '(repeat (plist :key-type (choice (const :key)
                                          (const :title)
                                          (const :group))
                        :value-type sexp))
  :group 'keemacs)

(defun keemacs-favorites--parse (favorites)
  "Return the usable items of FAVORITES, a list of favorite spec plists.
Each usable item becomes (KEY TITLE-REGEXP GROUP-REGEXP), either regexp
and KEY possibly nil (the keyed menu assigns keys to nil-key items).
Unusable items -- no :title and no :group, non-string regexps, or a
duplicate :key -- are dropped with a message, not an error."
  (let ((items nil) (keys nil))
    (dolist (favorite favorites)
      (let* ((raw-key (plist-get favorite :key))
             (title (plist-get favorite :title))
             (group (plist-get favorite :group))
             (key (cond ((characterp raw-key) raw-key)
                        ((and (stringp raw-key)
                              (= (length raw-key) 1))
                         (aref raw-key 0)))))
        (cond
         ((not (and (or (null title) (stringp title))
                    (or (null group) (stringp group))))
          (message "keemacs favorites: ignoring %S -- :title and :group should be strings"
                   favorite))
         ((and (or (null title) (string-empty-p title))
               (or (null group) (string-empty-p group)))
          (message "keemacs favorites: ignoring %S -- give :title or :group"
                   favorite))
         ((and key (memq key keys))
          (message "keemacs favorites: ignoring %S -- key ?%c is already used"
                   favorite key))
         (t
          (when key (push key keys))
          (push (list key title group) items)))))
    (nreverse items)))

(defun keemacs-favorites--match (spec entries)
  "Return the entries in ENTRIES matching any item of SPEC.
SPEC is a list of (KEY TITLE-REGEXP GROUP-REGEXP) items as returned by
`keemacs-favorites--parse'.  An item matches an entry when every
regexp it carries matches -- TITLE-REGEXP against the entry's Title,
GROUP-REGEXP against its full group path with trailing slash.  The
result is deduplicated on (group . title), the entry's identity: two
spec items may match the same entry, and it should only be offered
once."
  (let ((matches nil) (seen nil))
    (dolist (item spec)
      (let ((title-re (nth 1 item)) (group-re (nth 2 item)))
        (dolist (entry entries)
          (let* ((fields (cdr entry))
                 (title (cdr (assoc "Title" fields)))
                 (group (cdr (assoc "Group" fields)))
                 (identity (cons group title)))
            (when (and (or (null title-re)
                           (and title (string-match-p title-re title)))
                       (or (null group-re)
                           (and group (string-match-p group-re group))))
              (unless (member identity seen)
                (push identity seen)
                (push entry matches)))))))
    (nreverse matches)))

(defun keemacs-favorites--choice (item)
  "Return the `read-multiple-choice' menu entry for favorite ITEM.
The name is the item's title when given, else its group; the
description is the group, when the title was used as the name."
  (let ((key (nth 0 item)) (title (nth 1 item)) (group (nth 2 item)))
    (if title
        (list key title group)
      (list key group))))

(defconst keemacs-favorites--key-pool
  (append (string-to-list "123456789")
          (string-to-list "abcdefghijklmnopqrstuvwxyz")
          (string-to-list "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
  "Characters that may be hotkeys in a favorites menu.")

(defun keemacs-favorites--key-for (label used)
  "Return an unassigned key for the menu entry labelled LABEL.
USED is the list of characters already taken.  Two phases: the first
unused character of LABEL itself that is a regular set character --
digit, lowercase or uppercase letter; never punctuation -- falling back
to the first unused character of `keemacs-favorites--key-pool'
(a mnemonic -- \"github\" offers ?g when free; \"@Mail\" skips ?@ and
offers ?M)."
  (or (seq-find (lambda (c)
                  (and (not (memq c used))
                       (memq c keemacs-favorites--key-pool)))
                (string-to-list label))
      (seq-find (lambda (c) (not (memq c used)))
                keemacs-favorites--key-pool)
      (user-error "No unused keys left for the favorites menu -- set :key on some items")))

(defun keemacs-favorites--assign-keys (spec)
  "Return SPEC with a key on every item.
Items with a :key keep it.  A keyless item's key comes from
`keemacs-favorites--key-for', given its label -- the title when
the item has one, else its group -- and the keys taken so far."
  (let ((used (delq nil (mapcar (lambda (i) (nth 0 i)) spec))))
    (mapcar (lambda (item)
              (if (nth 0 item)
                  item
                (let ((key (keemacs-favorites--key-for
                            (or (nth 1 item) (nth 2 item)) used)))
                  (push key used)
                  (append (list key) (cdr item)))))
            spec)))

;;;###autoload
(defun keemacs-favorites (&optional favorites)
  "Select among the entries matching FAVORITES.
FAVORITES is a favorites spec -- a list of plists as documented in
`keemacs-favorites-default' -- and defaults to it.  Every entry
matching any spec item is offered in a completion list; RET runs
`keemacs-default-action' and \\[embark-act] opens the usual
embark action menu.  The spec's :key is not used by this command.  Each
entry is offered once, even when several spec items match it.  Returns
the chosen entry path."
  (interactive)
  (keemacs--require-db)
  (let* ((spec (keemacs-favorites--parse
                (or favorites keemacs-favorites-default)))
         (entries (and spec (keemacs--load-entries)))
         (matches (and spec entries
                       (keemacs-favorites--match spec entries))))
    (when (null spec)
      (user-error "No usable favorites in `keemacs-favorites-default'"))
    (when (null matches)
      (user-error "No entries match any of the favorites"))
    (let ((keemacs--selecting t))
      (let* ((chosen (consult--read
                      (mapcar (lambda (e)
                                (keemacs--format-candidate
                                 (car e) (cdr e)))
                              matches)
                      :prompt (keemacs--prompt "KeePass favorite: ")
                      :history keemacs-history
                      :category 'keemacs
                      :require-match t
                      :sort nil
                      :lookup #'consult--lookup-member))
             (path (keemacs--path-of chosen)))
        (if (null path)
            (user-error "no path on candidate")
          (when keemacs-default-action
            (funcall keemacs-default-action path))
          path)))))

;;;###autoload
(defun keemacs-favorites--embark-entry (entry)
  "Open the embark action menu for matched ENTRY (PATH . FIELDS).
The embark target is the entry's real path with the select action map --
copy, view, edit and the insert-at-point actions."
  (let ((path (car entry)))
    (let ((embark-target-finders
           (cons (lambda ()
                   (cons 'keemacs-select path))
                 embark-target-finders)))
      (embark-act))))

(defun keemacs-favorites-by-key (&optional favorites)
  "Choose a favorite from FAVORITES and act on what it matches.
FAVORITES is a favorites spec -- a list of plists as documented in
`keemacs-favorites-default' -- and defaults to it.  The
favorites are offered as a keyed menu (key, title or group, group);
picking one searches the database for the entries it matches.  A single
match goes straight to the embark action menu on that entry (view,
copy, insert, ...); several matches are offered in a keyed menu of
their own first, then the chosen entry goes to the same action menu.
The database to search is chosen with `keemacs-select-database-by-key'
when one is needed."
  (interactive)
  (keemacs--require-db #'keemacs-select-database-by-key)
  (let* ((spec (keemacs-favorites--assign-keys
                (keemacs-favorites--parse
                 (or favorites keemacs-favorites-default))))
         (entries (and spec (keemacs--load-entries))))
    (when (null spec)
      (user-error "No usable favorites in `keemacs-favorites-default'"))
    (let* ((choices (mapcar #'keemacs-favorites--choice spec))
           (chosen (read-multiple-choice "Favorite: " choices))
           (item (seq-find (lambda (i) (eq (nth 0 i) (car chosen))) spec))
           (matches (keemacs-favorites--match (list item) entries))
           (label (or (nth 1 item) (nth 2 item))))
      (pcase (length matches)
        (0 (user-error "No entries match the favorite %s" label))
        (1 (keemacs-favorites--embark-entry (car matches)))
        (_ (let* ((pseudo (mapcar (lambda (e)
                                    (list nil
                                          (keemacs--field (cdr e) "Title")
                                          (keemacs--field (cdr e) "Group")))
                                  matches))
                   (keyed (keemacs-favorites--assign-keys pseudo))
                   (choices (mapcar #'keemacs-favorites--choice keyed))
                   (chosen (read-multiple-choice "Matching entries: " choices))
                   (picked (seq-find (lambda (i) (eq (nth 0 i) (car chosen)))
                                     keyed))
                   (pick-title (nth 1 picked))
                   (pick-group (nth 2 picked))
                   (entry (seq-find
                           (lambda (e)
                             (and (string= (keemacs--field (cdr e) "Title")
                                           pick-title)
                                  (string= (keemacs--field (cdr e) "Group")
                                           pick-group)))
                           matches)))
              (if (null entry)
                  (user-error "no path on candidate")
                (keemacs-favorites--embark-entry entry))))))))

(defun keemacs--check-databases ()
  "Signal a clear error if `keemacs-databases' has the wrong shape.
Each element must be a database spec plist (see `keemacs-auth-db-spec-p')."
  (unless (listp keemacs-databases)
    (user-error "`keemacs-databases' must be a list of database spec \
plists, got %S" keemacs-databases))
  (dolist (entry keemacs-databases)
    (unless (keemacs-auth-db-spec-p entry)
      (user-error "Each element of `keemacs-databases' must be a \
database spec plist such as (:file \"...\") ; got %S" entry))))

(defun keemacs--ensure-database (&optional selector)
  "Make sure a database is selected.
If `keemacs-database' is already set and `keemacs-always-select-database'
is nil, leave it.  Otherwise, if `keemacs-databases' has exactly one
entry, select it automatically; if it has several, prompt them with
SELECTOR -- `keemacs-select-database' by default."
  (keemacs--check-databases)
  (when (or keemacs-always-select-database
            (null keemacs-database))
    (if (= 1 (length keemacs-databases))
        (setq keemacs-database (car keemacs-databases))
      (funcall (or selector #'keemacs-select-database))))
  keemacs-database)

;;;###autoload
(defun keemacs-select-database ()
  "Select the active KeePass database from `keemacs-databases'.
Completes over each entry's label (its `:name', or the file name)."
  (interactive)
  (keemacs--check-databases)
  (unless keemacs-databases
    (user-error "`keemacs-databases' is empty -- add your databases first"))
  (let* ((entries keemacs-databases)
         (labels (mapcar #'keemacs--spec-label entries))
         (chosen (completing-read "KeePass database: " labels nil t))
         (entry (car (seq-filter (lambda (e)
                                   (string= chosen (keemacs--spec-label e)))
                                 entries))))
    (setq keemacs-database entry)
    keemacs-database))

(defun keemacs-select-database-by-key ()
  "Select the active KeePass database with a hotkey menu.
Presents every database in `keemacs-databases' via
`read-multiple-choice': key, label (the `:name', or the file name), and
the file as the description.  A database whose spec carries a `:key'
keeps it; keyless databases are assigned one with
`keemacs-favorites--key-for', given their label and the keys already
taken -- the same rule the favorites menus use."
  (interactive)
  (keemacs--check-databases)
  (unless keemacs-databases
    (user-error "`keemacs-databases' is empty -- add your databases first"))
  ;; (LABEL . SPEC) for each database, then keys assigned label-wise.
  (let* ((pairs (mapcar (lambda (spec)
                          (cons (keemacs--spec-label spec) spec))
                        keemacs-databases))
         (used (delq nil (mapcar (lambda (e)
                                   (keemacs-auth-db-spec-key-char (cdr e)))
                                 pairs)))
         (keyed (mapcar (lambda (entry)
                          (let* ((label (car entry))
                                 (spec (cdr entry))
                                 (key (or (keemacs-auth-db-spec-key-char spec)
                                          (keemacs-favorites--key-for label used))))
                            (push key used)
                            (cons key entry)))
                        pairs))
         (choices (mapcar (lambda (keyed-entry)
                            (let* ((label (cdr keyed-entry))
                                   (spec (keemacs-auth-db-spec-normalize
                                          (cdr label)))
                                   (file (keemacs-auth-db-spec-file spec)))
                              (list (car keyed-entry)
                                    (car label)
                                    file)))
                        keyed)))
    (pcase (read-multiple-choice "Database: " choices)
      (`(,key . ,_)
       (let ((entry (cdr (cdr (seq-find (lambda (ke) (eq (car ke) key))
                                        keyed)))))
         (setq keemacs-database entry)
         keemacs-database)))))

;;;; Command keymap
;;
;; One sparse keymap so users can bind every keepass command with a single
;; line in their init:
;;
;;   (keymap-global-set "C-:" 'keemacs-command-map)
;;
;; `define-prefix-command' defines `keemacs-command-map' both as the
;; keymap variable and as a prefix command, and the autoload cookie makes
;; the binding work even before this file is loaded.

;;;###autoload (define-prefix-command 'keemacs-command-map)
(defvar keemacs-command-map)

;; Creating it here as well as in the autoloads makes the map work both for
;; a package install and for a plain load-path `require' in init.
(define-prefix-command 'keemacs-command-map)

;; `k' is the keemacs main screen -- the tree view.
(define-key keemacs-command-map (kbd "k") #'keemacs)
(define-key keemacs-command-map (kbd "t") #'keemacs-titles)
(define-key keemacs-command-map (kbd "g") #'keemacs-group)
(define-key keemacs-command-map (kbd "d") #'keemacs-select-database)
(define-key keemacs-command-map (kbd "f") #'keemacs-favorites)
(define-key keemacs-command-map (kbd "F") #'keemacs-favorites-by-key)
;; `f' was taken by the favorites; `c' clears the cached master password.
(define-key keemacs-command-map (kbd "c") #'keemacs-auth-forget-cached)
(define-key keemacs-command-map (kbd "a") #'keemacs-add)
(define-key keemacs-command-map (kbd "K") #'keemacs-select-database-by-key)
;; Group maintenance: uppercase, the entry-level sibling is lowercase.
(define-key keemacs-command-map (kbd "A") #'keemacs-add-group)
(define-key keemacs-command-map (kbd "D") #'keemacs-delete-group)

(provide 'keemacs)
;;; keemacs.el ends here
