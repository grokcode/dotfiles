;; -*- Mode: Emacs-Lisp -*-
;; PRCS - The Project Revision Control System
;; Copyright (C) 1997  Josh MacDonald
;;
;; Emacs major & minor modes for PRCS support.
;;
;; $PrcsModeVersion: 1.60 on Sun, 07 Feb 1999 15:23:03 -0500 $
;;
;; Original implementation, Josh MacDonald. Most of this file by Jesse
;; Glick <Jesse.Glick@netbeans.com>; concepts also due to Zack
;; Weinberg <zack@rabi.phys.columbia.edu>, Dan Nicolaescu
;; <done@ece.arizona.edu>; XEmacs code from Didier Verna
;; <verna@inf.enst.fr> with help from Vincent Hascoet
;; <hascoet@lep-philips.fr> and others. Current maintainer Jesse
;; Glick; please contact directly or CC me. Patches are welcome, but
;; please make sure the PrcsModeVersion is indicated. See accompanying
;; prcs-el-todo.txt for a list of known bugs & unimplemented features.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; USAGE: place prcs.el and prcs-hooks.el (or *.elc files) in your
;; Emacs site-lisp directory, and then put this in your .emacs:
;;
;; (require 'prcs-hooks)
;;
;; This require does not load much code. The main file will be loaded
;; when you visit a project file, or a file that looks as if it might
;; be PRCS Controlled (i.e. there is a project file above it
;; somewhere).

(eval-and-compile (setq load-path (cons ".." (cons "." load-path))))
(require 'lisp-mode)
(require 'emerge)
(require 'cl)
(require 'add-log)
(require 'prcs-hooks)
(eval-and-compile (setq load-path (cdr (cdr load-path))))

(defcustom prcs-sloppy-version-prompts nil
  "*Use fast but unsafe version prompts?
If true, then whenever a PRCS version number (or branch) is
prompted for that is normally supposed to be an existing version
\(branch), permit the user to enter any value without confirming its
existence, to save time on projects with many versions or a slow
repository. The calculation is done anyway if the user tries to use
TAB completion; this only affects typing in a value without
completion."
  :type 'boolean :group 'prcs)

(defcustom prcs-check-if-file-modified t
  "*Do asynchronous modification checks on files.
If true, check visited PRCS files to see if they have been
modified since last checkout. The check is done in the background to
avoid overhead."
  :type 'boolean :group 'prcs)

(defcustom prcs-timeout-on-modified-checks 15
  "*Timeout on modification checks?
If set to a number, that is treated as a number of seconds to wait
before killing off the PRCS status checker on a buffer, under the
assumption that it either (a) was hung, or (b) what is more likely,
Emacs totally forgot to run its sentinel & just left it."
  :type '(choice (const :tag "Disable" nil) number) :group 'prcs)

(defcustom prcs-auto-add-changelog t
  "*Automatically insert ChangeLog entries?
If non nil and the project contains a file matching named
ChangeLog in its root directory, prcs-mode will automatically insert
the contents of New-Version-Log into the ChangeLog."
  :type 'boolean :group 'prcs)

(defcustom prcs-ask-about-save t
  "*If not nil, PRCS checkin asks which buffers to save before checking in.
Otherwise, it saves all modified buffers without asking."
  :type 'boolean :group 'prcs)

(defcustom prcs-repository-alist nil
  "*Find repositories based on project name.
List of (REGEXP . REPOSITORY) pairs associating project names to
repositories. When (if any) REGEXP matches a file's project name,
REPOSITORY is used instead of the PRCS_REPOSITORY environment
variable. The first match is retained. E.g.:

'((\"sharedproject\" . \"/user/sharedaccount/PRCS\")
  (\"personalproject\" . \"/user/username/PRCS\"))"
  :type '(repeat (cons :format "%v"
		       (regexp :tag "Project Name")
		       (file :tag "Repository")))
  :group 'prcs)

(defvar prcs-my-repository nil
  "Preferred repository to use for this project file (buffer-local).
This takes precedence over `prcs-repository-alist', so you can e.g.
set this as a buffer-local variable in the magic line of a project
file, e.g.:

-*- mode: PRCS; prcs-my-repository: \"/foo/PRCS\" -*-")
(make-variable-buffer-local 'prcs-my-repository)

(defcustom prcs-extra-checkin-options '("--long-format")
  "*Extra options passed to checkin (list of strings)."
  :type '(repeat (string :tag "Option")) :group 'prcs)

(defcustom prcs-extra-diff-args '("--context")
  "*Extra options passed to diff (list of strings)."
  :type '(repeat (string :tag "Option")) :group 'prcs)

(defcustom prcs-display-path t
  "*If true, display the project path to a controlled file in its modeline."
  :type 'boolean :group 'prcs)

(defcustom prcs-output-buffer-divider
  ;; Don't want to actually see this in prcs.el diffs!
  (concat "------------------%<------------------%<"
	  "------------------%<------------------")
  "Divider between sections of PRCS output buffer."
  :type 'string :group 'prcs)

(defcustom prcs-output-buffer-name-function 'prcs-standard-output-buffer-name
  "Function to select name of the prcs output buffer, possibly based on PROJECT.
COMMAND is the PRCS command being executed (see
`prcs-command'). DISPLAYED indicates whether or not the command was
intended to be displayed.

May ignore any or all arguments (but best to retain DISPLAYED
distinction)."
  :type 'symbol :group 'prcs)

(defun prcs-output-buffer-name (project command displayed)
  "See variable `prcs-output-buffer-name-function'."
  (funcall prcs-output-buffer-name-function project command displayed))

(defun prcs-standard-output-buffer-name (project command displayed)
  (concat (if displayed "*prcs-output [" " *prcs-hidden-output [")
	  project "]*"))

(defcustom prcs-obarray-size 16383
  "*Suggested size for `prcs-obarray'.
Good numbers are either prime,
or 2^n-1. You only need to change this if you have a huge project, in
which case you want something on the order of 2-3 times the number of
files."
  :type 'number :group 'prcs)

;; XXX is it possible to install this in the Emacs exec dir?
(defcustom prcs-callback-program "prcs-callback"
  "*Program to run for callbacks, e.g. for EDiff support.
Should be
name of executable `prcs-callback' script distributed with prcs.el,
either present in your `exec-path' or a fully qualified name."
  :type 'string :group 'prcs)

(defcustom prcs-use-ediff-for-diffs 'ask
  "*Whether to invoke EDiff as the viewer during diffs.
Just make sure
the `prcs-callback' script is installed somewhere, and
`prcs-callback-program' finds it. Always use it if T, never if NIL, or
prompt otherwise."
  :type '(choice (const :tag "Never" nil)
		 (const :tag "Always" t)
		 (const :tag "Ask" ask)) :group 'prcs)

;; ----------------------------------------------------------

;; PRCS Minor Mode.
(defvar prcs-controlled-mode nil
  "True when in a PRCS-controlled buffer.")
;; Currently you cannot rely on these being set for some random
;; purpose; use one of the convenience functions to look up a project
;; parse &c. instead. They might not be set depending on how modeline
;; stuff is configured.
(defvar prcs-controlled-project)
(defvar prcs-controlled-version)
(defvar prcs-controlled-status)
(defvar prcs-controlled-path)
;; Safeguard against `normal-mode', as vc.el does for `vc-mode' var.
(dolist (minor-var '(prcs-controlled-mode prcs-controlled-project prcs-controlled-version prcs-controlled-status prcs-controlled-path))
  (make-variable-buffer-local minor-var)
  (put minor-var 'permanent-local t))

;; Project file descriptors.
(defconst prcs-descriptors
  '(Files Created-By-Prcs-Version Project-Description Project-Version Parent-Version
	  Version-Log New-Version-Log Checkin-Time Checkin-Login Populate-Ignore
	  Project-Keywords Merge-Parents New-Merge-Parents))
(defconst prcs-useful-symbols
  (mapcar 'symbol-name (append prcs-descriptors '(:symlink :no-keywords :directory)))
  "Symbols it is handy to refer to from the project file.
The real
symbols are all interned privately, so simple quotes will not work.")

;; Private obarray handling.
(defvar prcs-obarray (make-vector prcs-obarray-size 0)
  "Obarray for symbols PRCS Mode creates during a parse.")
;(eval-when (compile)
;  (dolist (var prcs-useful-symbols)
;    (set (intern (concat "prcs-" var)) nil)))
(eval-when (load eval)
  (dolist (var prcs-useful-symbols)
    (set (intern (concat "prcs-" var)) (intern var prcs-obarray))))

;; Cache state.
(defvar prcs-parse-cache nil "Cached parse of a PRCS project file.")
(make-variable-buffer-local 'prcs-parse-cache)
(defvar prcs-parse-cache-tick nil "Last time a PRCS project file was parsed.")
(make-variable-buffer-local 'prcs-parse-cache-tick)

;; ---------------------------------------------------------

(defun prcs-guess-repository (pbuffer)
  "Try to guess repository from `prcs-repository-alist' or `prcs-my-repository'."
  (save-excursion
    (set-buffer pbuffer)
    (or prcs-my-repository
	(let ((project-name (prcs-coerce (cadr (find prcs-Project-Version
						     (prcs-parse-prj-file-cached)
						     :key 'car
						     :test 'eq)))))
	  (cdr (find-if (lambda (rx)
			  (string-match rx project-name))
			prcs-repository-alist :key 'car))))))

;; ---------------------------------------------------------

(defconst prcs-prj-descriptor-regex (mapconcat 'symbol-name prcs-descriptors "\\|")
  "Descriptors.")

;; XXX use defface, ideally.
(defconst prcs-font-lock-keywords
  `(
   (,(concat "(\\(" prcs-prj-descriptor-regex "\\)\\>")
    1 font-lock-function-name-face)
   ("^\\s-*(\\(\\([^ \n\t;()\"]\\|\\\\.\\)+\\)\\s-+(\\(\\S-+/\\([^ \n\t;()\"]\\|\\\\.\\)+\\s-+[0-9.]+\\s-+[0-9]+\\))" (1 font-lock-type-face) (3 font-lock-string-face))
   ("^\\s-*(\\(\\([^ \n\t;()\"]\\|\\\\.\\)+\\)\\s-+(\\(\\([^ \n\t;()\"]\\|\\\\.\\)+\\)).*\\<:symlink\\>" (1 font-lock-type-face) (3 font-lock-reference-face))
   ("^\\s-*(\\(\\([^ \n\t;()\"]\\|\\\\.\\)+\\)\\s-+(\\s-*)" (1 font-lock-type-face))
   ("\\<:\\sw+\\>" 0 font-lock-keyword-face prepend)
   ("\\\\." 0 font-lock-string-face t)
   (";.*" 0 font-lock-comment-face t)
   )
  "PRCS project file fontification keywords.")

(defconst prcs-font-lock-defaults
  `(prcs-font-lock-keywords
    nil
    nil
    ,(if prcs-xemacs-p
         '((?+ . "w") (?- . "w") (?* . "w") (?/ . "w") (?. . "w") (?< . "w")
           (?> . "w") (?= . "w") (?! . "w") (?? . "w") (?$ . "w") (?% . "w")
           (?_ . "w") (?& . "w") (?~ . "w") (?^ . "w") (?: . "w"))
       '(("+-*/.<>=!?$%_&~^:" . "w")))
    beginning-of-defun
    (font-lock-comment-start-regexp . ";")
    (font-lock-mark-block-function . mark-defun))
  "PRCS project file fontification information.")

;; ---------------------------------------------------------

(defun* prcs-check-that-file-is-ok (&key
				    (buffer (current-buffer))
				    force
				    skip-modeline)
  "Handles background disk writes by PRCS and makes sure Emacs keeps everything in synch for BUFFER.

If the file has been written on disk since last visit/save, but is
marked unmodified here, it refreshes the buffer and sets the modeline,
after confirming with the user (unless FORCE is non-nil).

If the file has been written on disk but is modified, does the same
thing, but only after a sterner warning (which is never turned off).

If the file is untouched on disk (whether the buffer is modified or
not), this does not try to revert.

In any case, the PRCS modeline setting will be recalculated, unless a
modified buffer was left as is, or SKIP-MODELINE is true."
  (save-excursion
    (set-buffer buffer)
    (when (and (not (verify-visited-file-modtime buffer))
	       (if (buffer-modified-p)
		   (yes-or-no-p (concat "Really revert " (buffer-file-name) " from disk, discarding modifications? "))
		 (or force (y-or-n-p (concat "Refresh " (buffer-file-name) " from disk? ")))))
      (revert-buffer t t t))
    (when (and (not (buffer-modified-p))
	       (not skip-modeline))
      (prcs-update-file-status))))

(defun* prcs-get-visited-buffers (&key
				  (pbuffer (current-buffer)))
  "Return a list of all currently visited buffers.
Should correspond to
files in project file PBUFFER (not including the project file buffer
itself)."
  ;; XXX This loops through file list and checks for the
  ;; buffer. Possibly it should actually loop through the buffers and
  ;; check for them in the file list, if that would be significantly
  ;; faster (probably unlikely to be too important unless you have a
  ;; huge project).
  (let ((base-dir (file-name-directory (buffer-file-name pbuffer))))
    (remove-if-not 'identity
		   (mapcar (lambda (desc)
			     (let ((buf (get-file-buffer (expand-file-name (prcs-coerce (car desc))
									   base-dir))))
			       (and buf
				    (save-excursion
				      (set-buffer buf)
				      prcs-controlled-mode)
				    buf)))
			   (cdr (find prcs-Files (prcs-parse-prj-file-cached pbuffer)
				      :key 'car))))))

(defun* prcs-prompt-for-saves (&key
			       (pbuffer (current-buffer))
			       skip-modeline
			       (buflist (cons pbuffer (prcs-get-visited-buffers :pbuffer pbuffer))))
  "Save some files visited in PRCS controlled mode.
For all files mentioned in the project file PBUFFER (default
current), or BUFLIST if supplied, which are also in memory buffers and
modified, save them (asking first if `prcs-ask-about-save' is
non-nil). Includes project file. If SKIP-MODELINE is set, modeline
modification checks will be suppressed."
  (save-excursion
    (dolist (buf buflist)
      (set-buffer buf)
      (when (and (buffer-modified-p)
		 (or (not prcs-ask-about-save)
		     (y-or-n-p (concat "Save file " (buffer-file-name) "? "))))
	(if skip-modeline
	    (let ((prcs-check-if-file-modified nil))
	      (save-buffer))
	  (save-buffer))))))

(defun* prcs-prompt-for-refreshes (&key
				   (pbuffer (current-buffer))
				   skip-modeline
				   (buflist (cons pbuffer (prcs-get-visited-buffers :pbuffer pbuffer))))
  "Refresh some files visited in PRCS controlled mode.
For all files mentioned in the project file PBUFFER which are also
in memory buffers, make sure they are refreshed from disk as needed
with `prcs-check-that-file-is-ok', prompting in case of
`prcs-ask-about-save'. SKIP-MODELINE used as in
`prcs-check-that-file-is-ok'. BUFLIST as in
`prcs-prompt-for-saves'. Includes p-file."
  (dolist (buf buflist)
    (prcs-check-that-file-is-ok :buffer buf :force (not prcs-ask-about-save) :skip-modeline skip-modeline)))

(defun* prcs-parse-prj-file-cached (&optional (buffer (current-buffer)))
  "Retrieve the parse for this buffer, from cache or fresh parse.
Use instead of `prcs-parse-prj-file'."
  (prcs-check-that-file-is-ok :buffer buffer :skip-modeline t)
  (save-excursion
    (set-buffer buffer)
    (unless (eq major-mode 'prcs-mode)
      (error "Attempt to do a PRCS parse on something which is not really a project file. This probably indicates a bug in prcs.el; report to the author."))
    (when (> (buffer-modified-tick) prcs-parse-cache-tick)
      (setq prcs-parse-cache (prcs-parse-prj-file buffer))
      (setq prcs-parse-cache-tick (buffer-modified-tick)))
    prcs-parse-cache))

(defun prcs-coerce (object)
  "Coerce this number, string, or symbol into a string."
  (if object
      (typecase object
	(number (number-to-string object))
	(string object)
	(symbol (symbol-name object))
	(t (error "Weird object")))))

(defun* prcs-parse-prj-file (&optional (buffer (current-buffer)))
  "Return a simple list of all s-exps in project file.
May be symbols,
strings, numbers etc. (according to the superficial form). No
buffer-position info retained. Lists are natural.

Note that e.g. filenames are returned as symbols, so you probably want
to use `prcs-coerce' on them; they are interned in `prcs-obarray', so
you could find one by searching (via `eq') on `(intern \"...\"
prcs-obarray)'.

To compare against a standard keyword, e.g. `New-Version-Log', use the
variable `prcs-New-Version-Log' which will contain the properly
interned symbol.

`prcs-fixup-parse' is used to perform some postprocessing.

For speed, use `prcs-parse-prj-file-cached' instead.

This format is convenient for finding things: e.g. to get checkin time as
a string, try:

  (cadr (find prcs-Checkin-Time (prcs-parse-prj-file-cached BUFFER) :key 'car))"
  (message "Parsing project file...")
  (save-excursion
    (set-buffer buffer)
    (goto-char (point-min))
    (let (prj-sexps)
      (condition-case nil
	  ;; Optimistic first stab.
	  (while t
	    (push (let ((obarray prcs-obarray)) (read buffer)) prj-sexps))
	(end-of-file nil)
	(invalid-read-syntax
	 (prcs-escape-syntactic-nastiness)
	 ;; Try again.
	 (goto-char (point-min))
	 (setq prj-sexps nil)
	 (condition-case nil
	     ;; This had better work, else an error is really thrown.
	     (while t
	       (push (let ((obarray prcs-obarray)) (read buffer)) prj-sexps))
	   (end-of-file nil))))
      (let ((result (prcs-fixup-parse
                     (nreverse prj-sexps))))
        (message "Parsing project file...done")
        result))))

(defun prcs-escape-syntactic-nastiness ()
  "Escape symbols in current project buffers beginning with dot.
Operates heuristically, so is not foolproof, but mistakes
probably won't cause any harm. Skips over anything which font-lock
claims is a comment or string."
  (let ((orig-mod (buffer-modified-p))
	(orig-ro buffer-read-only))
    (when orig-ro
      (unless (y-or-n-p (format "PRCS Major Mode wants to escape filenames in read-only buffer %s. Continue? " (buffer-file-name)))
	(error "Cannot escape filenames for PRCS parse"))
      (setq buffer-read-only nil))
    (message "Escaping filenames...")
    (goto-char (point-min))
    (while (search-forward-regexp "(\\(\\.+\\)" nil t)
      (case (get-text-property (match-beginning 1) 'face)
	(font-lock-comment-face)
	(font-lock-string-face)
	(t (goto-char (match-beginning 1))
	   (dotimes (ignore (- (match-end 1) (match-beginning 1)))
	     (insert ?\\)
	     (forward-char 1))
	   (goto-char (match-end 0)))))
    (message "Escaping filenames...done")
    (when orig-ro (setq buffer-read-only t))
    (when (and (not orig-mod) (buffer-modified-p))
      (if orig-ro
	  (progn
	    (message "Warning: buffer %s is read-only; not attempting to save filename escapes." (buffer-file-name))
	    (set-buffer-modified-p nil))
	(when (or (not prcs-ask-about-save)
		  (y-or-n-p (concat "Save file " (buffer-file-name) "? ")))
	  (save-buffer))))))

(defconst prcs-careful-parses nil
  "If true, actually insert copies of new-style scoped modifiers into all the file descriptors they apply to.
Else skip this step, but still
remove them from the Files list so as not to interfere.

You only need to turn this on if you are changing code so as to
actually depend on attributes of files in a specific fashion. Leaving
it off is more efficient.")

(defun prcs-fixup-parse (parse)
  "Given a basic parse of a project file, transform it to give the illusion that it does not have multiple file lists.
Thus the parse
of the following:

    (Files :tag=haha :tag=hoho
      (foo (...))
      (bar (...) :no-keywords))
    (Files
      (baz (quux) :symlink))

will come out as if it were:

    (Files
      (foo (...) :tag=haha :tag=hoho)
      (bar (...) :no-keywords :tag=haha :tag=hoho)
      (baz (quux) :symlink))

With `prcs-careful-parses' off, this would be:

    (Files
      (foo (...))
      (bar (...) :no-keywords)
      (baz (quux) :symlink))"
  (let (files-sexps other-sexps)
    ;; Pull out Files lists from all others.
    (dolist (sexp (reverse parse))
      (if (eq (car sexp) prcs-Files)
	  (push sexp files-sexps)
	(push sexp other-sexps)))
    ;; Unified Files list will go in front, arbitrarily. Otherwise
    ;; there is no rearrangement of sexps.
    (cons (cons prcs-Files
		;; Join together all lists after processing...
		(mapcan (lambda (files-sexp)
			  (let (modifiers
				(actuals (cdr files-sexp))) ; Skip prcs-Files
			    ;; Find modifier keys.
			    (while (and actuals
					(atom (car actuals)))
			      (push (pop actuals) modifiers))
			    (setq modifiers (nreverse modifiers))
			    ;; Put them on the end of each descriptor.
			    (if (and prcs-careful-parses modifiers)
				(mapcar (lambda (desc)
					  (append desc modifiers))
					actuals)
			      ;; Speed hack:
			      actuals)))
			files-sexps))
	  other-sexps)))

;; PRCS-Emerge
;;
;; These functions allow the script 'emerge', distributed with PRCS, to
;; invoke emerge during merge.  It uses a recursive edit.  Probably should
;; know what you're doing.

(defun prcs-emerge-files (file-A file-B file-out quit-hooks)
  "Run Emerge on two files."
  (emerge-files-internal file-A file-B nil quit-hooks file-out))

(defun prcs::emerge-files-with-ancestor (file-A file-B file-ancestor file-out quit-hooks)
  "Run Emerge on two files, giving another file as the ancestor."
  (emerge-files-with-ancestor-internal file-A file-B file-ancestor nil quit-hooks file-out))

(defun prcs::emerge (work com sel out)
  "Run Emerge on two or three files."
  (message "prcs::emerge with files %s %s %s %s" work com sel out)
  (let (quit-hooks)
    (add-hook 'quit-hooks (function (lambda () (exit-recursive-edit))))
    (add-hook 'quit-hooks (` (lambda () (emerge-files-exit (, out)))))
    (if (equal com "/dev/null")
	(prcs-emerge-files work sel out quit-hooks)
      (prcs::emerge-files-with-ancestor work sel com out quit-hooks))
    (recursive-edit)
    (message "prcs::emerge finished")
    )
  )

;;; ---------------------- PRCS Major Mode ------------------

;; This is based on lisp-mode.el

(defvar prcs-mode-syntax-table lisp-mode-syntax-table)
(defvar prcs-mode-abbrev-table nil)

(define-abbrev-table 'prcs-mode-abbrev-table ())

(defun prcs-mode-variables ()
  (setq local-abbrev-table prcs-mode-abbrev-table)
  (set (make-local-variable 'paragraph-start) (concat page-delimiter "\\|$" ))
  (set (make-local-variable 'paragraph-separate) paragraph-start)
  (set (make-local-variable 'paragraph-ignore-fill-prefix) t)
  (set (make-local-variable 'fill-paragraph-function) 'lisp-fill-paragraph)
  ;; Adaptive fill mode gets in the way of auto-fill,
  ;; and should make no difference for explicit fill
  ;; because lisp-fill-paragraph should do the job.
  (set (make-local-variable 'adaptive-fill-mode) nil)
  (set (make-local-variable 'indent-line-function) 'lisp-indent-line)
  (set (make-local-variable 'indent-region-function) 'lisp-indent-region)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'outline-regexp) ";;; \\|(....")
  (set (make-local-variable 'comment-start) ";")
  ;; Look within the line for a ; following an even number of backslashes
  ;; after either a non-backslash or the line beginning.
  (set (make-local-variable 'comment-start-skip) "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\);+ *")
  (set (make-local-variable 'max-lisp-eval-depth) 10000)
  (set (make-local-variable 'max-specpdl-size) 10000)
  (set (make-local-variable 'comment-column) 40)
  (set (make-local-variable 'comment-indent-function) 'lisp-comment-indent)
  (set (make-local-variable 'font-lock-defaults) prcs-font-lock-defaults)
  (setq prcs-parse-cache nil)		; don't parse until requested
  (setq prcs-parse-cache-tick 0))	; ticks start at 1

(defvar prcs-mode-map nil "Keymap for `prcs-mode'.")

(defconst prcs-xemacs-menu
  '("PRCS"
    [ "Working diff"   prcs-diff :active t ]
    [ "Arbitrary diff" prcs-diff-with-prefix :active t :keys "C-u \\[prcs-diff]" ]
    [ "Info"           prcs-info :active t ]
    [ "Complex info"   prcs-info-with-prefix :active t :keys "C-u \\[prcs-info]" ]
    [ "Rekey"          prcs-rekey :active t ]
    "----"
    [ "Checkin"        prcs-checkin :active t ])
  "PRCS menu for XEmacs (not used on FSF).")

(when (not prcs-mode-map)
  (if prcs-xemacs-p
      ;; XEmacs
      (progn
        (setq prcs-mode-map (make-sparse-keymap))
        (set-keymap-name prcs-mode-map 'prcs-mode-map)
        (set-keymap-parents prcs-mode-map (list shared-lisp-mode-map))
        (define-key prcs-mode-map "\C-c\C-c" 'prcs-checkin)
        (define-key prcs-mode-map "\C-c\C-d" 'prcs-diff)
	(define-key prcs-mode-map "\C-c\C-i" 'prcs-info)
        (define-key prcs-mode-map "\C-c\C-r" 'prcs-rekey))
    ;; FSF Emacs
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map shared-lisp-mode-map)
      (define-key map "\C-c\C-c" 'prcs-checkin)
      (define-key map "\C-c\C-d" 'prcs-diff)
      (define-key map "\C-c\C-i" 'prcs-info)
      (define-key map "\C-c\C-r" 'prcs-rekey)
      (when prcs-use-toolbar
        (define-key map [menu-bar prcs]
          (cons "PRCS" (make-sparse-keymap "PRCS")))
        (define-key map [menu-bar prcs rekey]
          '("Rekey" . prcs-rekey))
        (define-key map [menu-bar prcs complex-info]
          '("Complex info" . prcs-info-with-prefix))
        (define-key map [menu-bar prcs simple-info]
          '("Info" . prcs-info))
        (define-key map [menu-bar prcs complex-diff]
          '("Arbitrary diff" . prcs-diff-with-prefix))
        (define-key map [menu-bar prcs simple-diff]
          '("Working diff" . prcs-diff))
        (define-key map [menu-bar prcs checkin]
          '("Checkin" . prcs-checkin)))
      (setq prcs-mode-map map))))

(defvar prcs-mode-hook '() "Hooks for `prcs-mode'.")

(defun prcs-mode ()
  "Major mode for editing PRCS project files.
Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{prcs-mode-map}
Entry to this mode calls the value of `prcs-mode-hook'
if that value is non-nil.

You can interact with PRCS from this buffer as well:

\\[prcs-checkin] checks in the project as it stands.
\\[prcs-diff] looks for differences from last checkin/checkout;
C-u \\[prcs-diff] also prompts for version numbers.
\\[prcs-info] gives a summary of project versions;
C-u \\[prcs-info] gives more flexibility.
\\[prcs-rekey] rekeys the project.
\\[prcs-jump-to-project-file] jumps here from other buffers.

Designed for FSF Emacs 20 or XEmacs 20.

Major user-visible features:

* Edit project file like the Lisp code it looks like.

* Asynchronous checking of file-modification status for controlled
files. You just need to visit a controlled file and start working on
it; you can tell by the modeline what state it is in.

* Support for using EDiff, the excellent Emacs file comparison suite,
for running diffs (in place of simple diff output). Turn on
`prcs-use-ediff-for-diffs'.

* Each project has a message buffer that shows all the PRCS commands
being run and their output. Scissor-marks visually separate commands.

* Tries to keep your buffers in synch with the disk when running PRCS
commands: offers to save buffers if they are modified, and to reload
them when PRCS modifies them.

* TAB completion on all prompts asking for PRCS version numbers or
branches.

* If configured, can have several different repositories for different
projects (otherwise default repository is used).

* Some menu/toolbar integration.

* Emacs backup files are normally turned off for you.

* Some support for a ChangeLog (New-Version-Log's will get added to
it).

Internal features:

* `prcs-command' provides easy way to run new PRCS commands with all
the special treatment the existing ones get. Both synchronous and
asynchronous commands, with both mutex and queue support. Supports
callbacks of Elisp code in place of diff/merge tools.

* Provides (pretty quickly) parsed versions of the project buffer (and
some convenience functions relating to that) so you can use its
structure."
  (interactive)
  (kill-all-local-variables)
  (use-local-map prcs-mode-map)
  (when (and prcs-xemacs-p prcs-use-toolbar)
    (set-buffer-menubar current-menubar)
    (add-submenu nil prcs-xemacs-menu))
  (set-syntax-table prcs-mode-syntax-table)
  (setq major-mode 'prcs-mode)
  (setq mode-name "PRCS")
  (prcs-mode-variables)
  (run-hooks 'prcs-mode-hook))

(dolist (desc prcs-descriptors) (put desc 'lisp-indent-function 0))

;; ------------ PRCS Generic Command Interface ----------

(defun* prcs-output-buffer (&key
			    (pbuffer (current-buffer))
			    (command "command")
			    (displayed t)
			    divide
			    switch-to)
  "Get a ready-to-use output buffer given PBUFFER.
COMMAND and DISPLAYED may be as in `prcs-output-buffer-name'.
If DIVIDE is `t', put in a divider at the end.
If it is `back', go back to the last divider.
If nil, just retrieve the buffer.
If SWITCH-TO is non-nil, display the buffer too."
  (let* ((orig-buf (current-buffer))
	 (project-name (prcs-coerce
			(cadr (find prcs-Project-Version
				    (prcs-parse-prj-file-cached pbuffer)
				    :key 'car))))
	 (output-buffer (get-buffer-create (prcs-output-buffer-name project-name command displayed))))
    (if switch-to (pop-to-buffer output-buffer)
      (set-buffer output-buffer))
    (goto-char (point-max))
    (cond
     ((eq divide t)
      (insert prcs-output-buffer-divider "\n")
      (goto-char (point-max)))
     ((eq divide 'back)
      (let ((pos (search-backward prcs-output-buffer-divider nil t)))
	(if pos (goto-char pos)))))
    (when (not switch-to) (set-buffer orig-buf))
    output-buffer))

(defvar prcs-command-locks nil
  "Locks held by running asynch processes, as alist from tokens to processes.
(Tokens may
be any s-expression.)")

(defvar prcs-command-queues nil
  "Queues in effect.
This is an alist from queue identifiers (any
s-exp) to lists of queued-up commands to run, each of which is just
the argument list to `prcs-command' verbatim. If a queue is missing,
it is empty; if it has no elements, that means that one process is
running in it; any more elements are other processes waiting in
decreasing order of precedence.")

(defvar prcs-command-processes nil
  "All asynch processes running, as alist from processes to other info.
TIMER is the timer object if a timeout was specified, else NIL.
This other info is copied from `prcs-command' arguments:

    (SUBCOMMAND REAL-REFRESH DISPLAY ERROR-ON-FAIL EXCLUSIVE SENTINEL SENTINEL-DATA SKIP-MODELINE TIMER QUEUE BUFFER PBUFFER CALLBACK CALLBACK-DATA)")

(defun prcs-command-reset ()
  "Reset PRCS command state information in case of trouble.
Should not
normally be needed."
  (interactive)
  (setq prcs-command-locks nil)
  (setq prcs-command-queues nil)
  (setq prcs-command-processes nil))

;; XXX Reimplement synch processes using asynch calls + sleep (with
;; special sentinel to wake up sleeper). This would simplify the code
;; and make synch processes more flexible (i.e. could have callbacks,
;; etc.).
;;
;; XXX determine effect of locks & queues on mod. checks for
;; save/refresh actions implicit in command
;;
;; XXX use unwind-protect or the like to remove locks &c. as needed
;;
;; XXX handle case where command completes so quickly (thank you
;; Josh!) that Emacs is actually fooled into thinking that the file on
;; disk is not any newer than the buffer, & does not try to refresh!
;; This happened to me during a checkin. Maybe an arg like
;; :refresh-force which would refresh all the requested buffers
;; whether they seemed to need it or not. Or it could just take a list
;; of buffers (i.e. p-file) that would have to be refreshed. Or maybe
;; checkin should just check for this condition.
;;
;; XXX Permit sentinels to be specified on synch commands for ease of
;; switching.
;;
;; XXX Permit `buffer' arg to actually be a list of relative
;; filenames, in which case a buffer-valued `pbuffer' arg (default
;; current buffer) would be used to indicate the project buffer. Then
;; all these controlled files would be passed as args to the command
;; (with `what'=`self'); and any of them which were visited would have
;; the usual save/refresh semantics.
;;
;; XXX Permit `asynch' arg to have third option, `interactive', which
;; would start an asynch process but put it on a pty and display that
;; pty buffer (in a special project interaction buffer). Termination
;; of process would still run the usual sentinel, refresh etc.
;;
;; XXX Option to lock buffers on asynch (interactive) commands.
;; Treated exactly like save/refresh. Affected buffers would be made
;; read-only for duration of command, discouraging accidental editing
;; of files that PRCS is currently working on. Default on (turned off
;; by e.g. `prcs-check-if-file-modified').
(defun* prcs-command (subcommand &rest all &key
				 options
				 (buffer (current-buffer))
				 what
				 extra-args
				 (save 'as-needed)
				 (refresh 'as-needed)
				 (display t)
				 (error-on-fail t)
				 asynch
				 (killable t)
				 exclusive
				 queue immediate
				 sentinel
				 sentinel-data
				 timeout
				 output-buffer
				 callback
				 callback-data
				 skip-modeline)
  "Run a PRCS command.
Runs a PRCS command SUBCOMMAND (one word, or \"admin
subsubcommand\") with options OPTIONS (a list, `-f' is implicit).
BUFFER is a PRCS-controlled buffer which serves as the reference point
for everything. The command is always run from the correct directory
with the project file as first argument; WHAT controls the second
argument: none if `nil' (i.e. whole project), project file if `t', or
if `self', the correct relative path to BUFFER \(even if that is the
project file); EXTRA-ARGS if any are also appended (they are not
options!).

SAVE determines whether to prompt to save buffers: `nil' means none,
`t' means all visited buffers for this project, `p-file' means project
file only, list of buffers does just those (include the project file
buffer if you need it), `as-needed' (default) means acc. to value of
WHAT. Similar for REFRESH, after command termination. SKIP-MODELINE
`t' skips modeline update at end (refreshed buffers only); if
`saves-too', does not even refresh if it saves buffers. DISPLAY
\(default on) means show the output buffer (this is always written to
in any case); normally the regular PRCS output buffer is used, but
this may be overridden with OUTPUT-BUFFER. ERROR-ON-FAIL signals an
error if PRCS completed in a failure condition (status 1 does not
signal an error).

If ASYNCH is true, the command is run asynchronously (and the process
object is returned instead); KILLABLE (default true) means it can be
killed off at Emacs exit; EXCLUSIVE if set to a symbol or some
s-expression puts a kind of lock named by the EXCLUSIVE argument
\(typically containing a buffer & some symbol describing the type of
lock) and will only run if no such lock already exists (removed on
exit); SENTINEL may be given as a function accepting the process
object, the BUFFER, an exit flag (NIL for timeout, see below, or a
number for an exit status, or T for some sort of erroneous event), and
if desired SENTINEL-DATA for state, to be run upon exit. If TIMEOUT is
set, process will be stopped after that many seconds if still running,
with the sentinel being called with the timeout flag set. If QUEUE is
set to some s-expression identifying a queue, only one process may be
running in that queue at once, and the others will wait to start.

If CALLBACK is set, it should be a function accepting the process
object, the BUFFER, a list of strings representing the requested
command arguments, and possibly extra arguments CALLBACK-DATA for
state, and returning an integer. In this case, several things are
changed. First of all, the command is run on a tty. Secondly,
`PRCS_DIFF_COMMAND' (clearing `PRCS_DIFF_OPTIONS') or
`PRCS_MERGE_COMMAND' (according to the value of SUBCOMMAND) are set to
the value of `prcs-callback-program'. Then, a filter is run on the
output stream; whenever the magic token from the callback script is
recognized, the callback function is called with the appropriate
arguments that the callback script received, and its return value
determines the exit status of the callback script. Otherwise, things
proceed as normal.

XXX Note that locks & queues currently apply only to asynchronous
commands; there is no way to lock out a synchronous command based on
them (it would only make sense in any case for locking out one
synchronous command from a set of asynchronous ones).

XXX Note that callbacks are currently only supported on asynch
processes. Also, do not try to run two callbacks processes at once on
the same output buffer!

Returns the exit status for a synch process; the process for a
successful asynch process; for a locked-out asynch process, a list of
the existing process; or for an asynch process waiting for a queue,
nil."
  (when (and display output-buffer)
    (error "It is unsupported to DISPLAY a non-default OUTPUT-BUFFER"))
  ;; Handle queues right up front. Basically: if this process needs to
  ;; wait, put it at the end of the appropriate queue & return nil at
  ;; once. If it does not, make an empty queue so that subsequent
  ;; invocations will block correctly. Note that the internal-only
  ;; :immediate option is used to run this process anyway, since it
  ;; was just now pulled off the queue.
  (when (and asynch queue (not immediate))
    (let ((waiting (assoc queue prcs-command-queues)))
      (if waiting
	  (progn
	    ;; Note that the buffer must be specified since the
	    ;; default is context-dependent.
	    (nconc waiting (list (list* subcommand :buffer buffer all)))
	    (return-from prcs-command nil))
	(push (list queue) prcs-command-queues))))
  (let* ((filename (buffer-file-name buffer))
	 (info (prcs-is-prcs-controlled filename :get-descriptor-too t))
	 (pfilename (car info))
	 (pfilebase (file-name-nondirectory pfilename))
	 (pfiledir (file-name-directory pfilename))
	 (filenamerel (file-relative-name filename pfiledir))
	 (desc (cadr info))
	 (pbuffer (or (get-file-buffer pfilename) (find-file-noselect pfilename)))
	 (output (or output-buffer (prcs-output-buffer :pbuffer pbuffer :command subcommand :displayed display)))
	 return-result)
  (save-excursion
    (set-buffer pbuffer)
    ;; Unless a specific list is passed in which does not contain the
    ;; p-buffer, we always want to save/refresh that!
    (flet ((interpret-s-or-r (s-or-r)
			     (if (listp s-or-r) s-or-r ; nil counts!
			       (cons pbuffer
				     (case s-or-r
				       ((p-file) nil)
				       ((t) (prcs-get-visited-buffers :pbuffer pbuffer))
				       ((as-needed)
					(case what
					  ((nil) (prcs-get-visited-buffers :pbuffer pbuffer))
					  ((t) nil)
					  ;; If buffer = p-buffer, don't repeat.
					  ((self) (if desc (list buffer)))
					  (t (error))))
				       (t (error)))))))
      (let* ((real-save (interpret-s-or-r save))
	     (real-refresh (interpret-s-or-r refresh))
	     (default-directory pfiledir)
	     (command-args (append (list subcommand "--force" "--plain-format")
				   (let ((repo (prcs-guess-repository pbuffer)))
				     (if repo (list (concat "--repository=" repo))))
				   options
				   (list pfilebase)
				   (case what
				     ((nil) nil)
				     ((t) (list pfilebase))
				     ((self) (list filenamerel))
				     (t (error "What is `what' %S?" what)))
				   extra-args))
	     (command-string (mapconcat 'identity (cons prcs-program-name command-args) " "))
	     (command-string-underline (if display (concat (make-string (length command-string) ?-) "\n") "")))
	(prcs-prompt-for-saves :pbuffer pbuffer :skip-modeline (eq skip-modeline 'saves-too) :buflist real-save)
	(unless output-buffer (prcs-output-buffer :pbuffer pbuffer :divide t :command subcommand :displayed display))
	(if asynch
	    (progn
	      (when exclusive
		(save-excursion
		  (set-buffer buffer)
		  (let ((existing (cdr (assoc exclusive prcs-command-locks))))
		    (if existing
			(setq return-result (list existing))))))
	      (unless return-result
		(unless output-buffer
		  (set-buffer output)
		  (goto-char (point-max))
		  (insert command-string "\n" command-string-underline)
		  (goto-char (point-max))
		  (set-buffer pbuffer))
		(let* ((process-connection-type (if callback 'pty nil))
		       (process-environment
			(append process-environment
				(if callback
				    (cond
				     ((string-equal subcommand "diff")
				      (list (concat "PRCS_DIFF_COMMAND=" (expand-file-name prcs-callback-program))))
				     ((string-equal subcommand "merge")
				      (list (concat "PRCS_MERGE_COMMAND=" (expand-file-name prcs-callback-program))))
				     (t (error "You may not use a callback with subcommand %s!" subcommand)))
				  nil)))
		       (process (apply 'start-process
				       (append
					(list
					 command-string
					 output ; XXX or (list buffer t)?
					 prcs-program-name)
					command-args))))
		  (when callback
		    (unless (process-tty-name process)
		      (error "Process should be using a PTY but system won't allow it!"))
		    ;; XXX try to do this soon after process creation.
		    ;; In principle PRCS could have started, done
		    ;; something useful, run the script, and outputted
		    ;; the whole magic token before we even hit this
		    ;; line, which would cause it to not recognize the
		    ;; token block and the script to hang waiting, but
		    ;; I think this is pretty unlikely. To fix, store
		    ;; another field in prcs-command-processes, being
		    ;; the (point) right before the process was
		    ;; created; the filter would check this field when
		    ;; it runs, and if set would clear it, but also
		    ;; start its search from that point rather than
		    ;; from the start of text insertion; thus it would
		    ;; take a look at the earlier stuff.
		    (set-process-filter process 'prcs-command-callback-filter)
		    )
		  (setq return-result process)
		  (when exclusive
		    (save-excursion
		      (set-buffer buffer) ; XXX why?
		      (push (cons exclusive process) prcs-command-locks)))
		  (set-process-sentinel process 'prcs-command-dispatch-sentinel)
		  (process-kill-without-query process (not killable))
		  (push (list process
			      subcommand real-refresh display error-on-fail exclusive
			      sentinel sentinel-data skip-modeline
                              (if timeout
				  (if prcs-xemacs-p
				      ;; ITimer has been spectacularly unreliable for me. This works fine.
				      (add-timeout timeout 'prcs-command-dispatch-sentinel process)
				      (run-at-time timeout nil 'prcs-command-dispatch-sentinel process)))
			      queue buffer pbuffer callback callback-data)
			prcs-command-processes))))
	  ;; Synchronous.
	  (progn
	    (unless output-buffer
	      (when display
		(display-buffer output))
	      (set-buffer output)
	      (goto-char (point-max))
	      (insert command-string "\n" command-string-underline)
	      (goto-char (point-max))
	      (set-buffer pbuffer))
	    (let ((status (apply 'call-process
				 (append
				  (list prcs-program-name nil (list output (not output-buffer)) display)
				  command-args))))
	      (case status
		((0 1)
		 (setq return-result status)
		 (prcs-prompt-for-refreshes :pbuffer pbuffer :skip-modeline skip-modeline
					    :buflist real-refresh))
		(t (if error-on-fail (error "PRCS command failed: %S" status)
		     (setq return-result status)))))
	    (when (and display (not output-buffer)) (prcs-output-buffer :pbuffer pbuffer :divide 'back :switch-to t :command subcommand :displayed display))
	    )))))
  return-result))

(defun prcs-command-dispatch-sentinel (process &optional ignore)
  "Dispatch PROCESS to the user-specified sentinel.
IGNORE is the
event happening to it, which will be missing in the event of a timeout."
  (let ((timed-out (not ignore))
	(info (cdr (assq process prcs-command-processes))))
    (when (not info) (error "Process did not seem to originate from `prcs-command'!"))
    (when timed-out (message "Asynch process stopped by a timeout"))
    (let ((subcommand (nth 0 info))
	  (real-refresh (nth 1 info))
	  (display (nth 2 info))
	  (error-on-fail (nth 3 info))
	  (exclusive (nth 4 info))
	  (sentinel (nth 5 info))
	  (sentinel-data (nth 6 info))
	  (skip-modeline (nth 7 info))
	  (timer (nth 8 info))
	  (queue (nth 9 info))
	  (buffer (nth 10 info))
	  (pbuffer (nth 11 info)))
      ;; Clear the timer if necessary.
      (when (and timer (not timed-out))
	(if prcs-xemacs-p
	    (disable-timeout timer)
	  (cancel-timer timer)))
      ;; Remove lock if any.
      (when exclusive
	(setq prcs-command-locks (delete* exclusive prcs-command-locks :test 'equal :key 'car)))
      ;; Remove from the process list.
      (setq prcs-command-processes (delete* process prcs-command-processes :test 'eq :key 'car))
      ;; Calculate exit status and maybe run the sentinel.
      (let ((exit-status
	     (if timed-out nil
	       (case (process-status process)
		 (exit (case (process-exit-status process)
			 (0 0)
			 (1 1)
			 (t (message "PRCS asynch process exited with an error") t)))
		 (run (message "PRCS asynch process was still running when stopped, perhaps due to timeout")
		      t)
		 (signal (message "PRCS asynch process caught some signal") t)
		 (t (message "Weird process status") t)))))
	(set-process-sentinel process nil)
	(delete-process process)
	(when (or (numberp exit-status) (not error-on-fail))
	  ;; Run the sentinel.
	  (when sentinel (apply sentinel process buffer exit-status sentinel-data))
	  ;; Perform refreshes & display.
	  (prcs-prompt-for-refreshes :pbuffer pbuffer :skip-modeline skip-modeline :buflist real-refresh)
	  (when display (prcs-output-buffer :pbuffer pbuffer :divide 'back :switch-to t :command subcommand :displayed t)))
	;; Finally, clear the queue if applicable.
	(when queue
	  (let ((waiting (assoc queue prcs-command-queues)))
	    (when (not waiting) (error "Should have been a queue for this process!"))
	    (if (cdr waiting)
		;; There was >=1 process waiting on this one.
		(let ((next-process (cadr waiting)))
		  ;; Pull it off.
		  (setf (cdr waiting) (cddr waiting))
		  (apply 'prcs-command (append next-process '(:immediate t))))
	      ;; This queue is now free.
	      (setq prcs-command-queues (delete* queue prcs-command-queues :test 'equal :key 'car)))))
	;; Signal an error now if needed.
	(when (and error-on-fail (not (numberp exit-status)))
	  (error "PRCS asynch command failed"))))))

(defconst prcs-command-callback-token (concat "MaGiC-PrCs-" "CaLlBaCk-ToKeN"))
(defun prcs-command-callback-filter (process input)
  "Look for a magic token in the current process' output, and if found call a callback function automagically."
  ;; Skeleton cribbed from Elisp manual. Basic modification is this:
  ;; whenever a block of text is inserted, after insertion, go to
  ;; beginning of the line where mark was originally, and look for
  ;; magic token subsequently in buffer; since magic token must be all
  ;; on one line (not necessarily at beginning, tho) and followed by a
  ;; newline, this should find the token even if the output containing
  ;; the token etc. was broken up between buffering blocks. When
  ;; found, the filter is temporarily disabled, and the process is
  ;; asked to produce output until the arg count is found, then asked
  ;; to produce more until all the args are found. When gathered
  ;; together, the callback is called, and the result is sent back to
  ;; the process which had better exit with it. Also, the magic output
  ;; is deleted for the user's benefit. Got all that?
  (let ((old-buffer (current-buffer)))
    (unwind-protect
	(let (moving start-search-point end-search-point)
	  (set-buffer (process-buffer process))
	  (setq moving (= (point) (process-mark process)))
	  (save-excursion
	    (goto-char (process-mark process))
	    (setq start-search-point (point))
	    (insert input)
	    (set-marker (process-mark process) (setq end-search-point (point)))
	    ;; Check for token now.
	    (goto-char start-search-point)
	    (beginning-of-line)
	    (when (search-forward (concat prcs-command-callback-token "\n")
				  end-search-point
				  t)
	      ;; Got it! We are now on next line waiting for arg count number.
	      (set-process-filter process nil)
	      (let ((arg-count-point (point)))
		(while (not (progn (goto-char arg-count-point)
				   (looking-at "^\\([0-9]+\\)\n")))
		  (unless (accept-process-output process 5)
		    (error "Callback script appears hung")))
		;; Got arg count.
		(let ((arg-count (string-to-int (match-string 1))))
		  (while (< (count-lines arg-count-point (process-mark process))
			    (+ 1 arg-count))
		    (unless (accept-process-output process 5)
		      (error "Callback script appears hung")))
		  ;; Got args. Retrieve them.
		  (goto-char arg-count-point)
		  (forward-line 1)
		  (let ((args nil))
		    (dotimes (idx arg-count)
		      (let ((start (point)))
			(forward-line 1)
			(setq args
			      (nconc args
				     (list (buffer-substring start (- (point) 1)))))))
		    ;; Delete this stuff.
		    (goto-char arg-count-point)
		    (search-backward (concat prcs-command-callback-token "\n"))
		    (let ((start-of-junk (point)))
		      (forward-line (+ 2 arg-count))
		      (delete-region start-of-junk (point)))
		      ;; Could use w/ buffer-invisibility-spec for debugging.
		      ;; Subst this for delete-region above.
;;		      (add-text-properties start-of-junk (point)
;;					   '(invisible
;;					     prcs-command-callback-invisibility
;;					     intangible
;;					     t)))
		    ;; Run callback.
		    (let* ((info (cdr (assq process prcs-command-processes)))
			   (status (apply
				   (nth 12 info)
				   process
				   (nth 10 info)
				   args
				   (nth 13 info))))
		      (process-send-string process (format "%d\n" status))))))
	      ;; All set; restore the old filter.
	      (set-process-filter process 'prcs-command-callback-filter)))
	  (if moving (goto-char (process-mark process))))
      (set-buffer old-buffer))))

;; ------------------- Version Completion --------------

(defvar prcs-version-history nil
  "History list for `prcs-read-version-with-minibuffer'.")
;; Retarded language doesn't have decent closures.
(defvar *prcs-current-version-completions* nil)
(defvar *prcs-current-version-branch-only* nil)
(defvar *prcs-current-version-project* nil)
(defvar *prcs-current-version-allow-head* nil)
(defvar *prcs-current-version-default* nil)
;; XXX should have option to cache last-calculated version list (for
;; this project) on the assumption that it has not changed since then.
;; Diff could use this; currently it has to calculate it twice.
(defun* prcs-read-version-with-minibuffer
    (&key
     branch-only
     require-match
     (allow-head t)
     allow-working
     (pbuffer (prcs-find-project-file-buffer))
     (default (if allow-working ""
		(let ((version-info (find prcs-Project-Version (prcs-parse-prj-file-cached pbuffer) :test 'eq :key 'car)))
		  (if branch-only
		      (prcs-coerce (nth 2 version-info))
		    (concat (prcs-coerce (nth 2 version-info)) "." (prcs-coerce (nth 3 version-info)))))))
     (prompt (concat
	      (if require-match "Existing ")
	      "PRCS "
	      (if branch-only "branch" "version")
	      " in "
	      (prcs-coerce (nth 1 (find prcs-Project-Version (prcs-parse-prj-file-cached pbuffer) :test 'eq :key 'car)))
	      (if allow-working " (blank for working)")
	      " (TAB to complete): ")))
  "Read a PRCS version number from the minibuffer, with tab-completion available.
If BRANCH-ONLY is given, actually read a branch name, not a
version number. PROMPT is the prompt to use in the minibuffer.
REQUIRE-MATCH forces an existing PRCS version/branch to be used.
DEFAULT gives a default value; if unspecified this is taken from the
repository version of the current buffer, when in PRCS Controlled
Mode, or the working version if that is allowed. ALLOW-WORKING permits
a blank entry to be treated as the working version and accepted;
otherwise the user is not allowed to specify a blank version.
ALLOW-HEAD (default on) permits \"@\" to be used for branches or minor
versions. PBUFFER (sensibly defaulted) gives a project buffer to associate
with.

Note that REQUIRE-MATCH forces the versions to be calculated unless
ALLOW-WORKING is set and the user requests the working version, or
unless the users enters the prompted default. Otherwise this is put
off until a completion is requested by the user.

Return value is a string containing the branch/version, or `nil' for
the working version. \"@\" will be left as typed."
  (when (not pbuffer) (error "You must specify a project buffer"))
  (when (and allow-working branch-only) (error "You cannot permit a working version as a branch; this makes no sense"))
  (do ((result nil)
       (*prcs-current-version-completions* nil)
       (*prcs-current-version-project* pbuffer)
       (*prcs-current-version-branch-only* branch-only)
       (*prcs-current-version-allow-head* allow-head)
       (*prcs-current-version-default* default)
       ;; Doesn't seem to do anything:
       (minibuffer-completion-confirm t))
      ((and result (or allow-working (not (string-equal result ""))))
       (if (string-equal result "") nil result))
    (setq result (completing-read prompt 'prcs-version-completer nil
				  (and require-match (not prcs-sloppy-version-prompts))
				  default 'prcs-version-history))))
(defun prcs-version-completer (string predicate type)
  "Used by `prcs-read-version-with-minibuffer' to complete versions (or branches).
Just does a regular completion, but does not try to
list existing versions until a completion is actually requested."
  (if (and (eq type 'lambda) (string-equal string *prcs-current-version-default*))
      t					; Quick, need not calc.
    (progn
      (unless *prcs-current-version-completions*
	(prcs-version-enumerate))
      (case type
	((nil) (try-completion string *prcs-current-version-completions* predicate))
	((t) (all-completions string *prcs-current-version-completions* predicate))
	;; XXX might be good to do a "prcs info -r<string>" to check
	;; for this case, though this is more work; doing this would
	;; probably obviate the need for
	;; `prcs-sloppy-version-prompts'. Similarly, would be possible
	;; to use "prcs info -r<string>\*" to perform
	;; completions---would this actually make anything faster?
	((lambda) (find string *prcs-current-version-completions* :test 'string-equal :key 'car))))))
(defun prcs-version-enumerate ()
  "Actually builds the list of completions for `prcs-version-completer'."
  ;; XXX ideally process filters would be used rather than a temporary
  ;; buffer, but it does not appear possible to use them on a
  ;; synchronous process, and calling wait-for-process manually etc.
  ;; seems like a pain
  (message "Getting project versions, please wait...")
  (let ((branches nil)
	(versions nil))
    (let ((output (generate-new-buffer "PRCS version info output")))
      (prcs-command "info"
		    :options '("--quiet")
		    :buffer *prcs-current-version-project*
		    :save nil
		    :refresh nil
		    :display nil
		    :skip-modeline t
		    :output-buffer output)
      (setq *prcs-current-version-completions* nil)
      (save-excursion
	(set-buffer output)
	(goto-char (point-min))
	(let ((rx (concat "^" (prcs-coerce (nth 1 (find prcs-Project-Version (prcs-parse-prj-file-cached *prcs-current-version-project*) :test 'eq :key 'car)))
			  " +\\(\\([^ ]+\\)\\.[0-9]+\\) ")))
	  (while (/= (point) (point-max))
	    (unless (looking-at rx) (error "Weird line"))
	    (push (match-string 1) versions)
	    (pushnew (match-string 2) branches :test 'string-equal)
	    (forward-line))))
      (kill-buffer output))
    (setq *prcs-current-version-completions*
	  (mapcar (lambda (x) (list x))
		  (if *prcs-current-version-branch-only* branches versions)))
    (when *prcs-current-version-allow-head*
      (if *prcs-current-version-branch-only*
	  (push '("@") *prcs-current-version-completions*)
	(dolist (branch (cons "@" branches))
	  (push (list (concat branch ".@")) *prcs-current-version-completions*)))))
  (message "Getting project versions, please wait...done"))

;; ------------- Useful PRCS Commands ------------

(defun prcs-ensure-buffer-is-live ()
  "Ensure that the current buffer is in fact in PRCS Controlled Mode."
  (unless prcs-controlled-mode
    (error "This buffer is not considered PRCS-controlled! Probably the project is not active in your repository, etc. Use M-x prcs-controlled-mode to correct the situation if needed.")))

(defun prcs-checkin (&optional noact)
  "Checkin the current project from working dir.
Prompts for version log if necessary, and major version.
With prefix arg, takes no action, just shows what would happen."
  (interactive "P")
  (prcs-ensure-buffer-is-live)
  (let* ((prj (prcs-parse-prj-file-cached (current-buffer)))
	 (log (cadr (find prcs-New-Version-Log prj :key 'car))))
    (when (not log)
      (goto-char (point-min))
      (error "Can't find New-Version-Log"))
    (when (and (zerop (length log))
	       (y-or-n-p "No version log entered, enter one first? "))
      (goto-char (point-min))
      ;; Best effort to find it:
      (search-forward-regexp "(New-Version-Log[ \t\n]+\"" nil t)
      (error "Checkin aborted."))
    (when (and prcs-auto-add-changelog (not noact) (not (zerop (length log))))
      (prcs-add-changelog (current-buffer) log))
    (let* ((major (prcs-coerce (caddr (find prcs-Project-Version prj :key 'car))))
	   (new-major (prcs-read-version-with-minibuffer
		       :default major
		       :allow-head t
		       :branch-only t
		       :require-match nil
		       :prompt "PRCS major version to checkin onto: "))
	   (exited (prcs-command
		    "checkin"
		    :display t
		    ;; Save time: we know what the modelines should
		    ;; be. In particular, newly added files should now
		    ;; read "", not "+", just like everything else.
		    :skip-modeline 'saves-too
		    :options (append
			      (list (concat "--revision=" new-major ".@"))
			      (if noact (list "--no-action"))
			      prcs-extra-checkin-options))))
      (let* ((new-prj (prcs-parse-prj-file-cached (current-buffer)))
	     (new-p-v (find prcs-Project-Version new-prj :key 'car))
	     (new-project (prcs-coerce (cadr new-p-v)))
	     (new-version (concat (prcs-coerce (caddr new-p-v)) "."
				  (prcs-coerce (cadddr new-p-v))))
	     (new-status ""))
	(save-excursion
	  (dolist (buf (cons (current-buffer) (prcs-get-visited-buffers)))
	    (set-buffer buf)
	    (setq prcs-controlled-project new-project)
	    (setq prcs-controlled-version new-version)
	    (setq prcs-controlled-status new-status))))
      (if (not (= exited 0))
	  (message "PRCS exited with non-zero status--failed"))
      )
    )
  )

(defun prcs-add-changelog (buffer message)
  (when (find (intern (file-name-nondirectory (change-log-name)) prcs-obarray)
	      (cdr (find prcs-Files (prcs-parse-prj-file-cached buffer) :key 'car))
	      :key 'car)
    (save-window-excursion
      (message "Adding a ChangeLog entry...")
      ;; XXX the "current defun" could plausibly be the project
      ;; version, perhaps, so you would get something like:
      ;;
      ;; foo.prj (release.17): ...
      ;;
      ;; Alternatively, this could be messily tacked onto the
      ;; name+address line above it.
      (let ((add-log-current-defun-function (lambda () nil)))
	;; XXX note that this will insert the name of the current
	;; buffer, i.e. the project file. There does not seem to be
	;; any good way to override that.
	(add-change-log-entry))
      ;; XXX should this trim outer newlines or somesuch?
      (insert message)
      ;; XXX might be nice to have it pause & wait for user to do
      ;; superficial edits, esp. as the indentation is probably
      ;; screwy.
      (save-buffer)
      (message "Adding a ChangeLog entry...done")
      )
    )
  )

(defun prcs-diff (&optional repo-versions)
  "List differences in the working version from the repository version.
With a prefix argument, prompts for versions to compare.

When invoked with the EDiff support, normally prompted for, if there
are any differences, an EDiff directory session will be spawned.
Repository versions are placed in a special temporary area, while the
normal working files are used for the newer files when comparing
against a working version. Normally the temporary files are deleted
upon exiting the session group, but if you edit them you will be asked
first. See the EDiff manual for help on how to use EDiff, especially
in directory diff mode."
  (interactive "P")
  (prcs-ensure-buffer-is-live)
  (let* ((extra-args (if prcs-extra-diff-args (append (list "--") prcs-extra-diff-args) nil))
	 (prj (prcs-parse-prj-file-cached (current-buffer)))
	 (pv (find prcs-Parent-Version prj :key 'car))
	 (cv (find prcs-Project-Version prj :key 'car))
	 (default-cv-string (concat (prcs-coerce (caddr cv)) "."
				      (prcs-coerce (cadddr cv))))
	 (pv-string (if repo-versions
			(prcs-read-version-with-minibuffer
			 :prompt "Older version: "
			 :default (concat (prcs-coerce (caddr pv)) "."
					  (prcs-coerce (cadddr pv)))
			 :allow-head t
			 :allow-working nil
			 :require-match t)
		      default-cv-string))
	 (cv-string (if repo-versions
			(prcs-read-version-with-minibuffer
			 :prompt "Newer version (blank for working): "
			 :default default-cv-string
			 :allow-head t
			 :allow-working t
			 :require-match t)
		      nil))
	 (options (if repo-versions
		      (if cv-string
			  (list
			   (concat "--revision=" pv-string)
			   (concat "--revision=" cv-string))
			(list (concat "--revision=" pv-string)))
		    nil)))
    (if (or (eq t prcs-use-ediff-for-diffs)
	    (and prcs-use-ediff-for-diffs
		 (y-or-n-p "Use EDiff instead of regular output? ")))
	(let* ((new-is-working (not cv-string))
	       (scratch-dir (concat (make-temp-name "/tmp/prcs-ediff") "/"))
	       (old-store-dir (concat scratch-dir pv-string "/"))
	       (new-store-dir (if new-is-working default-directory (concat scratch-dir cv-string "/")))
	       (p-file-name (file-relative-name (buffer-file-name)))
	       ;; XXX Note: current PRCS version, for some reason,
	       ;; does _not_ call external diff script for the project
	       ;; file. I consider this a bug; so if your p-file is
	       ;; modified, it will simply display in the regular
	       ;; output area, not under EDiff.
	       (index-list (list (list nil old-store-dir new-store-dir nil nil)))
	       ;; Don't auto delete any scratch files which user actually modified.
	       (scratch-file-mod-times (list '("dummy"))))
	  (make-directory scratch-dir t)
	  (prcs-command
	   "diff"
	   :extra-args extra-args
	   :options options
	   :skip-modeline t
	   ;; Do not use :display nil--we still need display for
	   ;; project file (due to current bug), and also for things
	   ;; like renames, deletes, and adds.
	   :callback 'prcs-ediff-callback
	   :callback-data (list old-store-dir new-store-dir index-list new-is-working scratch-file-mod-times)
	   :asynch t
	   :sentinel 'prcs-ediff-sentinel
	   :sentinel-data (list new-is-working index-list scratch-dir old-store-dir new-store-dir scratch-file-mod-times
				(concat "From " pv-string " to "
					(if new-is-working (concat default-cv-string "(w)") cv-string)))))
      ;; Regular diff.
      (case (prcs-command
	     "diff"
	     :extra-args extra-args
	     :options options
	     :skip-modeline t)
	(1 (message "There were differences."))
	(0 (message "No differences."))))))
(defun prcs-diff-with-prefix ()
  "Like `prcs-diff', but with automatic prefix argument."
  (interactive)
  (prcs-diff t))
(defun prcs-ediff-sentinel (process buffer status new-is-working index-list scratch-dir old-store-dir new-store-dir scratch-file-mod-times description)
  "Run EDiff session (probably)."
  (case status
    (nil (error "Timed out on diff!"))
    (0 (message "No differences.")
       (prcs-ediff-cleanup scratch-dir scratch-file-mod-times))
    (1 (require 'ediff)
     (require 'ediff-mult)
     (ediff-show-meta-buffer
      (ediff-prepare-meta-buffer
       'ediff-filegroup-action
       index-list
       (concat "*Ediff over PRCS Diff Panel: " description)
       'ediff-redraw-directory-group-buffer
       ;; Don't ask.
       'xxxxxxprcs-version-diff
       `((lambda nil
	   (make-local-hook 'ediff-quit-session-group-hook)
	   (add-hook 'ediff-quit-session-group-hook (lambda nil (prcs-ediff-cleanup ,scratch-dir ',scratch-file-mod-times)) nil t)
	   (setq ediff-session-action-function 'ediff-files)
	   (setq ediff-dir-difference-list
		 '((nil ,old-store-dir ,new-store-dir nil))))))))))
(defun prcs-ediff-cleanup (scratch-dir scratch-file-mod-times)
  "Clean up temp files.
Anything which has a modified buffer which the
user does not want to kill, or which was added after EDiff setup time,
or modified on disk thereafter, is left behind."
  (labels ((del-dir (dir)
		    (let ((ok t))
		      (dolist (entry (directory-files dir t "[^.].*\\|\\.[^.].*" t))
			(if (file-regular-p entry)
			    (let* ((buf (get-file-buffer entry))
				   (this-ok (and (or (not buf) (kill-buffer buf))
						 (let ((orig-mod-entry (assoc* entry scratch-file-mod-times :test 'string-equal)))
						   (or (and orig-mod-entry
							    (equal (cdr orig-mod-entry)
								   (nth 5 (file-attributes entry))))
						       (yes-or-no-p (format "File %s was created or modified since PRCS EDiff creation; really delete it now? " entry)))))))
			      (when this-ok (delete-file entry))
			      (setq ok (and ok this-ok)))
			  (if (file-directory-p entry)
			      (setq ok (and (del-dir entry) ok))
			    ;; Weird-ass.
			    (delete-file entry))))
		      (when ok (delete-directory dir))
		      ok)))
    (unless (del-dir scratch-dir)
      (message "Warning: directory %s was left around as some changed files or buffers in it are still in existence." scratch-dir))))
(defun prcs-ediff-callback (process buffer args
				   old-dir new-dir index-list new-is-working scratch-file-mod-times)
  "Just copy the indicated temp files into a good place in the old and new dirs."
  ;; Ignore options.
  (while (eq ?- (aref (car args) 0))
    (pop args))
  (labels ((extract-relfile (string)
			    (unless (string-match "^[^/ ]+\\.[0-9]+\\((w)\\)?/\\([^ ]+\\) " string)
			      (error "Weird PRCS label: `%s'" string))
			    (match-string 2 string)))
    (let* ((old-label (nth 0 args))
	   (old-file (nth 1 args))
	   (new-label (nth 2 args))
	   (new-file (nth 3 args))
	   (old-relfile (extract-relfile old-label))
	   (new-relfile (extract-relfile new-label))
	   (old-store-file (concat old-dir old-relfile))
	   (new-store-file (concat new-dir new-relfile)))
      (nconc index-list (list (list old-store-file new-store-file nil)))
      (labels ((copy (a b)
		     (make-directory (file-name-directory b) t)
		     (copy-file a b nil t)
		     (nconc scratch-file-mod-times (list (cons b (nth 5 (file-attributes b)))))))
	(copy old-file old-store-file)
	(unless new-is-working
	  (copy new-file new-store-file)))))
  1)

(defun prcs-info (&optional complex)
  "List summary of project version information.
With a prefix argument, prompt for several options."
  (interactive "P")
  (prcs-ensure-buffer-is-live)
  (if complex
      ;; XXX impossible to do minor-version range w/ current version
      ;; of PRCS
      (let* ((rev-style (completing-read "Type of revisions to look at (TAB to complete): "
					 '(("single") ("branch") ("wildcard"))
					 nil t))
	     (revisions (cond
			 ((string-equal rev-style "single")
			  (prcs-read-version-with-minibuffer :require-match t))
			 ((string-equal rev-style "branch")
			  (concat (prcs-read-version-with-minibuffer :require-match t :branch-only t) ".*"))
			 (t (read-from-minibuffer "Version wildcard: "))))
	     ;; Skipping sort order for simplicity
	     (lengthiness (completing-read "Level of detail (TAB to complete): "
					   '(("brief") ("medium") ("long"))
					   nil t)))
	(prcs-command "info" :skip-modeline t
		      :options (list* (concat "--revision=" revisions)
				      (cond
				       ((string-equal lengthiness "medium") '("--long-format"))
				       ((string-equal lengthiness "long") '("--long-long-format"))
				       (t nil)))))
    (prcs-command "info" :skip-modeline t)))
(defun prcs-info-with-prefix ()
  "Like `prcs-info', but with automatic prefix argument."
  (interactive)
  (prcs-info t))

(defun prcs-rekey ()
  "Rekey the project."
  (interactive)
  (prcs-ensure-buffer-is-live)
  (prcs-command "rekey" :skip-modeline t :display nil))

;;;;;; PRCS Minor Mode (for PRCS-controlled files, incl. the p-file)

;; Modeline display.
(or (assq 'prcs-controlled-mode minor-mode-alist)
    (push '(prcs-controlled-mode
	    (" " prcs-controlled-project ":" prcs-controlled-version
	     (prcs-controlled-status prcs-controlled-status "?")
	     prcs-controlled-path))
	  minor-mode-alist))

(add-hook 'after-save-hook 'prcs-update-file-status)

(defvar prcs-natural-b-i nil
  "Whether backups were naturally inhibited on a buffer anyway.")
;; XXX should have some hooks
(defun prcs-controlled-mode (&optional toggle info)
  "Toggle (or with prefix, turn on or off acc. to value) PRCS Controlled Mode.

This minor mode is used for buffers that are to be considered under
PRCS control, including the project file itself (which is also in PRCS
Major Mode).

By default, your modeline will indicate: the project name you are
working in; the current project version; whether the file is
unmodified (no mark), modified (marked with an asterisk), or newly
added (marked with a plus sign); and what the path is to this file
within the project. E.g.:

    foo.cc  (C++ foobase:release.99*/src/foolib/ Font)

For the time being, if you want to perform any real PRCS actions,
please go to the project file (and see the documentation for
`prcs-mode'). In the future you may be able to run commands from here.

\\[prcs-controlled-mode] Toggle PRCS Controlled minor mode.
\\[prcs-jump-to-project-file] Go to controlling project-file."
  (interactive "P")
  (let ((old-pcm prcs-controlled-mode))
    (setq prcs-controlled-mode
	  (if toggle (> (prefix-numeric-value toggle) 0) (not old-pcm)))
    (when (and (not prcs-controlled-mode) old-pcm)
      ;; Off.
      (setq backup-inhibited prcs-natural-b-i))
    (when (and prcs-controlled-mode (not old-pcm))
      ;; We are turning it on.
      (make-local-variable 'backup-inhibited)
      (make-local-variable 'prcs-natural-b-i)
      (setq prcs-natural-b-i backup-inhibited)
      (unless vc-make-backup-files (setq backup-inhibited t))
      (prcs-update-file-status (current-buffer) info))))

(defun* prcs-update-file-status (&optional (buffer (current-buffer)) maybe-info)
  "Update modeline status acc. to PRCS.
BUFFER is buffer to check;
INFO if any is existing result of `prcs-is-prcs-controlled' with
second arg true."
  (when prcs-controlled-mode
    (save-excursion
      (set-buffer buffer)
      (let* ((info (or maybe-info (prcs-is-prcs-controlled (buffer-file-name) :get-descriptor-too t)))
	     (p-file (car info))
	     (desc (cadr info))
	     (pbuf (or (get-file-buffer p-file) (find-file-noselect p-file)))
	     (prj (prcs-parse-prj-file-cached pbuf))
	     (p-v (find prcs-Project-Version prj :key 'car)))
	(setq prcs-controlled-project (prcs-coerce (cadr p-v)))
	(setq prcs-controlled-version (concat (prcs-coerce (caddr p-v)) "."
					      (prcs-coerce (cadddr p-v))))
	(setq prcs-controlled-path
	      (if (and desc prcs-display-path)
		  (let* ((logical-path (concat "/" (prcs-coerce (car desc))))
			 (posn (search "/" logical-path :from-end t)))
		    (substring logical-path 0 (+ posn 1)))
		""))
	(setq prcs-controlled-status (if (and desc (not (cadr desc))) "+" nil))
	(when (and prcs-check-if-file-modified
		   (not prcs-controlled-status))
	  (let ((result
		 (prcs-command
		  "diff"
		  :what 'self
		  :display nil
		  :options '("--quiet")
		  :extra-args '("--" "--brief")
		  :save nil
		  :refresh nil
		  :skip-modeline t
		  :asynch t
		  ;; XXX better would be to also include project
		  ;; repository, so that would not lock project that
		  ;; happened to have the same name in a different
		  ;; place
		  :exclusive (list buffer 'modification-check)
		  :queue prcs-controlled-project
		  :timeout prcs-timeout-on-modified-checks
		  :sentinel-data nil
		  :sentinel 'prcs-update-file-status-sentinel)))
	    (cond
	     ((null result))		; waiting for the check
	     ((processp result))	; doing it now
	     ((listp result) (message "Check on %S locked out" buffer))
	     (t (message "What result is %S?!" result)))))))))
(defun prcs-update-file-status-sentinel (process buffer exit)
  (if (buffer-file-name buffer)
      (save-excursion
	(set-buffer buffer)
	(when prcs-controlled-mode
	  (setq prcs-controlled-status (case exit
					 (0 "")
					 (1 "*")
					 (t "???")))))
    (message "PRCS-controlled buffer no longer exists")))

;; XXX use consistently
(defun* prcs-find-project-file-buffer (&optional (buffer (current-buffer)))
  "Find the project file (if any) controlling BUFFER."
  (let ((pfile-name (prcs-is-prcs-controlled (buffer-file-name buffer))))
    (if pfile-name (get-file-buffer pfile-name))))
(defun* prcs-find-project-file-parse (&optional (buffer (current-buffer)))
  "Find the parse of the project file (if any) controlling BUFFER."
  (let ((pbuffer (prcs-find-project-file-buffer buffer)))
    (if pbuffer (prcs-parse-prj-file-cached pbuffer))))
(defun* prcs-find-project-attribute (attribute &optional (buffer (current-buffer)))
  "Find the specified ATTRIBUTE (e.g. `prcs-Files') of the project file controlling BUFFER.
Returns whole s-exp starting with ATTRIBUTE."
  (let ((parse (prcs-find-project-file-parse buffer)))
    (if parse (find attribute parse :test 'eq :key 'car))))

(defun prcs-jump-to-project-file ()
  "Jump to the nearest applicable project file from here."
  (interactive)
  (let* ((here (or (buffer-file-name) default-directory))
	 (prj (or (prcs-is-prcs-controlled here)
		  (car (prcs-is-potentially-prcs-controlled here)))))
    (if prj
	(if (string-equal (buffer-file-name) prj)
	    (message "Already here!")
	  (pop-to-buffer (or (get-file-buffer prj)
			     (find-file-noselect prj))))
      (error "Could not find applicable project file!"))))

(provide 'prcs)
