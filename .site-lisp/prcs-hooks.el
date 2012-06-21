;; This file is associated with `prcs.el' and shares its copyright and
;; disclaimers. Contact Jesse Glick <Jesse.Glick@netbeans.com> for
;; information and to report bugs.
;;
;; $PrcsModeVersion: 1.60 on Sun, 07 Feb 1999 15:23:03 -0500 $

(eval-and-compile (setq load-path (cons ".." (cons "." load-path))))
(require 'custom)
;; XXX would be nice but a pain to remove this requirement:
(require 'cl)
;; Generally already loaded into Emacs image; need `C-x v' keymap to
;; already exist, `vc-follow-symlinks', `vc-follow-link':
(require 'vc-hooks)
(eval-and-compile (setq load-path (cdr (cdr load-path))))

(autoload 'prcs-jump-to-project-file "prcs" nil t)
(autoload 'prcs-mode "prcs" nil t)
(autoload 'prcs-controlled-mode "prcs" nil t)
(autoload 'prcs-guess-repository "prcs" nil t)

;; ----------------------------------------------------------------
;; Variables required here.

(defgroup prcs nil
  "PRCS version-control system in Emacs (see also certain variables in VC)."
  :load 'prcs
  ;; Preferably put under VC. But XEmacs 20.4 does not seem to have a
  ;; vc.el with customization, so I leave it here.
  :group 'tools)

(defcustom prcs-xemacs-p (search "XEmacs" emacs-version)
  "*True if running XEmacs (vs. FSF Emacs).
20.4 currently required."
  :type 'boolean :group 'prcs)

(defcustom prcs-use-toolbar t
  "*Whether to do things to the menubar/toolbar for PRCS support."
  :type 'boolean :group 'prcs)

(defcustom prcs-program-name "prcs"
  "*Name of the prcs executable."
  :type 'string :group 'prcs)

(defvar prcs-active-has-been-checked nil
  "Has the active status of this project buffer been checked yet?
`nil', `good', or `bad'.")
(make-variable-buffer-local 'prcs-active-has-been-checked)
(put 'prcs-active-has-been-checked 'permanent-local t)

;; ----------------------------------------------------------------
;; Menubar setup.

;; Note that these bindings are global. This is actually kind of nice
;; since you can `C-x v p' from e.g. a directory listing to get to the
;; nearest applicable project file.
(global-set-key "\C-xvp" 'prcs-jump-to-project-file)

(if prcs-xemacs-p
    ;; XEmacs
    (progn
      (add-menu-button '("Tools" "VC") "----")
      (add-menu-button '("Tools" "VC")
                       [ "Jump to PRCS project" prcs-jump-to-project-file t ])
      )
  ;; FSF Emacs
  (when prcs-use-toolbar
    (define-key vc-menu-map [separator3] '("----"))
    (define-key global-map [menu-bar tools vc prcs-jump]
      '("Jump to PRCS project" . prcs-jump-to-project-file))
    ))

;; ----------------------------------------------------------------
;; Major mode setup.

(pushnew '("\\.prj\\'" . prcs-mode) auto-mode-alist :test 'equal)

;; ----------------------------------------------------------------
;; Minor mode setup.

(add-hook 'find-file-hooks 'prcs-maybe-put-into-controlled-mode)

(defun prcs-maybe-put-into-controlled-mode ()
  "Hook to put buffers into PRCS Controlled mode where appropriate.
Should notice if you ought to be following a symlink."
  (let* ((link (file-symlink-p (buffer-file-name)))
	 (link-ctld (and link (prcs-is-prcs-controlled (expand-file-name link))))
	 (info (prcs-is-prcs-controlled (buffer-file-name) :get-descriptor-too t)))
    (if link
	(if link-ctld
	    ;; Probably want to follow it.
	    (if (or (eq vc-follow-symlinks t)
		    ;; Already have visited it properly.
		    (and vc-follow-symlinks (get-file-buffer (abbreviate-file-name (file-chase-links (buffer-file-name)))))
		    (and (eq vc-follow-symlinks 'ask) (yes-or-no-p "Symbolic link to PRCS-controlled source file; follow link? ")))
		;; Follow it.
		(progn
		  (vc-follow-link)
		  (message "Followed link to %s" (buffer-file-name))
		  (prcs-maybe-put-into-controlled-mode))
	      ;; All right, caveat.
	      (progn
		(message "Warning: editing through the link bypasses version control")
		;; Do not run PRCS controlled mode on it even if in
		;; Files list as symlink! The buffer is letting you
		;; edit the destination, not the link itself.
		))
	  ;; Target is not controlled.
	  (when info
	    (message "Warning: visiting a PRCS-listed link to a non-PRCS-controlled file")))
      ;; Plain file.
      (when info (prcs-controlled-mode 1 info)))))

(defun* prcs-is-prcs-controlled (name &key get-descriptor-too treat-as-non-p-file)
  "Is this file listed in the (Files) section of some enclosing project file?
If so, returns the name of that project file. Lower-down project
files take precedence.

If GET-DESCRIPTOR-TOO is non-nil, will return instead a list of the project
file name, then the file descriptor list: e.g.

	(.../foo.prj (bar/baz\\.c (foo/39_bar 1.2 666) :tag=mytag))

For the project file itself, just returns that name (or a list of that
name, if GET-DESCRIPTOR-TOO is set, since there is no actual
descriptor).

XXX maybe should warn if multiple p-files contain it, to detect
possibly confusing use of subprojects."
  (if (and (not treat-as-non-p-file) (string-match "\\.prj$" name))
      (let ((if-ok (if get-descriptor-too (list name) name))
	    (pbuf (or (get-file-buffer name) (find-file-noselect name))))
	(save-excursion
	  (set-buffer pbuf)
	  ;; Deal with case that project file was mistakenly put into
	  ;; Lisp Mode (by older versions of PRCS) or Fundamental Mode
	  ;; (due to some random error). If it is in neither of these,
	  ;; nor in PRCS Major Mode, most likely it is some other file
	  ;; format that happens to end in *.prj, e.g. some sort of
	  ;; IDE build file, and the user has probably overridden the
	  ;; `auto-mode-alist' accordingly. Let them do so & rely on
	  ;; the magic mode header only, not extension.
	  (when (and (memq major-mode '(lisp-mode fundamental-mode))
		     (not prcs-active-has-been-checked)	; don't ask too many times!
		     (yes-or-no-p (format "File %s might be a PRCS project file but it showed up in `%s' instead. Force it into PRCS Major Mode? " name major-mode)))
	    (prcs-mode))
	  (if (eq major-mode 'prcs-mode)
	      ;; XXX this block could be put into prcs.el to save
	      ;; space here.
	      (progn
		(unless prcs-active-has-been-checked
		  (setq prcs-active-has-been-checked
			;; Try to see whether or not this pfile is
			;; really recognized by PRCS as an active
			;; project, really in the repository, etc.
			;; Weeds out many common errors.
			(let ((output (generate-new-buffer "*PRCS activeness check*")))
			  (condition-case err
			      (case (let ((default-directory (file-name-directory name))
					  (repo (prcs-guess-repository pbuf)))
				      (message "Checking whether %s is really active..." name)
				      (prog1
					  (apply 'call-process
						 prcs-program-name nil (list output t) nil
						 "info" "--force" "--revision=."
						 (append
						  (if repo (list (concat "--repository=" repo)) nil)
						  (list (file-name-nondirectory name))))
					(message "Checking whether %s is really active...done" name)))
				(0 (kill-buffer output)
				   'good)
				(t (message "PRCS did not like project file %s: see buffer %S" name output)
				   (pop-to-buffer output) ; may not work
				   'bad))
			    (file-error (message "Could not run PRCS: %S" err)
					'bad)))))
		(case prcs-active-has-been-checked
		  (good if-ok)
		  (bad nil)))
	    ;; OK, not a project file.
	    (prcs-is-prcs-controlled name :get-descriptor-too get-descriptor-too :treat-as-non-p-file t))))
    ;; Not a p-file.
    (save-excursion
      (block iterate
	(dolist (try (prcs-is-potentially-prcs-controlled name))
	  (let ((buf (get-file-buffer try)))
	    ;; Avoid refreshing font-lock whenever possible; it incs ticks!
	    (when (not buf) (setq buf (find-file-noselect try)))
	    (set-buffer buf)
	    (if (eq major-mode 'prcs-mode)
		;; Do not consider p-files that are not thought active
		;; (really in repository).
		(if prcs-controlled-mode
		    ;; Various vars & functions here should already be
		    ;; loaded, since we are in PRCS Major Mode and
		    ;; presumably prcs.el is loaded.
		    (let ((found (find (intern
					(file-relative-name name (file-name-directory try))
					prcs-obarray)
				       (cdr (find prcs-Files
						  (prcs-parse-prj-file-cached buf)
						  :key 'car))
				       :key 'car)))
		      (when found
			(return-from iterate (if get-descriptor-too (list try found) try))))
		  (message "Warning: %s found not in PRCS-Controlled mode, ignoring...(probably this project was not accessible in the repository!)" try))
	      (message "Warning: *.prj file %s found in %s (not PRCS Major Mode), will be ignored...(check for weird -*- lines or `auto-mode-alist')" try major-mode))))
	nil))))

(defun prcs-is-potentially-prcs-controlled (name)
  "Provides list of potential names of project files that could be controlling this file.
Returns nil if none, of course, so can be used
as a predicate. More specific (lower-down) project files are listed
first. (No particular order if at same level.)

XXX: could become confused if you have some controlled files in a
directory symlinked to from the actual working dir, but not present
in it. This is pretty obscure.

XXX should perhaps use `file-truename'. Full implications of symlinks
need to be worked out."
  (let* ((dir (expand-file-name (file-name-directory name)))
	 (potential nil))
    ;; Probably not the most elegant algorithm!
    (do ((idx 0)
	 (stop nil))
	(stop nil)
      (if (string-match "/" dir idx)
	  (progn
	    (setq idx (match-end 0))
	    (let* ((test-dir (substring dir 0 (1+ (match-beginning 0))))
		   (test-files (save-excursion
				 ;; ICK!!!!!!!!! Bug in directory-files: sometimes,
				 ;; depending on what buffer you are currently in,
				 ;; running directory-files dies with "Wrong type
				 ;; argument: buffer-or-string-p, t" for no apparent
				 ;; reason. Edebug claims this occurs within an
				 ;; ange-ftp hook which calls directory-files again!
				 ;; The choice of buffers in which this occurs is
				 ;; reproducible but apparently random.
				 (set-buffer (get-buffer-create "*scratch*"))
				 (directory-files test-dir t "^[A-Za-z0-9#%^_+:,][A-Za-z0-9#%^_+:,-=.]*\\.prj$"))))
	      (setq potential (nconc potential test-files))))
	(setq stop t)))
    (reverse potential)))

;; ----------------------------------------------------------------

(provide 'prcs-hooks)
