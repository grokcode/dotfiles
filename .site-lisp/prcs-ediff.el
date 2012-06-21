;; $Id: prcs-ediff.el 1.2 Tue, 23 Feb 1999 17:56:28 -0800 jmacd $

(eval-and-compile (setq load-path (cons ".." (cons "." load-path))))
(require 'ediff)
(require 'prcs)
(eval-and-compile (setq load-path (cdr (cdr load-path))))

(defvar prcs-merge-last-control-window nil)

(make-variable-buffer-local 'prcs-merge-state)
(set-default 'prcs-merge-state nil)

(defun prcs-merge-start-hook ()
  (setq prcs-merge-last-control-window (current-buffer)))

(defun prcs-merge-startup (merge-state)
  (message "Starting PRCS-ediff.")
  (save-excursion
    (set-buffer prcs-merge-last-control-window)
    (message (concat "Running in buffer: "
		     (buffer-name prcs-merge-last-control-window)))
    (setq prcs-merge-last-control-window nil)
    (message (concat "State: " (prin1-to-string merge-state)))
    (setq prcs-merge-state merge-state)
    (let ((a-buf (ediff-get-buffer 'A))
	  (b-buf (ediff-get-buffer 'B))
	  (c-buf (ediff-get-buffer 'C))
	  (anc-buf (ediff-get-buffer 'Ancestor)))
      (save-excursion
	(if a-buf
	    (progn
	      (message "Handling working buffer")
	      (set-buffer a-buf)
	      (rename-buffer (cdr (assq 'working-label merge-state))
			     'unique)))
	(if b-buf
	    (progn
	      (message "Handling selected buffer")
	      (set-buffer b-buf)
	      (rename-buffer (cdr (assq 'selected-label merge-state))
	      'unique)))
	(if anc-buf
	    (progn
	      (message "Handling common buffer")
	      (set-buffer anc-buf)
	      (rename-buffer (cdr (assq 'common-label merge-state))
	      'unique)))
	(if c-buf
	    (progn
	      (message "Handling merge buffer")
	      (set-buffer c-buf)
	      (rename-buffer (concat (cdr (assq 'output-file merge-state))
				     " (merging into)")
	      'unique)))))))

(defun prcs-merge-quit-hook ()
  (if prcs-merge-state
      (let ((state prcs-merge-state))
	(save-excursion
	  (mapcar
	   (lambda (which)
	     (let ((b (ediff-get-buffer which)))
	       (if b (kill-buffer b))))
	   '(A B Ancestor))
	  (let ((c-buf (ediff-get-buffer 'C)))
	    (set-buffer c-buf)
	    (write-file (cdr (assq 'output-file state)) 'confirm)
	    (kill-buffer c-buf))
	  (signal-process (cdr (assq 'process state))
			  (cdr (assq 'signal state)))))))

(add-hook 'ediff-mode-hook 'prcs-merge-start-hook)
(add-hook 'ediff-quit-hook 'prcs-merge-quit-hook)

(provide 'prcs-ediff)
