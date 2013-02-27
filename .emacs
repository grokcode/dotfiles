;; turn off toolbar icons
(tool-bar-mode 0)

;; don't show the welcome splash screen
(setq inhibit-startup-message t)

;; Load custom themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'berkeley t)

;; Setup the windows to be on the left and full screen vertically,
;; also set up some colors and the default font
(setq default-frame-alist
      '((cursor-color . "lightsteelblue")
	(cursor-type . box)
	(foreground-color . "black")
	(background-color . "ghostwhite")
	(font . "Inconsolata-16")
	))

;; Add Marmalade user packages
(require 'package)
(add-to-list 'package-archives ; original Emacs Lisp Package Archive
             '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives ; user-contributed repository
             '("marmalade" . "http://marmalade-repo.org/packages/"))

;; dont open the backtrace buffer on error
;(setq debug-on-error nil)


;; Add hooks for puppet mode
(autoload 'puppet-mode "puppet-mode" "Major mode for editing puppet manifests")
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))
