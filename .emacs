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

;; dont open the backtrace buffer on error
;(setq debug-on-error nil)

