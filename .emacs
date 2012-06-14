;; turn off toolbar icons
(tool-bar-mode 0)

;; don't show the welcome splash screen
(setq inhibit-startup-message t)

;; Setup the windows to be on the left and full screen vertically,
;; also set up some colors and the default font
(setq default-frame-alist
      '((top . 0) (left . 0)
	(width . 100) (height . 80)
	(cursor-color . "lightsteelblue")
	(cursor-type . box)
	(foreground-color . "black")
	(background-color . "ghostwhite")
	(font . "-unknown-Inconsolata-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")))


;; setup nXhtml for html and php dev
(load "~/.site-lisp/nxhtml/autostart.el")

;; handle some different file types
(add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))


;; modify the color scheme a bit
(set-face-foreground 'font-lock-preprocessor-face "Purple" )
(set-variable font-lock-preprocessor-face 'font-lock-preprocessor-face) 

;; dont open the backtrace buffer on error
(setq debug-on-error nil)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(mumamo-background-chunk-major ((((class color) (min-colors 88) (background light)) (:background "#feffee"))))
 '(mumamo-background-chunk-submode1 ((((class color) (min-colors 88) (background light)) (:background "#f9f5ff")))))
