;; turn off toolbar icons
(tool-bar-mode 0)

;; don't show the welcome splash screen
(setq inhibit-startup-message t)

;; Fix cocoa srgb colors
(setq ns-use-srgb-colorspace t)

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
(setq package-enable-at-startup nil)
(package-initialize)

;; Disable popup dialogs (broken on OS X)
(defadvice yes-or-no-p (around prevent-dialog activate)
  "Prevent yes-or-no-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))
(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  "Prevent y-or-n-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))

;; Auto trim whitespace only on lines I edit
(require 'ws-trim)
(global-ws-trim-mode t)

;; Set css and less mode indents to 2 lines
(add-hook 'css-mode-hook
          (lambda ()
            (setq css-indent-offset 2)
            (setq c-basic-offset 2)
            (setq indent-tabs-mode nil)))

;; dont open the backtrace buffer on error
;(setq debug-on-error nil)

;; Add hooks for puppet mode
(autoload 'puppet-mode "puppet-mode" "Major mode for editing puppet manifests")
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))


;; Add hooks for php mode
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))


;; Add hooks for web mode
(autoload 'web-mode "web-mode" "Major mode for editing web templates." t)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

