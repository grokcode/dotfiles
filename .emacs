;; turn off toolbar icons
(tool-bar-mode 0)

;; don't show the welcome splash screen
(setq inhibit-startup-message t)

;; Fix cocoa srgb colors
(setq ns-use-srgb-colorspace t)

;; Load custom themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'berkeley t)

;; Turn on soft wrapping at word boundaries
(global-visual-line-mode t)

;; Default wrap at 80 chars
(setq-default fill-column 80)

;; Don't implicitly split windows vertically
(setq split-width-threshold nil)

;; Turn on column number display
(setq column-number-mode t)

;; Setup the windows to be on the left and full screen vertically,
;; also set up some colors and the default font
(setq default-frame-alist
      '((cursor-color . "lightsteelblue")
	(cursor-type . box)
	(foreground-color . "black")
	(background-color . "ghostwhite")
	(font . "Inconsolata-16")
	))

;; Initialize elpa backages
(package-initialize)

;; Add Elpa, Melap user packages
(require 'package)
(add-to-list 'package-archives ; original Emacs Lisp Package Archive
	     '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))
(setq package-enable-at-startup nil)
(unless package--initialized (package-initialize t))

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


;; Enable flycheck for syntax checking
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Set css and less mode indents to 2 chars
(add-hook 'css-mode-hook
          (lambda ()
            (setq css-indent-offset 2)
            (setq c-basic-offset 2)
            (setq indent-tabs-mode nil)))

;; Set javascript indents to 4 spaces
(setq js-indent-level 4)

;; Set go mode indents to 4 chars
(add-hook 'go-mode-hook
          (lambda ()
            (setq tab-width 4)
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
(add-hook 'php-mode-hook
          (lambda ()
            (setq c-basic-offset 4)
            (setq indent-tabs-mode nil)))


;; Add hooks for ruby mode
(add-to-list 'auto-mode-alist '("Fastfile" . ruby-mode))


;; Add hooks for kivy mode
(autoload 'kivy-mode "kivy-mode" "Major mode for kivy code")
(add-to-list 'auto-mode-alist '("\\.kv$" . kivy-mode))


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


;; Add hooks for nxml mode. Use hide-show minor mode
(add-hook 'nxml-mode-hook 'hs-minor-mode)
; C-c h does expand/collapse in nxml mode
(add-hook 'nxml-mode-hook
          (lambda () (local-set-key (kbd "C-c h") 'hs-toggle-hiding)))
(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"

               "<!--"
               sgml-skip-tag-forward
               nil))


;; Default to spaces when indenting
(setq-default indent-tabs-mode nil)

;; Enable downcase-region
(put 'downcase-region 'disabled nil)

;; M-x iwb indents all the things
(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))
(put 'upcase-region 'disabled nil)


;; Replace audible bell with modeline flash
(defun my-terminal-visible-bell ()
  "A friendlier visual bell effect."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line))

(setq visible-bell nil
      ring-bell-function 'my-terminal-visible-bell)
