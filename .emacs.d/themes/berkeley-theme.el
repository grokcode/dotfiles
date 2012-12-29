;; Color theme based on default theme at UC Berkeley CS Department.
;; Created by jess@grokcode.com

(deftheme berkeley
  "Berkeley color theme")

(custom-theme-set-faces
 'berkeley
 '(default ((t (:family "Inconsolata" :foundry "apple" :width normal :height 160 :weight normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :foreground "black" :background "ghostwhite" :stipple nil :inherit nil))))
 '(cursor ((t (:background "lightsteelblue"))))

 '(region ((t (:background "lightgoldenrod2"))))
 '(highlight ((t (:background "darkseagreen2"))))
 '(modeline ((t (:background "grey75" :foreground "grey20" :weight light :box (:line-width -1 :color "grey40" :style nil) ))))
 '(modeline-inactive ((t (:background "grey90" :foreground "grey20" :weight light :box (:line-width -1 :color "grey40" :style nil) ))))
 '(minibuffer-prompt ((t (:foreground "mediumblue"))))
 '(font-lock-builtin-face ((t (:foreground "orchid"))))
 '(font-lock-comment-face ((t (:foreground "firebrick"))))
 '(font-lock-constant-face ((t (:foreground "cadetblue"))))
 '(font-lock-function-name-face ((t (:foreground "blue1"))))
 '(font-lock-keyword-face ((t (:foreground "#a020f0"))))
 '(font-lock-string-face ((t (:foreground "rosybrown"))))
 '(font-lock-type-face ((t (:foreground "forestgreen"))))
 '(font-lock-variable-name-face ((t (:foreground "darkgoldenrod"))))
 '(font-lock-warning-face ((t (:bold t :foreground "red1"))))
 '(isearch ((t (:background "magenta3" :foreground "lightskyblue1"))))
 '(lazy-highlight ((t (:background "paleturquoise"))))
 '(isearch-lazy-highlight-face ((t (:background "paleturquoise"))))
 '(link ((t (:foreground "royalblue3" :underline t))))
 '(link-visited ((t (:foreground "royalblue3" :underline t))))
 
 '(flyspell-duplicate ((t (:foreground "#fcaf3e"))))
 '(flyspell-incorrect ((t (:foreground "#cc0000"))))
 
 '(org-date ((t (:foreground "LightSteelBlue" :underline t))))
 '(org-hide ((t (:foreground "#2e3436"))))
 '(org-todo ((t (:inherit font-lock-keyword-face :bold t))))
 '(org-level-1 ((t (:inherit font-lock-function-name-face))))
 '(org-level-2 ((t (:inherit font-lock-variable-name-face))))
 '(org-level-3 ((t (:inherit font-lock-keyword-face))))
 '(org-level-4 ((t (:inherit font-lock-string-face))))
 '(org-level-5 ((t (:inherit font-lock-constant-face))))
 
 '(comint-highlight-input ((t (:italic t :bold t))))
 '(comint-highlight-prompt ((t (:foreground "#8ae234"))))

 '(paren-face-match ((t (:inherit show-paren-match-face))))
 '(paren-face-match-light ((t (:inherit show-paren-match-face))))
 '(paren-face-mismatch ((t (:inherit show-paren-mismatch-face))))
 '(persp-selected-face ((t (:foreground "#729fcf"))))
 '(show-paren-match-face ((t (:background "turquoise"))))
 '(show-paren-mismatch-face ((t (:background "#a020f0" :foreground "white"))))

 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch))))))

(provide-theme 'berkeley)
