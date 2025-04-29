;;; Source https://gist.github.com/dsedivec/65eff752ec3aa3b652977c6e681bd609

(require 'flycheck)

(flycheck-define-checker python-ruff
  "A Python syntax and style checker using the ruff utility.
To override the path to the ruff executable, set
`flycheck-python-ruff-executable'.
See URL `http://pypi.python.org/pypi/ruff'."
  :command ("ruff"
            "check"
            "--output-format=pylint"
	    (eval (when buffer-file-name
                    (concat "" buffer-file-name)))
            )
  :standard-input nil
  :error-filter (lambda (errors)
                  (let ((errors (flycheck-sanitize-errors errors)))
                    (seq-map #'flycheck-flake8-fix-error-level errors)))
  :error-patterns
  ((warning line-start
            (file-name) ":" line ":" (optional column ":") " ["
            (id (one-or-more (any alpha)) (one-or-more digit)) "] "
            (message (one-or-more not-newline))
            line-end))
  :modes (python-mode python-ts-mode))

(add-to-list 'flycheck-checkers 'python-ruff)

(provide 'flycheck-ruff)
