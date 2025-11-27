;; -*- lexical-binding: t; -*-

(bind-keys
 ("C-/" . comment-or-uncomment-region))

(add-hook 'prog-mode-hook
          (lambda ()
            (flyspell-prog-mode)
            (setq show-trailing-whitespace t)))

(advice-add 'comment-or-uncomment-region :before
            (lambda (&rest args)
              "When called interactively with no region, comment or uncomment the current line."
              (interactive
               (if mark-active (list (region-beginning) (region-end))
                 (list (line-beginning-position) (line-end-position))))))

(use-package company
  :diminish company-mode
  :config
  (add-hook 'text-mode-hook
            (lambda ()
              (when (boundp 'company-backends)
                (setq company-backends (delete 'company-dabbrev company-backends)))))
  (global-company-mode))

(use-package company-quickhelp
  :config
  (company-quickhelp-mode))

(use-package flycheck
  :bind
  (("C-." . flycheck-next-error)
   ("C-," . flycheck-previous-error))
  :config
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  (global-flycheck-mode))

(use-package flycheck-color-mode-line
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

(use-package flycheck-pos-tip
  :config
  (flycheck-pos-tip-mode))

(provide 'dotfiles-prog)
