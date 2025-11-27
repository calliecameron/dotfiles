;; -*- lexical-binding: t; -*-

(use-package flycheck-cask
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-cask-setup))

(use-package flycheck-package
  :config
  (flycheck-package-setup))

(use-package lisp-mode
  :ensure nil
  :config
  (require 'ielm)

  (defvar dotfiles--ielm-popped-from nil)

  (defun dotfiles-pop-to-ielm ()
    (interactive)
    (setq dotfiles--ielm-popped-from (current-buffer))
    (dotfiles--create-or-pop-to-buffer
     "*ielm*"
     (lambda () (ielm) (current-buffer))))

  (bind-keys
   :map emacs-lisp-mode-map
   ("C-;" . eval-region)
   ("C-:" . eval-buffer)
   ("s-i" . dotfiles-pop-to-ielm)
   ("C-S-i" . dotfiles-pop-to-ielm))

  (defun dotfiles-pop-from-ielm ()
    (interactive)
    (dotfiles--try-pop-to-buffer dotfiles--ielm-popped-from))

  (bind-keys
   :map inferior-emacs-lisp-mode-map
   ("s-i" . dotfiles-pop-from-ielm)
   ("C-S-i" . dotfiles-pop-from-ielm))

  (add-to-list 'auto-mode-alist '("Cask\\'" . emacs-lisp-mode)))

(provide 'dotfiles-lisp)
