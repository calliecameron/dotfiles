;; -*-Emacs-Lisp-*-

;; Emacs 25 wants this here, even though we call it later...
(package-initialize)

(let ((dotfiles-load-path (getenv "DOTFILES_CORE_DIR")))
  (if dotfiles-load-path
      (progn
        (add-to-list 'load-path dotfiles-load-path)
        (require 'dotfiles))
    (message "Can't find dotfiles.")))
