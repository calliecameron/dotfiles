;; -*-Emacs-Lisp-*-

(let ((dotfiles-load-path (getenv "DOTFILES_CORE_DIR")))
  (if dotfiles-load-path
      (progn
        (add-to-list 'load-path dotfiles-load-path)
        (require 'dotfiles))
    (message "Can't find dotfiles.")))
