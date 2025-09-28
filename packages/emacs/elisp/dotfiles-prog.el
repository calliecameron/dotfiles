;;; dotfiles-prog.el --- -*- lexical-binding: t -*- programming modes configuration

;;; Commentary:

;; Programming modes configuration.

;;; Code:

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


(dotfiles-use-package tex-site
  :ensure auctex
  :config
  (setq
   TeX-auto-save t
   TeX-parse-self t))

(progn
  (add-to-list 'auto-mode-alist '("Cask\\'" . emacs-lisp-mode)))

(dotfiles-use-package cc-mode
  :mode ("\\.nc\\'" . c-mode)
  :config
  (setq
   c-default-style "bsd"
   c-basic-offset 4
   c-tab-always-indent t))

(dotfiles-use-package company
  :diminish company-mode
  :config
  (add-hook 'text-mode-hook
            (lambda ()
              (when (boundp 'company-backends)
                (setq company-backends (delete 'company-dabbrev company-backends)))))
  (global-company-mode))

(dotfiles-use-package company-quickhelp
  :config
  (company-quickhelp-mode))

(dotfiles-use-package csv-mode)

(dotfiles-use-package flycheck
  :bind
  (("C-." . flycheck-next-error)
   ("C-," . flycheck-previous-error))
  :config
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  (global-flycheck-mode))

(dotfiles-use-package flycheck-cask
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-cask-setup))

(dotfiles-use-package flycheck-checkbashisms
  :config
  (flycheck-checkbashisms-setup))

(dotfiles-use-package flycheck-color-mode-line
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

(dotfiles-use-package flycheck-package
  :config
  (flycheck-package-setup))

(dotfiles-use-package flycheck-pos-tip
  :config
  (flycheck-pos-tip-mode))

(dotfiles-use-package git-gutter
  :diminish git-gutter-mode
  :config
  (setq
   git-gutter:update-interval 2
   git-gutter:visual-line t
   git-gutter:hide-gutter t)
  (set-face-background 'git-gutter:modified "yellow")
  (set-face-background 'git-gutter:added "green")
  (set-face-background 'git-gutter:deleted "red")
  (global-git-gutter-mode)
  (bind-keys
   ("s-." . git-gutter:next-hunk)
   ("s-," . git-gutter:previous-hunk)))

(dotfiles-use-package helm-make
  :bind
  ("<f6>" . helm-make))

(progn
  (require 'lisp-mode)
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
   ("C-S-i" . dotfiles-pop-from-ielm)))

(dotfiles-use-package magit
  :config
  (setq
   magit-diff-refine-hunk 'all
   magit-diff-paint-whitespace t
   magit-diff-highlight-trailing t
   magit-status-show-untracked-files 'all)
  (bind-keys
   ("s-g" . magit-status)
   ("C-S-g" . magit-status)
   ("<f3>" . magit-dispatch-popup)
   ("<f4>" . magit-file-popup)))

(dotfiles-use-package make-mode
  :mode ("\\.make\\'" . makefile-mode))

(dotfiles-use-package markdown-mode
  :mode
  ("\\.markdown\\'" "\\.md\\'" "\\.mkd\\'" "\\.text\\'"))

(progn
  (require 'nxml-mode)
  (defun dotfiles-xml-pretty-print-region (start end)
    "Pretty format XML markup in region START END.  Insert linebreaks to separate tags that have nothing but whitespace between them, and indent the markup by using nxml's indentation rules."
    (interactive "r")
    (save-excursion
      (nxml-mode)
      (goto-char start)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region start end))))

(dotfiles-use-package octave
  :mode ("\\.m\\'" . octave-mode)
  :config
  (defun octave-indent-comment ()
    "A function for `smie-indent-functions' (which see)."
    (save-excursion
      (back-to-indentation)
      (cond
       ((octave-in-string-or-comment-p) nil)
       ((looking-at-p "\\(\\s<\\)\\1\\{2,\\}") 0))))
  (add-hook 'octave-mode-hook
            (lambda ()
              (setq octave-comment-char ?%)
              (setq comment-start "% ")
              (setq comment-add 0))))

(dotfiles-use-package sh-script
  :mode ("\\.zsh\\'" . dotfiles--zsh-auto-mode-setup)
  :config
  (defun dotfiles--zsh-auto-mode-setup ()
    "Zsh scripts should be in zsh mode."
    (shell-script-mode)
    (sh-set-shell "zsh" t nil)))

(dotfiles-use-package yaml-mode)


(provide 'dotfiles-prog)

;;; dotfiles-prog.el ends here
