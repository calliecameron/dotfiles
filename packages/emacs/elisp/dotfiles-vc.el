;; -*- lexical-binding: t; -*-

(use-package git-gutter
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

(use-package git-modes)

(use-package transient)

(use-package magit
  :bind
  (("s-g" . magit-status)
   ("C-S-g" . magit-status)
   ("<f3>" . magit-dispatch-popup)
   ("<f4>" . magit-file-popup))
  :config
  (setq
   magit-diff-refine-hunk 'all
   magit-diff-paint-whitespace t
   magit-diff-highlight-trailing t
   magit-status-show-untracked-files 'all))

(provide 'dotfiles-vc)
