;; -*- lexical-binding: t; -*-

(use-package cc-mode
  :ensure nil
  :mode ("\\.nc\\'" . c-mode)
  :config
  (setq
   c-default-style "bsd"
   c-basic-offset 4
   c-tab-always-indent t))

(provide 'dotfiles-c)
