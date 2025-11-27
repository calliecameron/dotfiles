;; -*- lexical-binding: t; -*-

(use-package csv-mode)

(use-package markdown-mode)

(use-package nxml-mode
  :ensure nil
  :config
  (defun dotfiles-xml-pretty-print-region (start end)
    "Pretty format XML markup in region START END.  Insert linebreaks to separate tags that have nothing but whitespace between them, and indent the markup by using nxml's indentation rules."
    (interactive "r")
    (save-excursion
      (nxml-mode)
      (goto-char start)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region start end))))

(use-package yaml-mode)

(provide 'dotfiles-misc)
