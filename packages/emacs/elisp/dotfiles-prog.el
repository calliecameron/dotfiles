;;; dotfiles-prog.el --- -*- lexical-binding: t -*- programming modes configuration

;; Copyright (C) 2021  Callie Cameron

;; Author: Callie Cameron

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

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

(dotfiles-use-package bison-mode)

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
  (global-company-mode))

(dotfiles-use-package company-quickhelp
  :config
  (company-quickhelp-mode))

(dotfiles-use-package compile
  :config
  (setq
   compilation-ask-about-save nil
   compilation-scroll-output t
   compilation-always-kill t)

  (defvar dotfiles-compile-command nil "Command used to compile a whole program, e.g. 'make'.  If nil, try and guess based on the files in the directory.")
  (defvar dotfiles-compile-clean-command nil "Command used to clean a directory, e.g. 'make clean'.  If nil, try and guess based on the files in the directory.")
  (defvar dotfiles-compile-target-command nil "Command used to compile a single target (target is appended), e.g. 'make'.  If nil, try and guess based on the files in the directory.")

  (make-variable-buffer-local 'dotfiles-compile-command)
  (make-variable-buffer-local 'dotfiles-compile-clean-command)
  (make-variable-buffer-local 'dotfiles-compile-target-command)

  (put 'dotfiles-compile-command 'safe-local-variable 'stringp)
  (put 'dotfiles-compile-clean-command 'safe-local-variable 'stringp)
  (put 'dotfiles-compile-target-command 'safe-local-variable 'stringp)

  (defun dotfiles-compile ()
    "Quick way to compile without prompting for a command, and without saving the previous command."
    (interactive)
    (compile (dotfiles--smart-compile-command dotfiles-compile-command)))

  (defun dotfiles-compile-clean ()
    "Quick way to 'make clean' without prompting for a command."
    (interactive)
    (compile (dotfiles--smart-compile-clean-command dotfiles-compile-clean-command)))

  (defun dotfiles-compile-target (&optional target)
    "Run 'make TARGET' quickly.  Defaults to the current buffer, with some file-extension handling."
    (interactive (list
                  (read-string
                   (format "Make target (default %s): "
                           (dotfiles--compile-choose-extension (buffer-file-name))))))
    (if (not (string= target ""))
        (compile (concat
                  (dotfiles--smart-compile-target-command
                   dotfiles-compile-target-command)
                  " "
                  target))
      (compile (concat
                (dotfiles--smart-compile-target-command
                 dotfiles-compile-target-command)
                " "
                (dotfiles--compile-choose-extension (buffer-file-name))))))

  (defun dotfiles-close-compile-window ()
    "Close the compilation window without having to move into it."
    (interactive)
    (let ((win
           (get-window-with-predicate
            (lambda (window) (string= (buffer-name (window-buffer window)) "*compilation*"))
            nil
            'visible)))
      (when win
        (quit-window nil win))))

  (defun dotfiles--smart-compile-command (local)
    "Pick a compile command to use; use LOCAL if non-nil, else look at what files are available in the current directory."
    (if local
        local
      (let ((saved nil))
        (save-excursion
          ;; This forces the loading of directory-local variables, if they exist
          (find-file ".#dotfiles-compile-temp")
          (setq saved dotfiles-compile-command)
          (kill-buffer))
        (cond
         (saved
          saved)
         ((file-exists-p "SConstruct")
          "scons -Q")
         (t
          "make")))))

  (defun dotfiles--smart-compile-target-command (local)
    "Pick a compile command to use; use LOCAL if non-nil, else look at what files are available in the current directory."
    (if local
        local
      (let ((saved nil))
        (save-excursion
          ;; This forces the loading of directory-local variables, if they exist
          (find-file ".#dotfiles-compile-temp")
          (setq saved dotfiles-compile-target-command)
          (kill-buffer))
        (cond
         (saved
          saved)
         ((file-exists-p "SConstruct")
          "scons -Q")
         (t
          "make")))))

  (defun dotfiles--smart-compile-clean-command (local)
    "Pick a clean command to use; use LOCAL if non-nil, else look at what files are available in the current directory."
    (if local
        local
      (let ((saved nil))
        (save-excursion
          ;; This forces the loading of directory-local variables, if they exist
          (find-file ".#dotfiles-compile-temp")
          (setq saved dotfiles-compile-clean-command)
          (kill-buffer))
        (cond
         (saved
          saved)
         ((file-exists-p "SConstruct")
          "scons -Q -c")
         (t
          "make clean")))))

  (defun dotfiles--compile-choose-extension (file)
    "Automatically work out the compile target based on the extension of FILE."
    (if file
        (let ((base (file-name-sans-extension file))
              (ext (file-name-extension file)))
          (concat base
                  (cond ((or (string= ext "c")
                             (string= ext "h")
                             (string= ext "cpp")
                             (string= ext "hpp"))
                         ".o")
                        ((string= ext "java")
                         ".class")
                        ((string= ext "tex")
                         ".pdf")
                        (t
                         (concat "." ext)))))
      ""))

  (add-to-list 'compilation-finish-functions
               (lambda (buffer s)
                 (let ((has-errors nil))
                   (with-current-buffer buffer
                     (goto-char (point-min))
                     (ignore-errors
                       (compilation-next-error 1))
                     (when (not (eq (point) (point-min)))
                       (setq has-errors t)
                       (goto-char (point-min))))
                   (when (not (string= s "finished\n"))
                     (setq has-errors t))
                   (unless has-errors
                     (dotfiles-close-compile-window)))))

  (bind-keys
   ("<f5>" . dotfiles-compile)
   ("<f7>" . dotfiles-close-compile-window)
   ("<f8>" . dotfiles-compile-clean)))

(progn
  (require 'compilation-alert))

(dotfiles-use-package csv-mode)

(dotfiles-use-package dockerfile-mode)

(progn
  (require 'flex-mode)
  (add-to-list 'auto-mode-alist '("\\.l\\'" . flex-mode)))

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

(dotfiles-use-package flycheck-haskell
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-haskell-setup))

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

(dotfiles-use-package go-mode)

(dotfiles-use-package haskell-mode
  :diminish haskell-doc-mode haskell-indentation-mode
  :config
  (require 'haskell-mode)
  (require 'haskell-indentation)
  (require 'haskell-doc)
  (require 'haskell-process)
  (setq
   haskell-ask-also-kill-buffers nil
   haskell-interactive-popup-errors nil)
  (add-to-list 'completion-ignored-extensions ".hi")

  (defvar dotfiles-extra-ghci-args '() "Pass these args to GHCi when launching an interactive Haskell process.")
  (make-variable-buffer-local 'haskell-process-args-ghci)
  (make-variable-buffer-local 'dotfiles-extra-ghci-args)
  (put 'dotfiles-extra-ghci-args 'safe-local-variable 'flycheck-string-list-p)

  (defun dotfiles--haskell-file-module-name ()
    (save-excursion
      (beginning-of-buffer)
      (if (search-forward-regexp "^module +\\([a-zA-Z0-9\\.]+\\)" nil t)
          (match-string 1)
        (file-name-sans-extension (buffer-file-name)))))

  (defun dotfiles-haskell-interactive-reload ()
    (interactive)
    (when (eq major-mode 'haskell-interactive-mode)
      (end-of-buffer)
      (haskell-interactive-mode-kill-whole-line)
      (insert ":r")
      (haskell-interactive-mode-return)))

  (defun dotfiles-haskell-smart-interactive-switch ()
    (interactive)
    (when (eq major-mode 'haskell-mode)
      (if (and (boundp 'haskell-session) haskell-session)
          (progn
            (haskell-interactive-switch)
            (dotfiles-haskell-interactive-reload))
        (setq haskell-process-args-ghci
              (append (default-value 'haskell-process-args-ghci)
                      dotfiles-extra-ghci-args))
        (let ((module (dotfiles--haskell-file-module-name)))
          (haskell-interactive-switch)
          (end-of-buffer)
          (haskell-interactive-mode-kill-whole-line)
          (insert (concat ":l " module))
          (haskell-interactive-mode-return)))))
  (bind-keys
   :map haskell-mode-map
   ("s-i" . dotfiles-haskell-smart-interactive-switch)
   ("C-S-i" . dotfiles-haskell-smart-interactive-switch))

  (bind-keys
   :map haskell-interactive-mode-map
   ("s-i" . dotfiles-haskell-interactive-reload)
   ("C-S-i" . dotfiles-haskell-interactive-reload))

  (add-hook 'haskell-mode-hook
            (lambda ()
              (turn-on-haskell-doc)
              (turn-on-haskell-indentation))))

(dotfiles-use-package helm-make
  :bind
  ("<f6>" . helm-make))

(dotfiles-use-package helm-projectile
  :config
  (helm-projectile-on)
  (defun dotfiles-helm-ag-dwim ()
    (interactive)
    (if (projectile-project-p)
        (helm-projectile-ag)
      (helm-ag)))
  (bind-keys
   ("s-f" . dotfiles-helm-ag-dwim)
   ("C-S-f" . dotfiles-helm-ag-dwim)))

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
   magit-diff-highlight-trailing t)
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

(dotfiles-use-package projectile
  :diminish projectile-mode
  :config
  (projectile-global-mode)
  (setq
   projectile-mode-line nil ; smart-mode-line handles this instead
   frame-title-format '((:eval
                         (if (projectile-project-p)
                             (replace-regexp-in-string "%" "%%" (format "[%s] " (projectile-project-name)))
                           ""))
                        "%b")
   icon-title-format frame-title-format)
  (bind-keys
   ("s-p" . projectile-command-map)
   ("C-S-p" . projectile-command-map)))

(progn
  (require 'reftex)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t))

(dotfiles-use-package scala-mode)

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
