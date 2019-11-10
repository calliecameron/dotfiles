;;; dotfiles-prog.el --- -*- lexical-binding: t -*- programming modes configuration

;; Copyright (C) 2016  Callum Cameron

;; Author: Callum Cameron

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
  :pin melpa-stable
  :config
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  (ergoemacs-package dotfiles-keys-flycheck
      :bind
    (("C-." . flycheck-next-error)
     ("C-," . flycheck-previous-error)))
  (global-flycheck-mode))

(dotfiles-use-package flycheck-cask
  :pin melpa-stable
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-cask-setup))

(dotfiles-use-package flycheck-checkbashisms
  :pin melpa-stable
  :config
  (flycheck-checkbashisms-setup))

(dotfiles-use-package flycheck-color-mode-line
  :pin melpa-stable
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

(dotfiles-use-package flycheck-haskell
  :pin melpa-stable
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-haskell-setup))

(dotfiles-use-package flycheck-package
  :pin melpa-stable
  :config
  (flycheck-package-setup))

(dotfiles-use-package flycheck-pos-tip
  :config
  (flycheck-pos-tip-mode))

(dotfiles-use-package git-gutter
  :pin melpa-stable
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
  :pin melpa-stable
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

(dotfiles-use-package helm-dash
  :config
  (require 'eww)
  (setq
   helm-dash-min-length 0
   helm-dash-docsets-path (expand-file-name
                           (concat user-emacs-directory "/dash-docsets"))
   helm-dash-browser-func 'dotfiles--helm-dash-eww
   helm-dash-enable-debugging nil)

  (defvar dotfiles--helm-dash-eww-buffer nil)

  (defun dotfiles--helm-dash-eww (url)
    "Pop to an existing eww buffer if possible."
    (interactive)
    (dotfiles--create-or-pop-to-buffer
     (lambda (buf) (eq buf dotfiles--helm-dash-eww-buffer))
     (lambda () (eww-setup-buffer) (current-buffer)))
    (setq dotfiles--helm-dash-eww-buffer (current-buffer))
    (unless (eq eww-current-url url)
      (eww-browse-url url)))

  (defconst dotfiles-required-docsets '("Bash" "Bootstrap_4" "C" "C++" "CSS" "Emacs_Lisp" "Haskell" "HTML" "Java_SE8" "JavaScript" "jQuery" "jQuery_UI" "LaTeX" "Markdown" "MATLAB" "NumPy" "Python_2" "Python_3" "SciPy" "Qt_5" "Matplotlib")
    "Helm-dash docsets that will be installed.")

  (defun dotfiles--docset-installed (docset)
    (let* ((fixed-names-alist
            '(("Bootstrap_4" . "Bootstrap 4")
              ("Emacs_Lisp" . "Emacs Lisp")
              ("Java_SE8" . "Java")
              ("jQuery_UI" . "jQuery UI")
              ("Python_2" . "Python 2")
              ("Python_3" . "Python 3")
              ("Qt_5" . "Qt")))
           (fixed-name
            (assoc-string docset fixed-names-alist)))
      (helm-dash-docset-path (if fixed-name (cdr fixed-name) docset))))

  (dotfiles--generic-require-packages
   "docset"
   'dotfiles--docset-installed
   'helm-dash-install-docset
   dotfiles-required-docsets
   "dotfiles-install-missing-docsets")

  (defun dotfiles-install-missing-docsets ()
    "Install missing helm-dash docsets."
    (interactive)
    (dotfiles--generic-install-missing-packages "docset" 'dotfiles--docset-installed 'helm-dash-install-docset dotfiles-required-docsets))

  (defun dotfiles-configure-mode-docset (mode-hook docsets)
    (add-hook mode-hook (lambda () (setq-local helm-dash-docsets docsets))))

  (dotfiles-configure-mode-docset 'c-mode-hook '("C"))
  (dotfiles-configure-mode-docset 'c++-mode-hook '("C" "C++" "Qt"))
  (dotfiles-configure-mode-docset 'css-mode-hook '("Bootstrap 4" "CSS" "HTML" "JavaScript" "jQuery" "jQuery UI"))
  (dotfiles-configure-mode-docset 'emacs-lisp-mode-hook '("Emacs Lisp"))
  (dotfiles-configure-mode-docset 'ielm-mode-hook '("Emacs Lisp"))
  (dotfiles-configure-mode-docset 'haskell-mode-hook '("Haskell"))
  (dotfiles-configure-mode-docset 'html-mode-hook '("Bootstrap 4" "CSS" "HTML" "JavaScript" "jQuery" "jQuery UI"))
  (dotfiles-configure-mode-docset 'java-mode-hook '("Java"))
  (dotfiles-configure-mode-docset 'javascript-mode-hook '("Bootstrap 4" "CSS" "HTML" "JavaScript" "jQuery" "jQuery UI"))
  (dotfiles-configure-mode-docset 'LaTeX-mode-hook '("LaTeX"))
  (dotfiles-configure-mode-docset 'markdown-mode-hook '("Markdown"))
  (dotfiles-configure-mode-docset 'octave-mode-hook '("MATLAB"))
  (dotfiles-configure-mode-docset 'sh-mode-hook '("Bash"))
  (dotfiles-configure-mode-docset 'term-mode-hook '("Bash"))

  (defun dotfiles-set-python-docsets (version)
    (setq-local helm-dash-docsets `(,(if (eq version 2)
                                         "Python 2"
                                       "Python 3")
                                    "NumPy"
                                    "SciPy"
                                    "Matplotlib")))

  (add-hook 'python-mode-hook (lambda () (dotfiles-set-python-docsets 3)))

  (defvar dotfiles--term-help-fn 'man)
  (make-variable-buffer-local 'dotfiles--term-help-fn)

  (bind-keys
   ("M-/" . (lambda ()
              (interactive)
              (call-interactively
               (if (eq major-mode 'term-mode)
                   dotfiles--term-help-fn
                 'helm-dash-at-point))))))

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
  :pin melpa-stable
  :config
  (setq
   magit-diff-refine-hunk 'all
   magit-diff-paint-whitespace t
   magit-diff-highlight-trailing t)
  (global-magit-file-mode)
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
  :pin melpa-stable
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

(dotfiles-use-package sr-speedbar
  :config
  (require 'sr-speedbar)
  (setq
   ezimage-use-images t
   speedbar-show-unknown-files t
   speedbar-use-images t
   sr-speedbar-right-side nil)
  (bind-keys
   ("s-s" . sr-speedbar-toggle)
   ("C-S-s" . sr-speedbar-toggle)))

(dotfiles-use-package projectile-speedbar)

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
