;;; dotfiles-ui.el --- -*- lexical-binding: t -*- user interface settings

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

;; User interface settings.

;;; Code:

(setq
 backup-directory-alist nil
 case-fold-search t
 face-font-selection-order '(:width :height :weight :slant)
 inhibit-startup-message t
 initial-major-mode 'text-mode
 initial-scratch-message nil
 large-file-warning-threshold 100000000
 require-final-newline t
 ring-bell-function 'ignore
 scroll-conservatively 10000
 scroll-error-top-bottom t
 standard-indent 4
 switch-to-buffer-preserve-window-point t
 tab-always-indent 'complete
 vc-make-backup-files t
 vc-follow-symlinks t
 vc-handled-backends nil
 x-alt-keysym 'meta)

(setq-default
 major-mode 'text-mode
 indent-tabs-mode nil
 tab-width 4)

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'display-buffer-alist
             '("*" . (display-buffer-reuse-window
                      . ((reusable-frames . visible)))))
(add-to-list 'kill-emacs-query-functions
             (lambda ()
               (and (if (eq (call-process "dotfiles-repo-is-clean" nil nil nil dotfiles-dir) 0)
                        t
                      (y-or-n-p "Dotfiles has uncommitted changes; quit anyway? "))
                    (if (eq (call-process "dotfiles-repo-is-clean" nil nil nil dotfiles-private-dir) 0)
                        t
                      (y-or-n-p "Dotfiles private has uncommitted changes; quit anyway? ")))))

(when dotfiles-on-android
  (set-face-attribute 'default nil :height 150))


(dotfiles--file-openers
 '(dotfiles-variables
   dotfiles-generic-aliases
   dotfiles-bash-aliases
   dotfiles-zsh-aliases
   dotfiles-emacs-init
   dotfiles-local-variables
   dotfiles-local-emacs))

(defmacro dotfiles--local-aliases (var-name shell)
  (let* ((filename (concat dotfiles-local-aliases "." (eval shell)))
         (docstring (format "Open %s." filename)))
    `(defun ,var-name ()
       ,docstring
       (interactive)
       (find-file ,filename))))

(dotfiles--local-aliases dotfiles-local-generic-aliases "sh")
(dotfiles--local-aliases dotfiles-local-bash-aliases "bash")
(dotfiles--local-aliases dotfiles-local-zsh-aliases "zsh")

(defun dotfiles--create-or-pop-to-buffer (buffer creator)
  "Display a buffer in another window, or if no suitable buffer is found, create one and diaplay it.

If BUFFER is a string, it is the name of the buffer to find; if it is a predicate, use the first buffer for which it returns true.  CREATOR is used to create a new buffer if no suitable existing one is found, and should return the new buffer."
  (pop-to-buffer
   (let* ((initial-buf (current-buffer))
          (buf
           (if (stringp buffer)
               (get-buffer buffer)
             (-first buffer (buffer-list)))))
     (if buf
         buf
       (setq buf (funcall creator))
       (switch-to-buffer initial-buf)
       buf))
   '((display-buffer-reuse-window
      display-buffer-pop-up-window)
     . ((inhibit-same-window . t)))))

(defun dotfiles--try-pop-to-buffer (buf)
  "Pop to BUF, or display an error message if not possible."
  (if (and buf
           (buffer-live-p buf))
      (pop-to-buffer
       buf
       '((display-buffer-reuse-window
          display-buffer-pop-up-window)
         . ((inhibit-same-window . t))))
    (message "Buffer no longer exists.")))

(advice-add 'scroll-up :around
            (lambda (orig-func &rest args)
              "Keep cursor in the same column when scrolling up, instead of always going back to the beginning of the line.  From http://en.wikipedia.org/wiki/User:Gwern/.emacs."
              (let ((col (current-column)))
                (apply orig-func args)
                (move-to-column col))))

(advice-add 'scroll-down :around
            (lambda (orig-func &rest args)
              "Keep cursor in the same column when scrolling down, instead of always going back to the beginning of the line.  From http://en.wikipedia.org/wiki/User:Gwern/.emacs."
              (let ((col (current-column)))
                (apply orig-func args)
                (move-to-column col))))

(defun dotfiles-smart-home-key ()
  "Move to the beginning of visual line, beginning of logical line, or first non-whitespace character, depending on point position."
  (interactive "^")
  (let ((oldpoint (point)))
    (when (and visual-line-mode (not truncate-lines))
      (beginning-of-visual-line))
    (when (= oldpoint (point))
      (beginning-of-line)
      (when (= oldpoint (point))
        (back-to-indentation)))))

(defun dotfiles-smart-end-key ()
  "Move to the end of visual line or end of logical line, depending on point position."
  (interactive "^")
  (let ((oldpoint (point)))
    (when (and visual-line-mode (not truncate-lines))
      (end-of-visual-line))
    (when (= oldpoint (point))
      (end-of-line))))

(defun dotfiles-smart-scroll-left ()
  "Scroll left only if all line-wrapping is disabled (my idea of `scroll left' is the opposite of Emacs's)."
  (interactive "^")
  (when truncate-lines
    (let ((oldpoint (point)))
      (beginning-of-line)
      (let ((dist (- oldpoint (point))))
        (goto-char oldpoint)
        (left-char (min (- (window-width) 2) dist))))))

(defun dotfiles-smart-scroll-right ()
  "Scroll right only if all line-wrapping is disabled (my idea of `scroll right' is the opposite of Emacs's)."
  (interactive "^")
  (when truncate-lines
    (let ((oldpoint (point)))
      (end-of-line)
      (let ((dist (- (point) oldpoint)))
        (goto-char oldpoint)
        (right-char (min (- (window-width) 2) dist))))))

(defun dotfiles-split-window-below-switch ()
  "Split the current window vertically, and select the new (bottom) window."
  (interactive)
  (select-window (split-window-below)))

(defun dotfiles-split-window-right-switch ()
  "Split the current window horizontally, and select the new (right) window."
  (interactive)
  (select-window (split-window-right)))

(defun dotfiles-goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input."
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

(defun dotfiles-insert-around-region (start end beforetext aftertext)
  "Insert BEFORETEXT before region START END, and AFTERTEXT after it.  If the region is inactive, insert around point instead."
  (interactive "r\nsString before: \nsString after: ")
  (if mark-active
      (save-excursion
        (goto-char end)
        (insert aftertext)
        (goto-char start)
        (insert beforetext))
    (insert beforetext)
    (let ((pos (point)))
      (insert aftertext)
      (goto-char pos))))

(defun dotfiles-rename-file-and-buffer (new-name)
  "Renames the current buffer and the file it is visiting to NEW-NAME.  From http://www.cabochon.com/~stevey/blog-rants/my-dot-emacs-file.html."
  (interactive "FNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun dotfiles-open-external (file)
  "Open FILE in the default external program.  Basically `browse-url-xdg-open' with completion specifically for files."
  (interactive "fOpen file: ")
  (browse-url-xdg-open (expand-file-name file)))

(defun dotfiles-find-file-on-path (file)
  "Open a FILE located on the 'PATH' environment variable."
  (interactive
   (list
    (s-trim (read-shell-command "Open executable on PATH: "))))
  (let ((candidate (executable-find file)))
    (if candidate
        (find-file candidate)
      (message "Executable '%s' not found." file))))

(defun dotfiles-cut-to-register ()
  "Cut text to a register instead of the kill ring."
  (interactive)
  (when (use-region-p)
    (call-interactively 'copy-to-register)
    (call-interactively 'delete-region)))

(defun dotfiles-insert-register ()
  "Insert register, with more useful behaviour."
  (interactive)
  (setq current-prefix-arg t)
  (call-interactively 'insert-register))

(defun dotfiles-force-insert-four-spaces ()
  "For when text mode's tab behaviour becomes too annoying."
  (interactive)
  (insert "    "))

(defun dotfiles-utf8ify ()
  "Convert buffer into Unix-line-ended UTF-8."
  (interactive)
  (set-buffer-file-coding-system 'utf-8-unix))

(defun dotfiles-asciify-punctuation ()
  "Convert a selection of non-ASCII puntuation (e.g. curly quotes) into ASCII (LaTeX) equivalents."
  (interactive)
  (save-excursion
    (-each
     '(("–" . "--")
       ("’" . "'")
       ("‘" . "`")
       ("  " . " ")
       ("“" . "``")
       ("”" . "''")
       ("…" . "..."))
     (lambda (x) (query-replace (car x) (cdr x) nil (point-min) (point-max))))))

(defvar dotfiles--original-mode-line nil)
(make-variable-buffer-local 'dotfiles--original-mode-line)

(defun dotfiles-toggle-mode-line ()
  "Toggle the mode line for the current buffer."
  (interactive)
  (if dotfiles--original-mode-line
      (progn
        (setq mode-line-format dotfiles--original-mode-line)
        (setq dotfiles--original-mode-line nil))
    (setq dotfiles--original-mode-line mode-line-format)
    (setq mode-line-format nil)))

(bind-keys*
 ("C-x DEL". keyboard-quit)
 ("C-x <deletechar>" . keyboard-quit)
 ("C-x C-g" . keyboard-quit)
 ("C-c C-g" . keyboard-quit)
 ("C-c c" . save-buffers-kill-emacs)
 ("C-<prior>" . dotfiles-smart-scroll-left)
 ("C-<next>" . dotfiles-smart-scroll-right)
 ("C-x <" . dotfiles-smart-scroll-left)
 ("C-x >" . dotfiles-smart-scroll-right)
 ("<mouse-6>" . dotfiles-smart-scroll-left)
 ("<mouse-7>" . dotfiles-smart-scroll-right)
 ("<C-mouse-5>" . text-scale-decrease)
 ("<C-mouse-4>" . text-scale-increase)
 ("s-c" . copy-to-register)
 ("C-S-c" . copy-to-register)
 ("s-x" . dotfiles-cut-to-register)
 ("C-S-x" . dotfiles-cut-to-register)
 ("s-v" . dotfiles-insert-register)
 ("C-\\" . switch-to-buffer)
 ("s-\\" . dotfiles-buffer-map)
 ("C-|" . dotfiles-buffer-map)
 ("s-o" . dotfiles-open-map)
 ("s-u" . package-list-packages)
 ("C-S-u" . package-list-packages)
 ("s-k" . describe-personal-keybindings)
 ("C-S-k" . describe-personal-keybindings)
 ([remap goto-line] . dotfiles-goto-line-with-feedback)
 ([remap move-beginning-of-line] . dotfiles-smart-home-key)
 ([remap move-end-of-line] . dotfiles-smart-end-key)
 ([remap split-window-below] . dotfiles-split-window-below-switch)
 ([remap split-window-right] . dotfiles-split-window-right-switch)
 ("<s-down>" . dotfiles-split-window-below-switch)
 ("<s-right>" . dotfiles-split-window-right-switch)
 ("<C-S-down>" . dotfiles-split-window-below-switch)
 ("<C-S-right>" . dotfiles-split-window-right-switch)
 ("<s-up>" . (lambda () (interactive) (split-window-below)))
 ("<s-left>" . (lambda () (interactive (split-window-right))))
 ("<C-S-up>" .(lambda () (interactive) (split-window-below)))
 ("<C-S-left>" . (lambda () (interactive (split-window-right))))
 ("C-c <left>" . windmove-left)
 ("C-c <right>" . windmove-right)
 ("C-c <up>" . windmove-up)
 ("C-c <down>" . windmove-down)
 ("<M-left>" . windmove-left)
 ("<M-right>" . windmove-right)
 ("<M-up>" . windmove-up)
 ("<M-down>" . windmove-down))

(bind-keys
  :map visual-line-mode-map
  ([remap move-beginning-of-line] . dotfiles-smart-home-key)
  ([remap move-end-of-line] . dotfiles-smart-end-key))

(bind-keys
 :map dotfiles-buffer-map
 ("b" . bury-buffer)
 ("c" . count-words)
 ("f" . fill-region)
 ("m" . dotfiles-toggle-mode-line)
 ("p" . pwd)
 ("r" . dotfiles-rename-file-and-buffer)
 ("u" . dotfiles-utf8ify)
 ("v" . dotfiles-insert-register)
 ("=" . balance-windows))

(bind-keys
 :map dotfiles-open-map
 ("l" . dotfiles-open-files-map)
 ("o" . dotfiles-open-external)
 ("p" . dotfiles-find-file-on-path)
 ("u" . browse-url-xdg-open))

(bind-keys
 :map dotfiles-open-files-map
 ("b" . dotfiles-bash-aliases)
 ("e" . dotfiles-emacs-init)
 ("g" . dotfiles-generic-aliases)
 ("l" . dotfiles-open-local-files-map)
 ("v" . dotfiles-variables)
 ("z" . dotfiles-zsh-aliases))

(bind-keys
 :map dotfiles-open-local-files-map
 ("b" . dotfiles-local-bash-aliases)
 ("e" . dotfiles-local-emacs)
 ("g" . dotfiles-local-generic-aliases)
 ("v" . dotfiles-local-variables)
 ("z" . dotfiles-local-zsh-aliases))

;; Access to the function keys on keyboards without function keys
(when dotfiles-on-android
  (-each
      '(("C-!" . "<f1>")
        ("C-\"" . "<f2>")
        ("C-£" . "<f3>")
        ("C-$" . "<f4>")
        ("C-%" . "<f5>")
        ("C-^" . "<f6>")
        ("C-&" . "<f7>")
        ("C-*" . "<f8>")
        ("C-(" . "<f9>")
        ("C-)" . "<f10>")
        ("C-_" . "<f11>")
        ("C-+" . "<f12>"))
    (lambda (k)
      (define-key key-translation-map (kbd (car k)) (kbd (cdr k))))))

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(add-hook 'text-mode-hook (lambda () (setq show-trailing-whitespace t)))


(blink-cursor-mode -1)
(line-number-mode 1)
(column-number-mode 1)
(global-font-lock-mode 1)
(delete-selection-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(auto-insert-mode 1)


(use-package ergoemacs-mode
  :pin melpa-stable
  :config
  (if (fboundp 'ergoemacs-ini-mode)
      (progn
        ;; Stable version
        (setq
         ergoemacs-theme-options '((save-options-on-exit off))
         ergoemacs-theme "standard"
         ergoemacs-keyboard-layout "gb"
         ergoemacs-ctl-c-or-ctl-x-delay 0.2
         ergoemacs-handle-ctl-c-or-ctl-x 'both
         ergoemacs-use-menus t
         ergoemacs-smart-paste nil
         ergoemacs-mode-line nil)
        (ergoemacs-ini-mode)
        (add-hook 'ergoemacs-mode-hook
                  (lambda ()
                    (global-set-key (kbd "M-m") 'newline)
                    (define-key ergoemacs-keymap (kbd "C-e") 'switch-window)
                    (define-key ergoemacs-keymap (kbd "C-S-o") 'dotfiles-open-map)
                    (global-set-key (kbd "C-.") nil)
                    (global-set-key (kbd "<f8>") nil)
                    (global-set-key (kbd "<f12>") 'toggle-frame-fullscreen)
                    (global-set-key (kbd "C-/") 'comment-or-uncomment-region))))
    (progn
      ;; Unstable version
      (setq
       ergoemacs-theme-options '((save-options-on-exit off))
       ergoemacs-theme "standard"
       ergoemacs-keyboard-layout "gb"
       ergoemacs-ctl-c-or-ctl-x-delay 0.2
       ergoemacs-handle-ctl-c-or-ctl-x 'both
       ergoemacs-smart-paste nil
       ergoemacs-mode-line nil)
      (ergoemacs-mode)))

  (defun dotfiles--startup-buffer ()
    (let ((buf (get-buffer "untitled")))
      (if buf
          buf
        (ergoemacs-new-empty-buffer))))
  (setq initial-buffer-choice 'dotfiles--startup-buffer))

(progn
  (setq
   save-abbrevs t
   abbrev-file-name (f-join this-package-conf-dir "emacs-abbrevs.el"))
  (when (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))
  (advice-add 'abbrev-insert :after (lambda (&rest args)
                                      (abbrev-put (car args) :count 0)))
  (add-hook 'text-mode-hook 'abbrev-mode))

(use-package ace-isearch
  :pin melpa-stable
  :diminish ace-isearch-mode
  :config
  (global-ace-isearch-mode))

(use-package ace-jump-mode
  :pin melpa-stable
  :bind
  (("s-j" . ace-jump-mode)
   ("C-S-j" . ace-jump-mode)))

(use-package alert
  :pin melpa-stable
  :config
  (if (or dotfiles-on-android
          dotfiles-on-windows)
      (setq alert-default-style 'fringe)
    (setq alert-default-style 'notifications)))

(use-package autorevert
  :config
  (setq auto-revert-verbose nil)
  (bind-keys
   :map dotfiles-buffer-map
   ("a" . auto-revert-mode))
  (add-hook 'dired-mode-hook 'auto-revert-mode)
  (add-hook 'doc-view-mode-hook 'auto-revert-mode))

(use-package back-button
  :diminish back-button-mode
  :bind
  ("<mouse-8>" . back-button-global-backward)
  ("<mouse-9>" . back-button-global-forward)
  ("<C-mouse-8>" . back-button-local-backward)
  ("<C-mouse-9>" . back-button-local-forward)
  :config
  (back-button-mode))

(use-package bm
  :bind
  (("<left-fringe> <mouse-5>" . bm-next-mouse)
   ("<left-fringe> <mouse-4>" . bm-previous-mouse)
   ("<left-fringe> <mouse-1>" . bm-toggle-mouse)
   ("C-#" . bm-toggle)
   ("M-#" . bm-remove-all-current-buffer)))

(use-package dictionary
  :pin melpa-stable
  :config
  (when (executable-find "dictd")
    (setq dictionary-server "localhost"))
  (bind-keys
   :map dotfiles-open-map
   ("d" . dictionary-search)))

(use-package doc-view
  :config
  (setq doc-view-continuous t))

(use-package ediff
  :config
  (setq  ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package flyspell
  :diminish flyspell-mode
  :config
  (setq
   ispell-dictionary "en_GB"
   flyspell-issue-message-flag nil
   flyspell-large-region 1)
  (bind-keys*
   ("C->" . flyspell-goto-next-error))
  (add-hook 'flyspell-mode-hook
            (lambda () (define-key flyspell-mode-map (kbd "C-;") nil)))
  (add-hook 'text-mode-hook 'flyspell-mode)
  (defvar dotfiles-auto-spellcheck t "Automatically spellcheck newly loaded buffers.  Set to nil if spellchecking makes things too slow.")
  (defvar dotfiles-auto-spellchecked nil "Has this buffer been auto-spellchecked?")
  (make-variable-buffer-local 'dotfiles-auto-spellchecked)
  (defun dotfiles-auto-spellcheck-buffer ()
    "Spellcheck a new buffer."
    (when (and
           flyspell-mode
           dotfiles-auto-spellcheck
           (not dotfiles-auto-spellchecked))
      (setq dotfiles-auto-spellchecked t)
      (with-local-quit
        (flyspell-buffer))))
  (run-with-idle-timer 1 t 'dotfiles-auto-spellcheck-buffer))

(use-package framemove
  :config
  (setq framemove-hook-into-windmove t))

(use-package guide-key
  :pin melpa-stable
  :diminish guide-key-mode
  :config
  (setq
   guide-key/guide-key-sequence '("C-x" "C-c" "C-h" "s-o" "C-S-o" "s-\\" "C-|" "s-p" "C-S-p")
   guide-key/recursive-key-sequence-flag t)
  (guide-key-mode 1))

(use-package helm
  :pin melpa-stable
  :diminish helm-mode
  :config
  (require 'helm-config)
  (helm-mode 1))

(use-package helm-bm
  :bind
  ("C-'" . helm-bm))

(use-package helm-flyspell
  :config
  (bind-keys*
   ("C-<" . helm-flyspell-correct)))

(use-package highlight-indentation
  :pin melpa-stable
  :diminish highlight-indentation-mode)

(use-package hl-line
  :config
  (global-hl-line-mode 1)
  (make-variable-buffer-local 'global-hl-line-mode))

(progn
  (require 'man)
  (add-hook 'window-size-change-functions
            (lambda (frame)
              (walk-windows
               (lambda (win)
                 (with-current-buffer (window-buffer win)
                   (when (eq major-mode 'Man-mode)
                     (Man-update-manpage))))
               nil
               frame)))

  (bind-keys*
   ("M-?" . man)))

(progn
  (require 'org)
  (setq
   org-log-done 'time
   org-support-shift-select t
   org-return-follows-link t
   org-link-abbrev-alist '(("search" . "https://www.google.co.uk/#q=%h")
                           ("map" . "https://maps.google.co.uk/maps?q=%h")
                           ("scholar" . "http://scholar.google.co.uk/scholar?q=%h")))

  (bind-keys
   :map org-mode-map
   ("C-t" . org-todo))

  (add-hook 'org-mode-hook
            (lambda ()
              (when (boundp 'ergoemacs--for-org-mode-hook)
                (setq ergoemacs--for-org-mode-hook nil))
              (setcdr (assq 'file org-link-frame-setup) 'find-file)
              (unless overriding-terminal-local-map
                (setq overriding-terminal-local-map (make-keymap)))
              (bind-keys
               :map overriding-terminal-local-map
               ("<M-left>" . windmove-left)
               ("<M-right>" . windmove-right)
               ("<M-up>" . windmove-up)
               ("<M-down>" . windmove-down))))

  (defvar dotfiles-org-linkify-suffix ".org" "If dotfiles-org-linkify's text doesn't contain a dot, append this suffix to the link file name.")
  (make-variable-buffer-local 'dotfiles-org-linkify-suffix)

  (defun dotfiles-org-linkify (start end &optional suffix)
    "Convert text in region into an `org-mode' hyperlink, linking to a
file with the same name as region.  Text containing a dot is
considered a filename, and used as the link's target without
modification.  Text without a dot has SUFFIX (which defaults to
dotfiles-org-linkify-suffix) appended."
    (interactive "r")
    (let ((region (buffer-substring start end)))
      (dotfiles-insert-around-region start end
                                        (concat
                                         "[[file:"
                                         region
                                         (if (cl-find ?. region)
                                             ""
                                           (if suffix
                                               suffix
                                             dotfiles-org-linkify-suffix))
                                         "][")
                                        "]]"))))

(use-package pdf-tools
  :config
  (pdf-tools-install t t))

(use-package rainbow-delimiters
  :pin melpa-stable
  :config
  (add-hook 'text-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package recentf
  :config
  (setq recentf-max-saved-items 100)
  (recentf-mode 1)
  (add-to-list 'recentf-exclude
               (lambda (f)
                 (or
                  (s-starts-with? (expand-file-name user-emacs-directory)
                                  (expand-file-name f))
                  (string= (expand-file-name f)
                           (expand-file-name "~/.ido.last"))))))

(use-package savehist
  :config
  (setq savehist-additional-variables '(search ring regexp-search-ring))
  (savehist-mode 1))

(use-package saveplace
  :config
  (setq save-place-file (expand-file-name ".places" user-emacs-directory))
  (setq-default save-place t))

(use-package smartparens
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1)
  (setq sp-autoescape-string-quote nil))

(use-package solarized-theme
  :config
  (defvar dotfiles-current-theme (if dotfiles-on-android 'solarized-dark 'solarized-light))
  (defun dotfiles-toggle-theme ()
    "Toggle between light and dark themes."
    (interactive)
    (let ((new-theme
           (if (eq dotfiles-current-theme 'solarized-dark)
               'solarized-light
             'solarized-dark)))
      (setq dotfiles-current-theme new-theme)
      (load-theme new-theme t)))
  (load-theme dotfiles-current-theme t)
  (bind-keys
   :map dotfiles-buffer-map
   ("-" . dotfiles-toggle-theme)))

(use-package smart-mode-line
  :config
  (setq
   sml/theme 'automatic
   sml/no-confirm-load-theme t
   sml/name-width '(16 . 44)
   sml/col-number-format "%c"
   sml/line-number-format "%l")
  (sml/setup))

(use-package switch-window
  :bind
  ([remap other-window] . switch-window))

(use-package syntax-subword
  :config
  (global-syntax-subword-mode))

(use-package transpose-frame
  :config
  (bind-keys
   :map dotfiles-buffer-map
   ("<up>" . flip-frame)
   ("<down>" . flip-frame)
   ("<left>" . flop-frame)
   ("<right>" . flop-frame)
   ("t" . transpose-frame)))

(use-package undo-tree
  :diminish undo-tree-mode
  :bind
  (:map undo-tree-map
        ("C-/" . nil)))

(progn
  (require 'uniquify)
  (setq
   uniquify-buffer-name-style 'forward
   uniquify-separator "/"
   uniquify-after-kill-buffer-p t
   uniquify-ignore-buffers-re "^\\*"))

(progn
  (global-visual-line-mode 1)
  (diminish 'visual-line-mode))

(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

(use-package webjump
  :config
  (setq
   webjump-sites '(("google" . [simple-query
                                "https://www.google.co.uk/"
                                "https://www.google.co.uk/search?q="
                                ""])
                   ("maps" . [simple-query
                              "https://maps.google.co.uk/"
                              "https://maps.google.co.uk/maps?q="
                              ""])
                   ("scholar" . [simple-query
                                 "https://scholar.google.co.uk/"
                                 "http://scholar.google.co.uk/scholar?q="
                                 ""])))
  (bind-keys
   :map dotfiles-open-map
   ("w" . webjump)))

(use-package winner
  :config
  (bind-keys
   :map dotfiles-buffer-map
   ("z" . winner-undo)
   ("y" . winner-redo))
  (winner-mode 1))


(provide 'dotfiles-ui)

;;; dotfiles-ui.el ends here
