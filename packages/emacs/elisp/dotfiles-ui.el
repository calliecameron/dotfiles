;;; dotfiles-ui.el --- -*- lexical-binding: t -*- user interface settings

;; Copyright (C) 2023  Callie Cameron

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

;; User interface settings.

;;; Code:

(setq
 backup-directory-alist nil
 case-fold-search t
 face-font-selection-order '(:width :height :weight :slant)
 inhibit-startup-screen t
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
 x-alt-keysym 'meta
 x-gtk-use-system-tooltips nil) ; due to hidpi scaling bugs

(setq-default
 major-mode 'text-mode
 indent-tabs-mode nil
 tab-width 4)

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'display-buffer-alist
             '("*" . (display-buffer-reuse-window
                      . ((reusable-frames . visible)))))

(defun dotfiles--repo-is-clean (dir)
  (eq (call-process "dotfiles-repo-is-clean" nil nil nil dir) 0))

(defun dotfiles--repo-blocks-quit (arg)
  (let ((dir (if (consp arg) (car arg) arg))
        (name (if (consp arg) (cdr arg) nil)))
    (if (dotfiles--repo-is-clean dir)
        nil
      (not
       (y-or-n-p
        (format
         "%s has uncommitted changes; quit anyway? "
         (if name name dir)))))))

(defun dotfiles--should-quit ()
  (-none?
   'dotfiles--repo-blocks-quit
   `((,dotfiles-dir . "Dotfiles")
     (,dotfiles-private-dir . "Dotfiles private")
     ,@dotfiles--package-roots-list)))

(add-to-list 'kill-emacs-query-functions 'dotfiles--should-quit)

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

(defun dotfiles-text-scale-reset ()
  "Reset text scale."
  (interactive)
  (text-scale-set 0))

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


(dotfiles-use-package wakib-keys
  :diminish wakib-keys
  :config
  (wakib-keys)

  (defun dotfiles--startup-buffer ()
    (let ((buf (get-buffer "untitled")))
      (if buf
          buf
        (wakib-new-empty-buffer)
        (get-buffer "untitled"))))
  (setq initial-buffer-choice 'dotfiles--startup-buffer)

  (bind-keys
   :map wakib-keys-overriding-map
   ("C-q" . save-buffers-kill-emacs)
   ("C-<prior>" . dotfiles-smart-scroll-left)
   ("C-<next>" . dotfiles-smart-scroll-right)
   ("C-;" . nil)
   ("M-:" . eval-expression)
   ("M-#" . nil)
   ("M-2" . delete-window)
   ("C-S-o" . dotfiles-open-map)))

(bind-keys
 ("<mouse-6>" . dotfiles-smart-scroll-left)
 ("<mouse-7>" . dotfiles-smart-scroll-right)
 ("C-x <" . dotfiles-smart-scroll-left)
 ("C-x >" . dotfiles-smart-scroll-right)
 ("C-0" . dotfiles-text-scale-reset)
 ("s-c" . copy-to-register)
 ("C-S-c" . copy-to-register)
 ("s-x" . dotfiles-cut-to-register)
 ("C-S-x" . dotfiles-cut-to-register)
 ("s-v" . insert-register)
 ("s-u" . package-list-packages)
 ("C-S-u" . package-list-packages)
 ("s-k" . describe-personal-keybindings)
 ("C-S-k" . describe-personal-keybindings)
 ("<s-down>" . dotfiles-split-window-below-switch)
 ("<s-right>" . dotfiles-split-window-right-switch)
 ("<s-up>" . (lambda () (interactive) (split-window-below)))
 ("<s-left>" . (lambda () (interactive) (split-window-right)))
 ("<C-M-S-down>" . dotfiles-split-window-below-switch)
 ("<C-M-S-right>" . dotfiles-split-window-right-switch)
 ("<C-M-S-up>" . (lambda () (interactive) (split-window-below)))
 ("<C-M-S-left>" . (lambda () (interactive) (split-window-right)))
 ("<M-left>" . windmove-left)
 ("<M-right>" . windmove-right)
 ("<M-up>" . windmove-up)
 ("<M-down>" . windmove-down)
 ("C-c <left>" . windmove-left)
 ("C-c <right>" . windmove-right)
 ("C-c <up>" . windmove-up)
 ("C-c <down>" . windmove-down)
 ("M-m" . newline)
 ("<f12>" . toggle-frame-fullscreen)
 ("s-m" . dotfiles-toggle-mode-line)
 ("s-\\" . dotfiles-buffer-map)
 ("C-|" . dotfiles-buffer-map)
 ("s-o" . dotfiles-open-map)
 ("C-l" . goto-line)
 ([remap goto-line] . dotfiles-goto-line-with-feedback)
 ([remap move-beginning-of-line] . dotfiles-smart-home-key)
 ([remap move-end-of-line] . dotfiles-smart-end-key)
 ([remap split-window-below] . dotfiles-split-window-below-switch)
 ([remap split-window-right] . dotfiles-split-window-right-switch))

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
 ("v" . insert-register)
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

(dotfiles-use-package ace-jump-mode
  :bind
  (("s-j" . ace-jump-mode)
   ("C-S-j" . ace-jump-mode)))

(dotfiles-use-package alert
  :config
  (setq alert-default-style 'notifications))

(dotfiles-use-package auto-dark
  :config
  (setq custom-safe-themes t)
  (setq auto-dark-themes '((solarized-dark) (solarized-light)))
  (auto-dark-mode))

(dotfiles-use-package autorevert
  :config
  (setq auto-revert-verbose nil)
  (bind-keys
   :map dotfiles-buffer-map
   ("a" . auto-revert-mode))
  (add-hook 'dired-mode-hook 'auto-revert-mode)
  (add-hook 'doc-view-mode-hook 'auto-revert-mode))

(dotfiles-use-package back-button
  :diminish back-button-mode
  :bind
  ("<mouse-8>" . back-button-global-backward)
  ("<mouse-9>" . back-button-global-forward)
  ("<C-mouse-8>" . back-button-local-backward)
  ("<C-mouse-9>" . back-button-local-forward)
  :config
  (back-button-mode))

(dotfiles-use-package bm
  :bind
  (("<left-fringe> <mouse-5>" . bm-next-mouse)
   ("<left-fringe> <mouse-4>" . bm-previous-mouse)
   ("<left-fringe> <mouse-1>" . bm-toggle-mouse)
   ("C-#" . bm-toggle))
  :config
  (bind-keys
   :map wakib-keys-overriding-map
   ("M-#" . bm-remove-all-current-buffer)))

(dotfiles-use-package connection)

(dotfiles-use-package link)

(dotfiles-use-package doc-view
  :config
  (setq doc-view-continuous t))

(dotfiles-use-package ediff
  :config
  (setq  ediff-window-setup-function 'ediff-setup-windows-plain))

(dotfiles-use-package flyspell
  :diminish flyspell-mode
  :config
  (setq
   ispell-dictionary "en_GB"
   flyspell-issue-message-flag nil
   flyspell-large-region 1)
  (bind-keys
   ("C->" . flyspell-goto-next-error))
  (bind-keys
   :map flyspell-mode-map
   ("C-;" . nil)
   ("C-," . nil)
   ("C-." . nil))
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

(dotfiles-use-package guide-key
  :diminish guide-key-mode
  :config
  (setq
   guide-key/guide-key-sequence '("C-e" "C-d" "C-h" "s-o" "C-S-o" "s-\\" "C-|" "s-p" "C-S-p")
   guide-key/recursive-key-sequence-flag t)
  (guide-key-mode 1))

(dotfiles-use-package helm
  :diminish helm-mode
  :bind
  (("C-\\" . helm-mini))
  :config
  (helm-mode 1)
  (helm-adaptive-mode 1)
  (helm-popup-tip-mode 1)
  (helm-top-poll-mode 1))

(dotfiles-use-package helm-bm
  :bind
  ("C-'" . helm-bm))

(dotfiles-use-package helm-flyspell
  :config
  (bind-keys
   ("C-<" . helm-flyspell-correct)))

(dotfiles-use-package ace-isearch
  :diminish ace-isearch-mode
  :config
  (setq ace-isearch-input-length 2)
  (global-ace-isearch-mode))

(dotfiles-use-package highlight-indentation
  :diminish highlight-indentation-mode)

(dotfiles-use-package hl-line
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

  (bind-keys
   ("M-?" . man)))

(dotfiles-use-package mode-icons
  :config
  (mode-icons-mode))

(progn
  (require 'org)
  (setq
   org-log-done nil
   org-return-follows-link t
   org-link-abbrev-alist '(("search" . "https://www.google.co.uk/#q=%h")
                           ("map" . "https://maps.google.co.uk/maps?q=%h")
                           ("scholar" . "http://scholar.google.co.uk/scholar?q=%h")))

  (setcdr (assq 'file org-link-frame-setup) 'find-file)

  (bind-keys
   :map org-mode-map
   ("C-t" . org-todo))

  (add-hook 'org-mode-hook
            (lambda ()
              (setq org-support-shift-select 'always)
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

(dotfiles-use-package rainbow-delimiters
  :config
  (add-hook 'text-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(dotfiles-use-package recentf
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

(dotfiles-use-package savehist
  :config
  (setq savehist-additional-variables '(search ring regexp-search-ring))
  (savehist-mode 1))

(dotfiles-use-package saveplace
  :config
  (setq save-place-file (expand-file-name ".places" user-emacs-directory))
  (save-place-mode))

(dotfiles-use-package smartparens
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1)
  (setq sp-autoescape-string-quote nil))

(dotfiles-use-package solarized-theme)

(dotfiles-use-package smart-mode-line
  :config
  (setq
   sml/theme 'automatic
   sml/no-confirm-load-theme t
   sml/name-width '(16 . 44)
   sml/col-number-format "%c"
   sml/line-number-format "%l"
   sml/show-eol t)
  (sml/setup))

(dotfiles-use-package switch-window
   :bind
   ([remap other-window] . switch-window))

(dotfiles-use-package syntax-subword
  :config
  (global-syntax-subword-mode))

(dotfiles-use-package transpose-frame
  :config
  (bind-keys
   :map dotfiles-buffer-map
   ("<up>" . flip-frame)
   ("<down>" . flip-frame)
   ("<left>" . flop-frame)
   ("<right>" . flop-frame)
   ("t" . transpose-frame)))

(dotfiles-use-package undo-tree
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

(dotfiles-use-package volatile-highlights
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

(dotfiles-use-package winner
  :config
  (bind-keys
   :map dotfiles-buffer-map
   ("z" . winner-undo)
   ("y" . winner-redo))
  (winner-mode 1))


(provide 'dotfiles-ui)

;;; dotfiles-ui.el ends here
