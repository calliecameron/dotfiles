;;; term-debug.el --- Debugging support for term.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Callum Cameron

;; Author: Callum Cameron
;; Keywords: terminals

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

;; Debugging support for term.el.

;;; Code:

(require 'term)

(define-minor-mode term-debug-mode
  "Dump all output from this terminal's process to a buffer, as a debugging aid."
  nil
  " term-debug"
  nil
  (when term-debug-mode
    (unless (advice-member-p 'term-debug--tracing-func 'term-emulate-terminal)
      (advice-add 'term-emulate-terminal :before #'term-debug--tracing-func))

    (let ((term-buffer (current-buffer))
          (output-buffer (get-buffer-create (term-debug--buffer-name))))

      (with-current-buffer output-buffer
        (term-debug-output-mode)
        (setq term-debug--output-term term-buffer)
        (when (> (buffer-size) 0)
          (if (y-or-n-p "The debug buffer already has content; clear it? ")
              (let ((buffer-read-only nil))
                (delete-region (point-min) (point-max)))
            (term-debug--output-append "\n\n\n")))
        (term-debug--output-append (format "Debugging %s at %s\n" (buffer-name term-buffer) (current-time-string))))

      (display-buffer
       output-buffer
       '((display-buffer-reuse-window display-buffer-pop-up-window) . nil)))))

(defun term-debug-pop-to-debug-buffer ()
  "Switch to this terminal's associated debug buffer."
  (interactive)
  (when term-debug-mode
    (pop-to-buffer (term-debug--buffer-name))))

(defun term-debug-dwim ()
  "Start debugging, or switch to the output buffer if already debugging."
  (interactive)
  (when (eq major-mode 'term-mode)
    (if term-debug-mode
        (term-debug-pop-to-debug-buffer)
      (term-debug-mode))))

(defvar term-debug--output-term nil "The terminal buffer associated with an output buffer.")
(make-variable-buffer-local 'term-debug--output-term)

(define-derived-mode term-debug-output-mode fundamental-mode "Term-Debug-Output"
  "Major mode used in term debug output buffers."
  (setq buffer-read-only t))

(define-key term-debug-output-mode-map (kbd "s-i") 'term-debug-output-pop-to-term)
(define-key term-debug-output-mode-map (kbd "C-S-i") 'term-debug-output-pop-to-term)

(defun term-debug-output-pop-to-term ()
  "Switch to this debug buffer's associated terminal."
  (interactive)
  (when (and (eq major-mode 'term-debug-output-mode)
             term-debug--output-term)
    (if (buffer-live-p term-debug--output-term)
        (pop-to-buffer term-debug--output-term)
      (message "The associated terminal no longer exists."))))

(defun term-debug--buffer-name ()
  "The name of this terminal's debug buffer."
  (concat (buffer-name) " debug"))

(defun term-debug--tracing-func (proc str)
  (when term-debug-mode
    (with-current-buffer (get-buffer-create (term-debug--buffer-name))
      (term-debug--output-append
       (concat
        (propertize " " 'face 'term-debug-output-delimiter)
        str
        (propertize " " 'face 'term-debug-output-delimiter))))))

(defface term-debug-output-separator
  '((default :weight bold)
    (((class color) (min-colors 16) (background light)) :background "Cyan")
    (((class color) (min-colors 88) (background dark))  :background "DeepSkyBlue")
    (((class color) (min-colors 16) (background dark))  :background "Cyan")
    (((class color)) :background "blue"))
  "Used to separate terminal output lines.")

(defface term-debug-output-delimiter
  '((default :weight bold)
    (((class color) (min-colors 16) (background light)) :background "Green")
    (((class color) (min-colors 88) (background dark))  :background "ForestGreen")
    (((class color) (min-colors 16) (background dark))  :background "Green1")
    (((class color)) :background "green"))
  "Used to separate terminal output lines.")

(defun term-debug--output-append (str)
  (let ((old-point-max (point-max)))
    (goto-char old-point-max)
    (let ((buffer-read-only nil))
      (insert (format "%s\n%s" str (propertize "\n" 'face 'term-debug-output-separator))))
    (mapc
     (lambda (window)
       (when (eq (window-point window) old-point-max)
         (set-window-point window (point-max))))
     (get-buffer-window-list (current-buffer) nil t))))


(provide 'term-debug)

;;; term-debug.el ends here
