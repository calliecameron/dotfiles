;;; dotfiles.el --- Main dotfiles file.        -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Callie Cameron

;; Author: Callie Cameron <cjcameron7@gmail.com>

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

;; Main dotfiles file.

;;; Code:

;; Bootstrap packages

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)

(use-package alert)
(use-package dash :config (dash-enable-font-lock))
(use-package diminish)
(use-package epl)
(use-package f)
(use-package persistent-soft)
(use-package s)


(defvar dotfiles-ignored-packages nil
  "Packages which should not be installed on this machine.  Note that this is for package.el packages, not dotfiles packages.")

(defmacro dotfiles-use-package (name &rest args)
  "Wrapper for use-package which only installs packages not in dotfiles-ignored-packages."
  (declare (indent 1))
  `(unless (-contains? dotfiles-ignored-packages ',name)
     (use-package ,name ,@args)))

(put 'dotfiles-use-package 'lisp-indent-function 'defun)

(defconst dotfiles--use-package-font-lock-keywords
  '(("(\\(dotfiles-use-package\\)\\_>[ \t']*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-constant-face nil t))))

(font-lock-add-keywords 'emacs-lisp-mode dotfiles--use-package-font-lock-keywords)


;; Environment variables
(defmacro dotfiles--env-vars (var-names)
  "Import environment variables."
  (let ((real-var-names (eval var-names)))
    (cons
     'progn
     (-map
      (lambda (var-name)
        `(let* ((os-name (s-replace "-" "_" (s-upcase (symbol-name ',var-name))))
                (val (getenv os-name)))
           (if val
               (defconst ,var-name val)
             (error "Environment variable %s is not set" os-name))))
      real-var-names))))

(defmacro dotfiles--file-openers (var-names)
  "Create functions which open the files in the given variables."
  (let ((real-var-names (eval var-names)))
    (cons
     'progn
     (-map
      (lambda (var-name)
        (let ((docstring (format "Open %s." (eval var-name))))
          `(defun ,var-name ()
             ,docstring
             (interactive)
             (find-file ,var-name))))
      real-var-names))))


(dotfiles--env-vars
 '(dotfiles-dir
   dotfiles-os
   dotfiles-linux-variant
   dotfiles-core-dir
   dotfiles-variables
   dotfiles-generic-aliases
   dotfiles-bash-aliases
   dotfiles-zsh-aliases
   dotfiles-emacs-init
   dotfiles-local-variables
   dotfiles-local-aliases
   dotfiles-local-emacs
   dotfiles-package-scripts
   dotfiles-package-install-dir
   dotfiles-package-ignore-file
   dotfiles-private-dir
   dotfiles-package-roots))

(setq custom-file (f-full (f-join user-emacs-directory "emacs-custom.el")))

(define-prefix-command 'dotfiles-buffer-map)
(define-prefix-command 'dotfiles-open-map)
(define-prefix-command 'dotfiles-open-files-map)
(define-prefix-command 'dotfiles-open-local-files-map)


;; Package stuff
(defun dotfiles--generic-count-missing-packages (installed-fn package-list)
  "Count how many packages are not installed."
  (-reduce '+
           (-map
            (lambda (package) (if (funcall installed-fn package) 0 1))
            package-list)))

(defun dotfiles--generic-install-missing-packages (name installed-fn install-fn package-list)
  "Install any missing packages."
  (-each
      package-list
    (lambda (package)
      (when (not (funcall installed-fn package))
        (message "Installing %s '%s'..." name package)
        (funcall install-fn package))))
  (unless noninteractive
    (dotfiles--generic-packages-notify-callback
     (format "All required %ss installed. Please restart Emacs." name)
     "Save and Exit"
     'save-buffers-kill-emacs)))

(defun dotfiles--generic-packages-notify-callback (message callback-text callback &optional title extra)
  "Display a notification MESSAGE with a callback action.
The message is displayed in the echo area, and using the system's
notification mechanism.  The system notification has a button reading
CALLBACK-TEXT.  The function CALLBACK is called when the button is
clicked.  TITLE is the system notification title; defaults to
\"Emacs\".  If EXTRA is not nil, it is appended to the echo area
message."
  (message "%s%s" message (if extra extra ""))

  (notifications-notify
   :title (if title title "Emacs")
   :body message
   :urgency 'normal
   :actions (list callback-text callback-text)
   :on-action (lambda (_a _b) (funcall callback))))

(defun dotfiles--generic-require-packages (name installed-fn install-fn package-list mx-name)
  "Make sure the packages specified in PACKAGE-LIST are installed; works for any notion of package using INSTALLED-FN and INSTALL-FN."
  (if noninteractive
      (dotfiles--generic-install-missing-packages name installed-fn install-fn package-list)
    (let ((num-missing (dotfiles--generic-count-missing-packages installed-fn package-list)))
      (when (> num-missing 0)
        (dotfiles--generic-packages-notify-callback
         (format "%d required %s%s missing." num-missing name (if (= num-missing 1) " is" "s are"))
         "Install"
         (lambda () (dotfiles--generic-install-missing-packages name installed-fn install-fn package-list))
         "Emacs"
         (format " Please run M-x %s." mx-name))))))


;; Local stuff; loaded early, so it can influence later behaviour.
(load dotfiles-local-emacs t)


;; Load dotfiles packages
(load (f-join dotfiles-package-scripts "load-packages.el"))


;; Load stuff saved through the 'customise' system
(load custom-file t)


;; Updates
(advice-add 'package-list-packages :after
            (lambda (&rest _args)
              (persistent-soft-store 'last-update-time (current-time) "dotfiles")))

(let* ((last-update-time (persistent-soft-fetch 'last-update-time "dotfiles"))
       (update-now (or (not last-update-time) (time-less-p
                                               (time-add last-update-time (days-to-time 28))
                                               (current-time)))))
  (when update-now
    (dotfiles--generic-packages-notify-callback
     "It's been a while since packages were checked for updates."
      "Check now"
      'package-list-packages
      "Emacs"
      " Please run M-x package-list-packages.")))


(provide 'dotfiles)
;;; dotfiles.el ends here
