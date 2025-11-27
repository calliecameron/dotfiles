;; -*- lexical-binding: t; -*-

(defmacro dotfiles--env-vars (var-names)
  "Import environment variables."
  (let ((real-var-names (eval var-names)))
    (cons
     'progn
     (mapcar
      (lambda (var-name)
        `(let* ((os-name (string-replace "-" "_" (upcase (symbol-name ',var-name))))
                (val (getenv os-name)))
           (if val
               (defconst ,var-name val)
             (error "Environment variable %s is not set" os-name))))
      real-var-names))))

(dotfiles--env-vars
 '(dotfiles-dir
   dotfiles-linux-variant
   dotfiles-core-dir
   dotfiles-variables
   dotfiles-generic-aliases
   dotfiles-bash-aliases
   dotfiles-zsh-aliases
   dotfiles-emacs-init
   dotfiles-emacs-lock
   dotfiles-local-variables
   dotfiles-local-aliases
   dotfiles-local-emacs
   dotfiles-package-scripts
   dotfiles-package-install-dir
   dotfiles-private-dir
   dotfiles-package-roots))

(defmacro dotfiles--file-openers (var-names)
  "Create functions which open the files in the given variables."
  (let ((real-var-names (eval var-names)))
    (cons
     'progn
     (mapcar
      (lambda (var-name)
        (let ((docstring (format "Open %s." (eval var-name))))
          `(defun ,var-name ()
             ,docstring
             (interactive)
             (find-file ,var-name))))
      real-var-names))))

(define-prefix-command 'dotfiles-buffer-map)
(define-prefix-command 'dotfiles-open-map)
(define-prefix-command 'dotfiles-open-files-map)
(define-prefix-command 'dotfiles-open-local-files-map)

(setq custom-file (expand-file-name "emacs-custom.el" user-emacs-directory))

;; Elpaca installer; https://github.com/progfolio/elpaca
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
;; End elpaca installer

(setq elpaca-lock-file (expand-file-name "elpaca-lock.el" user-emacs-directory))
(unless
    (eq (call-process "dotfiles-home-link" nil nil nil dotfiles-emacs-lock elpaca-lock-file) 0)
  (error "Failed to link elpaca lockfile"))

(add-hook
 'elpaca-after-init-hook
 (lambda ()
   (elpaca-write-lock-file elpaca-lock-file)))

(setq use-package-always-ensure t)

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

(use-package dash :config (dash-enable-font-lock))
(use-package s)
(use-package f)
(use-package diminish)
(elpaca-wait)

;; Load local stuff early, so it can influence later behaviour
(load dotfiles-local-emacs t)

;; Load dotfiles packages
(load (expand-file-name "load-packages.el" dotfiles-package-scripts))

;; Load stuff from the 'customise' system
(elpaca-wait)
(load custom-file t)

(provide 'dotfiles)
