(defun dotfiles--package-ignored (package)
  (eq (call-process "dotfiles-package-ignored" nil nil nil package) 0))

(defun dotfiles--package-installed (package)
  (eq (call-process "dotfiles-package-installed" nil nil nil package) 0))

(defun dotfiles--package-has-installer (package-source-dir)
  (eq (call-process "dotfiles-package-has-installer" nil nil nil package-source-dir) 0))

(defun dotfiles--load-packages (this-package-root)
  (when (f-directory? this-package-root)
    (-each
        (-sort 'string< (f-directories this-package-root))
      (lambda (this-package-source-dir)
        (let*
            ((this-package-name (f-filename this-package-source-dir))
             (this-package-install-dir (f-join dotfiles-package-install-dir this-package-name))
             (this-package-elisp-dir (f-join this-package-source-dir "elisp"))
             (original-working-dir default-directory))
          (unless (dotfiles--package-ignored this-package-name)
            (when (or
                   (dotfiles--package-installed this-package-name)
                   (not (dotfiles--package-has-installer this-package-source-dir)))
              (when (f-directory? this-package-elisp-dir)
                (add-to-list 'load-path this-package-elisp-dir))
              (cd this-package-source-dir)
              (load (f-join this-package-source-dir "emacs") t)
              (cd original-working-dir))))))))

(defconst dotfiles--package-roots-list (s-split ":" dotfiles-package-roots))

(mapc 'dotfiles--load-packages dotfiles--package-roots-list)
