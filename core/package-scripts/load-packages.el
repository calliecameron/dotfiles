(defun dotfiles--load-packages (package-conf-root)
  (when (f-directory? package-conf-root)
    (-each
        (-sort 'string< (f-directories package-conf-root))
      (lambda (this-package-conf-dir)
        (let*
            ((this-package-name (f-filename this-package-conf-dir))
             (this-package-install-dir (f-join dotfiles-package-install-dir this-package-name))
             (this-package-installed-file (f-join dotfiles-package-install-dir (concat this-package-name ".installed")))
             (this-package-elisp-dir (f-join this-package-conf-dir "elisp"))
             (original-working-dir default-directory))
          (when (or
                 (f-exists? this-package-installed-file)
                 (not (f-exists? (f-join this-package-conf-dir "setup.bash"))))
            (when (f-directory? this-package-elisp-dir)
              (add-to-list 'load-path this-package-elisp-dir))
            (cd this-package-conf-dir)
            (load (f-join this-package-conf-dir "emacs") t)
            (cd original-working-dir)))))))
