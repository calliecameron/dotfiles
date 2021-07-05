(dotfiles-use-package elpy
  :diminish elpy-mode
  :config
  (bind-keys
   :map elpy-mode-map
   ("M-<left>" . nil)
   ("M-<right>" . nil)
   ("M-<up>" . nil)
   ("M-<down>" . nil))
  (when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode))
  (setq elpy-rpc-backend "jedi")

  (defun company-yasnippet-or-completion ()
    "Solve company yasnippet conflicts."
    (interactive)
    (let ((yas-fallback-behavior
           (apply 'company-complete-common nil)))
      (yas-expand)))

  (add-hook 'company-mode-hook
            (lambda ()
              (substitute-key-definition
               'company-complete-common
               'company-yasnippet-or-completion
               company-active-map)))

  (defun dotfiles--venv-setup (command)
    (setq elpy-rpc-python-command command)
    (when (eq major-mode 'python-mode)
      (pyvenv-restart-python)))

  (add-hook 'pyvenv-post-activate-hooks (lambda () (dotfiles--venv-setup "python")))
  (add-hook 'pyvenv-post-deactivate-hooks (lambda () (dotfiles--venv-setup "python3")))
  (dotfiles--venv-setup "python3")
  (elpy-enable)
  (pyvenv-tracking-mode)

  (defun dotfiles-pyvenv-dir-locals (workon)
    (interactive "MWork on virtualenv: ")
    (add-dir-local-variable 'python-mode 'pyvenv-workon workon)))

(dotfiles-use-package flycheck-pycheckers
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-pycheckers-setup)
  (setq flycheck-pycheckers-checkers '(pylint mypy3))
  (setq flycheck-pycheckers-venv-root (getenv "WORKON_HOME"))
  ; Fallback to .pylintrc rather than using pycheckers' builtin config
  (setq flycheck-pycheckers-ignore-codes nil)
  (setq flycheck-pycheckers-enable-codes nil))

(dotfiles-use-package python
  :mode (("SConscript\\'" . python-mode)
         ("SConstruct\\'" . python-mode)))
