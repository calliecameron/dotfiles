;; -*- lexical-binding: t; -*-

(defconst dotfiles--term-max-lines 20000)

(defmacro dotfiles--term-setup
    (name
     mode
     buffer-name
     create-buffer
     hook char-mode
     in-char-mode
     line-mode
     self-input
     paste
     send-next-key)
  (let ((new-fn (intern (format "%s-new" name)))
        (next-fn (intern (format "%s-next" name)))
        (prev-fn (intern (format "%s-prev" name)))
        (toggle-sub-mode-fn (intern (format "%s-toggle-sub-mode" name)))
        (send-next-key-fn (intern (format "%s-send-next-key" name)))
        (interactive-command-var (intern (format "%s-interactive-command" name)))
        (interactive-command-fn (intern (format "%s-do-interactive-command" name)))
        (override-var (intern (format "%s-override" name)))
        (override-map (intern (format "%s-override-map" name))))
    `(progn
       (defun ,new-fn ()
         "Create a new term buffer."
         (interactive)
         (let ((prog (getenv "SHELL"))
               (term-num 1))
           (while (get-buffer (,buffer-name term-num))
             (setq term-num (1+ term-num)))
           (,create-buffer prog term-num)))

       (defun ,next-fn (&optional prev)
         "Switch to the next term buffer, or create one if none."
         (interactive)
         (let ((bufs
                (->> (buffer-list)
                     (-filter (lambda (b)
                                (eq (buffer-local-value 'major-mode b) ',mode)))
                     (-map 'buffer-name)
                     (-sort (if prev 'string> 'string<)))))
           (if bufs
               (switch-to-buffer
                (get-buffer
                 (if (eq (length bufs) 1)
                     (car bufs)
                   (-first
                    (lambda (b) (not (string= b (buffer-name))))
                    (-rotate
                     (-
                      (let ((i (-elem-index (buffer-name) bufs)))
                        (if i
                            i
                          0)))
                     bufs)))))
             (,new-fn))))

       (defun ,prev-fn ()
         "Switch to the previous term buffer, or create one if none."
         (interactive)
         (,next-fn t))

       (advice-add 'save-buffers-kill-emacs :before-while
                   (lambda (&rest args)
                     (let ((buf (-first
                                 (lambda (b)
                                   (eq (buffer-local-value 'major-mode b)
                                       ',mode))
                                 (buffer-list))))
                       (if buf
                           (progn
                             (switch-to-buffer buf)
                             (message "Terminal buffers exist; close them first.")
                             nil)
                         t))))

       (add-hook ',hook
                 (lambda ()
                   (visual-line-mode -1)))

       (advice-add ',char-mode :after
                   (lambda (&rest args)
                     (hl-line-mode -1)))

       (advice-add ',line-mode :after
                   (lambda (&rest args)
                     (hl-line-mode)))

       (defun ,toggle-sub-mode-fn ()
         "Toggle between line and char mode."
         (interactive)
         (if ,in-char-mode
             (progn
               (setq ,override-var nil)
               (,line-mode))
           (,char-mode)
           (setq ,override-var t)))

       (defun ,send-next-key-fn ()
         "Send the next key to the terminal.  Use this to send keys that Emacs would usually capture, e.g. to send C-x to the terminal, use M-x dotfiles-term-send-next-key RET C-x."
         (interactive)
         (unless ,in-char-mode
           (,char-mode))
         (,send-next-key))

       (defvar ,interactive-command-var nil)
       (make-variable-buffer-local ',interactive-command-var)

       (defun ,interactive-command-fn ()
         (interactive)
         (if ,interactive-command-var
             (call-interactively ,interactive-command-var)
           (message "Interactive command not set")))

       ;; Override global keys set by wakib-keys
       (defvar ,override-var nil)
       (make-variable-buffer-local ',override-var)
       (add-hook ',hook (lambda () (setq ,override-var t)))
       (defvar ,override-map (make-sparse-keymap))
       (add-to-list 'emulation-mode-map-alists
                    (list (cons ',override-var ,override-map)))

       (bind-keys
        :map ,override-map
        ("C-d" . ,self-input)
        ("C-z" . ,self-input)
        ("C-c" . ,self-input)
        ("C-x" . ,self-input)
        ("C-v" . ,paste)
        ("C-r" . ,self-input)
        ("C-s" . ,self-input)
        ("C-f" . ,self-input)
        ("C-o" . ,self-input)
        ("C-q" . ,send-next-key-fn)))))


(use-package eat
  :config
  (setq
   eat-buffer-name "*eat-term*"
   eat-term-scrollback-size (* dotfiles--term-max-lines 100)
   eat-show-title-on-mode-line nil)

  (dotfiles--term-setup
   dotfiles-eat-term
   eat-mode
   (lambda (term-num) (format "%s<%d>" eat-buffer-name term-num))
   (lambda (prog term-num) (eat prog term-num))
   eat-mode-hook
   eat-semi-char-mode
   eat--semi-char-mode
   eat-emacs-mode
   eat-self-input
   eat-yank
   eat-quoted-input)

  (add-hook 'eat-exit-hook
            (lambda (proc)
              (kill-buffer (process-buffer proc))))

  (defun dotfiles--eat-term-update-mode-line (&rest args)
    (setq
     mode-line-process
     (if (not eat--semi-char-mode)
         (list
          (propertize
           "l"
           'help-echo "mouse-1: Switch to char mode"
           'mouse-face 'mode-line-highlight
           'local-map
           '(keymap
             (mode-line keymap (down-mouse-1 . eat-semi-char-mode)))))
       nil))
    (force-mode-line-update))

  (advice-add 'eat-semi-char-mode :after
              'dotfiles--eat-term-update-mode-line)

  (advice-add 'eat-emacs-mode :after
              'dotfiles--eat-term-update-mode-line)

  (bind-keys
   :map eat-mode-map
   ("C-;" . dotfiles-eat-term-toggle-sub-mode)
   ("s-i" . dotfiles-eat-term-do-interactive-command)
   ("C-S-i" . dotfiles-eat-term-do-interactive-command))

  (bind-keys
   ("<C-f9>" . dotfiles-eat-term-new)
   ("<C-f10>" . dotfiles-eat-term-prev)
   ("<C-f11>" . dotfiles-eat-term-next)
   ("M-{" . dotfiles-eat-term-prev)
   ("M-}" . dotfiles-eat-term-next)))


(use-package term
  :ensure nil
  :config
  (setq
   term-buffer-maximum-size dotfiles--term-max-lines
   term-scroll-to-bottom-on-output t)

  (defun dotfiles--term-name (n)
    (format "terminal<%d>" n))

  (dotfiles--term-setup
   dotfiles-term
   term-mode
   (lambda (term-num) (format "*%s*" (dotfiles--term-name term-num)))
   (lambda (prog term-num) (ansi-term prog (dotfiles--term-name term-num)))
   term-mode-hook
   term-char-mode
   (term-in-char-mode)
   term-line-mode
   term-send-raw
   term-paste
   (lambda () (let ((key (read-key "Send key to terminal: ")))
                (term-send-raw-string (string key)))))

  (advice-add 'ansi-term :after
              (lambda (&rest args)
                (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)
                (set-process-sentinel
                 (get-buffer-process (current-buffer))
                 (lambda (proc change)
                   (when (string-match "\\(finished\\|exited\\)" change)
                     (kill-buffer (process-buffer proc)))))))

  (advice-add 'term-update-mode-line :after
              (lambda (&rest args)
                (setq
                 mode-line-process
                 (if (term-in-line-mode)
                     (list
                      (propertize
                       "l"
                       'help-echo "mouse-1: Switch to char mode"
                       'mouse-face 'mode-line-highlight
                       'local-map
                       '(keymap
                         (mode-line keymap (down-mouse-1 . term-char-mode)))))
                   nil))
                (force-mode-line-update)))

  (bind-keys
   :map term-mode-map
   ("C-;" . dotfiles-term-toggle-sub-mode)
   ("s-i" . dotfiles-term-do-interactive-command)
   ("C-S-i" . dotfiles-term-do-interactive-command))

  (bind-keys
   :map term-raw-map
   ("C-;" . dotfiles-term-toggle-sub-mode)
   ("s-i" . dotfiles-term-do-interactive-command)
   ("C-S-i" . dotfiles-term-do-interactive-command)
   ("C-h" . nil)
   ("C-w" . nil)
   ("C-\\" . nil)
   ("M-:" . nil)
   ("M-x" . nil)
   ("M-2" . nil)
   ("M-3" . nil))

  (bind-keys
   ("<f9>" . dotfiles-term-new)
   ("<f10>" . dotfiles-term-prev)
   ("<f11>" . dotfiles-term-next)
   ("C-}" . dotfiles-term-prev)
   ("C-{" . dotfiles-term-next)))


(use-package term-cmd
  :after term
  :config
  (defvar dotfiles--term-in-pager nil "Whether a term is currently in the pager.")
  (make-variable-buffer-local 'dotfiles--term-in-pager)

  (defun dotfiles--term-pager-on (c a)
    (setq dotfiles--term-in-pager t))

  (defun dotfiles--term-pager-off (c a)
    (setq dotfiles--term-in-pager nil))

  (add-to-list 'term-cmd-commands-alist '("term-pager-on" . dotfiles--term-pager-on))
  (add-to-list 'term-cmd-commands-alist '("term-pager-off" . dotfiles--term-pager-off))

  (defun dotfiles-term-mwheel-scroll-down (&optional arg)
    "Respond to a mouse wheel scroll down."
    (interactive)
    (if (and dotfiles--term-in-pager (term-in-char-mode))
        (progn
          (term-send-up)
          (term-send-up)
          (term-send-up)
          (term-send-up)
          (term-send-up))
      (scroll-down arg)))

  (defun dotfiles-term-mwheel-scroll-up (&optional arg)
    "Respond to a mouse wheel scroll up."
    (interactive)
    (if (and dotfiles--term-in-pager (term-in-char-mode))
        (progn
          (term-send-down)
          (term-send-down)
          (term-send-down)
          (term-send-down)
          (term-send-down))
      (scroll-up arg)))

  (add-hook 'term-mode-hook
            (lambda ()
              (make-local-variable 'mwheel-scroll-down-function)
              (make-local-variable 'mwheel-scroll-up-function)
              (setq mwheel-scroll-down-function 'dotfiles-term-mwheel-scroll-down)
              (setq mwheel-scroll-up-function 'dotfiles-term-mwheel-scroll-up)))

  (defun dotfiles--term-inotify-callback (c a)
    "Respond to a completed command."
    (alert
     (format "File '%s' changed in %s" a (buffer-name))
     :title "Emacs"))

  (add-to-list 'term-cmd-commands-alist '("term-inotify" . dotfiles--term-inotify-callback)))


(use-package term-alert
  :after term-cmd eat
  :config
  (bind-keys
   :map term-mode-map
   ("C-#" . term-alert-next-command-toggle)
   ("M-#" . term-alert-all-toggle)
   ("C-'" . term-alert-runtime))

  (bind-keys
   :map term-raw-map
   ("C-#" . term-alert-next-command-toggle)
   ("M-#" . term-alert-all-toggle)
   ("C-'" . term-alert-runtime))

  (bind-keys
   :map eat-mode-map
   ("C-#" . term-alert-next-command-toggle)
   ("M-#" . term-alert-all-toggle)
   ("C-'" . term-alert-runtime))

  (bind-keys
   :map eat-semi-char-mode-map
   ("M-#" . term-alert-all-toggle)))


(use-package term-debug
  :ensure nil
  :after term
  :config
  (setq-default dotfiles-term-interactive-command 'term-debug-dwim))

(provide 'dotfiles-term)
