;; TODO - this is still a bit buggy after upgading emacs and ergoemacs

(use-package term
  :config
  (setq
   term-buffer-maximum-size 20000
   term-scroll-to-bottom-on-output t)

  (advice-add 'term-char-mode :after
              (lambda (&rest args)
                (setq global-hl-line-mode nil)))

  (advice-add 'term-line-mode :after
              (lambda (&rest args)
                (setq global-hl-line-mode t)))

  (advice-add 'save-buffers-kill-emacs :before-while
              (lambda (&rest args)
                (let ((buf (-first
                            (lambda (b)
                              (eq (buffer-local-value 'major-mode b)
                                  'term-mode))
                            (buffer-list))))
                  (if buf
                      (progn
                        (switch-to-buffer buf)
                        (message "Terminal buffers exist; close them first.")
                        nil)
                    t))))

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

  (add-hook 'term-mode-hook
            (lambda ()
              (visual-line-mode -1)))

  (advice-add 'ansi-term :after
              (lambda (&rest args)
                (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)
                (set-process-sentinel
                 (get-buffer-process (current-buffer))
                 (lambda (proc change)
                   (when (string-match "\\(finished\\|exited\\)" change)
                     (kill-buffer (process-buffer proc)))))))

  ;; Used to use multi-term for this, but it's such a pain to
  ;; configure, and it's a wiki package too, that we just do some
  ;; quick setup of our own here instead.
  (defun dotfiles--term-name (n)
    (format "terminal<%d>" n))

  (defun dotfiles-new-term ()
    "Create a new term-mode buffer."
    (interactive)
    (let ((prog (getenv "SHELL"))
          (term-num 1))
      (while (get-buffer (format "*%s*" (dotfiles--term-name term-num)))
        (setq term-num (1+ term-num)))
      (ansi-term prog (dotfiles--term-name term-num))))

  (defun dotfiles-next-term (&optional prev)
    "Switch to the next term-mode buffer, or create one if none."
    (interactive)
    (let ((bufs
           (->> (buffer-list)
                (-filter (lambda (b)
                           (eq (buffer-local-value 'major-mode b) 'term-mode)))
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
        (dotfiles-new-term))))

  (defun dotfiles-prev-term ()
    "Switch to the previous term-mode buffer, or create one if none."
    (interactive)
    (dotfiles-next-term t))

  (defun dotfiles-term-toggle-sub-mode ()
    "Toggle between line and char mode."
    (interactive)
    (if (term-in-char-mode)
        (term-line-mode)
      (term-char-mode)))

  (defun dotfiles-term-send-next-key ()
    "Send the next key to the terminal.  Use this to send keys that Emacs would usually capture, e.g. to send C-x to the terminal, use M-x dotfiles-term-send-next-key RET C-x."
    (interactive)
    (unless (term-in-char-mode)
      (term-char-mode))
    (let ((key (read-key "Send key to terminal: ")))
      (term-send-raw-string (string key))))


  (defvar dotfiles--term-interactive-command nil)
  (make-variable-buffer-local 'dotfiles--term-interactive-command)

  (defun dotfiles-term-do-interactive-command ()
    (interactive)
    (call-interactively dotfiles--term-interactive-command))

  (bind-keys
   :map term-mode-map
   ("C-;" . dotfiles-term-toggle-sub-mode)
   ("C-c C-c" . term-interrupt-subjob)
   ("C-q" . dotfiles-term-send-next-key)
   ("s-i" . dotfiles-term-do-interactive-command)
   ("C-S-i" . dotfiles-term-do-interactive-command))

  (bind-keys
   :map term-raw-map
   ("C-;" . dotfiles-term-toggle-sub-mode)
   ("C-c C-c" . term-interrupt-subjob) ; todo
   ("C-c C-e" . term-send-esc) ; todo
   ;; ("C-r" . term-send-reverse-search-history) ; todo
   ("C-v" . term-paste)
   ;; ("<C-backspace>" . term-send-backward-kill-word) ; todo
   ("C-q" . dotfiles-term-send-next-key)
   ;; ("<C-right>" . term-send-forward-word) ; todo
   ;; ("<C-left>" . term-send-backward-word) ; todo
   ;; ("<C-delete>" . term-send-forward-kill-word) ; todo
   ("s-i" . dotfiles-term-do-interactive-command)
   ("C-S-i" . dotfiles-term-do-interactive-command)
   ("C-z" . term-send-raw)
   ("C-x" . nil)
   ("C-c" . nil)
   ("C-h" . nil)
   ("C-o" . nil)
   ("C-w" . nil)
   ("C-\\" . nil)
   ;; ("M-/" . nil) ; todo
   ;; ("M-:" . nil) ; todo
   ("M-a" . nil))

  (bind-keys
   ("<f9>" . dotfiles-new-term)
   ("<f10>" . dotfiles-prev-term)
   ("<f11>" . dotfiles-next-term)
   ("C-}" . dotfiles-next-term)
   ("C-{" . dotfiles-prev-term)))


(use-package term-cmd
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

  (add-to-list 'term-cmd-commands-alist '("term-inotify" . dotfiles--term-inotify-callback))

  (defvar dotfiles--term-in-ipython nil)
  (make-variable-buffer-local 'dotfiles--term-in-ipython)

  (defvar dotfiles--ipython-popped-from nil)

  (defun dotfiles--term-ipython-start (c a)
    (setq
     dotfiles--term-in-ipython t
     dotfiles--term-help-fn 'helm-dash-at-point
     dotfiles--term-interactive-command 'dotfiles-pop-from-ipython)
    (dotfiles-set-python-docsets (string-to-number a)))

  (defun dotfiles--term-ipython-exit (c a)
    (setq
     dotfiles--term-in-ipython nil
     dotfiles--term-help-fn 'man)
    (setq-local helm-dash-docsets '("Bash"))
    (kill-local-variable 'dotfiles--term-interactive-command))

  (add-to-list 'term-cmd-commands-alist '("ipython-start" . dotfiles--term-ipython-start))
  (add-to-list 'term-cmd-commands-alist '("ipython-exit" . dotfiles--term-ipython-exit))

  (defun dotfiles-pop-to-ipython ()
    (interactive)
    (setq dotfiles--ipython-popped-from (current-buffer))
    (dotfiles--create-or-pop-to-buffer
     (lambda (buf) (buffer-local-value 'dotfiles--term-in-ipython buf))
     (lambda ()
       (multi-term)
       (term-send-string (get-buffer-process (current-buffer)) "py")
       (term-send-input)
       (current-buffer))))

  (require 'python)
  (bind-keys
   :map python-mode-map
   ("s-i" . dotfiles-pop-to-ipython)
   ("C-S-i" . dotfiles-pop-to-ipython))

  (defun dotfiles-pop-from-ipython ()
    (interactive)
      (dotfiles--try-pop-to-buffer dotfiles--ipython-popped-from)))

(use-package term-alert
  :config
  (bind-keys
   :map term-mode-map
   ("C-#" . term-alert-next-command-toggle)
   ;; ("M-#" . term-alert-all-toggle) ; todo
   ("C-'" . term-alert-runtime))

  (bind-keys
   :map term-raw-map
   ("C-#" . term-alert-next-command-toggle)
   ;; ("M-#" . term-alert-all-toggle) ; todo
   ("C-'" . term-alert-runtime)))

(progn
  (require 'term-debug)
  (setq-default dotfiles--term-interactive-command 'term-debug-dwim))
