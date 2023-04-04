(setq delete-by-moving-to-trash t)

(when (executable-find "to-recycle-bin")
  (defun system-move-file-to-trash (filename)
    "Move FILENAME to the dotfiles recycle bin."
    (unless (= (call-process "to-recycle-bin" nil nil nil filename) 0)
      (message "Failed to move to the recycle bin"))))
