#!/usr/bin/emacs --script
; Args: mimetype app file

(defun usage ()
  (message "Usage: non-default-association.el mimetype app file")
  (kill-emacs 1))

(when (< (length argv) 3)
  (usage))

(setq mimetype (pop argv))
(setq app (pop argv))
(setq file (pop argv))

(find-file-existing file)

(setq header "[Added Associations]")
(setq assoc-line (search-forward header nil t))

(if assoc-line
    (progn
      (forward-paragraph)
      (setq following-blank-line (point))
      (narrow-to-region assoc-line following-blank-line)
      (goto-char assoc-line)
      (setq found (search-forward (concat mimetype "=" app) nil t))
      (when (not found)
        (goto-char following-blank-line)
        (insert mimetype "=" app)
        (newline)
        (save-buffer)))
  (goto-char (point-max))
  (newline)
  (insert header)
  (newline)
  (insert mimetype "=" app)
  (newline)
  (save-buffer))
