
(defun insert-now ()
  (interactive)
  (insert (format-time-string "%H:%M" (current-time))))

(defun sewing-machine (&optional length)
  (interactive "P")
  (if (equal length nil) (setq length 59))
  (back-to-indentation)
  (reindent-then-newline-and-indent)
  (previous-line)
  (indent-for-tab-command)
  (comment-indent)
  (insert "-")
  (while (< (current-column) length) (insert " -"))
  (back-to-indentation)
  (next-line))

