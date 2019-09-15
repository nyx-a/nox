
(setq wrid-directory     "~")
(setq wrid-file-format   "%Y-%m-%d-%a.diary.txt")
(setq wrid-header-format "%Y %-m-%-d %A\n\n")
(setq wrid-go-back-hours 4)


(defun wrid-current-time (&optional backday)
  (time-subtract
   (current-time)
   (+ (* wrid-go-back-hours 60 60)
      (* (if (equal backday nil) 0 backday) 24 60 60))))

(defun wrid-path (&optional time)
  (if (equal time nil) (setq time (wrid-current-time)))
  (format
   "%s/%s/%s"
   wrid-directory
   (format-time-string "%Y" time)
   (downcase (format-time-string wrid-file-format time))))

(defun wrid-insert-header (&optional time)
  (interactive "P")
  (insert
   (format-time-string
    wrid-header-format
    (if (equal time nil) (wrid-current-time) time))))

(defun wrid-find-file (&optional time)
  (find-file (wrid-path time))
  (if (zerop (buffer-size)) (wrid-insert-header time))
  (goto-char (point-max))
  (read-only-mode 0)
  (message
   (format-time-string
    "Writing diary .. %Y %-m/%-d %A" time)))

(defun wrid (&optional backday)
  "WRIte Diary / C-u Days back"
  (interactive "P")
  (wrid-find-file (wrid-current-time backday)))


(defun wrid-ymd (text)
  "WRIte Diary at specified date"
  (interactive "s(Year Month Day) ")
  (let* ((i (mapcar
             'string-to-number
             (seq-filter
              (lambda (x) (not (string-equal x "")))
              (split-string text "[^0-9]+"))))
         (y (elt i 0))
         (m (elt i 1))
         (d (elt i 2))
         (time (encode-time 0 0 0 d m y)))
    (wrid-find-file time)))
