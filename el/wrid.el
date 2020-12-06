
(setq wrid-directory     "~")
(setq wrid-file-suffix   ".diary.txt")
(setq wrid-header-format "%Y %-m-%-d %A\n\n")
(setq wrid-go-back-hours 4)


(defun wrid-current-time (&optional backday)
  (time-subtract
   (current-time)
   (+ (* wrid-go-back-hours 60 60)
      (* (if (equal backday nil) 0 backday) 24 60 60))))

(defun wrid-time-y (time)
  (format-time-string "%Y" time))

(defun wrid-time-m (time)
  (format-time-string "%m" time))

(defun wrid-time-d (time)
  (format-time-string "%d" time))

(defun wrid-time-w (time)
  (nth (string-to-number
        (format-time-string "%w" time))
       '("sun" "mon" "tue" "wed" "thu" "fri" "sat")))

(defun wrid-filename (time)
  (concat (string-join
           (list (wrid-time-y time)
                 (wrid-time-m time)
                 (wrid-time-d time)
                 (wrid-time-w time))
           "-")
          wrid-file-suffix))

(defun wrid-path (&optional time)
  (let ((time (or time (wrid-current-time))))
    (format "%s/%s/%s"
            wrid-directory
            (wrid-time-y time)
            (wrid-filename time))))

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
  (outline-show-subtree)
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
