
(defun random<1 (&optional number-of-decimal-places)
  ((lambda (scale)
     (/ (random scale) (float scale)))
   (expt 10 (or number-of-decimal-places 5))))

(defun coin-toss ()
  (if (zerop (random 2)) t nil))

(defun random-nth (list)
  (elt list (random (length list))))

;;
;; letter
;;

(setq lower-characters    (apply 'string (number-sequence ?a ?z)))
(setq upper-characters    (apply 'string (number-sequence ?A ?Z)))
(setq numeric-characters  (apply 'string (number-sequence ?0 ?9)))
(setq symbolic-characters " !\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~")



(defun make-password (length)
  "make random password"
  (interactive "nLength you need: ")
  (let ((tube ""))
    (dotimes (i length)
      (setq tube (format "%s%c" tube (random-nth lower-characters))))
    (insert tube)))

