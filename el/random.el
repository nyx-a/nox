
(defun 0to1 (&optional presicion)
  ((lambda (scale)
     (/ (random scale) (float scale)))
   (expt 10 (or presicion 5))))

(defun coin-toss (front back)
  (if (zerop (random 2)) front back))

(defun noise (value coefficient)
  (+ value (* value coefficient (0to1) (coin-toss +1 -1))))



(defun random-choice (list)
  (elt list (random (length list))))

(defun sequence (a b)
  (cons a (if (< a b) (sequence (1+ a) b))))

(setq chars (concat
             ;; " !#$%&'()*+,-./:;<=>?@[]^_`{|}~"
             ;;" !\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
             (apply 'string (sequence ?a ?z))
             (apply 'string (sequence ?A ?Z))
             (apply 'string (sequence ?0 ?9))))

(defun make-password (length)
  "make random password"
  (interactive "nLength you need: ")
  (let ((tube ""))
    (dotimes (i length)
      (setq tube (format "%s%c" tube (random-choice chars))))
    (insert tube)))

