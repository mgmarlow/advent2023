(defun read-lines (file)
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

(defvar data (read-lines "../data/day3.txt"))

(defvar data-array (make-vector (length data) 0))
(dotimes (i (length (car data)))
  (setf (aref data-array i) (make-vector (length (car data)) 0)))

;; Read raw input into data-array
(dotimes (rowi (length data))
  (let ((row (elt data rowi)))
    (dotimes (coli (length row))
      (let ((col (char-to-string (aref row coli))))
        (setf (aref (aref data-array rowi) coli) col)))))

;; From s.el
(defun s-numeric? (s)
  "Is S a number?"
  (declare (pure t) (side-effect-free t))
  (not (null (string-match-p "^[0-9]+$" s))))

(defun s-symbolic? (s)
  'stub)

(defun range-borders-symbol? (i j r)
  'stub)

(defun part-numbers ()
  (let (rst (i 0))
    ;; swap (car data) with the dolist
    (while (string-match "[0-9]+" (car data) i)
      (let ((starti (match-beginning 0))
            (endi (- (match-end 0) 1)))
        ;; search for symbols here using starti, endi indices
        (message "%d %d" starti endi))
      (setq i (match-end 0)))))
