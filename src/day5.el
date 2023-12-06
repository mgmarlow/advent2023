(defun read-lines (file)
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

(defvar data (read-lines "../data/day5.txt"))

(defun read-nums (line)
  (let (rst (i 0))
    (while (string-match "[0-9]+" line i)
      (push (string-to-number (match-string 0 line)) rst)
      (setq i (match-end 0)))
    (reverse rst)))

(defvar seeds (read-nums (car data)))

(defun read-maps (lines)
  (let (rst name cur)
    (dolist (line lines)
      (cond
       ((string-match-p "[0-9]" line 0) (push (read-nums line) cur))
       (t (progn
            (when name
              (push (cons name (reverse cur)) rst)
              (setq cur nil)
              (setq name nil))
            (setq name (car (split-string line " ")))))))
    (when name
      (push (cons name (reverse cur)) rst))
    (reverse rst)))

(defvar maps (read-maps (cdr data)))

(defun compute-destination (x map-range)
  (+ (elt map-range 0) (- x (elt map-range 1))))

(defun within-range? (x range length)
  (and (< x (+ range length))
       (>= x range)))

(defun destination (x map)
  (let ((matching-range (seq-find
                         (lambda (range)
                           (apply #'within-range? `(,x ,@(cdr range))))
                         (cdr map))))
    (if (null matching-range)
        x
      (compute-destination x matching-range))))

(defun traverse-maps (seed maps)
  (if (null maps)
      seed
    (traverse-maps (destination seed (car maps)) (cdr maps))))

;; Part 1
(apply #'min (mapcar (lambda (seed) (traverse-maps seed maps)) seeds))

;; Part 2
;; todo
