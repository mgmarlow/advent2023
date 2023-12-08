;;; day8.el --- Haunted Wasteland -*- lexical-binding: t; -*-

;;; Code:

(defun read-lines (file)
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

(defvar data (read-lines "../data/day8.txt"))

(defvar instructions (car data))

(defun parse-node (line)
  (let ((pair (split-string line " = ")))
    (cons
     (intern (car pair))
     (car (read-from-string (string-replace "," " ." (cadr pair)))))))

(defun build-node-map (lines)
  (mapcar #'parse-node lines))

(defvar nodes (build-node-map (cdr data)))

(defun get-instruction (i)
  (char-to-string (aref instructions (mod i (length instructions)))))

(defun destination (node i)
  (pcase (get-instruction i)
    ("L" (car (alist-get node nodes)))
    ("R" (cdr (alist-get node nodes)))
    (_ (error "Invalid instruction"))))

;; Part 1
(defun traverse ()
  (let ((node 'AAA) (i 0))
    (while (not (eq node 'ZZZ))
      (setq node (destination node i))
      (setq i (+ i 1)))
    i))

(traverse)

;; Part 2
(require 's)

(defun ending-node? (sym)
  (s-ends-with? "Z" (symbol-name sym)))

(defun starting-node? (sym)
  (s-ends-with? "A" (symbol-name sym)))

(defun alist-keys (alist)
  (mapcar 'car alist))

(defun gcd (a b)
  (if (= b 0)
      a
    (gcd b (mod a b))))

(defun lcm (a b)
  (/ (* a b) (gcd a b)))

(defun traverse-b ()
  (let ((starts (seq-filter #'starting-node? (alist-keys nodes)))
        rst)
    (dolist (start starts)
      (let ((cur start) (i 0))
        (while (not (ending-node? cur))
          (setq cur (destination cur i))
          (setq i (+ i 1)))
        (push i rst)))
    (seq-reduce #'lcm rst 1)))

(traverse-b)

;;; day8.el ends here
