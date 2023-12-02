(defun read-lines (file)
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

(defvar data (read-lines "../data/day2.txt"))

(defun parse-game (line)
  (string-match "Game \\([[:digit:]]*\\): " line)
  (cons
   (string-to-number (match-string 1 line))
   (mapcar (lambda (str)
             (mapcar (lambda (str)
                       (let ((parts (split-string (string-trim str) " ")))
                         (cons (intern (cadr parts)) (string-to-number (car parts)))))
                     (split-string str ",")))
           (split-string (substring line (match-end 0)) ";"))))

(defun parse-all-games (lst)
  (if (null lst)
      nil
    (cons (parse-game (car lst)) (parse-all-games (cdr lst)))))

(defvar game-alist (parse-all-games data))

(defun color-maxes (game)
  (let ((rounds (cdr game))
        (rst '((red . 0) (green . 0) (blue . 0))))
    (dolist (round rounds)
      (dolist (c '(red green blue))
        (when (> (alist-get c round 0) (alist-get c rst))
          (push (cons c (alist-get c round)) rst))))
    (cons (car game) rst)))

;; Part 1
(let ((sum 0))
  (dolist (max-pair (mapcar #'color-maxes game-alist))
    (let ((id (car max-pair))
          (max-alist (cdr max-pair)))
      (when (and (<= (alist-get 'red max-alist) 12)
                 (<= (alist-get 'green max-alist) 13)
                 (<= (alist-get 'blue max-alist) 14))
        (setq sum (+ sum id)))))
  sum)

;; Part 2
(defun sum (lst)
  (if (null lst)
      0
    (+ (car lst) (sum (cdr lst)))))

(sum (mapcar (lambda (max-pair)
               (let ((max-alist (cdr max-pair)))
                 (*
                  (alist-get 'red max-alist 1)
                  (alist-get 'green max-alist 1)
                  (alist-get 'blue max-alist 1))))
             (mapcar #'color-maxes game-alist)))
