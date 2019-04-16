(defpackage "TRIE"
  (:use "COMMON-LISP")
  (:export "MAKE-TRIE" "ADD-WORD" "SUBTRIE"
	   "TRIE-WORD" "TRIE-COUNT" "MAPC-TRIE"
	   "READ-WORDS"))

(defstruct (trie (:print-function
		   (lambda (n s d)
		     (format s "#<~S, wc: ~S>"
			     (trie-word n)
			     (trie-count n)))))
  wordp chars (count 0) children)

(defun add-word (str tr)
  (let ((chars (coerce str 'list)))
    (when (null (apply #'subtrie tr chars))
      (add-chars chars tr))
    tr))

(defun add-chars (chars tr)
  (incf (trie-count tr))
  (if (null chars) (setf (trie-wordp tr) t)
    (add-chars (cdr chars) (provide-subtrie (car chars) tr))))

(defun provide-subtrie (c tr)
  (or (subtrie tr c)
      (let ((new-child (cons c (make-trie
			     :chars (append-char (trie-chars tr) c)))))
	(push new-child (trie-children tr))
	(cdr new-child))))

(defun append-char (s c)
  (concatenate 'string s (string c)))

(defun subtrie (tr &rest chars)
  (if (or (null chars) (null tr)) tr
    (apply #'subtrie
	   (cdr (assoc (car chars) (trie-children tr)))
	   (cdr chars))))

(defun trie-word (tr)
  (if (trie-wordp tr) (trie-chars tr) nil))

(defun mapc-trie (fn tr)
  (dolist (c (trie-children tr))
    (funcall fn (car c) (cdr c)))
  tr)

(defun read-words (file tr)
  (with-open-file (str file :direction :input)
    (do* ((eof (list nil))
	  (word (read-line str nil eof)
		(read-line str nil eof)))
      ((eql word eof) tr)
      (add-word word tr))))
