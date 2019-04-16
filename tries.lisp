(defpackage "TRIE"
  (:use "COMMON-LISP")
  (:export "MAKE-TRIE" "ADD-WORD" "SUBTRIE"
	   "TRIE-WORD" "TRIE-COUNT" "MAPC-TRIE"
	   "READ-WORDS"))

(defstruct (trie (:print-function
		   (lambda (n s d)
		     (format s "#<~S, wc: ~S>"
			     (trie-word n)
			     (trie-wc n)))))
  wordp chars (wc 0) children)

(defun add-word (str tr)
  (if (apply #'subtrie tr (coerce str 'list)) tr
    (let ((c (char str 0)))
      (incf (trie-wc tr))
      (add-char c tr)
      (if (= 1 (length str)) (set-child-word c tr)
	(add-word (subseq str 1)
		  (cdr (assoc c (trie-children tr)))))
      tr)))

; helpers
(defun add-char (c tr)
  (when (null (assoc c (trie-children tr)))
    (push (cons c (make-trie
		    :chars (append-char (trie-chars tr) c)))
	  (trie-children tr))))

(defun set-child-word (c tr)
  (setf (trie-wordp (cdr (assoc c (trie-children tr)))) t)
  (incf (trie-wc (cdr (assoc c (trie-children tr))))))

(defun append-char (s c)
  (concatenate 'string s (string c)))
; /helpers

(defun subtrie (tr &rest chars)
  (if (or (null chars) (null tr)) tr
    (apply #'subtrie
	   (cdr (assoc (car chars) (trie-children tr)))
	   (cdr chars))))

(defun trie-word (tr)
  (if (trie-wordp tr) (trie-chars tr) nil))

(defun trie-count (tr)
  (trie-wc tr))

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
