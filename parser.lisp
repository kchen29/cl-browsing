;;;; Parse HTML using reader macros

;;; not all of them
(defvar void-tags '(br hr))

(defun |<-reader| (stream char)
  (declare (ignore char))
  (let* ((tag (read-delimited-list #\> stream t))
         (name (car tag)))
    (if (or (closing-tag-p name) (void-tag-p name))
        tag
        (cons name
              (loop with closing = (concatenate 'string "/"
                                                (symbol-name name))
                    for next = (read stream nil nil t)
                    if (string= (string (car next)) closing)
                      return output
                    else
                      collect next into output)))))

(defun closing-tag-p (name)
  (char= #\/ (char (string name) 0)))

(defun void-tag-p (name)
  (member name void-tags))

(set-macro-character #\> nil nil)        ; make sure > doesn't get parsed as part of a symbol
(set-macro-character #\< #'|<-reader|)

;;; test out reader macro
(defmacro html (&whole whole &rest rest)
  (declare (ignore rest))
  (print whole)
  nil)
