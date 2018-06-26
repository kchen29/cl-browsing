;;;;Parse HTML using reader macros

;;;not all of them
(defvar void-tags '(br hr))

(defun |<-reader| (stream char)
  (declare (ignore char))
  (let ((tag (read-delimited-list #\> stream t)))
    (if (or (closing-tag-p tag) (void-tag-p tag))
        tag
        (cons (car tag)
              (loop with closing = (list (concatenate 'string "/"
                                                      (symbol-name (car tag))))
                    for next = (read stream nil nil t)
                    if (equal next closing)
                      return output
                    else
                      collect next into output)))))

(defun closing-tag-p (tag)
  (char= #\/ (char (string (car tag)) 0)))

(defun void-tag-p (tag)
  (member tag void-tags))

(set-macro-character #\> nil nil)
(set-macro-character #\< #'|<-reader|)
