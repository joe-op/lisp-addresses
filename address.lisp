(defun make-address (&key name addr1 addr2 city state zip notes)
  (list :name name :addr1 addr1 :addr2 addr2 :city city :state state :zip zip :notes notes))

(defun identity (x) x)

(defun fold (el &key empty (full 'identity))
  (if el (apply full (list el)) empty))

(defun fold-str (str)
  (fold str :full (lambda (s) (format nil "~a" s)) :empty ""))

(defun const (x)
  (lambda (y) x))

(defun with-newlines (str &rest strs)
  "Concatenate strings, adding a newline to non-empty strings"
  (fold (car strs)
	:empty str
	:full (lambda (s)
		(let ((newstr (if (equal "" str) s
				  (if (equal "" s) str
				      (format nil "~a~%~a" str s)))))
		  (apply #'with-newlines newstr (cdr strs))))))

(defun join-strs (strs &key (sep ", ") prefix)
  (fold (car strs)
	:empty (fold-str prefix)
	:full (lambda (s)
		(let ((joined (fold prefix :empty s :full (lambda (p) (format nil "~a~a~a" p sep s)))))
		  (join-strs (cdr strs) :sep sep :prefix joined)))))

(defun print-address (addr)
  (let ((name (fold (getf addr :name) :empty "Anonymous address" :full (lambda (n) (format nil "~a" n))))
	(addr1 (fold-str (getf addr :addr1)))
	(addr2 (fold-str (getf addr :addr2)))
	(city (fold-str (getf addr :city)))
        (state (fold-str (getf addr :state)))
	(zip (fold-str (getf addr :zip)))
	(notes (fold (getf addr :notes) :empty "" :full (lambda (n) (format nil "Notes: ~a" n)))))
    (with-newlines
      name
      "-------------------"
      addr1
      addr2
      (join-strs (list (join-strs (list city state)) zip) :sep " ")
      notes)))
    
    

