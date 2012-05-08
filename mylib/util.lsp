
(defun open-file-as-rb (fname)
  (let ((fp (open fname :direction :input :element-type 'unsigned-byte :if-does-not-exist nil)))
    (if (eq fp nil)
        (error (format nil "Error in open-file-as-rb  ~A can not open~%" fname))
	fp)))

(defun open-file-as-rt (fname)
  (let ((fp (open fname :direction :input :if-does-not-exist nil)))
    (if (eq fp nil)
        (error (format nil "Error in open-file-as-rt  ~A can not open~%" fname))
	fp)))


(defun open-file-as-wb (fname)
  (open fname :direction :output :element-type 'unsigned-byte :if-exists :supersede))

(defun open-file-as-wt (fname)
  (open fname :direction :output :if-exists :supersede))

(defun close-files (flist)
  (dolist (fs flist 'done)
    (close fs)))


(defun my-command-line ()
  (or 
   #+CLISP *args*  
   #+LISPWORKS system:*line-arguments-list*
    nil))



                 
