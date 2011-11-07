
(defun open-file-as-rb (fname)
  (let ((fp (open fname :direction :input :element-type 'unsigned-byte  :if-does-not-exist nil)))
    (if (eq fp nil)
      (progn
        (format t "Error in open-file-as-rb  ~A can not open~%" fname)
        (abort :status 1))
      fp)))

(defun open-file-as-wb (fname)
  (open fname :direction :output :element-type 'unsigned-byte :if-exists :supersede))

(defun close-files (flist)
  (dolist (fs flist 'done)
    (close fs)))
