(defun remove-lines (file lines-to-remove)
  (let ((filtered-lines ""))
  (with-open-file (in file)
    (loop
      for line-number from 1
      for line = (read-line in nil nil)
      while line
      unless (string-equal lines-to-remove line)
      do (setf filtered-lines (concatenate 'string filtered-lines line (string #\NewLine)))))
    filtered-lines))

(defun overwrite-file! (file removed-lines &key (type :human))
  (with-open-file (in file
                      :direction :output
                      :if-exists :supersede
                      :if-does-not-exist :create) ; overwrite file
    (if (equal type :data)
    (format in "~s" removed-lines)
    (format in "~A" removed-lines))))

(defun show-dialog (dialog &key (justify "left"))
  (let* ((justification (format nil "--justify=~A" justify))
         (dialog-width (length dialog))
         (dialog-height (length (cl-ppcre:split "\\n" dialog)))
         (geometry (format nil "--geometry=~Ax~A+550+300" dialog-width (* 32 dialog-height))))
    (uiop:run-program `("yad" "--text-info" "--wrap" "--margins=20" ,geometry ,justification "--fore=#f2e5bc" "--back=#32302f")
                      :input
                      (uiop:process-info-output
                        (uiop:launch-program `("echo" ,dialog) :output :stream))
                      :output :string)))

(defun append->file (url desc path)
  (with-open-file (output path :direction :output
                          :if-exists :append :if-does-not-exist :create)
    (format output "~A | ~A~%" desc url )))

(defun get-directory-files (lst)
         (mapcar #'pathname-name lst))

(defun index-of (lst ele i)
  (cond ((null lst) nil)
        ((equal (car lst) ele) i)
        ((index-of (cdr lst) ele (1+ i)))))

(defun get-file-lines (file)
  (let ((lngth 0)) (with-open-file (stream file)
     (loop for line = (read-line stream nil)
           while line
           do (setf lngth (1+ lngth))))
    (format nil "~s" lngth)))

(defun launch-dmenu (lngth file)
  (string-trim '(#\NewLine) (uiop:run-program `("dmenu" "-l" ,lngth)
                     :input file
                     :output :string)))

(defun launch-dmenu-prompt (prompt)
  (string-trim '(#\NewLine) (uiop:run-program `("dmenu" "-l" "6" "-p" ,prompt) :output :string)))
