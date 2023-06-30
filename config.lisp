(defparameter *user-home* (namestring (user-homedir-pathname)))
(defparameter *config-path* (pathname (concatenate 'string *user-home* ".config/bkmks/config")))
(defparameter *url-file-path*  (pathname (concatenate 'string *user-home*  ".config/bkmks/files/")))
(defparameter *url-file-path-list*  (directory (merge-pathnames *url-file-path* "*")))
(defparameter *url-file-name* "urls")
(defparameter *url-full-path* (merge-pathnames *url-file-path* (pathname *url-file-name*)))

(defun load-config (file)
  (ensure-directories-exist file)
  (if (probe-file file)
         (modest-config:load-config file)
      (overwrite-file! file ;; quietly fail by creating a new config and set the defaults
                       (set-default-config)
                       :type :data)))

(defun set-config! (plist key val)
  (setf (getf plist key) val)
  (overwrite-file! *config-path* plist :type :data))

(defun check-config (config)
  ;; should a dialog box come up instead so the user can fix the error themselves?
  ;; Or just set it to the defaults?
  "check if things are valid in the config and handle them."
  (cond ((equal (getf config 'current-file) nil)
         (format t "setting current file to default")
         (set-config! config 'current-file 0))
        ((not (realp (getf config 'current-file)))
         (format t "file number not a number. Setting to default.")
         (set-config! config 'current-file 0))
        ((> (getf config 'current-file) (length (getf config 'files)))
         (format t "invalid file number. greater than. Setting to default")
         (set-config! config 'current-file 0))
        ((< (getf config 'current-file) 0)
         (format t "invalid file number. less than. Setting to default")
         (set-config! config 'current-file 0))
        (t (format t "nothing to do"))))

(defun set-default-config ()
  (list 'menu "dmenu" 'browser "firefox" 'files `(,*url-full-path*) 'current-file 0))

(defun sync-configuration ()
  (let ((config '()))
    (lambda ()
      (setf *url-file-path-list* (directory (merge-pathnames *url-file-path* "*")))

      (set-config! *config* 'files *url-file-path-list*)
      (setf config (modest-config:load-config *config-path*))
      config
      )))

(defun update-config ()
  (let ((config-sync (sync-configuration)))
    (progn
     (setf *config* (funcall config-sync))
     (set-config! *config* 'files *url-file-path-list*))))
