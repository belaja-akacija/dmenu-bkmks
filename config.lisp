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
