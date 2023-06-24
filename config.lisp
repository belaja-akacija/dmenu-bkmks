(defparameter *config-path* #P "~/.config/bkmks/config")
(defparameter *url-file-path* #P "~/.config/bkmks/files/")
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
  plist)

(defun set-default-config ()
  (list 'menu "dmenu" 'browser "firefox" 'files `(,*url-full-path*) 'current-file 0))
