;; config file tests

(defvar *test-config-path* #P "tests/test-config")

(defun load-config (file)
  (if (probe-file file)
      (modest-config:load-config file)
      (overwrite-file! *test-config-path* ;; quietly fail by creating a new config and set the defaults
                       (set-default-config)
                       :type :data)))

(defun set-config! (key val)
  (setf (getf *plist* key) val)
  *plist*)

(defun set-default-config ()
  (list 'menu "dmenu" 'browser "firefox" 'files `(,*url-full-path*)))
