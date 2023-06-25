;;;; Port of the bkmks dmenu script from the suckless website, with my own twist.

;;; TODO
;;; - Fix the need to put double quotes for certain links, when adding new entries

(defparameter *config* (load-config *config-path*))
(defparameter *browser* (getf *config* 'browser))
(defparameter *preferred-menu* (getf *config* 'menu))
(defparameter *files* (getf *config* 'files))
(defparameter *file-state* (getf *config* 'current-file))
(defparameter *current-file* (nth *file-state* *files*))

(defun show-usage ()
  (show-dialog (format nil "
bkms: unix bookmark management that sucks less. Lisp edition!
       usage:
         bkmks h[elp]
           show this help message
         bkmks a[dd] <url>
           add a new bookmark
         bkmks d[el] <selected entry>
           delete an entry
         bkmks [ls]
           show all bookmarks and go to link in prefered browser

           Configuration is done by directly editing the script.

           If you would prefer to have your bookmarks stored in an alternate locatation, there are also variables that can be changed for that. The default is /home/user/.bkmks/urls~%")))

(defun bkmks-check ()
  (ensure-directories-exist *url-file-path*)
  (cond ((null (probe-file *current-file*))
         (show-dialog (format nil "Error: No bookmarks found to display. Try adding some!~%~%")))
        (t (format nil "Everything OK."))))

(defun bkmks-display ()
  (update-config)
  ;; This is currently seeming quite messy and bulky
  ;(bkmks-check)
  (let ((bkmks-length  (string-trim `(#\NewLine) (uiop:run-program `("wc" "-l")
                                                                   :input *current-file*
                                                                   :output :string)))
        (raw-entry "")
        (filtered-entry "")
        (current-file (pathname-name *current-file*)))
    (setq raw-entry (uiop:run-program `("dmenu" "-l" ,bkmks-length "-p" ,current-file)
                                      :input *current-file*
                                      :output :string))
    (setq filtered-entry (cl-ppcre:scan-to-strings "(?<=\\|\\s).\+" (string-trim '(#\NewLine) raw-entry)))
    `(,filtered-entry ,(string-trim '(#\NewLine) raw-entry))))

(defun bkmks-add ()
  (update-config)
  (let ((desc ""))
    (if (null (nth 2 sb-ext:*posix-argv*))
        (show-dialog (format nil "Error: url must be provided.~%~%") :justify "center")
        (progn
          (setq desc (uiop:run-program `("dmenu" "-l" "6" "-p" "Description: ") :output :string))
          ;(print desc)
          (append->file (nth 2 sb-ext:*posix-argv*) (string-trim '(#\NewLine) desc) *current-file*)))))

(defun bkmks-del ()
  (bkmks-check)
  (let* ((entry (nth 1 (bkmks-display)))
        (removed-lines (remove-lines *current-file* entry)))
    (overwrite-file! *current-file* removed-lines)))

(defun bkmks-change ()
  (update-config)
  (let* ((files (get-directory-files *url-file-path-list*))
        (files-length (string (digit-char (length files))))
        (tmp #P "/tmp/bkmks-change.tmp")
        (raw-entry "")
        (filtered-entry ""))
    (overwrite-file! tmp (format nil "~{~A~%~}" files))
    (set-config! *config* 'files *url-file-path-list*)
    (update-config)
    (setq raw-entry (uiop:run-program `("dmenu" "-l" ,files-length)
                            :input tmp
                            :output :string))
    (setq filtered-entry (merge-pathnames (uiop:merge-pathnames* *url-file-path*) (pathname (string-trim '(#\NewLine) raw-entry))))
    (set-config! *config* 'current-file (index-of *url-file-path-list* filtered-entry 0))
    (update-config)
    ))

(defun bkmks-send ()
  (let ((entry (nth 0 (bkmks-display))))
  (uiop:run-program `(,*browser* ,entry))))

(defun main ()
  (update-config)
  (cond ((not (null (find (nth 1 sb-ext:*posix-argv*) '("add" "a") :test #'string-equal)))
         (bkmks-add))
        ((not (null (find (nth 1 sb-ext:*posix-argv*) '("del" "d") :test #'string-equal)))
         (bkmks-del))
        ((not (null (find (nth 1 sb-ext:*posix-argv*) '("chg" "c") :test #'string-equal)))
         (bkmks-change))
        ((not (null (find (nth 1 sb-ext:*posix-argv*) '("help" "h") :test #'string-equal)))
         (show-usage))
        ((not (null (find (nth 1 sb-ext:*posix-argv*) '("nil" "ls") :test #'string-equal)))
         (bkmks-send))
        (t (show-usage))))
