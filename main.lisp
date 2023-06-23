;;;; Port of the bkmks dmenu script from the suckless website, with my own twist.

;;; TODO
;;; - Fix the need to put double quotes for certain links, when adding new entries

(defparameter *browser* "firefox") ;; think about using xdg-open instead
(defparameter *url-file-path* #P "~/Documents/.bkmks/")
(defparameter *url-file-name* "urls")
(defparameter *url-full-path* (merge-pathnames *url-file-path* (pathname *url-file-name*)))

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
  (cond ((null (probe-file *url-full-path*))
         (show-dialog (format nil "Error: No bookmarks found to display. Try adding some!~%~%")))
        (t (format nil "Everything OK."))))

(defun bkmks-display ()
  ;; This is currently seeming quite messy and bulky
  (bkmks-check)
  (let ((bkmks-length  (string-trim `(#\NewLine) (uiop:run-program `("wc" "-l")
                                                                   :input *url-full-path*
                                                                   :output :string)))
        (raw-entry "")
        (filtered-entry ""))
    (setq raw-entry (uiop:run-program `("dmenu" "-l", bkmks-length)
                                      :input *url-full-path*
                                      :output :string))
    (setq filtered-entry (cl-ppcre:scan-to-strings "(?<=\\|\\s).\+" (string-trim '(#\NewLine) raw-entry)))
    `(,filtered-entry ,(string-trim '(#\NewLine) raw-entry))))

(defun bkmks-add ()
  (let ((desc ""))
    (if (null (nth 2 sb-ext:*posix-argv*))
        (show-dialog (format nil "Error: url must be provided.~%~%") :justify "center")
        (progn
          (setq desc (uiop:run-program `("dmenu" "-l" "6" "-p" "Description: ") :output :string))
          (print desc)
          (append->file (nth 2 sb-ext:*posix-argv*) (string-trim '(#\NewLine) desc) *url-full-path*)))))

(defun bkmks-del ()
  (bkmks-check)
  (let* ((entry (nth 1 (bkmks-display)))
        (removed-lines (remove-lines *url-full-path* entry)))
    (overwrite-urls! *url-full-path* removed-lines)))

(defun bkmks-send ()
  (let ((entry (nth 0 (bkmks-display))))
  (uiop:run-program `(,*browser* ,entry))))

(defun main ()
  (cond ((not (null (find (nth 1 sb-ext:*posix-argv*) '("add" "a") :test #'string-equal)))
         (bkmks-add))
        ((not (null (find (nth 1 sb-ext:*posix-argv*) '("del" "d") :test #'string-equal)))
         (bkmks-del))
        ((not (null (find (nth 1 sb-ext:*posix-argv*) '("help" "h") :test #'string-equal)))
         (show-usage))
        ((not (null (find (nth 1 sb-ext:*posix-argv*) '("nil" "ls") :test #'string-equal)))
         (bkmks-send))
        (t (show-usage))))
