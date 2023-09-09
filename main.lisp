;;;; Port of the bkmks dmenu script from the suckless website, with my own twist.

;;; TODO
;;; - Fix the need to put double quotes for certain links, when adding new entries
;;; - Make XDG compliant by putting the url files in ~/.local/share/bkmks/*
;;; - Create a data definition for EntryList to make it clear how you should be
;;;   passing the list as arguments to a function

(defparameter *config* (load-config *config-path*))
(defparameter *browser* "")
(defparameter *preferred-menu* "")
(defparameter *files* "")
(defparameter *file-state* "")
(defparameter *current-file* "")

;; TODO update the help
(defun show-usage ()
  (show-dialog (format nil "
      bkmks: unix bookmark management that sucks less. Lisp edition!

      usage:
        bkmks h[elp]
      show this help message
        bkmks a[dd] <url>
      add a new bookmark
        bkmks d[el] <selected entry>
      delete an entry
        bkmks m[v] <selected entry>
      move an entry from one category to another
        bkmks c[hg] <selected entry>
      change current bookmark category
        bkmks cata[dd] <selected entry>
      adds a new category
        bkmks catd[el] <selected entry>
      deletes a category
        bkmks [ls]
      show all bookmarks and go to link in prefered browser

      Configuration is done by editing the configuration file, located at /home/user/.config/bkmks/

      If you would prefer to have your bookmarks stored in an alternate location, there are also variables that can be changed for that. The default is /home/user/.config/bkmks/files/urls~%")))

(defun bkmks-check ()
  (update-globals)
  (ensure-directories-exist *url-file-path*)
  (cond ((null (probe-file *current-file*))
         (show-dialog (format nil "Error: No bookmarks found to display. Try adding some!~%~%")))
        (t (format nil "Everything OK."))))

(defun bkmks-get-categories ()
  (update-categories)
  (update-globals)
  (let* ((files (get-directory-files *url-file-path-list*))
         (files-length (format nil "~s" (length *url-file-path-list*)))
         (tmp #P "/tmp/bkmks-change.tmp")
         (raw-entry "")
         (filtered-entry ""))
    (overwrite-file! tmp (format nil "~{~A~%~}" files))
    (setq raw-entry (launch-dmenu files-length tmp))
    (setq filtered-entry (merge-pathnames *url-file-path* (pathname raw-entry)))
    `(,filtered-entry ,raw-entry)))


(defun bkmks-display-bkmks (cat)
  (update-globals)
  (let ((bkmks-length  (get-file-lines cat))
        (raw-entry "")
        (filtered-entry "")
        (current-file (pathname-name cat)))
    (setq raw-entry (launch-dmenu bkmks-length cat current-file))
    (format nil "~a" bkmks-length)
    (setq filtered-entry (cl-ppcre:scan-to-strings "(?<=\\|\\s).\+" (string-trim '(#\NewLine) raw-entry)))
    `(,filtered-entry ,(string-trim '(#\NewLine) raw-entry))))

;;; TODO: refactor to not use so much progn
(defun bkmks-add (&optional cli-link cli-path)
  (update-globals)
  (let ((desc "")
        (link ""))
    (if cli-link
        (append->file (car cli-link) (car (filter-entry cli-link)) cli-path)
        (progn (setf link (launch-dmenu-prompt "Link (paste w/ ctrl-y): "))
         (if (string= link "")
             (show-dialog (format nil "Error: url must be provided.~%~%") :justify "center")
             (progn
               (setf desc (launch-dmenu-prompt "Description: "))
               (append->file link (string-trim '(#\NewLine) desc) *current-file*)))))))

;; TODO: make better variable names
(defun bkmks-del (&optional cli-link cli-path)
  (bkmks-check)
  (let* ((entry nil)
         (cli-entry (nth 1 cli-link))
         (removed-lines (remove-lines *current-file* entry))
         (removed-lines-cli (remove-lines cli-path cli-entry)))
    (if cli-link
      (overwrite-file! cli-path removed-lines-cli)
       (progn
         (setf entry (nth 1 (bkmks-display-bkmks *current-file*)))
         (overwrite-file! *current-file* removed-lines)))))

;;; move an entry from one category to another

(defun bkmks-move ()
  (let ((from 0)
        (to 0)
        (from-file 0))
    (setf from (bkmks-get-categories))
    (setf to (bkmks-get-categories))
    (setf from-file (bkmks-display-bkmks (car from)))
    (bkmks-add from-file (car to))
    (bkmks-del from-file (car from))))

 (defun bkmks-change ()
  (let ((category (nth 0 (bkmks-get-categories))))
    (set-config! *config* 'current-file (index-of *url-file-path-list* category 0))
    (bkmks-send)))

(defun bkmks-del-category ()
  (let* ((category (nth 0 (bkmks-get-categories)))
         (index (getf *config* 'current-file))
         (prompt (string-downcase (launch-dmenu-prompt (format nil "(Deleting: ~A) Are you sure? (y/N): " (pathname-name category))))))
    (if (null (find prompt '("y" "yes") :test #'string-equal))
        nil
        (progn
          (set-config! *config* 'current-file (if (equal index 0) 0
                                                  (1- index)))
          (delete-file category)))
    (update-globals)))

(defun bkmks-add-category ()
  (let* ((category-name (launch-dmenu-prompt "Enter category name: "))
         (category-file (merge-pathnames *url-file-path* (pathname category-name))))
    (if (probe-file category-file)
        (show-dialog (format nil "This category already exists."))
        (overwrite-file! category-file ""))
    (update-globals)))

(defun bkmks-send ()
  (update-globals)
  (let ((entry (nth 0 (bkmks-display-bkmks *current-file*))))
    (uiop:run-program `(,*browser* ,entry))))

(defun main ()
  (update-home)
  (update-globals)
  (cond
    ((find (nth 1 sb-ext:*posix-argv*) '("add" "a") :test #'string-equal)
     (bkmks-add))
    ((find (nth 1 sb-ext:*posix-argv*) '("catadd" "cata" "ca") :test #'string-equal)
     (bkmks-add-category))
    ((find (nth 1 sb-ext:*posix-argv*) '("del" "d") :test #'string-equal)
     (bkmks-del))
    ((find (nth 1 sb-ext:*posix-argv*) '("catdel" "catd" "cd") :test #'string-equal)
     (bkmks-del-category))
    ((find (nth 1 sb-ext:*posix-argv*) '("chg" "c") :test #'string-equal)
     (bkmks-change))
    ((find (nth 1 sb-ext:*posix-argv*) '("mv" "m") :test #'string-equal)
     (bkmks-move))
    ((find (nth 1 sb-ext:*posix-argv*) '("help" "h") :test #'string-equal)
     (show-usage))
    ((find (nth 1 sb-ext:*posix-argv*) '("nil" "ls") :test #'string-equal)
     (bkmks-send))
    (t (show-usage))))
