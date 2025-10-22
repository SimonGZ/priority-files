(require 'ert)

(unless (featurep 'projectile)
  (defun projectile-project-root ()
    (error "projectile stub: not implemented"))
  (provide 'projectile))

(require 'priority-files)

(defun priority-files-test--with-temp-dir (fn)
  "Create a temporary directory, run FN with it, and clean up afterwards."
  (let ((dir (make-temp-file "priority-files-test" t)))
    (unwind-protect
        (funcall fn dir)
      (when (file-exists-p dir)
        (delete-directory dir t)))))

(defun priority-files-test--make-file (root relative &optional contents)
  "Create RELATIVE file under ROOT with CONTENTS and return absolute path."
  (let* ((path (expand-file-name relative root))
         (parent (file-name-directory path)))
    (make-directory parent t)
    (with-temp-file path
      (when contents
        (insert contents)))
    path))

(ert-deftest priority-files-resolve-entry-ignores-comments ()
  (priority-files-test--with-temp-dir
   (lambda (root)
     (should-not (priority-files--resolve-entry "# ignore" root)))))

(ert-deftest priority-files-resolve-static-entry ()
  (priority-files-test--with-temp-dir
   (lambda (root)
     (let ((file (priority-files-test--make-file root "docs/readme.md" "hello")))
       (should (equal file (priority-files--resolve-entry "docs/readme.md" root)))))))

(ert-deftest priority-files-resolve-static-entry-outside-root ()
  (priority-files-test--with-temp-dir
   (lambda (root)
     (should-not (priority-files--resolve-entry "../etc/passwd" root)))))

(ert-deftest priority-files-resolve-latest-modified ()
  (priority-files-test--with-temp-dir
   (lambda (root)
     (let* ((old-file (priority-files-test--make-file root "notes/old.md" "old"))
            (new-file (priority-files-test--make-file root "notes/new.md" "new"))
            (older-time (time-subtract (current-time) (seconds-to-time 120)))
            (newer-time (current-time)))
       (set-file-times old-file older-time)
       (set-file-times new-file newer-time)
       (should (equal new-file (priority-files--resolve-entry "latest notes/*.md" root)))))))

(ert-deftest priority-files-resolve-latest-created ()
  (priority-files-test--with-temp-dir
   (lambda (root)
     (let ((old-file (priority-files-test--make-file root "notes/old.md" "old")))
       (sleep-for 1)
       (let ((new-file (priority-files-test--make-file root "notes/new.md" "new")))
         ;; Ensure modification times preserve creation order if birth times are missing.
         (set-file-times old-file (time-subtract (current-time) (seconds-to-time 120)))
         (set-file-times new-file (current-time))
         (should (equal new-file (priority-files--resolve-entry "latest:created notes/*.md" root))))))))

(ert-deftest priority-files-resolve-latest-no-match ()
  (priority-files-test--with-temp-dir
   (lambda (root)
     (should-not (priority-files--resolve-entry "latest reports/*.org" root)))))

(provide 'priority-files-test)
