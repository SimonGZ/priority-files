;;; priority-files.el --- Quickly jump to priority files in a Projectile project -*- lexical-binding: t; -*-

;; Author: Simon Ganz <dev@simonganz.com>
;; URL: https://github.com/SimonGZ/priority-files
;; Version: 0.2
;; Package-Requires: ((emacs "26.1") (projectile "2.0.0"))
;; Keywords: convenience, project

;;; Commentary:

;; This package allows users to define and quickly jump to priority files within a
;; Projectile project. Priority files are listed in a `.priority-files` file at the
;; project root. This package provides commands to navigate and edit these files.

;; Usage:
;; - Use `priority-file-next` to jump to the top priority file.
;; - Use `edit-priority-files` to create or edit the `.priority-files` list.

;;; Code:

(require 'projectile)
(require 'cl-lib)

;;;###autoload
(defun get-priority-files ()
  "Get priority files from .priority-files in the project root."
  (let ((project-root (projectile-project-root))
        (priority-file ".priority-files")
        (files '()))
    (when project-root
      (let ((priority-path (expand-file-name priority-file project-root)))
        (when (file-exists-p priority-path)
          (with-temp-buffer
            (insert-file-contents priority-path)
            (goto-char (point-min))
            (while (not (eobp))
              (let ((line (string-trim (buffer-substring-no-properties
                                        (line-beginning-position)
                                        (line-end-position)))))
                (unless (string-empty-p line)
                  (push (expand-file-name line project-root) files)))
              (forward-line 1))))))
    (nreverse files)))

;;;###autoload
(defun priority-files-edit ()
  "Create or edit the .priority-files file in the current project."
  (interactive)
  (let ((project-root (projectile-project-root)))
    (if project-root
        (find-file (expand-file-name ".priority-files" project-root))
      (message "Not in a project"))))

;;;###autoload
(defun priority-files-next (&optional cycle)
  "Jump to the highest priority file in the current project.
With CYCLE argument, cycle through priority files in order.
When already viewing the top priority file, jumps to the second priority file
even without the CYCLE argument."
  (interactive "P")
  (let* ((priority-files (get-priority-files))
         (current-file (buffer-file-name))
         (target-file nil))

    (cond
     ;; No priority files found
     ((null priority-files)
      (message "No priority files defined. Create a .priority-files file in your project root."))

     ;; Currently viewing the top priority file - go to next one regardless of cycle argument
     ((and current-file
           (string= current-file (car priority-files))
           (> (length priority-files) 1))
      (setq target-file (nth 1 priority-files)))

     ;; Cycling through files
     (cycle
      (let ((current-index (cl-position current-file priority-files :test 'string=)))
        (if current-index
            ;; Currently on a priority file, go to next one
            (setq target-file (nth (mod (1+ current-index) (length priority-files))
                                   priority-files))
          ;; Not on a priority file, go to first one
          (setq target-file (car priority-files)))))

     ;; Not cycling, just go to first file
     (t
      (setq target-file (car priority-files))))

    (when target-file
      (if (file-exists-p target-file)
          (find-file target-file)
        (message "Priority file not found: %s" target-file)))))

;;;###autoload
(defun priority-files-add-file ()
  "Add the current file to the .priority-files list.
Creates the .priority-files file if it doesn't exist."
  (interactive)
  (let ((project-root (projectile-project-root))
        (current-file (buffer-file-name)))
    (if (and project-root current-file)
        (let* ((priority-path (expand-file-name ".priority-files" project-root))
               (relative-path (file-relative-name current-file project-root)))
          ;; Create the file if it doesn't exist
          (unless (file-exists-p priority-path)
            (with-temp-file priority-path
              (insert "")))

          ;; Check if the file is already in the list
          (with-temp-buffer
            (when (file-exists-p priority-path)
              (insert-file-contents priority-path))
            (goto-char (point-min))
            (if (search-forward relative-path nil t)
                (message "File already in priority list: %s" relative-path)
              ;; Add the file to the list
              (goto-char (point-max))
              ;; Add a newline if the file is not empty and doesn't end with one
              (when (and (> (buffer-size) 0)
                         (not (eq (char-before) ?\n)))
                (insert "\n"))
              (insert relative-path "\n")
              (write-region (point-min) (point-max) priority-path)
              (message "Added to priority files: %s" relative-path))))
      (message "Not in a project or no file is open"))))

(provide 'priority-files)

;;; priority-files.el ends here
