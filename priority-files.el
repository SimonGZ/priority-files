;;; priority-files.el --- Quickly jump to priority files in a Projectile project -*- lexical-binding: t; -*-

;; Author: Simon Ganz <dev@simonganz.com>
;; URL: https://github.com/SimonGZ/priority-files
;; Version: 0.3
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

(defconst priority-files--latest-directive-regexp
  "^latest\\(?:[:]\\([[:alnum:]-]+\\)\\)?[ \t]+\\(.+\\)$"
  "Regexp matching latest directives in .priority-files entries.")

(defvar priority-files--birth-time-warning-shown nil
  "Internal flag used to avoid repeating creation-time fallback warnings.")

(defun priority-files--parse-seconds-to-time (value)
  "Convert VALUE, a string of integral seconds, into an Emacs time value.
Return nil when VALUE is empty or represents an unavailable timestamp (e.g. -1)."
  (let ((trimmed (string-trim value)))
    (when (and (not (string-empty-p trimmed))
               (not (string= trimmed "-1")))
      (let ((seconds (ignore-errors (string-to-number trimmed))))
        (when (numberp seconds)
          (seconds-to-time seconds))))))

(defun priority-files--stat-birth-time (path)
  "Return PATH creation time using the system `stat' command when possible.
Returns nil if the command is unavailable or the timestamp cannot be determined."
  (let ((buffer (generate-new-buffer " *priority-files stat*")))
    (unwind-protect
        (pcase system-type
          ('darwin
           (when (zerop (call-process "stat" nil buffer nil "-f" "%B" path))
             (priority-files--parse-seconds-to-time
              (with-current-buffer buffer (buffer-string)))))
          ((or 'gnu/linux 'gnu/kfreebsd 'gnu 'gnu/hurd)
           (when (zerop (call-process "stat" nil buffer nil "-c" "%W" path))
             (priority-files--parse-seconds-to-time
              (with-current-buffer buffer (buffer-string)))))
          (_ nil))
      (when (buffer-name buffer)
        (kill-buffer buffer)))))

(defun priority-files--file-modification-time (attrs)
  "Return the modification time from ATTRS produced by `file-attributes'."
  (if (fboundp 'file-attribute-modification-time)
      (file-attribute-modification-time attrs)
    (nth 5 attrs)))

(defun priority-files--file-birth-time (path attrs)
  "Return the creation time for PATH using ATTRS, if available."
  (or (and (fboundp 'file-attribute-birth-time)
           (file-attribute-birth-time attrs))
      (and (fboundp 'file-attribute-creation-time)
           (file-attribute-creation-time attrs))
      (priority-files--stat-birth-time path)))

(defun priority-files--path-within-root-p (path root)
  "Return non-nil when PATH is within ROOT. Both arguments must be absolute."
  (let ((normalized-root (file-name-as-directory (expand-file-name root)))
        (normalized-path (expand-file-name path)))
    (string-prefix-p normalized-root normalized-path)))

(defun priority-files--resolve-static-entry (line root)
  "Resolve LINE as a static entry relative to ROOT. Return absolute path or nil."
  (let ((expanded (expand-file-name line root)))
    (if (priority-files--path-within-root-p expanded root)
        expanded
      (progn
        (message "Priority file outside project root ignored: %s" line)
        nil))))

(defun priority-files--collect-matches (glob root)
  "Return absolute file matches for GLOB relative to ROOT."
  (let* ((pattern (expand-file-name glob root))
         (matches (file-expand-wildcards pattern t)))
    (cl-remove-if-not #'file-regular-p matches)))

(defun priority-files--latest-match (matches selector)
  "Select the latest file from MATCHES using SELECTOR (either 'modified or 'created)."
  (let* ((selector (if (memq selector '(modified created)) selector 'modified))
         (annotated-attrs (mapcar (lambda (path)
                                    (cons path (file-attributes path)))
                                  matches))
         (selector (if (and (eq selector 'created)
                            (not (cl-some (lambda (entry)
                                            (priority-files--file-birth-time (car entry) (cdr entry)))
                                          annotated-attrs)))
                       (progn
                         (unless priority-files--birth-time-warning-shown
                           (setq priority-files--birth-time-warning-shown t)
                           (message "Birth times unavailable; falling back to modification time for latest:created"))
                         'modified)
                     selector))
         (annotated (mapcar (lambda (entry)
                              (let* ((path (car entry))
                                     (attrs (cdr entry))
                                     (stamp (pcase selector
                                              ('created (or (priority-files--file-birth-time path attrs)
                                                            (priority-files--file-modification-time attrs)))
                                              (_ (priority-files--file-modification-time attrs)))))
                                (cons path stamp)))
                            annotated-attrs)))
    (car (car (cl-sort annotated
                       (lambda (a b)
                         (time-less-p (cdr b) (cdr a))))))))

(defun priority-files--resolve-latest-entry (mode glob root)
  "Resolve a latest directive with MODE and GLOB relative to ROOT."
  (let* ((normalized (and mode (downcase mode)))
         (selector (cond
                    ((or (null normalized)
                         (string= normalized "modified"))
                     'modified)
                    ((string= normalized "created")
                     'created)
                    (t
                     (message "Unknown latest selector '%s'; supported selectors are modified, created" mode)
                     nil)))
         (matches (and selector (priority-files--collect-matches glob root))))
    (cond
     ((not selector) nil)
     ((null matches)
      (message "No matches found for latest directive: %s" glob)
      nil)
     (t
      (priority-files--latest-match matches selector)))))

(defun priority-files--resolve-entry (line root)
  "Resolve a single LINE from .priority-files for project ROOT.
Returns an absolute file path or nil when LINE should be ignored."
  (cond
   ((string-empty-p line) nil)
   ((string-prefix-p "#" line) nil)
   ((string-match priority-files--latest-directive-regexp line)
    (let ((mode (match-string 1 line))
          (glob (match-string 2 line)))
      (priority-files--resolve-latest-entry mode glob root)))
   (t
    (priority-files--resolve-static-entry line root))))

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
              (let* ((raw-line (buffer-substring-no-properties
                                (line-beginning-position)
                                (line-end-position)))
                     (line (string-trim raw-line))
                     (resolved (priority-files--resolve-entry line project-root)))
                (when resolved
                  (push resolved files)))
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
