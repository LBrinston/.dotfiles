;;; my-elisp.el --- Description -*- lexical-binding: t; -*-
;;
;; Author: Me T-T
;; Maintainer: Me T-T
;; Created: March 01, 2025
;; Modified: March 01, 2025
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defun my--scan-for-output-files (org-file)
  (let ((output-files '())
        (current-match t))
    ;; Get a buffer for the file, either one that is
    ;; already open or open a new one
    (with-current-buffer (or (get-file-buffer org-file)
                             (find-file-noselect org-file ))
      ;; Save the current buffer position
      (save-excursion
        ;; Go back to the beginning of the buffer
        (goto-char (point-min))
        (widen) ; remove any narrowing - just in case buffer is already open
        (message "Scanning buffer: %s" (buffer-name))

        ;; Loop until no more matches are found
        (while current-match
          ;; Search for blocks with a :tangle property
          ;; NOTE nil - silences search-forwards error on no match, would otherwise break the while
          (setq current-match (search-forward ":tangle " nil t))
          (when current-match
            (let ((output-file (thing-at-point 'filename t)))
              ;; If a file path was found, add it to the list
              (unless (or (not output-file)
                          (string-equal output-file "no")
                          (string-equal output-file "yes"))
                (setq output-files (cons output-file
                                         output-files))))))))
    output-files)
  )

;; This needs to be turned into a test function in the next block elow
(let ((output-files '()))
  (dolist (org-file my--dotfiles-org-files)
    (setq output-files
          (append output-files
                  (my--scan-for-output-files
                   (expand-file-name org-file
                                     my-dotfiles-dir)))))
  (message "Output Files: %S" output-files))
;; A simple test for my--scan-for-output-files
(defun test-scan-dotfiles ()
  (interactive)
  (let ((output-files '()))
    (dolist (org-file my--dotfiles-org-files)
      (setq output-files
            (append output-files
                    (my--scan-for-output-files
                     (expand-file-name org-file
                                       my-dotfiles-dir)))))
    (message "Output Files: %S" output-files))
  )

(provide 'my-elisp)
;;; my-elisp.el ends here
