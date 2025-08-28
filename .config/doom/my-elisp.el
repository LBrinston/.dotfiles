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
        (widen) ; remove any narrowing  just in case buffer is already open
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


;; [[file:~/.notes/emacs.org::*Emac's function or script to look for and pull a title for a link?][Emac's function or script to look for and pull a title for a link?:4]]
(defun shell-for-title (url)
  (interactive "sURL: ")
  "Calls a bash script to retrieve a title from the passed URL. Returns a formatted org-mode link"
  ;; string-trim removes any \n
  (let* ((title (string-trim (shell-command-to-string (concat "curl-for-title.sh " url))))
         (link (format "[[%s][%s]]" url title)))
    (when (called-interactively-p 'interactive)
      (insert link))
    link))
;; Emac's function or script to look for and pull a title for a link?:4 ends here

(provide 'my-elisp)
;;; my-elisp.el ends here
