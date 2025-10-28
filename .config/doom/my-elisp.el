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

(defun copy-face-color-at-point ()
  "Copy the foreground color of the face at point to the kill ring."
  (interactive)
  (let* ((face (get-text-property (point) 'face))
         ;; Sometimes, faces are lists!
         (color (if (listp face)
                    ;; So if they are, create an alist of face:foreground
                   (mapcar (lambda (f) (cons f (face-attribute f :foreground))) face)
                  ;; Otherwise, just one face so grab the :foreground from the face
                 (face-attribute face :foreground))))
    ;; Yoink it to the kill ring.
    (kill-new (format "%s" color))
    ;; Make it clear everything went according to plan.
    (message "Copied to kill ring: %s" color)))

;; Give it a hotkey
(map!
 :leader
 (:prefix ("c" . "code")
  :desc "Copy face color at point" "f" #'copy-face-color-at-point))

(dolist (org-file my--dotfiles-org-files)
(with-current-buffer (get-file-buffer (expand-file-name org-file
                                                        my-dotfiles-dir))
(message "File: %s" (buffer-file-name))))

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

;; [[file:~/.dotfiles/dotfiles.org::my/shell-for-title][my/shell-for-title]]
(defun my/shell-for-title (url)
  (interactive "sURL: ")
  "Calls a bash script to retrieve a title from the passed URL. Returns a formatted org-mode link"
  ;; string-trim removes any \n
  (let* ((title (string-trim (shell-command-to-string (concat "curl-for-title.sh " url))))
         (link (format "[[%s][%s]]" url title)))
    (when (called-interactively-p 'interactive)
      (insert link))
    link))
;; my/shell-for-title ends here

(defun my/org-mklist-headings (hlist)
  "Creates Org-mode headings from passed LIST."
  ;; Check if hlist is nil
  ;; Prompt interactively for list if nil (this way can be interactive or not)
  (interactive "sEnter headings (csv)")
  (when (stringp hlist)
    ;; Split the csv and remove any trailing whitespace (string-trim)
    (setq hlist (mapcar #'string-trim (split-string hlist ",")))
    (dolist (heading hlist)
      (org-insert-heading)
      (insert heading))
      ))

(provide 'my-elisp)
;;; my-elisp.el ends here
