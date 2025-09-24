;; [[file:~/.dotfiles/dotfiles.org::*Doctor - =doctor.el=][Doctor         - =doctor.el=:1]]
;;; doctor.el -*- lexical-bindingL t; no-byte-compile: t; -*-
;; Emacs - Check for Prefered Fonts
;; borrowed from tecosaur:
;; https://tecosaur.github.io/emacs-config/config.html#config-doctor
(let (required-fonts available-fonts missing-fonts)
  (setq required-fonts '("Fira ?Mono.*" "Overpass" "JuliaMono" "IBM Plex Sans"))

(setq available-fonts
        (delete-dups
         (or (font-family-list)
             (and (executable-find "fc-list")
                  (with-temp-buffer
                    (call-process "fc-list" nil t nil ":" "family")
                    (split-string (buffer-string) "[,\n]"))))))

  (setq missing-fonts
        (delq nil (mapcar
                   (lambda (font)
                     (unless (delq nil (mapcar (lambda (f)
                                                 (string-match-p (format "^%s$" font) f))
                                               available-fonts))
                       font))
                   required-fonts)))
  (if available-fonts
      (dolist (font missing-fonts)
        (warn! (format "Missing font: %s." font)))
    (warn! "Unable to check for missing fonts, is fc-list installed?")))


;; TODO - Rehome this
(if (string= (shell-command-to-string "xdg-mime query default text/org") "")
  (warn! "text/org is not a registered mime type.")
  (unless (string= (shell-command-to-string "xdg-mime query default text/org") "emacs-client.desktop\n")
    (warn! "Emacs(client) is not set up as the text/org handler.")))
(unless (executable-find "latex2text")
  (warn! "Couldn't find latex2text executable (from pylatexenc), will be unable to render LaTeX fragments in org→text exports."))
;; Doctor         - =doctor.el=:1 ends here
