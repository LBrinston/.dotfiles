;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;      user-mail-address "john@doe.com")

(defcustom my--dotfiles-dir "~/.dotfiles"
"The path to my personal dotfiles."
:type 'string
:group 'dotfiles)

(defcustom my--dotfiles-home "~"
  "The directory in which my-dotfiles-dir should reside, will probably always be ~."
  :type 'string
  :group 'dotfiles)

(defcustom my--dotfiles-org-files '()
  "The list of org-mode files under the `my--dotfiles-dir'
which contain configuration files that should be tangled"
  :type '(list string)
  :group 'dotfiles)

(setq dotcrafter-org-files '("dotfiles.org"))

;; Get the path to home
(defvar my/home (getenv "HOME"))

;; -- Add my personal functions early
(add-load-path!
 "my-elisp.el"
 )

;; -- Require my personal functions
;; ideally I shouldn't have to hard code this??
(require 'my-elisp "~/.dotfiles/.config/doom/my-elisp.el")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font'                -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font'            -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font'         -- for symbols
;; - `doom-serif-font'          -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;; Fire Mono

;; ref: https://emacsredux.com/blog/2021/12/22/check-if-a-font-is-available-with-emacs-lisp/
(defun my-font-available-p (font-name)
  (find-font (font-spec :name font-name)))

(defvar my-font-size 20
  "My preferred font size")

;; -- Pick the preferred font otherwise do nothing and let Doom select it's fallbacks

;; -- Doom font
(cond
 ((my-font-available-p "Fira Mono")
  (setq doom-font (font-spec :family "Fira Mono" :size my-font-size)
        doom-big-font (font-spec :family "Fira Mono" :size 36))
  ))
;; -- Doom Variable pitch font
(cond
 ((my-font-available-p "Overpass")
  (setq doom-variable-pitch-font (font-spec :family "Overpass" :size my-font-size))
  ))
;; -- Doom Symbol font
(cond
 ((my-font-available-p "JuliaMono")
  (setq doom-symbol-font (font-spec :family "JuliaMono" :size my-font-size))
  ))
;; -- Doom Serif font
(cond
 ((my-font-available-p "IBM Plex Sans")
  (setq doom-symbol-font (font-spec :family "IBM Plex Sans" :size my-font-size))
  ))
(cond
 ((my-font-available-p "IBM Plex Sans")
  (setq doom-unicode-font (font-spec :family "IBM Plex Sans" :size my-font-size))
  ))


;; ;; -- Doom emoji font
;; (setq doom-font (font-spec :family "Fira Mono" :size 20)
;;       doom-big-font (font-spec :family "Fira Mono" :size 36)
;;       doom-variable-pitch-font (font-spec :family "Overpass" :size 18)
;;       doom-symbol-font (font-spec :family "JuliaMono")
;;       doom-serif-font (font-spec :family "IBM Plex Sans" :size 22 :weight 'light)
;;       ;;      doom-emoji-font ()
;;       )

;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;;TODO: figure out per-theme overrides
;;TODO: comment-face toggle function: 1. readable vs background colout
;; -- Default is too hard to read
(custom-set-faces!
  `(font-lock-comment-face :foreground "#f5f5f5")
  )

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

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(setq doom-modeline-height 35)

;; When there is a "Time-stamp: <>" string in the first 10 lines of the file,
;; Emacs will write time-stamp information there when saving the file.
;; (Borrowed from http://home.thep.lu.se/~karlf/emacs.html)
(setq time-stamp-active t          ; Do enable time-stamps.
      time-stamp-line-limit 10     ; Check first 10 buffer lines for Time-stamp: <>
      time-stamp-format "Last changed %Y-%02m-%02d %02H:%02M:%02S by %u")
(add-hook 'write-file-hooks 'time-stamp) ; Update when saving.

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/.notes/")

;; Prevents getting an annoying error
(autoload 'org-eldoc-get-src-lang "org-eldoc")

(after! org-mode
(setq org-hide-emphasis-markers t
      org-use-sub-superscripts "{}"
      )
(setopt org-pretty-entities t))

(use-package! org-appear
  :hook '(org-mode . org-appear-mode)  ; Remember: hook implies
  ;;:hook (org-mode . org-appear-mode)  ; Remember: hook implies
  :config
  (setq org-appear-autoemphasis   t)  ; Show bold, italics, verbatim, etc.
  (setq org-appear-autolinks      t)  ; Show links
  (setq org-appear-autosubmarkers t)  ; Show sub- and superscripts
  )

(after! org-superstar

  ;; Provide a list of bullets to use
  ;; sticking with defaults for now
  ;; (setq org-superstar-headline-bultets-list '(?\d))

  (setq org-superstar-special-todo-items t)
  (setq org-superstar-remove-leading-stars t)
  (setq org-hide-leading-stars t)
  (setq org-ellipsis " ▾ ")

  (setq org-superstar-item-bullet-alist
        '((?* . ?•)
          (?+ . ?➤)
          (?- . ?•)))
  ;; Not sure how keen I am on this
  ;; left for reference
  ;; (setq org-superstar-todo-bullet-alist
  ;;       '(("TODO" . ?☐)
  ;;         ("NEXT" . ?✒)
  ;;         ("HOLD" . ?✰)
  ;;         ("WAITING" . ?☕)
  ;;         ("CANCELLED" . ?✘)
  ;;         ("DONE" . ?✔)))
  (defun my/set-org-heading-sizes ()
    "Set org heading sizes after theme loads."
    (let* ((variable-tuple
            (cond ((x-list-fonts "JuliaMono")       '(:font "JuliaMono"))
                  ((x-list-fonts "Overpass")        '(:font "Overpass"))
                  ((x-list-fonts "IBM Plex Sans")   '(:font "IBM Plex Sans"))
                  ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                  (nil (warn "Cannot find a Sans Serif Font."))))
           (base-font-color (face-foreground 'default nil 'default))
           (headline `(:inherit default :weight bold :foreground ,base-font-color)))

      (custom-theme-set-faces
       'user
       `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
       `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
       `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
       `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
       `(org-level-5 ((t (,@headline ,@variable-tuple))))
       `(org-level-6 ((t (,@headline ,@variable-tuple))))
       `(org-level-7 ((t (,@headline ,@variable-tuple))))
       `(org-level-8 ((t (,@headline ,@variable-tuple))))
       `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil)))))))

  (add-hook 'doom-load-theme-hook #'my/set-org-heading-sizes) ;; Apply if we switch themes
  (add-hook 'org-mode-hook #'my/set-org-heading-sizes) ;; Apply if we enter org-mode
  (my/set-org-heading-sizes) ;; Apply on startup (doom sets themes directly on start - doom-load-theme-hook is only called on switching themes)
  ); after!

(setq org-src-window-setup 'reorganize-frame)
(setq org-src-tab-acts-natively t)

(after! org
  ;; (org-babel-lob-ingest (expand-file-name "~/.config/doom/lib-babel.org"))
  ;; When using :comments header I always want an absolute link no relative (default)
  (setq org-babel-tangle-use-relative-file-links nil)
  )

(after! org
  (defun org-babel-noweb-wrap (&optional regexp)
    "Return regexp matching a Noweb reference."
    (let ((result (rx-to-string
                   `(and (or "<<" "#<<")
                         (group
                          (not (or " " "\t" "\n"))
                          (? (*? any) (not (or " " "\t" "\n"))))
                         (or ">>" ">>#")))))
      result)))

(after! org
  (require 'ansi-color)

  (defun my--org-babel-display-ansi-colors ()
    "Process ANSI color codes in code block results."
    (when-let ((beg (org-babel-where-is-src-block-result))
               (end (save-excursion (goto-char beg) (forward-line) (org-babel-result-end))))
      (ansi-color-apply-on-region beg end)))

  (add-hook 'org-babel-after-execute-hook 'my--org-babel-display-ansi-colors)
  )

;; -- Images
(after! org
  (when (display-graphic-p)
    (pixel-scroll-precision-mode t)
    )

  ;; Center all images unless other-wise instructed
  (setq org-image-align 'center)
  )

;; -- Link Formatting
(setq org-download-link-format "[[file:%s]]\n")

;; -- Where to save the images
;; Default so that we *could* provide a file-local-var
(setq-default
org-download-method 'directory
org-download-image-dir
(if (and (buffer-file-name) (file-exists-p (buffer-file-name)))
      (concat ".assets/images/" (file-name-base (buffer-file-name)))
    nil)
;;(concat ".assests/images/" (file-name-base))
org-download-heading-lvl nil)

(setq org-download-abbreviate-filename-function #'file-relative-name)

(setq org-download-timestamp "%Y%m%d-%H%M%S_")

(setq org-download-screenshot-method
      "gnome-screenshot -a -f %sa")

;; This will remove the #+DOWNLOADED annotation
;;(setq org-download-annotate-function (lambda (_) "Return empty string" ""))

(after! org
  (defun get-assets-dir-path ()
    "Return path to .assets directory for current buffer. Creates the directory if it doesn't exist, prompt for confirmation."
    (interactive)
    (let ((file-path (buffer-file-name)))
      (cond
       ;; Case 1 - buffer for an unsaved file
       ((null file-path)
        (message "Buffer is not visiting a file.")
        nil)
       ;; Case 2 - file exists
       (t
        ;; Check if the .assets directory already exists
        (let* ((directory (file-name-directory file-path))
               (assets-dir (concat directory ".assets")))
          (unless (file-directory-p assets-dir)
            (if (yes-or-no-p (format "Create assets directory at %s? " assets-dir))
                (make-directory assets-dir t)
              (setq assets-dir (expand-file-name "~/.notes/assets/"))
              (message "Using default dir: %s" assets-dir))
            )
          assets-dir)))))

  (defun set-assets-dir-path ()
    (interactive)
    (let ((assets-dir (get-assets-dir-path)))
      (when assets-dir
        (customize-set-variable 'org-yank-image-save-method assets-dir))
      ))

  ;; (advice-add 'set-assets-dir-path :before #'yank-media)
  (advice-add 'yank-media :before #'set-assets-dir-path)


  ;; (customize-set-variable 'org-yank-image-save-method (expand-file-name (get-assets-dir-path)))
  ;; -- Only hardcode for debugging purposes
  (customize-set-variable 'org-yank-image-save-method (expand-file-name "~/.notes/.assets"))

  ;; org-yank-image-save-method can accept two args:
  ;; 'attach - use attach (and therefore all configuration of behaviour is done via attach)
  ;; OR a /path/to/a/dir
  (setq org-yank-dnd-method 'file-link)

  ;; org-download came with it's own Delete image at point function, yank-media does not (unless you use attach)
  ;;ref: https://www.reddit.com/r/emacs/comments/tdseci/org_how_can_i_remove_link_at_point_and_trash/
  (defun org-remove-link-and-trash-linked-file ()
    "Remove `org-mode' link at point and trash linked file."
    (interactive)
    (let* ((link (org-element-context))
           (path (org-element-property :path link)))
      (move-file-to-trash path)
      (delete-region (org-element-property :begin link)
                     (org-element-property :end link))
      ))

  (map!
   :leader
   :prefix ("i m" . "Media")
   :n :desc "Yank media" "i"  #'yank-media
   )
  (map!
   ;; :map org-mode-map
   :leader
   :prefix ("i m" . "Media")
   :n :desc "Trash org-link" "d" #'org-remove-link-and-trash-linked-file
   )
  )

(after! org
  (require 'org-id)
)

(after! org-id
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  )

(use-package! org-super-links
  :after org
  :bind (("C-c s s" . org-super-links-link)
         ("C-c s l" . org-super-links-store-link)
         ("C-c s C-l" . org-super-links-insert-link)
         )
  ;; TODO - configure link formatting?
  )
(map!
 :leader
 :prefix ("m l b" . "Backlink")
 :n :desc "org-super-links-store-link" "s" #'org-super-links-store-link
 :prefix ("m l b" . "Backlink")
 :n :desc "org-super-links-insert-link" "i" #'org-super-links-insert-link
 :prefix ("m l b" . "Backlink")
 :n :desc "org-super-links-link" "S" #'org-super-links-link
 :prefix ("m l b" . "Backlink")
 :n :desc "org-super-links-link" "d" #'org-super-links-delete-link
 :prefix ("m l b" . "Backlink")
 :n :desc "org-super-links-convert-to-super" "c" #'org-super-links-convert-link-to-super
 )

(after! org
  ;; HTML can do 6 so can we!
  (setq org-export-headline-levels 6)

  ;; Advice - ask where to save exported org-file
  ;; slight modification of: https://stackoverflow.com/a/47850858
(defun org-export-output-file-name-modified (orig-fun extension &optional subtreep pub-dir)
  (unless pub-dir
    (setq pub-dir (read-directory-name "Export directory: " "exported-org-files/"))
    (unless (file-directory-p pub-dir)
      (make-directory pub-dir)))
  (apply orig-fun extension subtreep pub-dir nil))
(advice-add 'org-export-output-file-name :around #'org-export-output-file-name-modified)
;; Set LaTeXML as the equation converter
;; dep: LaTeXML (apt or site)
(setq org-latex-to-mathml-convert-command
      "latexmlmath %i --presentationmathml=%o")
;; WIP - needs testing and configuration
;; Define path mappings
(setq org-link-path-mappings
      '(("/mount/shared" . "R:")
        ("/home/shared" . "S:")
        ("/mnt/fileserver" . "\\\\fileserver")))

;; Custom function to convert file links for ODT export
(defun my-org-odt-link-converter (path desc format)
  "Convert file paths for Windows when exporting to ODT."
  (when (and (string= format "odt")
             (string-match "^file:" path))
    (let ((file-path (substring path 5))) ; Remove "file:" prefix
      ;; Apply path mappings
      (dolist (mapping org-link-path-mappings)
        (when (string-prefix-p (car mapping) file-path)
          (setq file-path (replace-regexp-in-string
                          (regexp-quote (car mapping))
                          (cdr mapping)
                          file-path))))
      ;; Convert forward slashes to backslashes for Windows
      (setq file-path (replace-regexp-in-string "/" "\\\\" file-path))
      ;; Return the converted path
      (concat "file:" file-path))))

;; Hook it into the ODT exporter
;; Want to do this conditionally
;; (setq org-export-filter-link-functions '(my-org-odt-link-converter))
  )

(defun nix-to-win-path(nix-path)
  (interactive)
  "Takes a unix style path and converts it to a windows UNC path."
  (let
      ((win-path nix-path) ; initialize to the passed path
       (nix-mnt (concat (expand-file-name "~") "/Artemis"))
       (win-mnt "///Artemis")
       (matched nil)
       )
    (let
        (
         ;; Project
         (project "/Project")
         ;; Public
         (public "/Public")
         ;; Add more here
         )
      ;; -- Check if the passed link contains any fileshare paths
      (cond
       ;; Project
       ((string-match-p (concat nix-mnt project) win-path)
        (setq win-path (string-replace (concat nix-mnt project) (concat win-mnt project) win-path))
        (setq matched t))
       ;; Public
       ((string-match-p (concat nix-mnt public) win-path)
        (setq win-path (string-replace (concat nix-mnt public) (concat win-mnt public) win-path))
        (setq matched t)
        )
       ;; Add more here
       ) ;cond
      ); inner-let
    ;; -- Flip all our slashes
    (if matched
        win-path
      nix-path
      )
    ))

(defun my-org-odt-link-filter (text backend info)
  "Convert Unix fileshare paths to Windows UNC paths in ODT export."
  ;; Filters need to return _something_ if we're
  (if (org-export-derived-backend-p backend 'odt)
    ;; Only process if it's a file link
    ;; -- regex the file path out of the odt xml
    (if (string-match "xlink:href=\"file://\\([^\"]+\\)\"" text)

        ;; -- progn is need to bundle our message debug and let* into a single clause under the if (otherwise the let* would be seen as the else clause)
        (progn
        (message "Running filter on: %s" (match-string 1 text)) ;debug

        ;; match-string retrieves the capture groups from string-match
        (let* ((unix-path (match-string 1 text))  ;; Path without file://
               (win-path (nix-to-win-path unix-path)))  ;; Convert path
          (string-replace (concat "file://" unix-path)
                         (concat "file://" win-path)
                         text)
          ) ; let*
        ) ; progn
      ;; Not a file link, return unchanged
      text)))

;;(after! org
;;(use-package! org-latex-preview
;;  :after org
;;  :hook ((org-mode . org-latex-preview-auto-mode))
;;  :config
(after! org
;;(pushnew! org-latex-preview--ignored-faces 'org-list-dt 'fixed-pitch)
  (setq org-latex-preview-numbered     t
        org-startup-with-latex-preview t
        org-latex-preview-width 0.6
        org-latex-preview-processing-indicator 'face
        ;;live previewing
        org-latex-preview-live-preview-fragments t
        org-latex-preview-auto-generate 'live
        org-latex-preview-debounce 0.5
        org-latex-preview-throttle 0.2
        org-latex-preview-live-preview-fragments nil
        ;;previewing preamble
        org-latex-create-formula-image-program 'dvisvgm ;svg looks crisp
        )
  )

(after! org-src
  (add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t))))

(use-package! ox-latex
  :config
  ;; (setq org-latex-pdf-process
  ;;       '("latexmk -pdflatex='%latex -shell-escape -bibtex -interaction=nonstopmode' -pdf -output-directory=%o -f %f"))

  ;; Default packages
  (setq org-export-headline-levels 5)
  ;; NOTE -- DO NOT change the default-packages-alist
  ;; NOTE -- _BE CAREFULE_ trying to configure org-latex-packages-alist
  (add-to-list 'org-latex-packages-alist '("" "xcolor" t))
  )

(use-package! engrave-faces
  :init
  (setq engrave-faces-themes
        '((default .
           (;; faces.el --- excluding: bold, italic, bold-italic, underline, and some others
            (default                             :short "default"             :slug "D"   :foreground "#000000" :background "#ffffff" :family "Monospace")
            (variable-pitch                      :short "var-pitch"           :slug "vp"  :foreground "#000000"                       :family "Sans Serif")
            (shadow                              :short "shadow"              :slug "h"   :foreground "#7f7f7f")
            (success                             :short "success"             :slug "sc"  :foreground "#228b22" :weight bold)
            (warning                             :short "warning"             :slug "w"   :foreground "#ff8e00" :weight bold)
            (error                               :short "error"               :slug "e"   :foreground "#ff0000" :weight bold)
            (link                                :short "link"                :slug "l"   :foreground "#ff0000")
            (link-visited                        :short "link"                :slug "lv"  :foreground "#ff0000")
            (highlight                           :short "link"                :slug "hi"  :foreground "#ff0000")
            ;; font-lock.el
            (font-lock-comment-face              :short "fl-comment"          :slug "c"   :foreground "#b22222")
            (font-lock-comment-delimiter-face    :short "fl-comment-delim"    :slug "cd"  :foreground "#b22222")
            (font-lock-string-face               :short "fl-string"           :slug "s"   :foreground "#8b2252")
            (font-lock-doc-face                  :short "fl-doc"              :slug "d"   :foreground "#8b2252")
            (font-lock-doc-markup-face           :short "fl-doc-markup"       :slug "m"   :foreground "#008b8b")
            (font-lock-keyword-face              :short "fl-keyword"          :slug "k"   :foreground "#9370db")
            (font-lock-builtin-face              :short "fl-builtin"          :slug "b"   :foreground "#483d8b")
            (font-lock-function-name-face        :short "fl-function"         :slug "f"   :foreground "#0000ff")
            (font-lock-variable-name-face        :short "fl-variable"         :slug "v"   :foreground "#a0522d")
            (font-lock-type-face                 :short "fl-type"             :slug "t"   :foreground "#228b22")
            (font-lock-constant-face             :short "fl-constant"         :slug "o"   :foreground "#008b8b")
            (font-lock-warning-face              :short "fl-warning"          :slug "wr"  :foreground "#ff0000" :weight bold)
            (font-lock-negation-char-face        :short "fl-neg-char"         :slug "nc")
            (font-lock-preprocessor-face         :short "fl-preprocessor"     :slug "pp"  :foreground "#483d8b")
            (font-lock-regexp-grouping-construct :short "fl-regexp"           :slug "rc"                        :weight bold)
            (font-lock-regexp-grouping-backslash :short "fl-regexp-backslash" :slug "rb"                        :weight bold)
            ;; org-faces.el
            (org-block                           :short "org-block"           :slug "ob") ; forcing no background is preferable
            (org-block-begin-line                :short "org-block-begin"     :slug "obb") ; forcing no background is preferable
            (org-block-end-line                  :short "org-block-end"       :slug "obe") ; forcing no background is preferable
            ;; outlines
            (outline-1                           :short "outline-1"           :slug "Oa"  :foreground "#0000ff")
            (outline-2                           :short "outline-2"           :slug "Ob"  :foreground "#a0522d")
            (outline-3                           :short "outline-3"           :slug "Oc"  :foreground "#a020f0")
            (outline-4                           :short "outline-4"           :slug "Od"  :foreground "#b22222")
            (outline-5                           :short "outline-5"           :slug "Oe"  :foreground "#228b22")
            (outline-6                           :short "outline-6"           :slug "Of"  :foreground "#008b8b")
            (outline-7                           :short "outline-7"           :slug "Og"  :foreground "#483d8b")
            (outline-8                           :short "outline-8"           :slug "Oh"  :foreground "#8b2252")
            ;; highlight-numbers.el
            (highlight-numbers-number            :short "hl-number"           :slug "hn"  :foreground "#008b8b")
            ;; highlight-quoted.el
            (highlight-quoted-quote              :short "hl-qquote"           :slug "hq"  :foreground "#9370db")
            (highlight-quoted-symbol             :short "hl-qsymbol"          :slug "hs"  :foreground "#008b8b")
            ;; rainbow-delimiters.el
            (rainbow-delimiters-depth-1-face     :short "rd-1"                :slug "rda" :foreground "#707183")
            (rainbow-delimiters-depth-2-face     :short "rd-2"                :slug "rdb" :foreground "#7388d6")
            (rainbow-delimiters-depth-3-face     :short "rd-3"                :slug "rdc" :foreground "#909183")
            (rainbow-delimiters-depth-4-face     :short "rd-4"                :slug "rdd" :foreground "#709870")
            (rainbow-delimiters-depth-5-face     :short "rd-5"                :slug "rde" :foreground "#907373")
            (rainbow-delimiters-depth-6-face     :short "rd-6"                :slug "rdf" :foreground "#6276ba")
            (rainbow-delimiters-depth-7-face     :short "rd-7"                :slug "rdg" :foreground "#858580")
            (rainbow-delimiters-depth-8-face     :short "rd-8"                :slug "rdh" :foreground "#80a880")
            (rainbow-delimiters-depth-9-face     :short "rd-9"                :slug "rdi" :foreground "#887070")
            ;; Diffs
            (diff-added       :short "diff-added"       :slug  "diffa"  :foreground "#4F894C")
            (diff-changed     :short "diff-changed"     :slug  "diffc"  :foreground "#842879")
            (diff-context     :short "diff-context"     :slug  "diffco" :foreground "#525866")
            (diff-removed     :short "diff-removed"     :slug  "diffr"  :foreground "#99324B")
            (diff-header      :short "diff-header"      :slug  "diffh"  :foreground "#398EAC")
            (diff-file-header :short "diff-file-header" :slug  "difffh" :foreground "#3B6EA8")
            (diff-hunk-header :short "diff-hunk-header" :slug  "diffhh" :foreground "#842879")
            )))))

(after! ox-latex
  (setq org-latex-src-block-backend 'engraved)
  )

(after! org
  (use-package! org-fragtog
    :hook (org-mode . org-fragtog-mode)
    :config
    (ignore-error (org-latex-preview))
    )
  )

;; (with-eval-after-load 'org
;;   (require 'org-xopp)
;;   (org-xopp-setup)
;;   )

(setq org-todo-keywords
      '(
        ;; -- General
        (sequence "TODO(t!)" "IN-PROG(i!)" "WAITING(w@/!)" "|" "DONE(d!)")
        (sequence "|" "SUBMITTED(s!)")
        (sequence "|" "CANCALLED(c@)" "DELEGATED(e!)")
        (sequence "IDEA")
        (sequence "PRJ")
        (sequence "REMINDER(!r)")
        (sequence "CALL" "|" "CALLED(!)")
        (seqeunce "EMAIL(m!)" "|" "EMAILED(!)")
        (sequence "GROC" "|" "DONE")
        ;; -- Stuff?
        (sequence "BUY" "|" "SELL")
        ;; -- Media
        (sequence "TO-FIND" "|" "FOUND")
        (sequence "TO-READ" "|" "READ")
        (sequence "TO-WATCH" "|" "WATCHED")
        ;; -- Programming
        (sequence "EMACS")
        (sequence "EMACS-PACKAGE")
        (sequence "EMACS-CONFIG")
        (sequence "ISSUE" "|" "RESOLVED")
        (sequence "BUG" "KNOWCAUSE" "|" "FIXED") 
        (sequence "ERROR")
        ;; -- Work
        (sequence "APPLY" "|" "APPLIED")
        (sequence "QUESTION" "|" "ANSWER")
        (sequence "MEETING" "|" "MINUTES")
        ))

(setq org-todo-keyword-faces
      '(
        ;; -- General
        ("TODO"      . ( :foreground "red"          :weight bold))
        ("IN-PROG"   . ( :foreground "orange"       :weight bold))
        ("WAITING"   . ( :foreground "yellow"       :weight bold))
        ("DONE"      . ( :foreground "green"        :weight bold))
        ("IDEA"      . ( :foreground "deepskyblue1" :weight bold))
        ("CANCELLED" . ( :foreground "gray"         :weight bold))
        ;; -- Media
        ("TO-FIND"   . ( :foreground "yellow1"      :weight bold))
        ("TO-READ"   . ( :foreground "SteelBlue1"   :weight bold))
        ;; -- Emacs
        ("EMACS"     . ( :foreground "purple"       :weight bold))
        ;; -- Misc
        ("PRJ"       . ( :foreground "orange2"      :weight bold))
        ;; -- Media
        ("TO-FIND"   . ( :foreground "yellow1"      :weight bold))
        ("TO-READ"   . ( :foreground "steel blue"   :weight bold))
        ;; -- Programming
        ("EMACS"     . ( :foreground "MediumPurple1":weight bold))
        ("BUG"       . ( :foreground "marron1"      :weight bold))
        ("ERROR"     . ( :foreground "red3"         :weight bold))
        ;; -- Misc
        ("BUY"       . ( :foreground "spring green" :weight bold))
        ("SELL"      . ( :foreground "deep pink"    :weight bold))
        ;; -- Find Work
        ("APPLY"     . ( :foreground "red1"         :weight bold))
        ("APPLIED"   . ( :foreground "green yellow" :weight bold))
        ;; -- Do Work
        ("QUESTION"  . ( :foreground "cyan"         :weight bold))
        ("ANSWER"  .   ( :foreground "pale gren"    :weight bold))
        ("MEETING"  . (  :foreground "goldenrod1"   :weight bold))
        ("MINUTES"  . (  :foreground "yellow2"      :weight bold))
        ))

(setq org-tag-alist
'(
  ;; @ is syntax sugar
  ;;("TAG-NAME" . ?<speed-key>)

  ;; Places
  ("@home" . ?H)
  ("@work" . ?W)
  
  ;; Devices
  ("@computer" . ?C)
  ("@phone" . ?P) 
  
  ;; Activities
  ("@planning" . ?n)
  ("@email" . ?e)
  ("@call" . ?a)
  ("@errands" . ?r)
  ("@cad" . ?c)
  ("@writing" . ?w)
  ))

(after! org
  (setq org-log-done 'time)
  (setq org-log-note-clock-out t)
  (setq org-log-into-drawer "LOGBOOK") ; places state transitions into LOGBOOK drawer

  (setq org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      org-agenda-block-separator #x2501
      org-agenda-compact-blocks t
      org-agenda-start-with-log-mode t)

  ;; Performance
  (setq org-agenda-inhibit-startup t) ; Prevents agenda from triggering startup directives
  ) ; after! org

(after! org-agenda
  (setq org-agenda-custom-commands
        '(
          ;; Tag based commands
          ("u" "Untagged Tasks" tags-todo "-{.*}")
          ("p" "Planning" tags-todo "+@planning" ((org-agenda-overriding-header "Planning Tasks")))
          ;; Specific file based commands
        ;;; Capture file
        ;;; WIP - not working as of 2024-01-12
          ;;   ("c" "Capture File" (
          ;;                     (todo ".*" (org-agenda-overriding-header "Unprocessed Capture Items"))
          ;;                     )
          ;; (org-agenda-files '("~/.notes/capture.org"))
          ;; )
          ("c" "Capture File" (
                               ;; Unprocessed todo items
                               (todo ".*" ((org-agenda-files '("~/.notes/capture.org"))
                                           (org-agenda-overriding-header "Unprocessed Capture Items")))
                               ))
          ;; -- Work
          ("f" "Weekly DONE" (
                              (agenda "" ((org-agenda-overriding-header "Tasks Completed:")
                                          (org-agenda-skip-function '(org-agenda-skip-subtree-if 'nottodo 'done))
                                          (org-agenda-skip-scheduled-if-done nil) ;; don't search scheduled
                                          (org-agenda-skip-timestamp-if-done nil) ;; don't search
                                          (org-agenda-span 7)
                                          (org-agenda-use-time-grid nil)
                                          (setq org-agenda-show-all-dates nil)
                                          ))
                              ))
          ;; ("f" "Weekly DONE and IN-PROGRESS from this week"
          ;;  (agenda ""
          ;;          ((org-agenda-overriding-header "Tasks from this Week")
          ;;           (org-agenda-span 'week)
          ;;           (org-agenda-start-on-weekday 1) ;; Start on Monday
          ;;           (org-agenda-use-time-grid nil)
          ;;           (org-agenda-skip-function
          ;;            (org-agenda-skip-subtree-if 'todo '("DONE" "IN-PROGRESS")))
          ;;           (org-agenda-show-all-dates nil)
          ;;           (org-agenda-skip-scheduled-if-done nil)
          ;;           (org-agenda-skip-timestamp-if-done nil)
          ;;           (org-agenda-start-day "Mon")
          ;;           ))
          ;;  )
          ;;--
          )
        )
  )

(after! org-mode
  (keymap-set org-agenda-mode-map "C-o" #'casual-agenda-tmenu)
  ;; bindings to make jumping consistent between Org Agenda and Casual Agenda
  (keymap-set org-agenda-mode-map "M-j" #'org-agenda-clock-goto)
  (keymap-set org-agenda-mode-map "J" #'bookmark-jump)
  )

;; (after! org
;;   (setq org-super-agenda-groups
;;         '(
;;           (:name "Planning"
;;            :tag "planning"
;;            )
;;           (:name "Today"  ; Optionally specify section name
;;            :time-grid t  ; Items that appear on the time grid
;;            :todo "TODAY")  ; Items that have this TODO keyword
;;           )
;;         )
;;   (org-super-agenda-mode)
;;   )

(after! org
  (setq org-capture-templates
        '(
          ("t" "Todo" entry (file "~/.notes/capture.org")
           "* TODO %?\n  %i\n"
           :empty-lines-after 1)
          ;; -- Freeform - no preset todo keyword
          ("f" "Freeform" entry (file "~/.notes/capture.org")
           "* %?\n  %i\n"
           :empty-lines-after 1)

;; -- Life
("l" "Life")
("lt" "Todo" entry (file "~/.notes/agenda-life.org")
 "* TODO %?\n\n\t SCHEDULED: %^t DEADLINE: %^t\n:PROPERTIES:\n:CREATED:\t%U\n:END:\n%?"
 :empty-lines-after 1)
("lr" "Reminder" entry (file "~/.notes/agenda-life.org")
 "* REMINDER %?\nSCHEDULED: %^t DEADLINE: %^t\n:PROPERTIES:\n:CREATED:\t%U\n:END:\n%?"
 :empty-lines-after 1)
("lg" "Grocery" entry (file "~/.notes/grocery.org")
 "* GROC %^{item}\n\n:PROPERTIES:\n:CREATED:\t%U\n:END:\n%?"
 :empty-lines-after 1)
;;

;; -- School
("s" "School")
("sa" "Assignment" entry (file+headline "~/.notes/agenda-school.org" "Spring 2025")
 "* TODO %^{ECE441|ENGR297|ENGR446} - %^{Assignment|Worksheet} %^{#} \nSCHEDULED: %^t DEADLINE: %^t\n:PROPERTIES:\n:CREATED:\t%U\n:END:\n%?"
 :empty-lines-after 1 :prepend t)
("se" "Exam" entry (file+headline "~/.notes/agenda-school.org" "Spring 2025")
 "* TODO %^{ECE441|ENGR297|ENGR446} - %^{Quiz|Midterm|Final} \nSCHEDULED: %^t DEADLINE: %^t\n:PROPERTIES:\n:CREATED:\t%U\n:END:\n%?"
 :empty-lines-after 1 :prepend t)
("sr" "Reading" entry (file+headline "~/.notes/agenda-school.org" "Spring 2025")
 "* TODO %^{ECE441|ENGR297|ENGR446} - %^{Reading} - %^{Chapter|Pages|Section} \nSCHEDULED: %^t DEADLINE: %^t\n:PROPERTIES:\n:CREATED:\t%U\n:END:\n%?"
 :empty-lines-after 1 :prepend t)

;; ("sl" "499 log" entry (file+headline "~/.notes/agenda-school.org" "work-log")
;;  "* TODO %^{ECE441|ENGR297|ENGR446} - %^{Reading} - %^{Chapter|Pages|Section} \nSCHEDULED: %^t DEADLINE: %^t\n:PROPERTIES:\n:CREATED:\t%U\n:END:\n%?"
;;  :empty-lines-after 1 :prepend t)

;; -- Journal
("j" "Journal" entry (file+olp+datetree "~/.notes/journal.org")
 "* %U \n %?%i \n"
 :empty-lines-after 1)

("b" "Bookmark" entry (file "~/.notes/bookmarks.org")
 "* [[%x][%^{name}]] %^g\n:PROPERTIES:\n:CREATED: %U\n:END:")

("m" "Media")
("mm" "Movie" entry (file+headline "~/.notes/media.org" "Movies")
 "* TO-FIND Movie - %^{Title}\n:PROPERTIES:\n\t:CREATED:\t%U\n:END:\n%?"
 :empty-lines-after 1)
("mb" "Book" entry (file+headline "~/.notes/media.org" "Books")
 "* TO-FIND Books - %^{Title}\n:PROPERTIES:\n\t:CREATED:\t%U\n:END:\n%?"
 :empty-lines-after 1)
("mp" "Pocast" entry (file+headline "~/.notes/media.org" "Podcasts")
 "* TO-FIND Podcast - %^{Title}\n:PROPERTIES:\n\t:CREATED:\t%U\n:END:\n%?"
 :empty-lines-after 1)

;; -- Emacs
("e" "Emacs")
;; -- A Package to consider
("ep" "Emacs Package" entry (file+headline "~/.notes/emacs.org" "Improvements")
 "* EMACS-PACKAGE - %^{Package name}\n:PROPERTIES:\n:CREATED:\t%U\n:END:\n%?"
 :empty-lines-after 1 :prepend t)
;; -- A Package to consider with a link (assumed to be a web link)
("ew" "Emacs Package with link" entry (file+headline "~/.notes/emacs.org" "Improvements")
 "* EMACS-PACKAGE - %^{Package name}%^g\n:PROPERTIES:\n:CREATED:\t%U\n:END:\n[[%x][%^{Link description}]]\n%?"
 :empty-lines-after 1 :prepend t)
;; -- Configuration to perform
("ec" "Emacs Config" entry (file+headline "~/.notes/emacs.org" "Improvements")
 "* EMACS-CONFIG Org-mode - %^{Config}%^g\n:PROPERTIES:\n:CREATED:\t%U\n:END:\n%?"
 :empty-lines-after 1 :prepend t)
;; -- Config to perform with link (assumed to be a file system link)
("ei" "Emacs Config Link" entry (file+headline "~/.notes/emacs.org" "Improvements")
 "* EMACS-CONFIG Org-mode - %^{Config}\n:PROPERTIES:\n:CREATED:\t%U\n:END:\n%?\n%a"
 :empty-lines-after 1 :prepend t)
;; -- Emacs lisp to write
("el" "Emacs TODO" entry (file+headline "~/.notes/emacs.org" "Improvements")
 "* EMACS-LISP %^{Title}\n\n:PROPERTIES:\n:CREATED:\t%U\n:END:\n"
 :empty-lines-after 1 :prepend t)
;; -- Programming
("p" "Programming")
("pe" "Emacs TODO" entry (file+headline "~/.notes/emacs.org" "Improvements")
 "* EMACS %^{Title}\n\n:PROPERTIES:\n:CREATED:\t%U\n:END:\n %a"
 :empty-lines-after 1)

;; -- Work
("w" "Work")
;; - Find
("wa" "Job Application" entry (file+headline "~/.notes/employment.org" "2025 Job Hunt")
 "* APPLY [[%x][%^{Application Name}]]\n:PROPERTIES:\n:CREATED:\t%U\n:END:\n%?"
 :empty-lines-after 1 :prepend t)
;; -- Do
;; NOTES
("wn" "Work Notes")
("wnn" "Note" entry (file "~/.notes/agenda-work.org")
 "* NOTE %^{Note Title} - %^{Note Description}\n:PROPERTIES:\n:CREATED:\t%U\n:END:\n%?"
 :empty-lines-after 1 :prepend t)
("wnl" "Note with link" entry (file "~/.notes/agenda-work.org")
 "* NOTE %^{Note Title} - %^{Note Description}\n:PROPERTIES:\n:CREATED:\t%U\n:END:\n[[%^{Link}][%^{Description}]]\n%?"
 :empty-lines-after 1 :prepend t)
("wnf" "Note with file link" entry (file "~/.notes/agenda-work.org")
 "* NOTE %^{Note Title} - %^{Note Description}\n:PROPERTIES:\n:CREATED:\t%U\n:END:\n%A\n%?"
 :empty-lines-after 1 :prepend t)
;; ISSUE
("wi" "Work Issue")
("wii" "Issue" entry (file "~/.notes/agenda-work.org")
 "* ISSUE %^{Project name} - %^{Issue name} - %^{Issue description}\n:PROPERTIES:\n:CREATED:\t%U\n:END:\n%?"
 :empty-lines-after 1 :prepend t)
("wil" "Note with link" entry (file "~/.notes/agenda-work.org")
 "* ISSUE %^{Note Title} - %^{Note Description}\n:PROPERTIES:\n:CREATED:\t%U\n:END:\n[[%^{Link}][%^{Description}]]\n%?"
 :empty-lines-after 1 :prepend t)
;; BUG
("wb" "Bug" entry (file "~/.notes/agenda-work.org")
 "* BUG %^{Project name} - %^{Bug description}\n:PROPERTIES:\n:CREATED:\t%U\n:END:\n%?"
 :empty-lines-after 1 :prepend t)
;; QUESTION
("wq" "Question" entry (file "~/.notes/agenda-work.org")
 "* QUESTTION %^{Project name} - %^{Bug description}\n:PROPERTIES:\n:CREATED:\t%U\n:END:\n%?"
 :empty-lines-after 1 :prepend t)
 ;; TODO
("wt" "Work todos")
("wtt" "TODO" entry (file "~/.notes/agenda-work.org")
 "* TODO %^{Project Name} - %^{Task description}\n:PROPERTIES:\n:CREATED:\t%U\n:END:\n%?"
 :empty-lines-after 1 :prepend t)
("wtl" "TODO" entry (file "~/.notes/agenda-work.org")
 "* TODO %^{Project Name} - %^{Task description}\n:PROPERTIES:\n:CREATED:\t%U\n:END:\n[[%^{Link}][%^{Description}]]\n%?"
 :empty-lines-after 1 :prepend t)
("wtf" "TODO" entry (file "~/.notes/agenda-work.org")
 "* TODO %^{Project Name} - %^{Task description}\n:PROPERTIES:\n:CREATED:\t%U\n:END:\n%As\n%?"
 :empty-lines-after 1 :prepend t)

;; -- Test
("z" "Test capture" entry (file "~/.notes/test.org")
 "* TODO %^{PROMPT|Test1|Test2|Test3}\n\%?"
 :empty-lines-after 1)
))
)

(use-package! doct
  :commands doct)

;; (after org!
;;        (setq org-capture-templates
;;              (doct `(("Personal todo" :keys "t"
;;                       :icon ("nf-oct-checklist" :set "octicon" :color "green")
;;                       :file +org-capture-todo-file
;;                       :prepend t
;;                       :headline "Inbox"
;;                       :type entry
;;                       :template ("* TODO %?"
;;                                  "%i %a"))))))

(after! org
  (setq org-default-notes-file "~/.notes/capture.org")
  (setq org-refile-targets
        '((nil :maxlevel . 6)
          (org-agenda-files :maxlevel . 6))
        )
  )

(after! org-ql
  (transient-define-prefix my/org-ql-cheatsheet ()
    "Ripgrep cheatsheet"
    ["Syntax"
     ("todo:" "TODO - " ignore)
     ("ts:" "Timestamp - ts:on=today" ignore)
     ("ts-active:" "Timestamp - from=<yyyy-mm-dd>,to=<yyyy-mm-dd>" ignore)
     ("heading:" "Heading - " ignore)
     ("src:" "Source block - lang=<lang>" ignore)
     ("tags:" "-w pattern - Match whole words" ignore)]
    ["Logic"
     ("!" "Inversion" ignore)
     ("<,<=,>,>=,=" "Comparison as you would expect" ignore)
     ("$" "'pattern$' - End of line" ignore)
     ("|" "'foo|bar' - Foo OR bar" ignore)]
    ))

  (map! :map minibuffer-local-map
        "C-c ?" #'my/org-ql-cheatsheet)

(use-package! org-pandoc-import
  :after org)

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; -- Personal Configuration ------------------------------------------------------------------

;;-- Avy
  (map!
   :after avy
   :leader
   :n
   "s w" #'avy-goto-word-1
   :desc "Jump to a word!"
   ;;#'avy-goto-line
   )

(after! aphelia
  (map! :leader
        "b f" #'apheleia-format-buffer)

  ;; Add the yq formatter
  ;; NOTE: this is effectively what is already in the apheleia-formatters
  (setf (alist-get 'yq apheleia-formatters)
        '("yq" "--prettyPrint" "--no-colors" "--no-doc" "--input-format" "yaml"
          "--output-format" "yaml" (apheleia-formatters-indent nil "--indent")))

  (setf (alist-get 'yaml-mode apheleia-mode-alist)
        '(yq))
  )

(setq projectile-mode-line-function '(lambda () (format " Proj[%s]" (projectile-project-name))))

(setq projectile-per-project-compilation-buffer t)

;; -- Consult --
  (map!
   :after consult
   ;; 'mode-specific-map'
   :prefix "C-c"
   "M-x"   #'consult-mode-command
   "h"     #'consult-history
   "k"     #'consult-kmacro
   "m"     #'consult-man
   "i"     #'consult-info
   :map 'Info-mode-map :desc "Consult info search" [remap Info-search] #'consult-info
   ;; C-x bindings in `ctl-x-map'
   :prefix "C-x"
   "M-:"   #'consult-complex-command     ;; orig. repeat-complex-command
   "b"     #'consult-buffer              ;; orig. switch-to-buffer
   "4 b"   #'consult-buffer-other-window ;; orig. switch-to-buffer-other-window
   "5 b"   #'consult-buffer-other-frame  ;; orig. switch-to-buffer-other-frame
   "t b"   #'consult-buffer-other-tab    ;; orig. switch-to-buffer-other-tab
   "r b"   #'consult-bookmark            ;; orig. bookmark-jump
   "p b"   #'consult-project-buffer      ;; orig. project-switch-to-buffer
   ;; Custom M-# bindings for fast register access
   "M-#"   #'consult-register-load
   "M-'"   #'consult-register-store          ;; orig. abbrev-prefix-mark unrelated
   "C-M-#" #'consult-register
   ;; Other custom bindings
   "M-y"   #'consult-yank-pop                ;; orig. yank-pop
   ;; M-g bindings in `goto-map'
   :prefix "M-g"
   "e"     #'consult-compile-error
   "f"     #'consult-flycheck               ;; Alternative: consult-flyamake
   "g"     #'consult-goto-line             ;; orig. goto-line
   "M-g"   #'consult-goto-line           ;; orig. goto-line
   "o"     #'consult-outline               ;; Alternative: consult-org-heading
   "m"     #'consult-mark
   "k"     #'consult-global-mark
   "i"     #'consult-imenu
   "I"     #'consult-imenu-multi
   ;; M-s bindings in `search-map'
   :map 'override
   :prefix "M-s"
   "d"     #'consult-find                  ;; Alternative: consult-fd
   "c"     #'consult-locate
   "g"     #'consult-grep
   "G"     #'consult-git-grep
   "r"     #'consult-ripgrep
   "l"     #'consult-line
   "L"     #'consult-line-multi
   "k"     #'consult-keep-lines
   "u"     #'consult-focus-lines
   ;; Isearch integration
   "e"     #'consult-isearch-history
   :map isearch-mode-map
   "M-e"   #'consult-isearch-history         ;; orig. isearch-edit-string
   "e"     #'consult-isearch-history       ;; orig. isearch-edit-string
   "l"     #'consult-line                  ;; needed by consult-line to detect isearch
   "L"     #'consult-line-multi            ;; needed by consult-line to detect isearch
   ;; Minibuffer history
   ;; :map 'override
   ;; :map minibuffer-local-map
   ;; "M-s"   #'consult-history                 ;; orig. next-matching-history-element
   ;; "M-r"   #'consult-history                ;; orig. previous-matching-history-element
   )

(use-package! ace-window
  :config
  '(aw-leading-char-face
    :foreground "white" :background "red"
    :weight bold :height 2.5 :box (:line-width 10 :color "red"))
  (map! :map global-map
        "M-o" #'ace-window)

  )

(use-package! yasnippet
  :defer t
  :config
  ;; (setq-default yas-snippet-dirs `(,(expand-file-name "snippets/"
  ;;  user-emacs-directory)))
  ;;
  (add-to-list 'yas-snippet-dirs "~/.config/doom/snippets"
               )

  ;; ref: https://orgmode.org/manual/Conflicts.html#Conflicts
  (defun yas/org-very-safe-expand ()
    (let ((yas/fallback-behavior 'return-nil)) (yas/expand))

    (add-hook 'org-mode-hook
              (lambda ()
                (make-variable-buffer-local 'yas/trigger-key)
                (setq yas/trigger-key [tab])
                (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
                (define-key yas/keymap [tab] 'yas/next-field)))

  )
)

(map!
 :map org-mode-map
 :after yasnippet ;; Retain org-mode's native TAB functionality but allow yas-expand when a snippet is available
 :nvi [tab] yas-maybe-expand
 ;; Optionally, bind other keys for snippet navigation
 ;;:nvi "C-c n" #'yas-next-field
 ;;:nvi "C-c p" #'yas-prev-field
 )

(after! evil-multiedit
  (evil-multiedit-default-keybinds) ; for now we cheat and just the defaults

  ;; - For later
  ;; (map!
  ;;  :leader
  ;;  :prefix
  ;;  :n :desc
  ;;  )
  )

;; Different faces for different severities
(after! flymake
  (set-face-attribute 'flymake-error nil :underline '(:style wave :color "red"))
  (set-face-attribute 'flymake-warning nil :underline '(:style wave :color "orange"))
  (set-face-attribute 'flymake-note nil :underline '(:style wave :color "blue"))

    (setq flymake-diagnostic-at-point-display-diagnostic-function
        'flymake-diagnostic-at-point-display-diagnostic-buffer)
  )

(after! tramp

;; Hopefully disable tramp cache
;; -- https://emacs.stackexchange.com/questions/12904/tramp-history-files
;; TODO Find more robust method
(setq tramp-persistency-file-name "/tmp/.tramp")

  ;; -- Tramp connection configuration
  ;; How tramp iniates logins

  ;; Try to force bash instead of shell
  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "\\(/ssh\\|/sshx\\):lbrinston@ugls5:")
                     "remote-shell" "/usr/bin/bash"))
  ;; Try to force a login shell
  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "\\(/ssh\\|/sshx\\):lbrinston@ugls5:")
                     "login-args" '(("-l"))))

  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "/sshx:jdoe:")
                     "remote-shell" "/usr/bin/bash"))
  ;; Additional sshx-specific properties
  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "/sshx:jdoe:")
                     "login-shell" "/bin/bash"))

  ;; Ensure sshx uses proper shell arguments
  ;; This is a flag passed to shell
  ;; -c run next arg as command string (this is how we pass a command to a remote host)
  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "/sshx:jdoe:")
                     "shell-command-switch" "-c"))

  ;; -- recentf behaviour
  ;; Classicly TRAMP isn't terribly smart and triggers recentf when a connection is established
  ;; but doesn't know the difference between local and remote.

  ;; Disable automatic cleanup
  (setq recentf-auto-cleanup 'never)
  (setq recentf-keep '(file-exists-p file-readable-p))

  ;; Don't track remote files in recentf at all
  (after! recentf
    (add-to-list 'recentf-keep #'file-remote-p))

  ;; -- Making TRAMP faster (hopefully)
  (setq remote-file-name-inhibit-locks t
        tramp-use-scp-direct-remote-copying t
        remote-file-name-inhibit-auto-save-visited t)

  (setq tramp-copy-size-limit (* 1024 1024) ;; 1MB
        tramp-verbose 2)

  ;; -- Using direct asynchronous processes
  ;; Under consideration - this doesn't play nice with LSPs

  ;; (connection-local-set-profile-variables
  ;;  'remote-direct-async-process
  ;;  '((tramp-direct-async-process . t)))

  ;; (connection-local-set-profiles
  ;;  '(:application tramp :protocol "scp")
  ;;  'remote-direct-async-process)

  ;; (setq magit-tramp-pipe-stty-settings 'pty) ; necessary for magit to play nice with async
  )

(use-package! jinx
  ;; :hook (emacs-startup . global-jinx-mode)
  :hook ((prog-mode . jinx-mode)
         (org-mode  . jinx-mode)
         (text-mode . jinx-mode))
  ;; :config
  ;; (add-hook 'emacs-startup-hook #'global-jinx-mode)

  ;; consider setting jinx-exclude-faces
  :init
  (add-hook 'org-mode-hook #'jinx-mode)
  (add-hook 'text-mode-hook #'jinx-mode)
  :config
  (global-jinx-mode 1)


  ;; This sounds neat but I should learn to use the abbrev system to start
  ;;   (defun jinx--add-to-abbrev (overlay word)
  ;;     "Add abbreviation to `global-abbrev-table'.
  ;; The misspelled word is taken from OVERLAY.  WORD is the corrected word."
  ;;     (let ((abbrev (buffer-substring-no-properties
  ;;                     (overlay-start overlay)
  ;;                     (overlay-end overlay))))
  ;;       (message "Abbrev: %s -> %s" abbrev word)
  ;;       (define-abbrev global-abbrev-table abbrev word)))

  ;;   (advice-add 'jinx--correct-replace :before #'jinx--add-to-abbrev)

  )

;; (add-hook 'emacs-startup-hook #'global-jinx-mode)

(map!
 :map jinx-mode-map
 "M-$" #'jinx-correct-nearest
 "C-M-$" #'jinx-languages)

;; biblio
(after! citar
  (setq! citar-bibliography '("~/Documents/references/references.bib"))
  ;(setq! citar-library-paths '("~/references/library/files"))
  ;(setq! citar-notes-paths '("~/references/notes"))
  )

;; -- General dap-mode --
(after! dap-mode
  (dap-ui-mode 1)
  (dap-tooltip-mode 1) ; This does not seem to work
  ;;(tooltip-mode 1)                      ; Tooltip hover - otherwise minibuffer
  (map! :map dap-mode-map
        :leader
        :prefix "d"
        "n"   #'dap-next
        "i"   #'dap-step-in
        "o"   #'dap-step-over
        "c"   #'dap-continue
        "a"   #'dap-breakpoint-toggle
        )
  )

(use-package! matlab
  :config
  ;; associate .m file with the matlab-mode (major mode)
  (add-to-list 'auto-mode-alist '("\\.m$" . matlab-mode))

  ;; setup matlab-shell
  ;; swap out for symlink in /usr/local/bin
  (setq matlab-shell-command "/usr/local/bin/matlab")
  ;;  (setq matlab-shell-command "/usr/local/MATLAB/R2023a/bin/matlab")
  (setq matlab-shell-command-switches (list "-nodesktop" "-nosplash"))

  (setq matlab-indent-function t)
  ;; setup mlint for warnings and errors highlighting
  ;; (setq add-to-list 'mlint-programs "/usr/local/MATLAB/R2023a/bin/glnxa64/mlint") ;; add mlint program for Linux
  (setq org-babel-default-header-args:matlab
        '((:session . "*MATLAB*")))


  (defun matlab-org-session-advice (orig-fun &rest args)
    "Advice for org to reuse the *MATLAB* buffer"
    ;; ob-octave.el leverages both org-babel-matlab-emacs-link-wrapper-method and
    ;; org-babel-octave-wrapper-method when interacting with the *MATLAB* buffer.
    ;; Here we fix a couple items such as adding cd default-directory:
    (setq org-babel-matlab-emacs-link-wrapper-method
          (concat "\
    cd('" default-directory "');
    %s
    if ~exist('ans','var') ans=''; end
    if ischar(ans), fid = fopen('%s', 'w'); fprintf(fid, '%%s\\n', ans); fclose(fid);
    else, save -ascii %s ans
    end
    delete('%s')
    "))
    (setq org-babel-octave-wrapper-method
          (concat "\
    cd('" default-directory "');
    %s
    if ~exist('ans','var') ans=''; end
    if ischar(ans) || isstring(ans), fid = fopen('%s', 'w'); fprintf(fid, '%%s\\n', ans); fclose(fid);
    else, dlmwrite('%s', ans, '\\t')
    end"))
    (apply orig-fun args))

  (defun matlab-org-fixup-print (orig-fun session body result-type &optional matlabp)
    "Fixup figure print to make it work with MATLAB"
    ;; org 9.3 correctly does:     print -dpng figure.png
    ;; org 9.6.1 incorrectly does: print -dpng "figure.png"
    ;; and thus 9.6.1 creates on disk a file name containing quotes which is incorrect, so this
    ;; advice fixes that.
    (setq body (replace-regexp-in-string "^\\(print -dpng \\)\"\\([^\"]+\\)\"" "\\1\\2"  body t))
    (funcall orig-fun session body result-type matlabp))

  (defun org-export-dispatch-no-babel-advice (orig-fun &rest args)
    "Instruct babel to not evaluate code blocks (and hence no
prompt) during export, e.g. conversion of org to say html."
    (let* ((org-babel-default-header-args
            (cons '(:eval . "never-export") org-babel-default-header-args))
           (result (apply orig-fun args)))
      result))

  (eval-after-load 'ox
    '(progn
       ;; Make C-c C-e `org-export-dispatch' work without prompting to evaluate code blocks
       (advice-add 'org-export-dispatch :around #'org-export-dispatch-no-babel-advice)))

  ;; org babel for matlab - make all matlab code blocks execute in the same *MATLAB* session
  (eval-after-load "org"
    '(progn
       (advice-add 'org-babel-octave-evaluate-external-process :around #'matlab-org-session-advice)
       (advice-add 'org-babel-octave-evaluate-session :around #'matlab-org-session-advice)
       (advice-add 'org-babel-octave-evaluate :around #'matlab-org-fixup-print)))
  ;; src: https://github.com/mathworks/Emacs-MATLAB-Mode/blob/default/examples/matlab-and-org-mode/matlab-and-org-mode.org

  )

;; -- Python -- ;;

;; (after! python-mode
;;   (setq-hook! 'python-mode-hook +format-with 'black)
;;  (set-formatter! 'black '("black" "-") :modes '(python-mode))
;;   )

;; -- Linting
;; (add-hook 'python-mode-hook #'(lambda () (setq flycheck-checker 'python-pylint))) ;
;;(setq flycheck-python-pylint-args '("--py-version=3.10"))

;; -- Formatting
(after! lsp-mode
  ;; (setq lsp-pylsp-plugins-yapf-enabled t)
  ;; (setq lsp-pylsp-plugins-autopep8-enabled t)
  ;; (setq lsp-pylsp-plugins-black-enabled nil)


(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)  ;; clangd is fast

  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-modeline-code-actions-enable t)
  (setq lsp-enable-suggest-server-download nil)
  (setq lsp-warn-no-matched-clients nil)
  (setq lsp-idle-delay 0.1)
  ;; (gc-cons-threshold (* 100 1024 1024))
  ;; -- Args for clangd
  (setq lsp-clients-clangd-args
   '(
     ;;"-j=4" ;; for some reason this doesn't override the default -j=2 that is passed from somewhere
     "--header-insertion=never"
     "--all-scopes-completion"
     "--background-index"
     "--clang-tidy"
     "--compile-commands-dir=build"
     "--cross-file-rename"
     "--suggest-missing-includes"))
  )  ;; Disable LSP indentation

(after! dap-mode
  (setq dap-python-debugger 'debugpy)
  (setq dap-python-executable "python3")
  (setq dap-print-io t)
  (setq dap-python-debugger-break-on-exception t)

(dap-register-debug-template
 "Debug Server"
 (list :type "python"
       :args "-i"
       :cwd nil
       :env '(("DEBUG" . "1"))
       :target-module (expand-file-name "~/src/myapp/.env/bin/myapp")
       :request "launch"
       :name "My App"))
)

(after! eglot
  :config
  ;; clangd /should/ be an alias for most recent version of clangd installed on system (eg. clangd-18)
  (set-eglot-client! 'cc-mode '("clangd" "-j=3" "--clang-tidy" "/.local/lsp/bin/")) ;; clangd
  ;; -- Per machine lsp locations
  (when (string= (system-name) "terra")
    (setq exec-path (append exec-path `(
                                        (concat(getenv "HOME"))
                                        ))))
  )

(after! babel
  (setq org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar")
  (setq plantuml-default-exec-mode 'jar)
  ;; (setq plantuml-default-exec-mode 'binary)
  )

;; Indent bars mode
(setq
 indent-bars-color '(highlight :face-bg t :blend 0.15)
 indent-bars-pattern "."
 indent-bars-width-frac 0.1
 indent-bars-pad-frac 0.1
 indent-bars-zigzag nil
 indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1) ; blend=1: blend with BG only
 indent-bars-highlight-current-depth '(:blend 0.5) ; pump up the BG blend on current
 indent-bars-display-on-blank-lines t)

;; -- Ideally this is state dependent but this creates a nesting depth error for some reason
(after! vterm
  (when (modulep! :editor evil)
    (set-evil-initial-state! 'vterm-mode 'insert))
  (setq vterm-shell
        (or (executable-find "bash")
            (executable-find "sh")
            (executable-find "docker")
            "/bin/sh"))
  (setq vterm-tramp-shells '(("docker" "/bin/bash")
                             ("ssh" "/bin/bash")
                             ("sshx" "/bin/bash")
                             ("sudo" "/bin/bash")))

  ;; TODO - Figure out how to put vterm into copy mode on i -> m or n -> m

;; (map!
;;  :map vterm-mode-map
;;  "C-y" #'vterm-yank
;;  )

  )
