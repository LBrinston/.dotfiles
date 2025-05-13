;;; Compiled snippets and support files for `org-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'org-mode
                     '((">st" "* TODO ${1} - ${1:Assignment} ${2}\n	DEADLINE:  ${3}\n	SCHEDULED: ${3}\n	$0" "school TODO" nil nil nil "/home/bicolor/.my-emacs/snippets/org-mode/school TODO" nil nil)
                       (">tb" "#+title: $1\n#+author: Liam Brinston\n#+date: `(format-time-string \"%Y-%m-%d %H:%M:%S\")`\n#+options: latex-prefer-user-labels:t\n#+options: toc:nil\n#+setupfile: ~/.my-emacs/org-latex-setup.org \n$0" "org-title-block" nil nil nil "/home/bicolor/.my-emacs/snippets/org-mode/org-title-block" nil nil)
                       (">s" "#+BEGIN_SRC ${1}\n	${0}\n#+END_SRC" "org-src-block" nil nil nil "/home/bicolor/.my-emacs/snippets/org-mode/org-src-block" nil nil)
                       (">q" "#+BEGIN_QUOTE\n$0\n#+END_QUOTE" "org-quote" nil nil nil "/home/bicolor/.my-emacs/snippets/org-mode/org-quote" nil nil)
                       (">lb" "#+attr_latex: :float nil\n#+caption: \n\\begin{${1:$$(yas-choose-value '(\"equation\" \"align\"))}}\n	$0\n\\end{$1}\n#+latex: \\bigskip" "org latex block" nil nil nil "/home/bicolor/.my-emacs/snippets/org-mode/org-latex-block" nil nil)
                       (">cap" "  #+attr_html: :width $1% :align center\n  #+attr_latex: :float nil\n  #+caption: $2" "org-caption-image" nil nil nil "/home/bicolor/.my-emacs/snippets/org-mode/org-elisp-block" nil nil)
                       (">ml" "#+attr_latex: :float nil\n#+caption: \n#+BEGIN_SRC matlab :file ${1:figurename}.png :exports ${2:$$(yas-choose-value '(\"both\" \"code\" \"results\" \"both\"))} :results file graphics\n  clear all; close all;\n\n  $3\n#+END_SRC" "matlab-block" nil nil nil "/home/bicolor/.my-emacs/snippets/org-mode/matlab-block" nil nil)
                       (">g" "- [${1:X}] AM Watering\n- [${2:X}] PM Watering\n- [${3:X}] Turned compost\n- [${4:X}] Fed compost\n\n  =Notes=: ${5} \n" "garden-journal" nil nil nil "/home/bicolor/.my-emacs/snippets/org-mode/garden-journal" nil nil)
                       (">d" "#+BEGIN_SRC dot :file ${1:`(concat \"./images/\" (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))`}/${2:`(file-name-nondirectory (buffer-file-name)`}.svg :exports results\n$0\n#+END_SRCe" "dot block" nil nil nil "/home/bicolor/.my-emacs/snippets/org-mode/dot-block" nil nil)))


;;; Do not edit! File generated at Thu Nov 14 21:01:33 2024
