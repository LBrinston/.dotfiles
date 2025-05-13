;;; Compiled snippets and support files for `LaTeX-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'LaTeX-mode
                     '(("^{" "^{$1}$0" "superscript" nil nil nil "/home/bicolor/.my-emacs/snippets/LaTeX-mode/superscript" nil nil)
                       ("_{}" "_{$1}$0" "subscript" nil nil nil "/home/bicolor/.my-emacs/snippets/LaTeX-mode/subscript" nil nil)
                       ("\\frac" "\\frac{${1:numerator}}{${2:denominator}}$0" "\\frac{numerator}{denominator}" nil
                        ("math")
                        nil "/home/bicolor/.my-emacs/snippets/LaTeX-mode/frac" nil nil)
                       ("e^f" "e^{${1:-}j\\frac{$2\\pi}{$3}}$0" "exp-frac" nil nil nil "/home/bicolor/.my-emacs/snippets/LaTeX-mode/exp-frac" nil nil)))


;;; Do not edit! File generated at Thu Nov 14 21:01:33 2024
