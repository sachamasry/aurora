(TeX-add-style-hook
 "Aurora Development Journal"
 (lambda ()
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "hyperref"
    "fontspec"
    "graphicx"
    "longtable"
    "geometry")
   (LaTeX-add-labels
    "sec:orgfd2a3cc"
    "sec:org8f32310"
    "sec:orgb4eca3b"
    "sec:org69b395b"
    "sec:orgf720fff"
    "sec:org26abce6"
    "sec:orgbc997ed"
    "sec:orgadc7537"
    "sec:org4c6a5c1"
    "sec:org97528dd"
    "sec:org496537c"
    "sec:org15ec158"
    "sec:org813da34"
    "sec:orgd7d353d"
    "sec:org0380305"
    "sec:orge2389cf"
    "sec:org40b9325"
    "sec:orgc7dc460"))
 :latex)

