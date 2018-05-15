(TeX-add-style-hook
 "output"
 (lambda ()
   (TeX-run-style-hooks
    "latex2e"
    "reproduction"
    "first_run"
    "scrartcl"
    "scrartcl10"
    "longtable"))
 :latex)

