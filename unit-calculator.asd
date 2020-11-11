(asdf:defsystem #:unit-calculator
  :description "Library for describing and drawing bugscript."
  :author "Isoraķatheð Zorethan <isoraqathedh.zorethan@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:unit-formulas #:alexandria)
  :components ((:file "package")
               (:file "calculator")))
