;;;; dot-cons-tree.asd

(asdf:defsystem #:dot-cons-tree
  :description "This is a cons tree visualiser that uses graphviz."
  :author "Jacek Podkanski <ruby.object@googlemail.com>"
  :license  "Public domain"
  :version "0.0.1"
  :depends-on (#:serapeum)
  :serial t
  :components ((:file "package")
               (:file "dot-cons-tree")))
