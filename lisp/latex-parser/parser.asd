(in-package :asdf)
(defsystem "parser"
  :description "parser: Parse Latex into CLOS classes."
  :version "1.0"
  :author "T. V. Raman <tv.raman.tv@gmail.com>"
  :licence "GPL V2"
  :components
  (
   (:file "buffers")
   (:file "structs")
   (:file "document-component-classes")
   (:file "operator-class-names-mapping")
   (:file "math-classes"
    :depends-on ("document-component-classes" "operator-class-names-mapping"))
   (:file "macros-define-objects"
    :depends-on ("document-component-classes" "math-classes" ))
   (:file "parents-and-siblings"
    :depends-on ("document-component-classes" "math-classes"))
   (:file "table-driver"
    :depends-on ("special-variables" "math-classification"))
   (:file "special-variables" :depends-on ("structs"))
   (:file "math-classification")
   (:file "01-sections"
    :depends-on ("structs" "document-component-classes" "buffers"
                           "math-classes" "special-variables"
                           "math-classification" "table-driver"
                           "02-text-processing"))
   (:file "02-text-processing"
    :depends-on
    ("05-math-processing" "structs" "document-component-classes"
                          "math-classes" "buffers" "special-variables"
                          "math-classification" "table-driver"))
   (
    :file "03-tex-macro-expand"
    :depends-on
    (
     "structs"
     "document-component-classes"
     "math-classes"
     "buffers"
     "special-variables"
     "math-classification"
     "table-driver"
     ))
   (:file "05-math-processing"
    :depends-on
    (
     "structs"
     "document-component-classes"
     "math-classes"
     "buffers"
     "special-variables"
     "math-classification"
     "table-driver"
     ))
   (:file "precedence-of-operators"
    :depends-on
    (
     "math-classification" ))
   (
    :file "precedence-definitions"
    :depends-on
    (
     "precedence-of-operators"))
   (:file "quasi-prefix"
    :depends-on
    (
     "05-math-processing"
     "math-classes"
     "precedence-of-operators"))
   (:file "cardinal-numbers")))
