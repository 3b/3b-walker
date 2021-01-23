(defsystem 3b-walker
  :description "code walker lib"
  :version "0.0.1"
  :author "Bart Botta <00003b at gmail.com>"
  :license "MIT"
  :depends-on (alexandria closer-mop)
  :serial t
  :components ((:file "package")
               (:file "walker")
               (:file "copy-node")
               (:file "filter")))
