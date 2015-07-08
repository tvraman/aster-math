(in-package :user)
(proclaim '(optimize (compilation-speed 3) (safety 3) (speed 2)))

;;; Contains objects from CS-611 Notes.
;;; One file will have object definitions, and the rest reading rules.

;;; First load complete read-aloud system then this system 

(defsystem cs611-notes
    :components(
                
(:file "cs611-object-definitions")
(:file "cs611-macros") 
(:file "cs611-reading-rules"
       :depends-on ( "cs611-object-definitions"
                     "cs611-macros"))))
