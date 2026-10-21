(asdf:defsystem my-project
  :components ((:file "package")
               (:file "main")
               (:file "skill-tree-scene")
               (:file "title"))
  :depends-on (:trial
               :trial-glfw
               :trial-png
               :trial-alloy))
