(asdf:defsystem my-project
  :components ((:file "package")
               (:file "main")
               (:file "skill-tree-scene")
               (:file "title"))
  :depends-on (:deploy
               :trial
               :trial-glfw
               :trial-png
               :trial-alloy)
  :build-operation "deploy-op"
  :build-pathname "my-project"
  :entry-point "org.my.project:main"
  )
