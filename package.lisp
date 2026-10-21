(defpackage #:org.my.project
  (:use #:cl+trial)
  (:shadow #:main #:launch)
  (:local-nicknames
   (#:v #:org.shirakumo.verbose)
   (#:sequences #:org.shirakumo.trivial-extensible-sequences)
   (#:alloy #:org.shirakumo.alloy)
   (#:trial-alloy #:org.shirakumo.fraf.trial.alloy))
  (:export #:main #:launch))
