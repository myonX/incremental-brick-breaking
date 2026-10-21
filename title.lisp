(in-package #:org.my.project)

(define-shader-pass ui (org.shirakumo.fraf.trial.alloy:base-ui)
  ())

(defmethod setup-scene ((main main) scene)
           (let ((game (make-instance 'render-pass))
                 (ui (make-instance 'ui))
                 (combine (make-instance 'blend-pass)))
             (connect (port game 'color) (port combine 'a-pass) scene)
             (connect (port ui 'color) (port combine 'b-pass) scene)
             (trial-alloy:show-panel 'menu)
             (print "Hi")))

(defclass hud (org.shirakumo.fraf.trial.alloy:panel) ())

(defmethod initialize-instance :after ((hud hud) &key)
  (let ((layout (make-instance 'org.shirakumo.alloy.layouts.constraint:layout)))
    (alloy:enter "Hello!" layout :constraints `((:left 10) (:top 10) (:size 500 30)))
    (alloy:finish-structure hud layout NIL)))

(defclass menu (org.shirakumo.fraf.trial.alloy:menuing-panel) ())

(defparameter *a* 1)

(defmethod initialize-instance :after ((menu menu) &key)
  (let* ((layout (make-instance 'alloy:vertical-linear-layout))
         (focus (make-instance 'alloy:vertical-focus-list))
         (quit (alloy:represent "Quit" 'alloy:button :layout-parent layout :focus-parent focus))
         (game-start (alloy:represent "Game start" 'alloy:button :layout-parent layout :focus-parent focus)))
    (alloy:on (alloy:activate quit) ()
              (quit *context*))
    (alloy:on (alloy:activate game-start) ()
              (change-scene +main+ (make-instance 'game)))
    (alloy:finish-structure menu layout focus)))
;; (defmethod setup-scene ((main main) scene)
;;            (enter-game-object scene))
;; (b (alloy:represent *a* 'alloy:button :layout-parent layout :focus-parent focus))
