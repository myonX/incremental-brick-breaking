
;(progn (ql:quickload :trial) (ql:quickload :trial-alloy) (asdf:load-systems :my-project)(in-package #:org.my.project))
(in-package #:org.my.project)

(define-shader-pass ui (org.shirakumo.fraf.trial.alloy:base-ui)
  ())

(defmethod setup-scene ((main main) scene)
           (let ((game (make-instance 'render-pass))
                 (ui (make-instance 'ui))
                 (combine (make-instance 'blend-pass)))
             (connect (port game 'color) (port combine 'a-pass) scene)
             (connect (port ui 'color) (port combine 'b-pass) scene)
             (trial-alloy:show-panel 'menu)))

(defclass menu (org.shirakumo.fraf.trial.alloy:menuing-panel) ())

(defmethod initialize-instance :after ((menu menu) &key)
  (let* ((layout (make-instance 'org.shirakumo.alloy.layouts.constraint:layout))
         (menu-layout (make-instance 'alloy:vertical-linear-layout :cell-margins (alloy:margins 5) :min-size (alloy:size 200 40)))
         (focus (make-instance 'alloy:vertical-focus-list))
         (quit (alloy:represent "Quit" 'alloy:button :layout-parent menu-layout :focus-parent focus))
         (game-start (alloy:represent "Start" 'alloy:button :layout-parent menu-layout :focus-parent focus)))
    
    (alloy:on (alloy:activate quit) ()
              (quit *context*))
    (alloy:on (alloy:activate game-start) ()
              (change-scene +main+ (make-instance 'game)))
    (alloy:enter menu-layout layout :constraints `((:center :w) (:bottom 80) (:width 1000) (:height 200)))

    (alloy:finish-structure menu layout focus)))
