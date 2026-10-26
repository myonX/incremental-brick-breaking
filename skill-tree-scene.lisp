;(progn (ql:quickload :trial) (ql:quickload :trial-alloy) (ql:quickload :alloy)(asdf:load-systems :my-project)(in-package #:org.my.project))

(in-package #:org.my.project)

(define-shader-pass ui (org.shirakumo.fraf.trial.alloy:base-ui)
                    ())

(defclass skill-tree (pipelined-scene)
  ((score :initarg :score :initform 0 :accessor score)
   (own-machine-lv :initarg :own-machine-lv :initform 0 :accessor own-machine-lv)
   (duplicate-percentage :initarg :duplicate-percentage :initform 0 :accessor duplicate-percentage)
   (ball-speed :initarg :ball-speed :initform 50 :accessor ball-speed)))

(defmethod setup-scene ((main main) (scene skill-tree))
           (let ((game (make-instance 'render-pass))
                 (ui (make-instance 'ui))
                 (combine (make-instance 'blend-pass)))
             (connect (port game 'color) (port combine 'a-pass) scene)
             (connect (port ui 'color) (port combine 'b-pass) scene)
             (trial-alloy:show-panel 'skill-menu
                                     :own-machine-lv (own-machine-lv scene)
                                     :score          (score          scene)
                                     :duplicate-percentage (duplicate-percentage scene)
                                     :ball-speed (ball-speed scene))
             (print "Hi")))

(defclass skill-menu (org.shirakumo.fraf.trial.alloy:menuing-panel)
  ((score :initarg :score :initform 0 :accessor score)
   (own-machine-lv :initarg :own-machine-lv :initform 0 :accessor own-machine-lv)
   (duplicate-percentage :initarg :duplicate-percentage :initform 0 :accessor duplicate-percentage)
   (ball-speed :initarg :ball-speed :initform 50 :accessor ball-speed)))

(defparameter *a* 1)

(defmethod initialize-instance :after ((menu skill-menu) &key)
           (let* ((layout (make-instance 'org.shirakumo.alloy.layouts.constraint:layout))
                  (menu-layout (make-instance 'alloy:vertical-linear-layout :cell-margins (alloy:margins 5) :min-size (alloy:size 200 40)))
         (focus (make-instance 'alloy:vertical-focus-list))
         (quit (alloy:represent "Quit" 'alloy:button :layout-parent menu-layout :focus-parent focus))
         (game-start (alloy:represent "Game start" 'alloy:button
                                      :layout-parent menu-layout
                                      :focus-parent focus))
         (wider-own-machine (alloy:represent "Wider own-machine" 'alloy:button
                                             :layout-parent menu-layout
                                             :focus-parent focus))
         (duplicate-ball (alloy:represent "increase propably of Duplicate ball"
                                          'alloy:button
                                          :layout-parent menu-layout
                                          :focus-parent focus))
         (speed-up (alloy:represent "increase speed of ball"
                                          'alloy:button
                                          :layout-parent menu-layout
                                          :focus-parent focus)))
    (labels ((update ();ボタンの文面の処理
                     (setf (alloy:value wider-own-machine)
                           (cond ((<= 5 (score menu))
                                  "Purchase wider own-machine")
                                 ((< (score menu) 5) "need 5 scores")
                                 ((<= 10 (score menu)) "max")))
                     (setf (alloy:value duplicate-ball)
                           (cond ((<= 5 (score menu))
                                  "increase propably of Duplicate ball")
                                 ((< (score menu) 5) "need 5 scores")
                                 ((<= 100 (score menu)) "max")))
                     (setf (alloy:value speed-up)
                           (if (<= 5 (score menu))
                               "increase speed of ball"
                             "need 5 scores"))))
            (update)
            (alloy:on (alloy:activate quit) ()
                      (quit *context*))

            (alloy:on (alloy:activate game-start) ();ゲーム開始の処理
                      (change-scene +main+ (make-instance 'game
                                                          :own-machine-lv (own-machine-lv menu)
                                                          :score (score menu)
                                                          :duplicate-percentage (duplicate-percentage menu)
                                                          :ball-speed (ball-speed menu))))

            (alloy:on (alloy:activate wider-own-machine) ();幅を広げるボタンの処理
                      (when (and (<= 5 (score menu)) (< (own-machine-lv menu) 10))
                        (incf (own-machine-lv menu))
                        (decf (score menu) 5))
                      (update))

            (alloy:on (alloy:activate duplicate-ball) ();ボールの分裂
                      (when (and (<= 5 (score menu)) (< (duplicate-percentage menu) 100))
                        (incf (duplicate-percentage menu))
                        (decf (score menu) 5))
                      (update))

            (alloy:on (alloy:activate speed-up) ();ボールの速度上昇
                      (when (<= 5 (score menu))
                        (incf (ball-speed menu) 10)
                        (decf (score menu) 5))
                      (update))
            (alloy:enter menu-layout layout :constraints `((:center :w) (:top 80) (:width 1000) (:height 200)))
            (alloy:finish-structure menu layout focus))))
