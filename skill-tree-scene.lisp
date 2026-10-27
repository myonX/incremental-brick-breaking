;(progn (ql:quickload :trial) (ql:quickload :trial-alloy) (ql:quickload :alloy)(asdf:load-systems :my-project)(in-package #:org.my.project))

(in-package #:org.my.project)

(define-shader-pass ui (org.shirakumo.fraf.trial.alloy:base-ui)
                    ())

(defclass skill-tree (pipelined-scene)
  ((score :initarg :score :initform 0 :accessor score)
   (own-machine-lv :initarg :own-machine-lv :initform 0 :accessor own-machine-lv)
   (duplicate-percentage :initarg :duplicate-percentage :initform 0 :accessor duplicate-percentage)
   (ball-speed :initarg :ball-speed :initform 50 :accessor ball-speed)
   (income-multiple :initarg :income-multiple :initform 1 :accessor income-multiple)))

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
                                     :ball-speed (ball-speed scene)
                                     :income-multiple (income-multiple scene))
             (print "Hi")))

(defclass skill-menu (org.shirakumo.fraf.trial.alloy:menuing-panel)
  ((score :initarg :score :initform 0 :accessor score)
   (own-machine-lv :initarg :own-machine-lv :initform 0 :accessor own-machine-lv)
   (duplicate-percentage :initarg :duplicate-percentage :initform 0 :accessor duplicate-percentage)
   (ball-speed :initarg :ball-speed :initform 50 :accessor ball-speed)
   (income-multiple :initarg :income-multiple :initform 1 :accessor income-multiple)))

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
                                          :focus-parent focus))
         (double-income (alloy:represent "Earn double points"
                                         'alloy:button
                                         :layout-parent menu-layout
                                         :focus-parent focus)))
             (labels ((d-and-s->upgrade-cost (lv)
                                             (* 5 (1+ lv)))
                      (o-m-l->upgrade-cost (lv)
                                           (* 1 (1+ lv)))
                      (mul-income->upgrade-cost (mul)
                                                (expt 10 mul))
                 (update ();ボタンの文面の処理
                     (setf (alloy:value wider-own-machine)
                           (cond ((<= (o-m-l->upgrade-cost (own-machine-lv menu)) (score menu))
                                  "Purchase an upgrade to a wider paddle")
                                 ((< (score menu) (o-m-l->upgrade-cost (own-machine-lv menu)))
                                  (format () "Require ~A points" (o-m-l->upgrade-cost (own-machine-lv menu))))))
                     (setf (alloy:value duplicate-ball)
                           (cond ((<= 100 (duplicate-percentage menu)) "max")
                                 ((<= (d-and-s->upgrade-cost (duplicate-percentage menu)) (score menu))
                                  "Increase the probability of duplicating balls")
                                 ((< (score menu) (d-and-s->upgrade-cost (duplicate-percentage menu))) (format nil "Require ~A points" (d-and-s->upgrade-cost (duplicate-percentage menu))))))
                     (setf (alloy:value speed-up)
                           (if (<= (d-and-s->upgrade-cost (/ (- (ball-speed menu) 50) 10)) (score menu))
                               "Increase ball speed"
                             (format nil "Require ~A points" (d-and-s->upgrade-cost (/ (- (ball-speed menu) 50) 10)))))
                     (setf (alloy:value double-income)
                           (if (<= (mul-income->upgrade-cost (income-multiple menu))  (score menu))
                               "Multiply the income"
                             (format nil "Require ~A points" (mul-income->upgrade-cost (income-multiple menu)))))))
            (update)
            (alloy:on (alloy:activate quit) ()
                      (quit *context*))

            (alloy:on (alloy:activate game-start) ();ゲーム開始の処理
                      (change-scene +main+ (make-instance 'game
                                                          :own-machine-lv (own-machine-lv menu)
                                                          :score (score menu)
                                                          :duplicate-percentage (duplicate-percentage menu)
                                                          :ball-speed (ball-speed menu)
                                                          :income-multiple (income-multiple menu))))

            (alloy:on (alloy:activate wider-own-machine) ();幅を広げるボタンの処理
                      (when (and (<= (o-m-l->upgrade-cost (own-machine-lv menu)) (score menu)))
                        (decf (score menu) (o-m-l->upgrade-cost (own-machine-lv menu)))
                        (incf (own-machine-lv menu)))
                      (update))

            (alloy:on (alloy:activate duplicate-ball) ();ボールの分裂
                      (when (and (<= (d-and-s->upgrade-cost (duplicate-percentage menu)) (score menu)) (< (duplicate-percentage menu) 100))
                        (decf (score menu) (d-and-s->upgrade-cost (duplicate-percentage menu)))
                        (incf (duplicate-percentage menu)))
                      (update))

            (alloy:on (alloy:activate speed-up) ();ボールの速度上昇
                      (when (<= (d-and-s->upgrade-cost (/ (- (ball-speed menu) 50) 10)) (score menu))
                        (decf (score menu) (d-and-s->upgrade-cost (/ (- (ball-speed menu) 50) 10)))
                        (incf (ball-speed menu) 10))
                      (update))
            (alloy:on (alloy:activate double-income) ()
                      (when (<= (mul-income->upgrade-cost (income-multiple menu)) (score menu))
                        (decf (score menu) (mul-income->upgrade-cost (income-multiple menu)))
                        (setf (income-multiple menu) (* 2 (income-multiple menu))))
                      (update))
            (alloy:enter menu-layout layout :constraints `((:center :w) (:top 80) (:width 1000) (:height 1000)))
            (alloy:finish-structure menu layout focus))))
