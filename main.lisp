;ros dynamic-space-size=4096 --eval '(progn (asdf:load-system :my-project))' main.lisp
;;(progn (ql:quickload :trial) (ql:quickload :trial-alloy) (asdf:load-systems :my-project)(in-package #:org.my.project))

(in-package #:org.my.project)

(defclass main (trial:main) ()
  )

(defclass game (pipelined-scene)
  ((score :initarg :score :initform 0 :accessor score)
   (own-machine-lv :initarg :own-machine-lv :initform 0 :accessor own-machine-lv)
   (duplicate-percentage :initarg :duplicate-percentage :initform 0 :accessor duplicate-percentage)
   (ball-speed :initarg :ball-speed :initform 50 :accessor ball-speed)
   (income-multiple :initarg :income-multiple :initform 1 :accessor income-multiple))
  )


(define-action-set in-game)
(define-action move (directional-action in-game))
(define-action sacrifice (in-game))

;; --- 共通の基底クラス (位置同期と primitive 管理を一本化) ---
(define-shader-entity collision-body (rigidbody vertex-entity colored-entity transformed-entity)
  ((vertex-array :initform NIL)
   (color :initform (vec4 1 1 1 1))))

(defmethod physics-primitive ((body collision-body))
  (aref (physics-primitives body) 0))

(defmethod (setf physics-primitive) ((primitive primitive) (body collision-body))
  (setf (primitive-entity primitive) body)
  (setf (physics-primitives body) (vector primitive))
  (if (vertex-array body)
      (replace-vertex-data (vertex-array body) primitive)
      (setf (vertex-array body) (make-vertex-array primitive NIL))))

(defmethod shared-initialize :after ((body collision-body) slots &key primitive)
           (when primitive (setf (physics-primitive body) primitive)))

;; --- 各クラスの定義 ---
(define-shader-entity break-block (listener collision-body)
  ((color :initform (vec4 1 0 0 1))))

(define-shader-entity own-machine (listener collision-body)
  ((color :initform (vec4 0 0 1 1))))

(define-shader-entity ball (listener collision-body)
  ((color :initform (vec4 0 1 0 1))
   (velocity :initarg :velocity :initform (vec 3 3 0) :accessor velocity)))

(defun calc-own-machine-x-bsize (lv)
  (* 125 (+ 1 (* 0.2 lv))))

(defun enter-game-object (scene)
  ;; ボール: 見た目(scaling)と物理サイズ(radius)を合わせる
  (load-keymap)
  (enter (make-instance 'ball
                        :location (vec 100 100 0)
                        :name :ball
                        :primitive (make-sphere :radius 10.0))
         scene)
  
  ;; 自機: 見た目(250x30)と物理サイズ(bsize = 幅高の半分: 125x15)を合わせる
  (enter (make-instance 'own-machine
                        :location (vec (/ (width *context*) 2) 100 0)
                        :name :own-machine
                        :primitive (make-box :bsize (vec (calc-own-machine-x-bsize (own-machine-lv scene)) 20 50)))
         scene)
  
  ;; ブロック
  (dotimes (i 23)
    (dotimes (j 10)
      (enter (make-instance 'break-block
                            :name :break-block
                            :location (vec (* 80 (+ i 1)) (+ 600 (* 60 j)) 0)
                            :primitive (make-box :bsize (vec 40 30 50)))
             scene)))
  
  
  (enter (make-instance '2d-camera) scene)
  (enter (make-instance 'render-pass) scene))
(defmethod setup-scene ((main main) (scene game))
           ;; (enter (make-instance 'display-controller :font-size 50) scene)
           ;; (observe! (score scene) :title "score")
           ;; (observe! (duplicate-percentage scene) :title "duplicate probably:")
           ;; (observe! (own-machine-lv scene) :title "own machine lv")
           ;; (observe! (ball-speed scene) :title "ball-spped")
           (enter-game-object scene))

(setf +app-system+ "my-project")

(defun launch (&rest args)
  (let ((*package* #.*package*))
    (load-keymap)
    (setf (active-p (action-set 'in-game)) T)
    (apply #'trial:launch 'main args)))


(define-handler (collision-body pre-tick) ()
                (start-frame collision-body))

(defun go-to-skill-tree (scene)
  (change-scene +main+ (make-instance 'skill-tree
                                      :own-machine-lv (own-machine-lv scene)
                                      :score (score scene)
                                      :duplicate-percentage (duplicate-percentage scene)
                                      :ball-speed (ball-speed scene)
                                      :income-multiple (income-multiple scene))))

(defun duplicatep (percentage)
  (<= (random 100.0) percentage))

;; --- ボールの更新ハンドラ ---
(define-handler (ball tick) (dt)
  (let ((ball-loc (location ball))
        (ball-vel (velocity ball))
        (scene (container ball))
        (target-machine (node :own-machine (container ball)))
        (hit-array (make-array 1 :initial-element (make-hit))))
    
    ;; 1. 移動計算
    (nv+* ball-loc ball-vel (* (ball-speed scene) dt))
    
    ;; 移動後のボールのトランスフォーム（行列）を物理判定用に更新
    (start-frame ball)
    
    ;; 2. 壁のバウンス処理
    (when (< (vx ball-loc) 0)
      (setf (vx ball-loc) 0)
      (setf (vx ball-vel) (- (vx ball-vel))))
    (when (< (height *context*) (vy ball-loc))
      (setf (vy ball-loc) (height *context*))
      (setf (vy ball-vel) (- (vy ball-vel))))
    (when (< (width *context*) (vx ball-loc))
      (setf (vx ball-loc) (width *context*))
      (setf (vx ball-vel) (- (vx ball-vel))))
    (when (< (vy ball-loc) 0)
      (leave ball scene))

    ;; 衝突計算および応答用のローカル関数
    (flet ((resolve-collision (target)
             (start-frame target)
             (when (< 0 (org.shirakumo.fraf.trial.gjk:detect-hits
                         (physics-primitive ball)
                         (physics-primitive target)
                         hit-array 0 1))
               (let* ((hit (aref hit-array 0))
                      (normal (hit-normal hit))
                      (depth (hit-depth hit)))
                 ;; めり込み解除（押し出し）
                 (nv+* ball-loc normal depth)
                 ;; 速度の反射
                 (let ((dot (v. ball-vel normal)))
                   (when (< dot 0)
                     (v- ball-vel (v* normal (* 2.0 dot))))))))) ; 衝突した場合はボールの向きを返す

      ;; 3. 自機との衝突判定
      (when target-machine
        (let ((reflect-ball-vel (resolve-collision target-machine)));(vabs)
          (when reflect-ball-vel
            (let* ((paddle-loc (location target-machine))
                   (paddle-width (* 2 (calc-own-machine-x-bsize (own-machine-lv scene))))
                   (t-val (max -1.0 (min 1.0 (/ (- (vx ball-loc) (vx paddle-loc)) (* 0.5 paddle-width)))))
                   (alpha-max (/ pi 3.0))
                   (phi (- (/ pi 2.0) (* t-val alpha-max)))
                   (speed (vlength ball-vel))
                   (reflect (vec3 (* speed (cos phi)) (* speed (sin phi)) 0)))
              (setf (velocity ball) reflect)))))

      ;; 4. ブロックとの衝突判定および破壊処理及びボールの分裂
      (for:for ((entity over scene))
               (when (typep entity 'break-block)
                 (let ((reflect-ball-vel (resolve-collision entity)))
                   (when reflect-ball-vel
                     ;; 衝突が検知されたらそのブロックをシーンから削除
                     (leave entity scene)
                     ;;スコアを加算
                     (incf (score (container ball)) (income-multiple scene))
                     (when (duplicatep (duplicate-percentage scene))
                       (enter-and-load (make-instance 'ball
                                                    :location (location ball)
                                                    :velocity (v* (velocity ball) -1.0)
                                                    :name :ball
                                                    :primitive (make-sphere :radius 10.0))
                                     (container ball)
                                     +main+))
                     (setf (velocity ball) reflect-ball-vel)
                     
                     ;; 1フレームに複数ブロックを同時に叩き割らないようルプを抜ける
                     (return 't)))))

    (setf (vz (location ball)) 0))))
    ;(v:info :trial "------------------------------------")))

(define-handler (own-machine key-press) (key)
  (case key
    (:f (go-to-skill-tree (container own-machine)))))

;; --- 自機の更新ハンドラ --- 自機が画面外に出ないようにする
(define-handler (own-machine tick) (dt)

                ;;ブロックがなくなったら脱出
                (let ((scene (container own-machine)))
                  (when (= 0 (count-if (lambda (e) (typep e 'break-block)) scene))
                    (go-to-skill-tree (container own-machine))))

                ;;ボールがなくなったら脱出
                (let ((scene (container own-machine)))
                  (when (= 0 (count-if (lambda (e) (typep e 'ball)) scene))
                    (go-to-skill-tree (container own-machine))))
    
                (enter (debug-text (vec 0 1100 1)
                                   (format nil "~A" (score (container own-machine)))
                                   :scale 50)
                       (container own-machine))
  (let ((movement (directional 'move))
        (speed 650)
        (b-size (calc-own-machine-x-bsize (own-machine-lv (container own-machine))))
        (width (width *context*))
        (x-location (vx (location own-machine))))
    (cond ((and (< b-size x-location) (< (vx movement) 0))
           (incf (vx (location own-machine)) (* dt speed (vx movement))))
          ((and (< x-location (- width b-size)) (< 0 (vx movement)))
           (incf (vx (location own-machine)) (* dt speed (vx movement))))))
  (setf (vz (location own-machine)) 0))

(define-handler (pipelined-scene quit) ()
                (quit *context*))

(defun main ()
  (launch)
  (print 'hello))
