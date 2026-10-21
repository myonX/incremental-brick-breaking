(directional move
             (keys :one-of ((:w :a :s :d))))

;スキルツリーシーンでこのイベントが発火するとゲームシーンに戻る
(trigger sacrifice
  (key :one-of (:space)))

(trigger quit
         (key :one-of (:f1)))
