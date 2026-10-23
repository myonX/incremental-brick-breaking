(ql:quickload :trial)
;(ql:quickload :trial-alloy)
(ql:quickload :alloy)
(asdf:load-systems :my-project)
;; (load "main.lisp")
;; (load "keymap.lisp")
;; (load "main.lisp")
;; (load "package.lisp")
;; (load "skill-tree-scene.lisp")
;; (load "title.lisp")
;; 2. 【重要】保存する前に verbose のバックグラウンドスレッドを停止させる
(when (find-package :verbose)
  (funcall (find-symbol #:remove-global-controller :verbose)))

;; ;; もし他のスレッドも残っている場合に備えて強制終了させる場合（必要に応じて）
#+sb-thread
(dolist (thread (sb-thread:list-all-threads))
  (unless (eq thread sb-thread:*current-thread*)
    (ignore-errors (sb-thread:destroy-thread thread))))

(defun main (&rest argv)
  (declare (ignorable argv))
  (sb-ext:save-lisp-and-die "hello"
                          :toplevel #'org.my.project:main
                          :executable t))

(unless (find-package :roswell)
  (main))
