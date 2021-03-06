

(in-package #:lifegame)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; 画面更新（１）
;;;; 

;;; 画面更新要求フラグ
(defparameter *画面更新要求* nil)


;;; 画面更新を要求する（フラグを設定するのみ）
;;; @return t
(defun 画面更新要求設定 ()
  (setf *画面更新要求* t)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; 描画関連
;;;; 

;;; 線分などの描画要求のリスト
(defparameter *描画要求リスト* nil)


;;; 空間が描画された後で実行される描画の要求を追加
;;; @param 要求 要求を示すリスト
;;; @return 
(defun 描画要求追加 (&rest 要求)
  (push 要求 *描画要求リスト*)

  (画面更新要求設定)
  )

;;; 要求された描画を行なう
;;; @param 要求 描画内容を示すリスト
;;; @return 
(defun 描画要求実行 (要求)
  (case (car 要求)
    ;; ---------- 線分
    (:line
     (destructuring-bind (始点 終点 色)
         (cdr 要求)
       (sdl:draw-line-* (座標２-x 始点) (座標２-y 始点)
                        (座標２-x 終点) (座標２-y 終点)
                        :color 色
                        ) ;draw-line-*
       ) ;destructuring-bind
     )
    ;;  ---------- 矩形（枠）
    (:rectangle-*
     (destructuring-bind (px py 幅 高さ 色)
         (cdr 要求)
       (sdl:draw-rectangle-* px py 幅 高さ :color 色)
       ) ;destructuring-bind
     )
    ) ;case
  )

;;; 全ての要求された描画を実行。
;;; 描画実行後は、全要求を削除。
;;; @return nil
(defun 全描画要求実行 ()
  (dolist (要求 *描画要求リスト*)
    (描画要求実行 要求)
    ) ;dolist

  (setf *描画要求リスト* nil)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; 画面更新（２）
;;;; 

;;; 要求がある場合に画面を更新
;;; @return 
(defun 画面更新 ()
  (when *画面更新要求*
    ;; メッセージ描画
    (メッセージ描画)
    ;; 直線を引くなどの描画
    (全描画要求実行)

    ;; display-surface 更新
    (sdl:update-display)

    ;; 画面更新要求フラグオフ
    (setf *画面更新要求* nil)
    ) ;when

  ;; 画面更新時刻を更新
  (setf *画面更新時刻* (get-internal-real-time))
  )


