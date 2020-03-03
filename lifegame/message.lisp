

(in-package #:lifegame)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; メッセージ表示
;;;; 

;;; ゲームモード時の背景色
(defparameter *背景色於ゲーム* sdl:*black*)
;;; 編集モード時の背景色
(defparameter *背景色於編集* sdl:*blue*)
;;; 文字色
(defparameter *文字色* sdl:*white*)


;;; メッセージ表示の各属性
;;; @return プロパティリスト。
;;; 	キーは、:message / :fg-color / :bg-color
(defun メッセージ描画属性 ()
  (case *モード*
    (:ゲーム
     (list :message (format nil "[ゲームモード] j/k:速度調整(~2@a) スペース:モード切替  ESC/Q:終了"
                            *画面更新フレーム間隔*
                            ) ;format
           :fg-color *文字色*
           :bg-color *背景色於ゲーム*
           ) ;list
     )
    (:編集
     (list :message "[編集モード]  C:クリア  R:ランダム配置  スペース:モード切替  ESC/Q:終了"
           :fg-color *文字色*
           :bg-color *背景色於編集*
           ) ;list
     )
    ) ;case
  )

;;; ウィンドウにメッセージを描画
;;; @return 
(defun メッセージ描画 ()
  (let* ((フォント高さ (sdl:get-font-height))
         (x 0)
         (y (- *ウィンドウ高さ* フォント高さ))
         (w *ウィンドウ幅*)
         (h フォント高さ)

         (描画属性リスト (メッセージ描画属性))
         (メッセージ (getf 描画属性リスト :message))
         (文字色 (getf 描画属性リスト :fg-color))
         (背景色 (getf 描画属性リスト :bg-color))
         )
    (when 描画属性リスト
      (sdl:with-color (色 文字色)
        ;; メッセージの背景を塗りつぶす
        (sdl:draw-box-* x y w h :color 背景色)
        ;; メッセージ描画
        (sdl:draw-string-blended-* メッセージ x y :color 色)
        ) ;with-color
      ) ;when
    ) ;let*
  )


