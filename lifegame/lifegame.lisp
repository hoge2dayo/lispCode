

(in-package #:lifegame)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; メイン
;;;; 

;;; 画面更新間隔を過ぎたかどうか判定
;;; @return 
(defun 画面更新間隔を過ぎたか？ ()
  ;; ※定数 internal-time-units-per-second と同じ値は１秒に相当
  (let* ((画面更新時間 (truncate (* *画面更新フレーム間隔* internal-time-units-per-second)
                           *フレームレート*
                           ) ;truncate
                 )
         )
    (or (null *画面更新時刻*)
        (<= 画面更新時間 (- (get-internal-real-time) *画面更新時刻*))
        ) ;or
    ) ;let*
  )

;;; メイン処理
;;; @return 
(defun メイン ()
  (sdl:with-events ()
    (:quit-event () t)

    ;; ここで引数を指定して、キーを取得すべきである。
    ;; 実際にキーが押されてからこのハンドラが呼び出されるまでには時間差がある。
    ;; 引数でキーを取得しなかった場合、ハンドラ内で sdl:keys-down-p などを呼んでも、
    ;; 既にキーが離されていて、押下されたキーが取得できなくなる恐れがある。
    (:key-down-event (:key キー :state ステート)
     (if (or (sdl:key= キー :sdl-key-q)
             (sdl:key= キー :sdl-key-escape)
             ) ;or
         (sdl:push-quit-event)
       (キーハンドラ キー ステート)
       ) ;if
     )

    (:KEY-UP-EVENT (:key キー :state ステート)
     (キーハンドラ キー ステート)
     )

    (:MOUSE-MOTION-EVENT (:x px :y py)
     (マウス移動ハンドラ px py)
     )

    (:MOUSE-BUTTON-DOWN-EVENT (:button ボタン :state ステート)
     (マウスボタンハンドラ ボタン ステート)
     )

    (:MOUSE-BUTTON-UP-EVENT (:button ボタン :state ステート)
     (マウスボタンハンドラ ボタン ステート)
     )

    (:idle ()
     (case *モード*
       (:ゲーム
        (when (画面更新間隔を過ぎたか？)
          (世代交代)
          (空間描画)
          (画面更新)
          ) ;when
        )
       (:編集
        (空間描画)
        (生物カーソル描画)
        (画面更新)
        )
       ) ;case
     )
    ) ;with-events
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; 開始／初期化／終了処理
;;;; 

;;; デフォルトのフォント
(defparameter *デフォルトフォント*
  (make-instance 'sdl:ttf-font-definition
     :size 16
     :filename (merge-pathnames "mplus-2m-regular.ttf" *directory-path*)
    ) ;make-instance
 )

;;; ライフゲーム
;;; @return 
(defun lifegame ()
  (sdl:with-init ()
    (unwind-protect
        (progn
          ;; ---------- 初期化処理
          ;; フォントの設定
          (sdl:initialise-default-font *デフォルトフォント*)

          ;; ウィンドウの作成
          (sdl:window *ウィンドウ幅* *ウィンドウ高さ*)

          ;; フレームレート（[フレーム/秒]）の設定
          (setf (sdl:frame-rate) *フレームレート*)

          ;; 
          (生物画像テーブル作成)

          ;; ---------- メイン
          (メイン)
          ) ;progn

      ;; ---------- 終了処理
      ;; （なし）
      ) ;unwind-protect
    ) ;with-init
  )
