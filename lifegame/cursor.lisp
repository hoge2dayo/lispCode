

(in-package #:lifegame)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; カーソル関連
;;;; 

;;; カーソルの座標（構造体「生物座標」）
(defparameter *生物カーソル座標* nil)
;;; カーソルの描画色
(defparameter *生物カーソル色* (sdl:color :r 0 :g 255 :b 0))


;;; マウスカーソル座標をカーソル座標に変換して、
;;; グローバル変数 *生物カーソル座標* にセット
;;; @param px ピクセルでの X 座標
;;; @param py ピクセルでの Y 座標
;;; @return 
(defun 生物カーソル座標取得 (px py)
  (let* ((座標 (生物座標 px py))
         )
    (unless (equalp *生物カーソル座標* 座標)
      (setf *生物カーソル座標* 座標)
      ) ;unless
    ) ;let*
  )

;;; グローバル変数 *生物カーソル座標* にカーソル座標が設定されている時、
;;; その X 座標と Y 座標で指定の変数を束縛して body を評価。
;;; @param var1 X 座標に束縛する変数
;;; @param var2 Y 座標に束縛する変数
;;; @param &body body
(defmacro 生物カーソル座標使用 ((var1 var2) &body body)
  `(when *生物カーソル座標*
     (let ((,var1 (生物座標-x *生物カーソル座標*))
           (,var2 (生物座標-y *生物カーソル座標*))
           )
       ,@body
       ) ;let
     ) ;when
  )

;;; カーソル位置（グローバル変数 *生物カーソル座標* の位置）に生物設定
;;; @param 有無 +有+ / +無+
;;; @return 
(defun カーソル位置に生物設定 (有無)
  (生物カーソル座標使用 (x y)
    (setf (生物状態 *新空間* x y) 有無)
    ) ;生物カーソル座標使用
  )

;;; カーソル位置（グローバル変数 *生物カーソル座標* の位置）にカーソル描画
;;; @return 
(defun 生物カーソル描画 ()
  (生物カーソル座標使用 (x y)
    (描画要求追加 :rectangle-* (* x *生物長さ*) (* y *生物長さ*) *生物長さ* *生物長さ*
            *生物カーソル色*
            ) ;描画要求追加
    ) ;生物カーソル座標使用

  (画面更新要求設定)
  )


