

(in-package #:lifegame)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; 生物／空間
;;;; 

;;; 生物の座標
(defstruct (生物座標 (:include 座標２)))

;;; 生物が存在しない状態
(defconstant +無+ 0)
;;; 生物が存在する状態
(defconstant +有+ 1)


;;; 生物が存在できる幅
(defparameter *空間幅* 80)
;;; 生物が存在できる高さ
(defparameter *空間高さ* 57)

;;; 空間の全ての座標についてループする
;;; @param var-x X 座標に束縛される変数
;;; @param var-y Y 座標に束縛される変数
;;; @param &body body
(defmacro do-空間 ((var-x var-y) &body body)
  `(dotimes (,var-y *空間高さ*)
     (dotimes (,var-x *空間幅*)
       ,@body
       ) ;dotimes
     ) ;dotimes
  )


;;; 空間（生物を配置する空間）のデータ構造（配列）を作成
;;; @param w 幅
;;; @param h 高さ
;;; @return 
(defun 空間作成 (w h)
  (make-array (list w h)
              :element-type '(unsigned-byte 8)
              :initial-element +無+
              ) ;make-array
  )

;;; 画面に表示される空間
(defparameter *現世代* (空間作成 *空間幅* *空間高さ*))
;;; 現世代を基に作成される空間
(defparameter *次世代* (空間作成 *空間幅* *空間高さ*))


;;; 生物の幅および高さ（ピクセル単位）
(defparameter *生物長さ* 8)
;;; 生物の各状態（無／有）の画像
;;; （関数「生物画像テーブル作成」で作成）
(defparameter *生物画像テーブル* nil)
;;; 生物の描画色
(defparameter *生物色* (sdl:color :r 255 :g 255 :b 255))


;;; 生物の各状態を示す画像を作成
;;; @return 
(defun 生物画像テーブル作成 ()
  (let* ((元情報 `((,+無+ 1) (,+有+ 3)))
         (最大インデックス (apply #'max (mapcar #'car 元情報)))
         画像
         )
    ;; テーブルとする配列を作成
    (setf *生物画像テーブル* (make-array (1+ 最大インデックス)))

    (dolist (e 元情報)
      (setf (aref *生物画像テーブル* (car e))
            (prog1 (setf 画像 (sdl:create-surface *生物長さ* *生物長さ*))
              (sdl:draw-filled-circle-* 3 3 (cadr e) :surface 画像 :color *生物色*)
              ) ;prog1
            ) ;setf
      ) ;dolist
    ) ;let*
  )

;;; 空間全体（現世代）から生物を除去
;;; @return 
(defun 空間全体から生物除去 ()
  (do-空間 (x y) (setf (生物状態 *現世代* x y) +無+))
  )

;;; 空間全体（現世代）に乱数で生物を配置
;;; @param &optional 疎度。大きいほど配置確率が減る
;;; @return 
(defun 空間全体への生物乱数配置 (&optional (疎度 8))
  (do-空間 (x y)
    (setf (生物状態 *現世代* x y) (if (zerop (random 疎度)) +有+ +無+))
    ) ;do-空間
  )

;;; 空間の要素を取得
;;; @param 空間 
;;; @param x X 座標
;;; @param y Y 座標
;;; @return +無+ / +有+
(defun 生物状態 (空間 x y)
  (aref 空間 (mod x *空間幅*) (mod y *空間高さ*))
  )

;;; 空間の要素に値を設定
;;; @param 値 
;;; @param 空間 
;;; @param x X 座標
;;; @param y Y 座標
;;; @return 
(defun (setf 生物状態) (値 空間 x y)
  (setf (aref 空間 (mod x *空間幅*) (mod y *空間高さ*)) 値)
  )

;;; 空間に於ける指定位置周辺にいる生物の数
;;; @param 空間 
;;; @param x X 座標
;;; @param y Y 座標
;;; @return 生物の数
(defun 周りの生物数 (空間 x y)
  (+ (生物状態 空間 (1- x) (1- y))
     (生物状態 空間     x  (1- y))
     (生物状態 空間 (1+ x) (1- y))
     (生物状態 空間 (1- x)     y )
     (生物状態 空間 (1+ x)     y )
     (生物状態 空間 (1- x) (1+ y))
     (生物状態 空間     x  (1+ y))
     (生物状態 空間 (1+ x) (1+ y))
     ) ;+
  )

;;; 現世代の状態から、次世代の状態を作成／設定し、
;;; その後、次世代を現世代にする
;;; @return 
(defun 世代交代 ()
  ;; 現世代から次世代を設定
  (do-空間 (x y)
    (setf (生物状態 *次世代* x y)
          (case (周りの生物数 *現世代* x y)
            ;; 維持
            (2 (生物状態 *現世代* x y))
            ;; 発生
            (3 +有+)
            ;; その他は死滅
            (t +無+)
            ) ;case
          ) ;setf
    ) ;do-空間

  ;; 次世代を現世代にする
  (rotatef *現世代* *次世代*)
  )

;;; 次世代に於ける指定位置の生物を描画。
;;; （生物がいない場合も描画）
;;; @return 
(defun 生物描画 (x y)
  (let* ((画像 (aref *生物画像テーブル* (生物状態 *現世代* x y)))
         (px (* *生物長さ* x))
         (py (* *生物長さ* y))
         )
    (sdl:draw-surface-at-* 画像 px py)

    (画面更新要求設定)
    ) ;let*
  )

;;; 空間全体を描画
;;; @return 
(defun 空間描画 ()
  (do-空間 (x y) (生物描画 x y))
  )

;;; ピクセル座標を生物座標に変換
;;; @param px ピクセル単位での X 座標
;;; @param py ピクセル単位での Y 座標
;;; @return 
(defun 生物座標 (px py)
  (let* ((x (truncate px *生物長さ*))
         (y (truncate py *生物長さ*))
         )
    (when (and (<= 0 x (1- *空間幅*))
               (<= 0 y (1- *空間高さ*))
               ) ;and
      (make-生物座標 :x x :y y)
      ) ;when
    ) ;let*
  )

;;; ピクセル座標の二点間を結ぶ線分上に、生物を配置／除去する。
;;; @param 有無 +有+ / +無+
;;; @param px0 ピクセル単位での X 座標（始点）
;;; @param py0 ピクセル単位での Y 座標（始点）
;;; @param px1 ピクセル単位での X 座標（終点）
;;; @param py1 ピクセル単位での Y 座標（終点）
;;; @return 
(defun 直線補間による生物配置 (有無 px0 py0 px1 py1)
  (let* ((配置生物座標 nil)
         )
    (flet ((未配置座標への生物配置 (px py)
             (let* ((補間生物座標 (生物座標 px py)) ;ピクセル座標 => 生物座標
                    x
                    y
                    )
               ;; 生物座標で変化があった場合のみに処理
               (unless (or (null 補間生物座標)
                           (equalp 配置生物座標 補間生物座標)
                           ) ;or
                 (setf x (生物座標-x 補間生物座標)
                       y (生物座標-y 補間生物座標)
                       (生物状態 *現世代* x y) 有無
                       ) ;setf

                 (生物描画 x y)
                 (setf 配置生物座標 補間生物座標)
                 ) ;unless
               ) ;let*
             )
           )
      (直線補間呼び出し #'未配置座標への生物配置 px0 py0 px1 py1)
      ) ;flet
    ) ;let*
  )


