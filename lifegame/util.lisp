

(in-package #:lifegame)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; ユーティリティ
;;;; 

;;; 直線補間した結果の各座標を引数にして、関数を呼び出す。
;;; @param fnc コールバック関数。ラムダリストは (x y) 。
;;; @param x0 始点の X 座標
;;; @param y0 始点の Y 座標
;;; @param x1 終点の X 座標
;;; @param y1 終点の Y 座標
;;; @return 
(defun 直線補間呼び出し (fnc x0 y0 x1 y1)
  (let* ((w (abs (- x1 x0)))
         (dx (signum (- x1 x0)))
         (h (abs (- y1 y0)))
         (dy (signum (- y1 y0)))

         (x x0)
         (y y0)
         cnt
         )
    (cond
     ;; 横長
     ((<= h w)
      (setf cnt (truncate w 2))
      (dotimes (i (1+ w))
        (funcall fnc x y)
        (incf x dx)
        (when (<= (decf cnt h) 0)
          (incf cnt w)
          (incf y dy)
          ) ;when
        ) ;dotimes
      )
     ;; 縦長
     (t
      (setf cnt (truncate h 2))
      (dotimes (i (1+ h))
        (funcall fnc x y)
        (incf y dy)
        (when (<= (decf cnt w) 0)
          (incf cnt h)
          (incf x dx)
          ) ;when
        ) ;dotimes
      )
     ) ;cond
    ) ;let*
  )


