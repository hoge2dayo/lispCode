

(in-package #:lifegame)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; キー
;;;; 

;;; キーに特化した case
;;; 比較を関数 sdl:key= で行なう
;;; （使用例）
;;; (case-key key
;;;   (:sdl-key-a ...)
;;;   ((:sdl-key-q :sdl-key-escape) ...)
;;;  )
;;; @param val キーを示す値
;;; @param lis-phrase case に於ける節のような節
;;; 	ただし、t や otherwise 節は指定できない。
(defmacro case-key (val &rest lis-phrase)
  (let ((var-val (gensym))
        )
    `(let* ((,var-val ,val)
            )
       (cond
        ,@(mapcar #'(lambda (phrase)
                      (let* ((sym (car phrase))
                             (e (cdr phrase))
                             )
                        (if (symbolp sym)
                            `((sdl:key= ,var-val ',sym) ,@e)
                          `((member ,var-val ',sym :test #'sdl:key=) ,@e)
                          ) ;if
                        ) ;let*
                      ) ;lambda
                  lis-phrase
                  ) ;mapcar
        ) ;cond
       ) ;let*
    ) ;let
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; ハンドラ
;;;; 

;;; ゲームモード時のキーハンドラ
;;; @param キー
;;; @param ステート +押下+ / +解放+
;;; @return 
(defun ゲームに於けるキーハンドラ (キー ステート)
  (cond
   ;; 解放
   ((eql ステート +解放+)
    (case-key キー
       ;; ---------- 更新間隔短縮
       (:sdl-key-j (when (< 1 *画面更新フレーム間隔*)
                     (decf *画面更新フレーム間隔*)
                     ) ;when
        )
       ;; ---------- 更新間隔延長
       (:sdl-key-k (when (< *画面更新フレーム間隔* *フレームレート*)
                     (incf *画面更新フレーム間隔*)
                     ) ;when
        )
       ;; ---------- モード切替
       (:sdl-key-space (setf *モード* :編集))
       ) ;case-key
    )
   ) ;cond
  )

;;; 編集モード時のキーハンドラ
;;; @param キー
;;; @param ステート +押下+ / +解放+
;;; @return 
(defun 編集に於けるキーハンドラ (キー ステート)
  (cond
   ;; 解放
   ((eql ステート +解放+)
    (case-key キー
      ;; ---------- モード切替
      (:sdl-key-space (setf *モード* :ゲーム))
      ;; ---------- 生物を除去
      (:sdl-key-c
       (空間全体から生物除去)
       (空間描画)
       )
      ;; ---------- 生物をランダムで設定
      (:sdl-key-r
       (空間全体への生物乱数配置)
       (空間描画)
       )
      ) ;case-key
    )
   ) ;cond
  )

;;; キーハンドラ
;;; @param キー
;;; @param ステート +押下+ / +解放+
;;; @return 
(defun キーハンドラ (キー ステート)
  (cond
   ((eql *モード* :ゲーム) (ゲームに於けるキーハンドラ キー ステート))
   ((eql *モード* :編集) (編集に於けるキーハンドラ キー ステート))
   ) ;cond
  )

