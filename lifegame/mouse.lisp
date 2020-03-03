

(in-package #:lifegame)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; マウス
;;;; 

;;; マウスカーソルの座標
(defstruct (マウス座標 (:include 座標２)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; マウスボタン状態
;;;; 

;;;; ボタンを示す整数
;;;;
;;;; SDL-BUTTON-LEFT     (=1) / SDL-BUTTON-MIDDLE     (=2) / SDL-BUTTON-RIGHT (=3)
;;;; SDL-BUTTON-WHEEL-UP (=4) / SDL-BUTTON-WHEEL-DOWN (=5) / SDL-BUTTON-X1    (=6)
;;;; SDL-BUTTON-X2       (=7)


;;; 各マウスボタンの押下状態。各 bit は各ボタンに対応。
(defparameter *マウスボタン状態* 0)


;;; 指定マウスボタンの押下状態を取得
;;; グローバル変数 *マウスボタン状態* より取得
;;; @param ボタン ボタンを示す整数。
;;; @return 0=放されている / 1=押下
(defun マウスボタン状態 (ボタン)
  (ldb (byte 1 ボタン) *マウスボタン状態*)
  )

;;; 指定マウスボタンの押下状態を設定
;;; グローバル変数 *マウスボタン状態* を更新
;;; @param 押下状態 0=放されている / 1=押下
;;; @param ボタン ボタンを示す整数。
;;; @return 
(defun (setf マウスボタン状態) (押下状態 ボタン)
  (setf (ldb (byte 1 ボタン) *マウスボタン状態*) 押下状態)
  )

;;; 指定マウスボタンの押下状態を取得
;;; @param ボタン ボタンを示す整数。
;;; @param &optional ボタン状態。省略時 *マウスボタン状態*
;;; @return nil=放されている / t=押下
(defun マウスボタン押下状態か (ボタン &optional (ボタン状態 *マウスボタン状態*))
  (logbitp ボタン ボタン状態)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; マウス状態履歴
;;;; 

;;; マウスのボタン状態およびカーソル座標
(defstruct マウス状態
  ;; ボタンの状態
  ボタン状態
  ;; マウスカーソルの座標（構造体「マウス座標」）
  座標
  )

;;; 各要素は構造体「マウス状態」
(defparameter *マウス状態履歴* (make-array 2))


;;; マウス状態履歴を更新
;;; 破棄←［古い状態］←［新しい状態］←グローバル変数からの状態
;;; @return 
(defun マウス状態履歴更新 ()
  (shiftf (aref *マウス状態履歴* 1)
          (aref *マウス状態履歴* 0)
          (make-マウス状態 :ボタン状態 *マウスボタン状態* :座標 *マウスカーソル座標*)
   ) ;shiftf
  )

;;; 指定ボタンを押下しながらマウス移動あった場合に座標を返す
;;; @param ボタン 
;;; @return ２要素のリストあるいは nil
;;; 	リストの場合各要素は構造体「マウス座標」
(defun ボタン押下しながらのマウス移動 (ボタン)
  (let* ((状態1 (aref *マウス状態履歴* 1))
         (状態0 (aref *マウス状態履歴* 0))
         )
    (when (and (マウス状態-p 状態1)
               (マウス状態-p 状態0)
               (マウスボタン押下状態か ボタン (マウス状態-ボタン状態 状態1))
               ) ;and
      (list (マウス状態-座標 状態1) (マウス状態-座標 状態0))
      ) ;when
    ) ;let*
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; マウス移動ハンドラ
;;;; 

;;; マウスカーソルの座標
(defparameter *マウスカーソル座標* nil)

;;; ドラッグ移動を示す線分の色
(defparameter *ドラッグ移動線の色* (sdl:color :r 255 :g 0 :b 0))

;;; ゲームモード時のマウス移動ハンドラ
;;; @param px マウスカーソルの X 座標
;;; @param py マウスカーソルの Y 座標
;;; @return 
(defun マウス移動ハンドラ於ゲーム (px py)
  ;; （なし）
  )

;;; マウスが移動した軌跡に沿って、生物を配置／除去
;;; @param ボタン マウス移動中に押下していたボタン。
;;; 	（例）sdl:sdl-button-left
;;; @param 生物状態 +有+ / +無+
;;; @return 配置あるいは除去した場合 t 。
;;; 	それ以外の場合 nil 。
(defun マウス移動軌跡への生物配置／除去 (ボタン 生物状態)
  (let* ((移動 (ボタン押下しながらのマウス移動 ボタン))
         )
    (when 移動
      (destructuring-bind (始点 終点) ;始点および終点は構造体「マウス座標」
          移動
        (直線補間による生物配置 生物状態
                     (マウス座標-x 始点) (マウス座標-y 始点)
                     (マウス座標-x 終点) (マウス座標-y 終点)
                     ) ;直線補間による生物配置

        (描画要求追加 :line 始点 終点 *ドラッグ移動線の色*)
        ) ;destructuring-bind
      t
      ) ;when
    ) ;let*
  )

;;; 編集モード時のマウス移動ハンドラ
;;; @param px マウスカーソルの X 座標
;;; @param py マウスカーソルの Y 座標
;;; @return 
(defun マウス移動ハンドラ於編集 (px py)
  (or (マウス移動軌跡への生物配置／除去 sdl:sdl-button-right +無+) ;右ボタン押下で生物除去
      (マウス移動軌跡への生物配置／除去 sdl:sdl-button-left +有+)  ;左ボタン押下で生物配置
   ) ;or
  )

;;; マウス移動ハンドラ
;;; @param px マウスカーソルの X 座標
;;; @param py マウスカーソルの Y 座標
;;; @return 
(defun マウス移動ハンドラ (px py)
  ;; 最新 マウスカーソル座標 更新
  (setf *マウスカーソル座標* (make-マウス座標 :x px :y py))
  ;; 新しい座標（上で設定した *マウスカーソル座標* ）を履歴に追加
  (マウス状態履歴更新)

  ;; マウスカーソル座標から生物カーソルの位置を設定
  (生物カーソル座標取得 px py)

  (cond
   ((eql *モード* :ゲーム) (マウス移動ハンドラ於ゲーム px py))
   ((eql *モード* :編集) (マウス移動ハンドラ於編集 px py))
   ) ;cond
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; マウスボタンハンドラ
;;;; 

;;; マウスボタンハンドラ
;;; @param ボタン 
;;; @param 押下状態 +押下+ / +解放+
;;; @return 
(defun マウスボタンハンドラ (ボタン 押下状態)
  ;; 最新 マウスボタン状態（ *マウスボタン状態* ）更新
  (cond
   ;; ---------- 押下
   ((eql 押下状態 +押下+) (setf (マウスボタン状態 ボタン) 1))
   ;; ---------- 解放
   ((eql 押下状態 +解放+) (setf (マウスボタン状態 ボタン) 0))
   ) ;cond

  ;; 新しいボタン状態（ *マウスボタン状態* ）を履歴に追加
  (マウス状態履歴更新)
  )


