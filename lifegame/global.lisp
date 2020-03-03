

(in-package #:lifegame)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; グローバル
;;;; 

;;; フレームレート（[フレーム/秒]）
(defparameter *フレームレート* 10)
;;; 画面を更新するフレーム間隔。単位はフレーム数。
(defparameter *画面更新フレーム間隔* *フレームレート*)

;;; 画面を更新した時刻。
;;; 関数 get-internal-real-time からの戻り値。
(defparameter *画面更新時刻* nil)

;;; ウィンドウの幅（ピクセル単位）
(defparameter *ウィンドウ幅* 640)
;;; ウィンドウの高さ（ピクセル単位）
(defparameter *ウィンドウ高さ* 480)

;;; モード（:ゲーム / :編集）
(defparameter *モード* :編集)


;;; キー／マウスボタンの押下
(defconstant +押下+ sdl-cffi::sdl-pressed)
;;; キー／マウスボタンの解放
(defconstant +解放+ sdl-cffi::sdl-released)

;;; このファイルが存在するディレクトリのパス
;;; 註．関数 load は変数 *load-truename* をローディングされている
;;; 　　ファイルの「真の名前」に束縛する。
(defparameter *directory-path*
  (let* ((load-path *load-truename*)
         )
    (make-pathname :host (pathname-host load-path)
                   :directory (pathname-directory load-path)
                   ) ;make-pathname
    ) ;let*
  )


;;; 二次元座標
;;; 構造体「マウス座標」および「生物座標」に継承される。
(defstruct 座標２ x y)


