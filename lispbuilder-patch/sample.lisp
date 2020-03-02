

(in-package #:lispbuilder-patch)

(export '(font-example))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; LISPBUILDER-SDL で日本語表示
;;;; 

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

;;; 日本語フォントの準備
;;; ※フォント mplus-2m-regular.ttf の場合、表示の高さが
;;; 　指定サイズと一致しない様子
(defparameter *ttf-font*
  (make-instance 'sdl:ttf-font-definition
     :size 32
     :filename (merge-pathnames "mplus-2m-regular.ttf" *directory-path*)
    )
 )

;;; 日本語表示のサンプルウィンドウを表示
;;; @return 
(defun font-example ()
  (sdl:with-init ()
    ;; ウィンドウ作成
    (sdl:window 600 96 :title-caption "SDL-TTF Font Example" :icon-caption "SDL-TTF Font Example")
    ;; フレームレートの設定
    (setf (sdl:frame-rate) 30)

    ;; ウィンドウ全体の塗りつぶし
    (sdl:fill-surface sdl:*white* :surface sdl:*default-display*)

    ;; デフォルトフォントを指定
    (unless (sdl:initialise-default-font *ttf-font*)
      (error "FONT-EXAMPLE: Cannot initialize the default font."))

    (sdl:draw-string-solid-*   "Text UTF8 - Solid 日本語テスト"   0  0 :color sdl:*black*)
    (sdl:draw-string-shaded-*  "Text UTF8 - Shaded 日本語テスト"  0 32 sdl:*black* sdl:*yellow*)
    (sdl:draw-string-blended-* "Text UTF8 - Blended 日本語テスト" 0 64 :color sdl:*black*)

    ;; ウィンドウ表示の更新
    (sdl:update-display)

    ;; イベントループ
    (sdl:with-events ()
      ;; ---------- 終了イベント
      (:quit-event () t)
      ;; ---------- ウィンドウ露出イベント
      (:video-expose-event () (sdl:update-display))
      ;; ---------- キーボードイベント
      (:key-down-event ()
       (when (sdl:key-down-p :sdl-key-escape)
         (sdl:push-quit-event) ;終了イベントのプッシュ
        )
       )
     )
   )
 )


