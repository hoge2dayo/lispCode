

(in-package #:lispbuilder-patch)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; 日本語の文字（フォント）をも表示できるようにする為のパッチ
;;;; 
;;;; 参考サイト
;;;; masatoi’s blog
;;;; 2012-09-14
;;;; lispbuilder-sdl-ttfで日本語を表示する
;;;; https://masatoi.hateblo.jp/entry/20120914/1347600686
;;;; 


;;; 真の時、日本語の文字を表示できるようにする。
;;; 偽の時、元の処理を行う。（日本語の文字は文字化けする）
(defvar *render-utf8-flag* t)

;;; 元の処理を行うか否かを判断する分岐メソッド（ solid ）
;;; @param string 
;;; @param font
;;; @param color 
;;; @param free 
;;; @param cache 
;;; @return 
(defmethod sdl::_render-string-solid_ :around ((string string) (font ttf-font) (color color) free cache)
  (if *render-utf8-flag*
      (_render-utf8-*_ :solid string font color free cache) ;日本語の文字をも表示する
    (call-next-method)
    ) ;if
  )

;;; 元の処理を行うか否かを判断する分岐メソッド（ blended ）
;;; @param string 
;;; @param font
;;; @param color 
;;; @param free 
;;; @param cache 
;;; @return 
(defmethod sdl::_render-string-blended_ :around ((string string) (font ttf-font) (color color) free cache)
  (if *render-utf8-flag*
      (_render-utf8-*_ :blended string font color free cache) ;日本語の文字をも表示する
    (call-next-method)
    ) ;if
  )

;;; COLOR オブジェクトから 24bit の値を求める
;;; @param c COLOR オブジェクト
;;; @return 
(defun _color24_ (c)
  (+ (ash (b c) 16) (ash (g c) 8) (r c))
  )

;;; 日本語の文字をも表示できる処理。
;;; solid および blended のみ。（ shaded は別処理）
;;; @param type :solid / :blended
;;; @param string 
;;; @param font
;;; @param color 
;;; @param free 
;;; @param cache 
;;; @return 
(defun _render-utf8-*_ (type string font color free cache)
  (declare (ignore free))

  (let ((surf nil)
        (fnc nil)
        (fname nil)
        )
      (case type
        (:solid (setf fnc #'sdl-ttf-cffi::render-utf8-solid
                      fname "TTF_glue_RenderText_Solid"
                      ) ;setf
         )
        (:blended (setf fnc #'sdl-ttf-cffi::render-utf8-blended
                        fname "TTF_glue_RenderText_Blended"
                        ) ;setf
         )
        (t (error "引数 type が異常です。: ~a" type))
        ) ;case
      
      (with-foreign-color-copy (col-struct color)
        (setf surf (make-instance 'surface
                                  :fp (funcall fnc
                                               (fp font)
                                               string
                                               (if (cffi:foreign-symbol-pointer fname)
                                                   col-struct
                                                 (_color24_ color)
                                                 ) ;if
                                               ) ;render-utf8-solid
                                  ) ;make-instance
              ) ;setf
        ) ;with-foreign-color-copy
      (when cache
        (setf (cached-surface font) surf)
        ) ;when
      surf
    ) ;let
  )

;;; 元の処理を行うか否かを判断する分岐メソッド（ shaded ）
;;; @param string 
;;; @param fg-color 
;;; @param bg-color 
;;; @param font
;;; @param free 
;;; @param cache 
;;; @return 
(defmethod sdl::_render-string-shaded_ :around ((string string) (fg-color color) (bg-color color) (font ttf-font) free cache)
  (if *render-utf8-flag*
      (_render-utf8-shaded_ string fg-color bg-color font free cache) ;日本語などの文字をも表示する
    (call-next-method)
    ) ;if
  )

;;; 日本語の文字をも表示できる処理。
;;; shaded のみ。（ solid / blended は別処理）
;;; @param string 
;;; @param fg-color 
;;; @param bg-color 
;;; @param font
;;; @param free 
;;; @param cache 
;;; @return 
(defun _render-utf8-shaded_ (string fg-color bg-color font free cache)
  (declare (ignore free))

  (let ((surf nil))
    (with-foreign-color-copy (fg-struct fg-color)
      (with-foreign-color-copy (bg-struct bg-color)
        (multiple-value-bind (fg bg)
            (if (cffi:foreign-symbol-pointer "TTF_glue_RenderText_Shaded")
                (values fg-struct bg-struct)
              (values (_color24_ fg-color) (_color24_ bg-color))
              ) ;if
          (setf surf (make-instance 'surface
                                    :fp (sdl-ttf-cffi::render-utf8-shaded (fp font) string fg bg)
                                    ) ;make-instance
                ) ;setf
          ) ;multiple-value-bind
        ) ;with-foreign-color-copy
      ) ;with-foreign-color-copy
    (when cache
      (setf (cached-surface font) surf)
      ) ;when
    surf
    ) ;let
  )


