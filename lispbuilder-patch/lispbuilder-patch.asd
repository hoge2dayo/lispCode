
;;; ビルド／ロード用パッケージ
(defpackage #:lispbuilder-patch-system
  (:use #:cl #:asdf)
  )

(in-package #:lispbuilder-patch-system)


;;; LISPBUILDER-SDL の動作修正
(defsystem lispbuilder-patch
  :description "LISPBUILDER-SDL の動作修正。"
  :version "0.0.0.1"
  :author "hoge2dayo <hoge2dayo@gmail.com>"
  :licence "Public Domain"
  :depends-on (lispbuilder-sdl
               lispbuilder-sdl-ttf
               lispbuilder-sdl-ttf-cffi
               cffi
               )
  :perform (load-op :after (op lispbuilder-patch)
                    (pushnew :lispbuilder-patch *features*))
  :serial t
  :components ((:file "package")
               (:file "string-utf8")
               )
 )


