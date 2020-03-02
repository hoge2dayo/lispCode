

(in-package #:cl-user)


;;; 実装のパッケージ
(defpackage #:lispbuilder-patch
  (:use #:cl #:lispbuilder-sdl)
  (:documentation "LISPBUILDER-SDL の動作修正。")
  (:export
   ;; string-utf8.lisp
   #:*render-utf8-flag*
   )
 )


