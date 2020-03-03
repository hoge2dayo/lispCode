

;;; ビルド／ロード用パッケージ
(defpackage #:lifegame-system
  (:use #:cl #:asdf)
  )


(in-package #:lifegame-system)


;;; ライフゲーム
(defsystem lifegame
  :description "ライフゲーム。"
  :version "0.0.0.1"
  :author "hoge2dayo <hoge2dayo@gmail.com>"
  :licence "Public Domain"
  :depends-on (lispbuilder-patch
               )
  :perform (load-op :after (op lifegame)
                    (pushnew :lifegame *features*))
  :serial t
  :components ((:file "package")
               (:file "global")
               (:file "util")
               (:file "message")
               (:file "display")
               (:file "creature")
               (:file "mouse")
               (:file "key")
               (:file "cursor")
               (:file "lifegame")
               )
  )
