;;;; package.lisp

(defpackage #:oram
  (:use #:cl #:incudine #:incudine.util #:incudine.vug)
  (:import-from #:alexandria #:non-negative-fixnum #:positive-fixnum #:negative-fixnum)
  (:export :ftable
           :keynum->cps
           :gen-partials
           :create-aaliased-wtable
           :make-wtable
           :gen-aaliased-wtable
           :gen-square-wave
           :gen-saw-wave
           :gen-tri-wave
           :wosc
           :wsquare
           :wtri
           :wpulse
           :*square-wave*
           :*saw-wave*
           :*tri-wave*))
