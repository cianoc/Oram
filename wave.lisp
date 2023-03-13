(in-package :oram)

(declaim (ftype (function (buffer sample) sample) oscil-read)
         (inline oscil-read))
(defun oscil-read (buf pos)
  (declare (buffer buf)
           (sample pos)
           (optimize (speed 3)))
  "Reads a value from the buffer using pos. The beginning is 0 and the end is 1"
  (let ((idx (sample->fixnum (* pos (1- (buffer-size buf))) :roundp t)))
    (declare (type fixnum idx))
    (buffer-value buf idx)))

(declaim (ftype (function (buffer sample) sample) oscil-read-lin)
         (inline oscil-read-lin))
(defun oscil-read-lin (buf pos)
  (declare (buffer buf)
           (sample pos)
           (optimize (speed 3)))
  (let ((idx_f (* pos (1- (buffer-size buf)))))
    (declare (type sample idx_f))
    (multiple-value-bind (idx frac) (floor idx_f)
      (declare (type fixnum idx)
               (type sample frac))
      (+ (* frac (buffer-value buf idx))
         (* (- 1 frac) (buffer-value buf
                                     (logand (buffer-mask buf) (1+ idx))))))))

(defstruct (ftable (:constructor create-ftable (start log-start end log-end bufs)))
  (start 2.0f0 :type sample)
  (log-start 2.0f0 :type sample)
  (end 2.0f0 :type sample)
 ; (log-end 2.0f0 :type single-float)
  (log-end 2.0f0 :type sample)
  (bufs #() :type (simple-array buffer (*))))

(defun wtable--get-buf-num (ftable freq)
  (let ((freq (min (ftable-end ftable) (max freq (ftable-start ftable)))))
    (* (/ (- (log freq 2) (ftable-log-start ftable))
          (-  (ftable-log-end ftable) (ftable-log-start ftable)))
       (length (ftable-bufs ftable)))))


(declaim (ftype (function (fixnum) sample) keynum->cps)
         (inline keynum->cps))
(defun keynum->cps (key)
  (declare
   (fixnum key)
   (optimize (speed 3)))
  (sample (* (expt 2 (/ (- key 69.0) 12.0)) 440.0)))


(declaim (ftype (function (sample
                           function
                           &key (:vector boolean)) sequence) gen-partials))
(defun gen-partials (start-freq partial-f &key (vector nil))
  (declare
   (number start-freq)
   (function partial-f)
   (optimize (speed 3)))
  (let* ((times (floor (/ (/ *SAMPLE-RATE* 2.0) start-freq)))
         (vec (make-array times :element-type 'sample)))
    (declare (type fixnum times))
    (loop for x fixnum from 1 to times
          do (setf (aref vec (1- x))
                   (funcall partial-f x)))
    (if vector
        vec
        (coerce vec 'list))))

(defun create-aaliased-wtable (start-st end-st semitones partials oversample)
  (declare
   (fixnum start-st)
   (fixnum end-st)
   (fixnum semitones)
   (fixnum oversample))
   ;(optimize (speed 3)))
  (let* ((st start-st)
         (freq (keynum->cps st))
         (partials (if (vectorp partials) partials
                       (make-array (length partials) :initial-contents partials)))
         (wtables (ceiling (the fixnum (- end-st start-st)) semitones))
         (table-vec (make-array wtables :element-type 'buffer))
         (harmonics (length partials))
         (old-harmonics 0)
         (curr-buf)
         (buf-size (next-power-of-two (* harmonics oversample))))
    (declare
     (type fixnum wtables harmonics old-harmonics st buf-size)
     (type sample freq)
     (type (simple-array sample) partials))
    (loop for x fixnum from 0 below wtables
          do
             (if (not (= harmonics old-harmonics))
                 (if (= harmonics 1)
                     (setf curr-buf
                           *sine-table*)
                     (setf curr-buf
                           (make-buffer buf-size :fill-function
                                        (gen:partials
                                         (subseq (coerce partials 'list) 0 harmonics))))))
             (setf old-harmonics harmonics)
             (setf (aref table-vec x) curr-buf)
             (setf st (+ st semitones))
             (setf freq (keynum->cps st))
             (setf harmonics (the fixnum (floor (/ (/ *SAMPLE-RATE* (sample 2.0)) freq))))
             (setf buf-size (next-power-of-two (* harmonics oversample))))
    (let ((start-freq (keynum->cps start-st))
          (end-freq (keynum->cps end-st)))
      (create-ftable start-freq (log start-freq 2)
                   end-freq (log end-freq 2)
                   table-vec))))

(defun make-wtable (start-st end-st semitones oversample min-size gen-fun)
  (declare
   (fixnum start-st end-st semitones oversample min-size)
   (optimize (speed 3)))
  (let* ((st start-st)
         (freq (keynum->cps st))
         (wtables (ceiling (the fixnum (- end-st start-st)) semitones))
         (table-vec (make-array wtables :element-type 'buffer))
         (harmonics (the fixnum (floor (/ (/ *SAMPLE-RATE* (sample 2.0)) freq))))
         (old-harmonics 0)
         (curr-buf)
         (buf-size (next-power-of-two (* harmonics oversample))))
    (declare
     (type fixnum wtables harmonics old-harmonics st buf-size)
     (type sample freq))
    (loop for x fixnum from 0 below wtables
          do
             (if (not (= harmonics old-harmonics))
                 (if (= harmonics 1)
                     (setf curr-buf
                           *sine-table*)
                     (setf curr-buf
                           (make-buffer buf-size
                                        :fill-function
                                        (funcall gen-fun freq)))))
             (setf old-harmonics harmonics)
             (setf (aref table-vec x) curr-buf)
             (setf st (+ st semitones))
             (setf freq (keynum->cps st))
             (setf harmonics (the fixnum (floor (/ (/ *SAMPLE-RATE* (sample 2.0)) freq))))
             (setf buf-size (max min-size (next-power-of-two (* harmonics oversample)))))
    (let ((start-freq (keynum->cps start-st))
          (end-freq (keynum->cps end-st)))
      (create-ftable start-freq (log start-freq 2)
                   end-freq (log end-freq 2)
                   table-vec))))


(defun gen-aaliased-wtable (start-keynum end-keynum semitones oversample partial-f)
  (create-aaliased-wtable start-keynum end-keynum semitones
                          (gen-partials (keynum->cps start-keynum) partial-f :vector t) oversample))

;;;(defparameter *saw-wave*
;;;  (gen-aaliased-wtable 21 127 3 4 (lambda (x) (/ (sample 1) x))))

(defun gen-square-wave (start-freq)
  (declare
   (optimize (speed 3)))
  (lambda (foreign-pointer size)
    (declare (type foreign-pointer foreign-pointer)
             (type non-negative-fixnum size))
    (let ((num-harms (the fixnum (floor (/ (/ *SAMPLE-RATE* (sample 2)) start-freq) )))
          (size (1- size)))
      (declare (type fixnum num-harms))
      (with-samples ((max-val 0.0))
        (dotimes (i size)
          (let ((cur-val (sample 0)))
            (declare (sample cur-val))
            (loop for g fixnum from 1 to num-harms by 2
                  do
                     (let* ((phi (/ (* 2 pi i g) size))
                            (x (/ (* g pi) num-harms))
                            (sigma (/ (sin x) x)))
                       (declare (type sample phi))
                       (incf cur-val (* sigma
                                            (/ 1.0 g)
                                            (sin phi)))))
            (setf (smp-ref foreign-pointer i) cur-val)
            (setf max-val (max (abs cur-val) max-val))))
        (values foreign-pointer
                (if (zerop max-val) max-val (/ max-val))
                (not (zerop max-val)))))))

(defun gen-saw-wave (start-freq)
  (declare
   (optimize (speed 3)))
  (lambda (foreign-pointer size)
    (declare (type foreign-pointer foreign-pointer)
             (type non-negative-fixnum size))
    (let ((num-harms (the fixnum (floor (/ (/ *SAMPLE-RATE* (sample 2)) start-freq) )))
          (size (1- size)))
      (declare (type fixnum num-harms))
      (with-samples ((max-val 0.0))
        (dotimes (i size)
          (let ((cur-val (sample 0))
                (phs 1))
            (declare (fixnum phs) (sample cur-val))
                                        ;  (declare (sample cur-val))
            (loop for g fixnum from 1 to num-harms
                  do
                     (let* ((phi (/ (* 2 pi i g) size))
                            (x (/ (* g pi) num-harms))
                            (sigma (/ (sin x) x)))
                       (declare (type sample phi))
                       (incf cur-val (* phs
                                            (/ 1.0 g)
                                            (sin phi)
                                            sigma))
                       (setf phs (* phs -1))))
            (setf (smp-ref foreign-pointer i) cur-val)
            (setf max-val (max (abs cur-val) max-val))))
        (values foreign-pointer
                (if (zerop max-val) max-val (/ max-val))
                (not (zerop max-val)))))))

(defun gen-tri-wave (start-freq)
  (declare
   (optimize (speed 3)))
  (lambda (foreign-pointer size)
    (declare (type foreign-pointer foreign-pointer)
             (type non-negative-fixnum size))
    (let ((num-harms (the fixnum (floor (/ (/ *SAMPLE-RATE* (sample 2)) start-freq) )))
          (size (1- size)))
      (declare (type fixnum num-harms))
      (with-samples ((max-val 0.0))
        (dotimes (i size)
          (let ((cur-val (sample 0))
                (phs 1))
            (declare (fixnum phs) (sample cur-val))
                                        ;  (declare (sample cur-val))
            (loop for g fixnum from 1 to num-harms by 2
                  do
                     (let* ((phi (/ (* 2 pi i g) size))
                            (x (/ (* g pi) num-harms))
                            (sigma (/ (sin x) x)))
                       (declare (type sample phi))
                       (incf cur-val (* phs
                                            (/ 1.0 (* g g))
                                            (sin phi)
                                            sigma))
                       (setf phs (* phs -1))))
            (setf (smp-ref foreign-pointer i) cur-val)
            (setf max-val (max (abs cur-val) max-val))))
        (values foreign-pointer
                (if (zerop max-val) max-val (/ max-val))
                (not (zerop max-val)))))))

(defparameter *saw-wave* (make-wtable 21 127 3 4 1024 #'gen-saw-wave))
(defparameter *square-wave* (make-wtable 21 127 3 4 1024 #'gen-square-wave))
(defparameter *tri-wave* (make-wtable 21 127 3 4 1024 #'gen-tri-wave))



;; (defparameter *square-wave*
;;   (gen-aaliased-wtable 21 127 3 4 (lambda (x) (if (oddp x) (/ (sample 1.0) x) (sample 0.0)))))

;; (defparameter *triangle-wave*
;;   (gen-aaliased-wtable 21 127 3 4 (lambda (x) (if (oddp x) (/ (sample 1.0) (* x x)) (sample 0.0)))))

(define-vug-macro wosc (wtable freq amp phase)
  (with-vug-inputs ((fr freq)
                    (am amp)
                    (ph phase))
    `(with-samples ((inc (* ,fr *sample-duration*))
                    (curr-pos 0.0)
                    (a ,am)
                    (f ,fr)
                    (p ,ph))
       (with ((bufs (ftable-bufs ,wtable))
              (max_idx (1- (length bufs))))
         (prog1
             (* a (oscil-read-lin
                (aref (ftable-bufs ,wtable)
                      (min
                       (ceiling (wtable--get-buf-num ,wtable f))
                       max_idx))
                (nth-value 1 (floor (+ p curr-pos)))))
      (setf curr-pos (nth-value 1 (floor (+ curr-pos inc )))))))))

(define-vug wsquare (freq amp phase)
  (wosc *square-wave* freq amp phase))

(define-vug wsaw (freq amp phase)
  (wosc *saw-wave* freq amp phase))

(define-vug wtri (freq amp phase)
  (wosc *tri-wave* freq amp phase))

(define-vug wpulse (freq amp phase width)
  (with-samples ((f freq)
                 (a amp)
                 (p phase)
                 (w width))
    (- (wsaw f a p)
       (wsaw f a (+ p (* w 0.5))))))


;; (define-vug wsaw (freq amp phase)
;;   (:defaults 440 1 0)
;;   (with-samples ((inc (* freq *sample-duration*))
;;                  (curr-pos (init-only phase)))
;;     (with ((bufs (ftable-bufs *square-wave*))
;;            (max_idx (1- (length bufs))))
;;     (prog1
;;         (* amp (oscil-read-lin
;;                 (aref (ftable-bufs *square-wave*)
;;                       (min
;;                        (ceiling (wtable--get-buf-num *square-wave* freq))
;;                        max_idx))
;;                 (nth-value 1 (floor (+ phase curr-pos)))))
;;       (setf curr-pos (nth-value 1 (floor (+ curr-pos inc ))))))))

(dsp! simple (freq amp phase width)
  (out (wpulse freq amp phase width)))

(dsp-debug simple2 (freq amp phase)
  (out (wsquare freq amp phase)))
