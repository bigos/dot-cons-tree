;;;; dot-cons-tree.lisp
(declaim (optimize (speed 0) (debug 3)))

;;; loading and reloading
;; (push #p"~/Programming/Lisp/dot-cons-tree/" asdf:*central-registry*)
;; (ql:quickload :dot-cons-tree)
;; (in-package #:dot-cons-tree)
;; (dot-cons-tree:draw-graph '(1 (2.1 . 2.2) 3))

(in-package #:dot-cons-tree)

(defun flatten (x &optional acc)
  (if (atom x)
      (cons x acc)
      (flatten (car x)
               (if (null (cdr x))
                   acc
                   (flatten (cdr x) acc)))))

(defstruct person (name) (surname) (some-titles))
(defparameter *person* (make-person :name (cons "Mister" "Paul")
                                    :surname "Graham"
                                    :some-titles (list "author" "programmer" "thinker")))
;; show structure elements
;; (dot-cons-tree:draw-graph (list 1 '(2.1 . 2.2) *person*))

(defun instance-slots (i)
  (mapcar #'sb-mop:slot-definition-name
          (sb-mop:class-slots (class-of i))))

(defun analyse (x)
  (let ((results))
    (labels
        ((coll (dat sx osx)
           (list dat  (cons 'c  sx) (cons 'c osx)))
         (dive (a sx osx)
           (let ((isl (instance-slots  a)))
             (if isl
                 (progn
                   (push (coll a sx osx) results)
                   (loop for sl in isl do
                     (dive (slot-value a sl)
                           (cons (format nil "~A" sl) sx)
                           sx)))
                 (cond ((consp a)
                        (push (coll a sx osx) results)
                        (dive (car a) (cons 'a sx) sx)
                        (dive (cdr a) (cons 'd sx) sx))
                       (t
                        (push (coll a sx osx) results)))))))

      (dive x '(r) '(no-parent)))
    (reverse results)))

(defun symbol-downcase (x)
  (string-downcase (format nil "~A" x)))

(defun symbols-to-string (x)
  (reduce (lambda (a x) (concatenate 'string a (symbol-downcase x)))
          x :initial-value ""))

(defun attributes-to_string (x)
  (with-output-to-string (s)
    (format s "[~A]"
            (se:string-join
             (loop for ap in x
                   when ap
                     collect
                     (format nil "~a=~s" (car ap) (cdr ap)))
             ", "))))

(defun prepare-graph (x)
  (let ((results (analyse x)))
    (let ((atom-shapes (mapcar
                        (lambda (a)
                          (list (symbols-to-string (nth 1 a))

                                (if (instance-slots (car a))
                                    (list (cons "shape" "box")
                                          (cons "color" "yellow")
                                          (cons "style" "filled")
                                          (cons "label" (format nil "~S"  (type-of (nth 0 a)))))
                                    (list (cons "shape" (cond ((null (car a))
                                                               (if (equalp '(c a) (subseq  (nth 1 a) 0 2))
                                                                   "octagon"
                                                                   "diamond"))
                                                              (T "box")))
                                          (cons "label" (format nil "~S" (nth 0 a)))))))
                        (remove-if-not (lambda (x)
                                         (atom (car x)))
                                       results)))
          (connections (mapcar
                        (lambda (a)
                          (list (nth 0 a)
                                (format nil "~S -> ~S"
                                        (symbols-to-string (nth 2 a))
                                        (symbols-to-string (nth 1 a)))
                                (list
                                 (cons "color"
                                       (let ((colsym (nth 1 (nth 1 a))))
                                         (cond ((equalp colsym 'a) "red")
                                               ((equalp colsym 'd) "blue")
                                               (T "green"))))
                                 (cons "label"
                                       (let ((colsym (nth 1 (nth 1 a))))
                                         (cond ((equalp colsym 'a) "")
                                               ((equalp colsym 'd) "")
                                               (T (format nil "~A" colsym))))))))
                        (cdr results))))

      (with-output-to-string (g)
        (format g "digraph {~%")
        (loop for a in atom-shapes do
          (format g "~S ~A~%" (nth 0 a) (attributes-to_string (nth 1 a))))
        (format g "~%")
        (loop for c in connections do
          (format g "~A ~A~%"
                  (nth 1 c)
                  (attributes-to_string (nth 2 c))))
        (format g "}~%")))))

(defun draw-graph (x)
  (let ((filename "dot-graph")
        (extension "svg"))
    (let ((gv-file (format nil "/tmp/~A.gv" filename))
          (the-file(format nil "/tmp/~A.~A" filename extension)))
      (let ((options (list
                      (format nil "-T~A" extension)
                      gv-file
                      "-o"
                      the-file)))
        (format t "dot options ~A~%" options)
        (with-open-file (stream gv-file :direction :output :if-exists :supersede)
          (write-sequence (prepare-graph x) stream))
        (sb-ext:run-program "/usr/bin/dot" options)))))
