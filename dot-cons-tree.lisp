;;;; dot-cons-tree.lisp
(declaim (optimize (speed 0) (debug 3)))

;;; loading and reloading
;; (push #p"~/Programming/Lisp/dot-cons-tree/" asdf:*central-registry*)
;; (ql:quickload :dot-cons-tree)
;; (in-package #:dot-cons-tree)
;; (dot-cons-tree:draw-graph '(1 (2.1 . 2.2) 3))
;; (dot-cons-tree:draw-graph  (circular (list 1 2 3)))

(in-package #:dot-cons-tree)

(defun flatten (x &optional acc)
  (if (atom x)
      (cons x acc)
      (flatten (car x)
               (if (null (cdr x))
                   acc
                   (flatten (cdr x) acc)))))

(defun circular (items)
  (setf (cdr (last items)) items)
  items)

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
                        (if (gethash (car a) *seen*)
                            (push (coll (car a) sx osx) results)
                            (progn
                              (setf (gethash (car a) *seen*) t)
                              (dive (car a) (cons 'a sx) sx)))
                        (if (gethash (cdr a) *seen*)
                            (push (coll (cdr a) sx osx) results)
                            (progn
                              (setf (gethash (cdr a) *seen*) t)
                              (dive (cdr a) (cons 'd sx) sx))))
                       (t
                        (push (coll a sx osx) results)))))))

      (dive x '(r) '(no-parent)))
    (reverse results)))

(defun symbol-downcase (x)
  (string-downcase (format nil "~A" x)))

(defun symbols-to-string (x)
  (reduce (lambda (a x) (concatenate 'string a (symbol-downcase x)))
          x :initial-value ""))

(defun attributes-to-string (x)
  (with-output-to-string (s)
    (format s "[~A]"
            (se:string-join
             (loop for ap in x
                   when ap
                     collect
                     (format nil "~a=~s" (car ap) (cdr ap)))
             ", "))))

;; here we can find the documentation for the options
;; * Node, Edge and Graph Attributes
;; * Node Shapes
;; http://www.graphviz.org/documentation/
(defun prepare-node-attributes (a)
  (list (symbols-to-string (nth 1 a))
        (cond ((instance-slots (car a))
               (list (cons "shape" "note")
                     (cons "label" (format nil "~S"  (type-of (nth 0 a))))))
              ((null (car a))
               (list (cons "shape" (if (equalp '(c a) (subseq (nth 1 a) 0 2))
                                       "octagon"
                                       "circle"))
                     (cons "fontsize" (if (equalp '(c a) (subseq (nth 1 a) 0 2))
                                      "12"
                                      "6"))
                     (cons "label" (format nil "~S" (nth 0 a)))))
              (T
               (list (cons "shape" "box")
                     (cons "label" (format nil "~S" (nth 0 a))))))))

(defun prepare-connection-attributes (a)
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

(defun shorten-label (label nn)
  (if (> (length label)
         (* 2 nn))
      (format nil "~A...~A"
              (subseq label 0 nn)
              (subseq label (- (length label)
                             nn)))
      label))

(defun prepare-graph (x)
  (let ((results (analyse x)))
    (let ((leaf-nodes (mapcar #'prepare-node-attributes
                              (remove-if-not (lambda (x)
                                               (atom (car x)))
                                             results)))
          (branch-nodes (mapcar #'prepare-node-attributes
                                (remove-if (lambda (x)
                                             (atom (car x)))
                                           results)))
          (connections (mapcar #'prepare-connection-attributes
                               (cdr results))))
      (with-output-to-string (g)
        (format g "digraph {~%")
        (loop for a in leaf-nodes do
          (format g "~S ~A~%"
                  (nth 0 a)
                  (attributes-to-string (nth 1 a))))
        (format g "~%~%~%")
        (loop for a in branch-nodes do
          (format g "~S ~A~%"
                  (nth 0 a)
                  (attributes-to-string (list
                                         (cons "label"
                                               (format nil "~A"
                                                       (shorten-label (nth 0 a) 5)))))))
        (format g "~%~%~%")
        (format g "~A ~A~%"
                "node"
                (attributes-to-string (list (cons "color" "grey")
                                            (cons "fontcolor" "grey"))))
        (format g "~%")
        (loop for c in connections do
          (format g "~A ~A~%"
                  (nth 1 c)
                  (attributes-to-string (nth 2 c))))
        (format g "}~%")))))

(defun draw-graph (x)
  (let ((filename "dot-graph")
        (extension "svg"))
    (defparameter *seen* (make-hash-table :test #'equal))

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
