;;;; dot-cons-tree.lisp

(in-package #:dot-cons-tree)

(defun flatten (x &optional acc)
  (if (atom x)
      (cons x acc)
      (flatten (car x)
               (if (null (cdr x))
                   acc
                   (flatten (cdr x) acc)))))

(defun analyse (x)
  (let ((results))
    (labels
        ((coll (dat sx osx)
           (list dat  (cons 'c  sx) (cons 'c osx)))
         (dive (a sx osx)
           (if (consp a)
               (progn
                 (push (coll a sx osx) results)
                 (dive (car a) (cons 'a sx) sx)
                 (dive (cdr a) (cons 'd sx) sx))
               (push (coll a sx osx) results))))
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
             (loop for ap in x collect (format nil "~a=~s" (car ap) (cdr ap)))
             ", "))))

(defun prepare-graph (x)
  (let ((results (analyse x)))
    (let ((atom-shapes (mapcar
                        (lambda (a)
                          (list (symbols-to-string (nth 1 a))
                                (list (cons "shape" (if (null (car a)) "diamond" "box"))
                                      (cons "label" (format nil "~S" (nth 0 a))))))
                        (remove-if-not (lambda (x)
                                         (atom (car x)))
                                       results)))
          (connections (mapcar
                        (lambda (a)
                          (list (nth 0 a)
                                (format nil "~A -> ~A"
                                        (symbols-to-string (nth 2 a))
                                        (symbols-to-string (nth 1 a)))
                                (list
                                 (cons "color"
                                       (let ((colsym (nth 1 (nth 1 a))))
                                         (cond ((equalp colsym 'a) "red")
                                               ((equalp colsym 'd) "blue")
                                               (T "green")))))))
                        (cdr results))))
      (list 'aaa atom-shapes 'ccccc connections)
      (with-output-to-string (g)
        (format g "digraph {~%")
        (loop for a in atom-shapes do
          (format g "~A ~A~%" (nth 0 a) (attributes-to_string (nth 1 a))))
        (format g "~%")
        (loop for c in connections do
          (format g "~A ~A /* ~s */~%"
                  (nth 1 c)
                  (attributes-to_string (nth 2 c))
                  (format nil "~A" (nth 0 c))))
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
