#!/usr/bin/sbcl --script
#|
Licensed under 3-clause BSD License:
Copyright © 2010, Mathieu Lemoine
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
* Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.
* Neither the name of Mathieu Lemoine nor the
names of contributors may be used to endorse or promote products
derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY Mathieu Lemoine ''AS IS'' AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL Mathieu Lemoine BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
\(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION\) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
\(INCLUDING NEGLIGENCE OR OTHERWISE\) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#

;; TODO : break down code in smaller files

(cl:in-package #:cl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'alexandria)
  (require 'split-sequence)
  (require 'script-utility)
  (require 'cl-fad)
  (require 'parse-number))

(defpackage #:gams-dynamic-sets
  (:use #:cl #:alexandria #:split-sequence #:parse-number #:script-utility)
  (:shadowing-import-from #:fad #:list-directory))

(in-package #:gams-dynamic-sets)

(defun string-replace (str old new)
  "replace all occurences of old in str by new"
  (declare (string str old new))
  (let ((old-length (length old)))
    (the string
      (apply #'concatenate 'string
             (loop
                for index = 0 then (+ old-index old-length)
                for old-index = (search old str :start2 index)
                collect (subseq str index old-index)
                until (null old-index)
                collect new)))))

                                        ; Structures, global special vars and helpers

(defparameter *sets* ())

;; TODO : add min size
(defstruct (dynamic-set
             (:constructor make-dynamic-set
                           (name &optional max-size)))
  (name          nil                     :read-only t :type string)
  (max-size      nil                                  :type (or null integer))
  (stop-criteria nil                                  :type list))

(deftype set-name-descriptor () '(or string symbol))
(defun make-set-name (name)
  (declare (type set-name-descriptor name))
  (if (and (symbolp name) (not (null name)))
      (symbol-name name)
      name))

(defun find-set-from-name (name)
  (declare (type (or null set-name-descriptor) name))
  (when name
    (the dynamic-set
      (find (make-set-name name) *sets* :key #'dynamic-set-name :test #'string-equal))))

(defun set-point-p (set-point)
  (declare (hash-table set-point))
  (every (rcurry #'gethash set-point)
         *sets*))

(deftype set-point () '(satisfies set-point-p))

(defun make-set-point (&rest args)
  (let ((set-point (make-hash-table :test #'equalp :size (length *sets*))))
    (map nil (lambda (set)
               (setf (gethash set set-point)
                     (the integer (or (second
                                       (member (dynamic-set-name set) args
                                               :key #'make-set-name
                                               :test #'string-equal))
                                      0))))
         *sets*)
    (the set-point set-point)))

;; TODO (?) : replace depth-first by breadth-first
(defun generate-next-set-point (previous-set-point result-points
                                &aux (current-set (first *sets*)))
  (declare (type set-point previous-set-point)
           (type dynamic-set current-set))
  (let ((new-set-point (copy-hash-table previous-set-point)))
    (incf (gethash current-set new-set-point))
    (flet ((check-stop-criterion (stop-criterion)
             (declare (type stop-criterion stop-criterion))
             (funcall (stop-criterion-function stop-criterion)
                      current-set new-set-point result-points)))
      (if (and (notany #'check-stop-criterion *independent-stop-criteria*)
               (notany #'check-stop-criterion (dynamic-set-stop-criteria current-set)))
          (values new-set-point current-set)
          (let ((*sets* (rest *sets*)))
            (setf (gethash current-set new-set-point) 0)
            (generate-next-set-point new-set-point result-points))))))

(defun stringify-set-point (set-point)
  (declare (type set-point set-point))
  (the string
    (apply #'concatenate 'string
           (loop
              for (set . rest) on *sets*
              append (list
                      (dynamic-set-name set) ":"
                      (write-to-string (gethash set set-point)))
              when rest collect ","))))

(defun set-point->result-coordinate (set-point)
  (declare (type set-point set-point))
  (loop
     for set in *sets*
     collect (gethash set set-point)))

(defun set-point->result-known-dimensions (set-point)
  (declare (type set-point set-point))
  (loop
     for set in *sets*
     collect (or (dynamic-set-max-size set)
                 (gethash set set-point))))

(defparameter *set-point-loader* ())

(defun load-set-point (set-point)
  (declare (type set-point set-point))
  (with-open-file (stream *set-point-load*
                          :direction :output
                          :if-exists :supersede)
    (maphash (lambda (set current-size)
               (let ((name (dynamic-set-name set)))
                 (format stream "$phantom ~A\nSet ~:*~A /~{~A~^, ~}/;\n\n"
                         name
                         (or (mapcar (curry #'generate-set-element set)
                                     (iota current-size :start 1))
                             (list name)))))
             set-point)))

(defun generate-set-element (set point)
  (declare (type dynamic-set set)
           (unsigned-byte point))
  (the string
    (concatenate 'string
                 (string-downcase (dynamic-set-name set))
                 (write-to-string point))))

(defparameter *variables* ())

(defstruct (gams-variable
             (:constructor make-gams-variable (name dimension)))
  (name      nil :read-only t :type string)
  (dimension nil :read-only t :type integer))

(deftype derivation () '(member :independent :derived :family))
(deftype concret-stage () '(member :empty-set :first-element :additional-element))
(deftype abstract-stage () '(member :always :non-empty-set))
(deftype stage () '(or concret-stage abstract-stage))

(defstruct (strategy
             (:constructor %make-strategy
                           (file-name derivation set stage)))
  (file-name  nil :read-only t :type string)
  (derivation nil :read-only t :type derivation)
  (stage      nil :read-only t :type stage)
  (set        nil :read-only t :type (or null dynamic-set)))

(defun strategy-name (instance)
  (declare (type strategy instance))
  (the string (string-replace (strategy-file-name instance) "/" ":")))

(defun make-strategy (file-name derivation &optional set-name stage)
  "Create a new strategy to generate initial points."
  (declare (type derivation derivation)
           (type (or null stage) stage))
  (when (zerop (length file-name))
    (error "file name must be non empty"))
  (let ((set (find-set-from-name set-name)))
    (when (and (not (null set-name))
               (null set))
      (error "set-name must be an existing set name or null"))
    (when (member derivation '(:derived :family))
      (when (null set)
        (error "'derived' and 'family' derivations require a set name"))
      (when (member stage '(:empty-set :always))
        (error "'empty set' and 'always' stages are not allowed for 'derived' or 'family' derivations")))
    (when (and
           (eql derivation :family)
           (member stage '(:first-element :non-empty-set)))
      (error "'first element' and 'non-empty set' stages are not allowed for 'family' derivation"))
    (let ((stage (or stage
                     (ecase derivation
                       (:independent :always)
                       (:derived     :non-empty-set)
                       (:family      :additional-element)))))
      (%make-strategy file-name derivation set stage))))

(defun strategies-stage-filtering (strategies &optional (current-stage :empty-set) current-set)
  (declare (type concret-stage current-stage)
           (type (or null dynamic-set) current-set))
  (let* ((stage-values     (list* :always current-stage
                                  (unless (eql current-stage :empty-set)
                                    '(:non-empty-set))))
         (strategies-stage (remove-if (compose #'not (rcurry #'member stage-values))
                                      strategies
                                      :key #'strategy-stage)))
    (if current-set
        (remove-if (compose #'not (curry #'string-equal (dynamic-set-name current-set)))
                   strategies-stage
                   :key #'strategy-set))))

(defstruct (initial-point
             (:constructor %make-initial-point
                           (strategy set-point result-point set-index)))
  (strategy     nil :read-only t :type strategy)
  (set-point    nil :read-only t :type set-point)
  (result-point nil :read-only t :type (or null result-point))
  (set-index    nil :read-only t :type (or null unsigned-byte))
  (file-name    nil              :type (or null string)))

(defun initial-point-set (instance)
  (declare (type initial-point instance))
  (when (initial-point-result-point instance)
    (let ((old-set-point (initial-point-set-point
                          (result-point-initial-point
                           (initial-point-result-point instance))))
          (new-set-point (initial-point-set-point instance)))
      (loop
         for set in *sets*
         when (/= (gethash set old-set-point)
                  (gethash set new-set-point))
         return (the dynamic-set set)))))

(defun initial-point-history (instance)
  (declare (type initial-point instance))
  (with-accessors ((strategy initial-point-strategy)) instance
    (with-accessors ((strategy-derivation strategy-derivation)) strategy
      (the string
        (apply #'concatenate 'string
               (let ((local-begining (list
                                      "/" (strategy-name strategy) "("
                                      (stringify-set-point (initial-point-set-point instance))
                                      ")")))
                 (if (eql strategy-derivation :independent)
                     local-begining
                     (with-accessors ((current-set  initial-point-set)
                                      (result-point initial-point-result-point))
                         instance
                       (append (initial-point-history (result-point-initial-point result-point))
                               (list "," (result-point-solver result-point))
                               local-begining
                               (list ","
                                     (generate-set-element current-set
                                                           (gethash current-set
                                                                    (initial-point-set-point instance))))
                               (when (eql strategy-derivation :family)
                                 (list ","
                                       (generate-set-element current-set
                                                             (initial-point-set-index instance)))))))))))))

(defun make-initial-point (strategy set-point
                           &optional result-point set-index)
  (declare (type strategy  strategy)
           (type set-point set-point)
           (type (or null result-point) result-point)
           (type (or null unsigned-byte) set-index))
  (when (and (member (strategy-derivation strategy) '(:derived :family))
             (null result-point))
    (error "a derived result point must be derived from a previous point"))
  (when (and (eql (strategy-derivation strategy) :family)
             (null set-index))
    (error "a result point extracted from a family must have an iteration index"))
  (%make-initial-point strategy set-point result-point set-index))

(defun generate-initial-points (strategies set-point
                                &optional (current-stage :empty-set)
                                current-set previous-result-points)
  (declare (type concret-stage current-stage)
           (type set-point set-point)
           (type (or null dynamic-set) current-set))
  (let ((strategies      (strategies-stage-filtering strategies current-stage current-set))
        (old-set-indices (let ((current-set-size (or (gethash current-set set-point)
                                                     0)))
                           (when (>= current-set-size 2)
                             (iota (1- current-set-size) :start 1)))))
    (loop
       for strategy in strategies
       for %make-initial-point = (curry #'make-initial-point strategy set-point)
       when (eql (strategy-derivation strategy) :independent) collect (funcall %make-initial-point)
       when (eql (strategy-derivation strategy) :derived)     append  (mapcar %make-initial-point previous-result-points)
       when (eql (strategy-derivation strategy) :family)      append  (map-product %make-initial-point previous-result-points old-set-indices))))

(defun write-initial-point (initial-point &optional (directory "initial-points/"))
  (declare (type initial-point initial-point))
  (unless (ends-with #\/ directory)
    (error "directory must denote a directory (i.e. end with a '/')"))
  (let* ((history         (initial-point-history initial-point))
         (number          (length (list-directory directory)))
         (file            (concatenate 'string
                                       directory (write-to-string number) "."
                                       (subseq history (1+ (search "/" history :from-end t)))
                                       ".init"))
         (current-set     (initial-point-set initial-point))
         (new-set-element (generate-set-element current-set
                                                (gethash current-set
                                                         (initial-point-set-point initial-point))))
         (old-set-element (generate-set-element current-set (initial-point-set-index initial-point))))
    (setf (initial-point-file-name initial-point) file)
    (with-accessors ((strategy initial-point-strategy)) initial-point
      (with-accessors ((strategy-derivation strategy-derivation)) strategy
        (with-open-file (stream file :direction :output :if-does-not-exist :create)
          (princ (concatenate 'string "* " history "\n") stream)
          (unless (eql strategy-derivation :independent)
            (write-gams-point stream)
            (princ "\n" stream))
          (princ (concatenate 'string "$batinclude " (strategy-file-name strategy)) stream)
          (unless (eql strategy-derivation :independent)
            (princ (concatenate 'string  " " old-set-element) stream)
            (when (eql strategy-derivation :family)
              (princ (concatenate 'string " " new-set-element) stream)))
          (princ "\n" stream))))))

(defparameter *initial-point-loader* "")

(defun load-initial-point (initial-point)
  (declare (type initial-point initial-point))
  (with-open-file (stream *initial-point-loader*
                          :direction :output
                          :if-exists :supersede)
    (format stream "$include ~A" (initial-point-file-name initial-point)))
  (values))

(defstruct (stop-criterion
             (:constructor %make-stop-criterion
                           (name function)))
  (name        nil :read-only t :type symbol)
  (function    nil :read-only t :type function))

(defmacro sets-max-size (&rest args)
  `(setf
    ,@(loop
         for (set size) on args by #'cddr
         append (list `(dynamic-set-max-size (find-set-from-name ',set)) size))))

(defmacro make-stop-criterion (name
                               (&optional (set-var           (gensym "set/"))
                                          (set-point-var     (gensym "set-point/"))
                                          (result-points-var (gensym "result-points/")))
                               &body body
                               &aux (args (list set-var set-point-var result-points-var)))
  `(%make-stop-criterion ',name
                         (lambda ,args
                           (declare (type dynamic-set ,set-var)
                                    (type set-point ,set-point-var))
                           (declare (ignorable ,@args))
                           ,@body)))

(defparameter *independent-stop-criteria*
  (list
   (make-stop-criterion check-max-set-size (set set-point)
     (> (dynamic-set-max-size set)
        (gethash set set-point)))))

(defmacro def-stop-criterion (decl
                              (&optional (set-var           (gensym "set/"))
                                         (set-point-var     (gensym "set-point/"))
                                         (result-points-var (gensym "result-points/")))
                              &body body
                              &aux (args (list set-var set-point-var result-points-var)))
  (let ((name           (if   (listp decl) (first  decl) decl))
        (set-specific-p (when (listp decl) (second decl)))
        (set            (make-set-name (when (listp decl) (third decl)))))
    `(let ((stop-criterion (make-stop-criterion ,name ,args ,@body)))
       (push stop-criterion
             ,(if set-specific-p
                  `(dynamic-set-stop-criteria (find-set-from-name ,set))
                  '*independent-stop-criteria*)))))

(defstruct (point-key
             (:constructor make-point-key
                           (name &rest %indices
                                 &aux (indices (mapcar (lambda (indice)
                                                         (the string indice))
                                                       %indices)))))
  (name    nil :read-only t :type string)
  (indices nil :read-only t :type list))

(defun point-p (point)
  (every #'point-key-p (hash-table-keys point)))

(deftype point () '(satisfies point-p))

(defun write-gams-point (point &optional (stream *standard-output*))
  (declare (type point point))
  (maphash (lambda (key val)
             (format stream "~A.l~@[(~{'~A'~^,~})~] = ~A;\n"
                     (point-key-name key) (point-key-indices key)
                     val))
           point))

(defstruct (result-point
             (:constructor make-result-point*
                           (initial-point
                            solver solver-status solver-iteration solver-time
                            objective-value point)))
  (initial-point    nil :read-only t :type initial-point)
  (solver           nil :read-only t :type string)

  (solver-status    nil :read-only t :type unsigned-byte)
  (solver-iteration nil :read-only t :type unsigned-byte)
  (solver-time      nil :read-only t :type (real 0))
  (objective-value  nil :read-only t :type (or real nil))
  (point            nil :read-only t :type (or hash-table nil)))

(defun %feasible-point-p (solver-status)
  (declare (unsigned-byte solver-status))
  (<= solver-status 9))

(defun feasible-point-p (point)
  (declare (type result-point point))
  (%feasible-point-p (result-point-solver-status point)))

(defun make-result-point (initial-point solver
                          solver-status solver-iteration solver-time
                          objective-value point)
  (declare (type initial-point initial-point)
           (type (or null point) point))
  (when (and (or (null objective-value) (null point))
             (%feasible-point-p point))
    (error "infeasibility, non-optimality or unboundability are no excuses for a lack of data"))
  (make-result-point* initial-point solver
                      solver-status solver-iteration solver-time objective-value point))

(defun print-result-point (point &optional (stream *standard-output*))
  (declare (type result-point point))
  (format stream "initial point: ~A~%"
          (initial-point-file-name (result-point-initial-point point)))
  (format stream "solver: ~A~%" (result-point-solver point))
  (format stream "status: ~A~%" (result-point-solver-status point))
  (format stream "iteration count: ~A~%" (result-point-solver-iteration point))
  (format stream "time: ~A~%" (result-point-solver-time point))
  (format stream "objective value: ~A~%~%" (result-point-objective-value point))
  (write-gams-point (result-point-point point) stream)
  (princ #\Newline)
  (values))

(defun solve-gams-model (gams-model initial-point &rest solvers)
  (declare (type initial-point initial-point))
  #+sbcl (sb-ext:run-program "gams" (list* gams-model solvers))
  #-sbcl (error "This script currently only supports SBCL")
  (parse-result-point (concatenate 'string
                                   (subseq gams-model
                                           (1+ (find gams-model ".gms"
                                                     :from-end t)))
                                   ".lst")
                      initial-point))

                                        ; Parsing functions

(defun string-nth-column (str column)
  (declare (string str)
           (integer column))
  (the string
    (nth (1- column)
         (split-sequence #\Space str
                         :remove-empty-subseqs t
                         :count column))))

(defun begins-with (start sequence &key (test #'eql))
  (declare (type sequence sequence))
  (let ((length (length start)))
    (when (>= (length sequence) length)
      (funcall test start (subseq sequence 0 length)))))

(defun parse-sets (file)
  "Parse the dynamic sets file.
The file should contains a comma/new-line separated list of set descriptors.
A set descriptor is a text whose first word is the set's name,
eventualy with a max size in parenthesis, similarly to GAMS syntax."
  (with-open-file (dynamic-sets file)
    (loop
       for line = (read-line dynamic-sets nil nil) while line
       nconc (mapcar #'parse-set (split-sequence #\, line)))))

(defun parse-set (decl)
  (let ((decl (split-sequence #\( (subseq decl 0 (find #\Space decl)))))
    (assert (<= (length decl) 2))
    (make-dynamic-set (string-trim '(#\Space) (first decl))    ; set name
                      (let ((max-size (second decl)))
                        (when max-size
                          (parse-integer (subseq max-size 0 (find #\) max-size))))))))

(defun parse-variable-list (file)
  (with-open-file (variable-list file)
    (loop
       for line = (read-line variable-list nil nil)
       for columns = (split-sequence #\Space line
                                     :remove-empty-subseqs t
                                     :count 2)
       while line

       unless (let ((cols (length columns)))
                (or (zerop cols)
                    (not (starts-with #\Space line))
                    (and (= cols 1)
                         (string= ";" (first columns)))))
       collect (let ((decl (split-sequence #\( (first columns))))
                 (make-gams-variable (first decl)
                                     (length
                                      (split-sequence #\, (second decl)
                                                      :remove-empty-subseqs t)))))))

(defun parse-strategies (file)
  "Parse the strategy file.
The file should contains a strategy specification per line.
A strategy specification is as follow:
[comment:] strategy-file-name derivation [set [stage]]
derivation must be one of i, d or f;
set, if present, must be the name of an existing set;
stage, if present, must be one of e, 1, n, * or +.
Read external documentation to get more information on the strategies file."
  (with-open-file (strategies file)
    (loop
       for line = (read-line strategies nil nil) while line
       collect (parse-strategy line))))

(defun parse-strategy (line)
  "Parse a single strategy description"
  (destructuring-bind (comment line)
      (mapcar (curry #'string-trim '(#\Space))
              (split-sequence #\: line))
    (when (null line)
      (shiftf line comment ""))
    (destructuring-bind (file-name derivation set stage)
        (mapcar (curry #'string-trim '(#\Space))
                (split-sequence #\, line))
      (let ((derivation (ecase (coerce 'character derivation)
                          (#\i :independent)
                          (#\d :derived)
                          (#\f :family)))
            (stage   (ecase (and stage (coerce 'character stage))
                       (nil nil)
                       (#\e :empty-set)
                       (#\1 :first-element)
                       (#\n :additional-element)
                       (#\* :always)
                       (#\+ :non-empty-set))))
        (make-strategy file-name derivation set stage)))))

(defun parse-stop-criteria (file)
  (load file)
  (values))

(defun parse-result-point (file initial-point)
  (with-open-file (stream file :direction :input)
    (let ((solver)
          (solver-status)
          (solver-iteration)
          (solver-time)
          (objective-value)
          (point (make-hash-table :test 'equalp)))
      (loop
         for line = (read-line stream nil nil) while line

         when (begins-with "     SOLVER  " line :test #'string=)
         do (setq solver (string-nth-column line 2))

         when (begins-with "**** MODEL STATUS " line :test #'string=)
         do (setq solver-status (parse-integer
                                 (string-nth-column line 4)))

         when (begins-with "**** OBJECTIVE VALUE " line :test #'string=)
         do (setq objective-value (parse-real-number
                                   (string-nth-column line 4)))

         when (begins-with " ITERATION COUNT, LIMIT " line :test #'string=)
         do (setq solver-iteration (parse-integer
                                    (string-nth-column line 4)))

         when (begins-with " RESOURCE USAGE, LIMIT " line :test #'string=)
         do (setq solver-time (parse-positive-real-number
                               (string-nth-column line 4)))

         when (begins-with "---- VAR " line :test #'string=)
         do (parse-variables stream point line)
           (loop-finish))
      (make-result-point initial-point solver
                         solver-status solver-iteration solver-time
                         objective-value point))))

(defun parse-variables (stream point first-line)
  (declare (type stream stream)
           (hash-table point)
           (string first-line))
  (loop
     with line = first-line
     for variable-name = (string-nth-column line 3)
     for variable = (find variable-name *variables*
                          :key #'gams-variable-name
                          :test #'string-equal)

     do (assert (begins-with "---- VAR " line :test #'string=) ()
                "line must be the first line of a variable display")

     if variable
     do (setq line
              (if (zerop (gams-variable-dimension variable))
                  (parse-scalar-variable stream point line)
                  (parse-vector-variable stream point line)))
     else
     do (setq line (read-next-data-or-variable-line stream))

     until (begins-with "**** REPORT SUMMARY :" line :test #'string=)))

(defun read-next-data-or-variable-line (stream)
  (declare (type stream stream))
  (loop
     for line = (read-line stream)

     when (starts-with #\Page line)
     do (read-line stream)
       (read-line stream)

     until (or (begins-with "**** REPORT SUMMARY :" line :test #'string=)
               (begins-with "---- VAR " line :test #'string=)
               (and (> (length line) 0)
                    (alpha-char-p (elt line 0))))
     finally (return line)))

(defun parse-scalar-variable (stream point line)
  (declare (type stream stream)
           (hash-table point)
           (string line))
  (let ((columns (split-sequence #\Space line
                                 :remove-empty-subseqs t)))
    (assert (member (length columns) '(6 7) :test #'=))
    (setf (gethash (make-point-key (third columns)) point)
          (let ((value (fifth columns)))
            (if (string= "." value)
                0
                (the real (parse-number value)))))
    (read-next-data-or-variable-line stream)))

(defun parse-vector-variable (stream point first-line)
  (declare (type stream stream)
           (hash-table point)
           (string first-line))
  (let ((variable-name (string-nth-column first-line 3)))
    (loop
       for line = (read-next-data-or-variable-line stream)
       while (alpha-char-p (elt line 0))

       do (let ((columns (split-sequence #\Space line
                                         :remove-empty-subseqs t
                                         :count 3)))
            (setf (gethash (apply #'make-point-key variable-name
                                  (split-sequence #\. (first columns)))
                           point)
                  (let ((value (third columns)))
                    (if (string= "." value)
                        0
                        (the real (parse-number value))))))

       finally (return line))))

                                        ; main

(if (>= (length *arguments*) 8)
    ;; TODO : replace huge let by destructuring-bind
    (destructuring-bind (initial-points-directory
                         sets-list stop-criteria-list strategies-list
                         model-name variable-list
                         *set-point-load* *initial-point-loader*
                         . solvers)
        *arguments*
      (let ((*sets*                   (parse-sets  sets-list))
            (stop-criteria            (parse-stop-criteria stop-criteria-list))
            (strategies               (parse-strategies    strategies-list))
            (*variables*              (parse-variable-list variable-list)))
        (declare (ignore stop-criteria))
        (let* ((min-result) (max-result))
          (loop named main-loop
             for feasible-point-found-p = nil

             for (set-point current-set) = (list (make-set-point))
             then (let ((*sets* (if feasible-point-found-p
                                    *sets*
                                    (remove current-set *sets*))))
                    (multiple-value-list
                     (generate-next-set-point set-point result-points)))

             for result-points = (make-array (set-point->result-known-dimensions set-point)
                                             :element-type 'list :adjustable t)
             then (if set-point
                      (adjust-array result-points (set-point->result-known-dimensions set-point))
                      result-points)

             for result-point-coordinates = (when set-point
                                              (set-point->result-coordinate set-point))

             for initial-points = (when set-point
                                    (let ((stage (cond
                                                   ((null current-set) :empty-set)
                                                   ((= (gethash current-set set-point) 1) :first-element)
                                                   (t :additional-element))))
                                      (generate-initial-points strategies set-point
                                                               stage current-set result-points)))

             while set-point

             do (load-set-point set-point)
               (map 'nil (rcurry #'write-initial-point initial-points-directory) initial-points)
               (flet ((add-result (result)
                        (push result (apply #'aref result-points result-point-coordinates))
                        (when (or (null min-result)
                                  (< (result-point-objective-value result)
                                     (result-point-objective-value min-result)))
                          (setq min-result result))
                        (when (or (null max-result)
                                  (> (result-point-objective-value result)
                                     (result-point-objective-value max-result)))
                          (setq max-result result))
                        (when (feasible-point-p result)
                          (setq feasible-point-found-p t))))
                 (map 'nil
                      (lambda (initial-point)
                        (declare (type initial-point initial-point))
                        (load-initial-point initial-point)
                        (map 'nil
                             (compose #'add-result
                                      (curry #'solve-gams-model model-name initial-point))
                             solvers))
                      initial-points))

             finally (return-from main-loop result-points))
          (print-result-point min-result)
          (print-result-point max-result))))
    (unless (null *arguments*)
      (error "This script requires at least 7 arguments: initial-points-directory dynamic-sets stop-criteria strategies gams-model.gms variables.inc set-point.inc initializer.inc [solvers ...]")))
