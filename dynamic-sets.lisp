#!/usr/local/bin/sbcl-script
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

(cl:in-package #:cl)

(declaim (optimize debug))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'alexandria)
  (require 'GAMS-dynamic-sets)
  (require 'script-utility))

(defpackage #:GAMS-dynamic-sets-script
  (:use #:cl #:alexandria
        #:script-utility #:GAMS-dynamic-sets))

(in-package #:GAMS-dynamic-sets-script)


;; TODO replace positional arguments by keys arguments (--args)
;; TODO add an argument to store lst files
(if (= (length *arguments*) 9)
    (destructuring-bind (initial-points-directory
                         sets-list stop-criteria-list strategies-list
                         model-name variable-list
                         *set-point-loader* *initial-point-loader*
                         solvers-list)
        *arguments*
      (let* ((*sets*                   (parse-sets          sets-list))
             (stop-criteria            (parse-stop-criteria stop-criteria-list))
             (strategies               (parse-strategies    strategies-list))
             (*variables*              (parse-variable-list variable-list))
             (solvers                  (parse-solver-list   solvers-list)))
        (declare (ignore stop-criteria strategies))
        (let* ((min-result) (max-result))
          (loop named main-loop
             with feasible-point-found-p = t

             for previous-result-points = nil
             then (apply #'aref result-points result-point-coordinates)

             for (set-point current-set) = (list (make-set-point))
             then (let ((*sets* (if (or feasible-point-found-p
                                        (null current-set)
                                        (< (gethash current-set set-point)
                                           (dynamic-set-min-size current-set)))
                                    *sets*
                                    (remove current-set *sets*))))
                    (multiple-value-list
                     (generate-next-set-point set-point result-points)))

             for result-points = (make-array (set-point->result-known-dimensions set-point)
                                             :element-type 'list :initial-element nil
                                             :adjustable t)
             then (if set-point
                      (adjust-array result-points (set-point->result-known-dimensions set-point)
                                    :initial-element nil)
                      result-points)

             for result-point-coordinates = (when set-point
                                              (set-point->result-coordinate set-point))

             for initial-points = (when set-point
                                    (let ((stage (cond
                                                   ((null current-set) :empty-set)
                                                   ((= (gethash current-set set-point) 1) :first-element)
                                                   (t :additional-element))))
                                      (generate-initial-points set-point stage
                                                               current-set previous-result-points)))

             while set-point

             do (load-set-point set-point)
               (map 'nil (rcurry #'write-initial-point initial-points-directory) initial-points)
               (setq feasible-point-found-p nil)
               (flet ((add-result (result)
                        (push result (apply #'aref result-points result-point-coordinates))
                        (when (feasible-point-p result)
                          (setq feasible-point-found-p t)
                          (when (or (null min-result)
                                    (< (result-point-objective-value result)
                                       (result-point-objective-value min-result)))
                            (setq min-result result))
                          (when (or (null max-result)
                                    (> (result-point-objective-value result)
                                       (result-point-objective-value max-result)))
                            (setq max-result result)))))
                 (map 'nil
                      (lambda (initial-point)
                        (declare (type initial-point initial-point))
                        (load-initial-point initial-point)
                        (map 'nil
                             (lambda (solver)
                               (add-result
                                (solve-GAMS-model model-name initial-point solver
                                                  #'store-GAMS-error)))
                             solvers))
                      initial-points))

             finally (return-from main-loop result-points))
          (format t "**** MINIMAL SOLUTION ****~%")
          (print-result-point min-result)
          (format t "**** MAXIMAL SOLUTION ****~%")
          (print-result-point max-result)
          (when *GAMS-errors*
            (format t "**** ERROR POINTS ****~%~%")
            (map 'nil (lambda (error-point)
                        (destructuring-bind (exit-code GAMS-model initial-point solvers) error-point
                          (format t "exit-code: ~A~%model: ~A~%solvers: ~A~%initial point: ~A {{~A}}~%~%"
                                  exit-code GAMS-model solvers
                                  (initial-point-file-name initial-point)
                                  (initial-point-history   initial-point))))
                 *GAMS-errors*)))))
    (unless (and nil (null *arguments*))
      (error "This script requires 9 arguments: initial-points-directory dynamic-sets stop-criteria strategies GAMS-model.gms variables.inc set-point.inc initializer.inc solvers~%~A" *arguments*)))
