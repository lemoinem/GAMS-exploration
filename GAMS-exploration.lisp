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
  (require 'GAMS-exploration)
  (require 'script-utility)
  (require 'priority-fifo))

(defpackage #:GAMS-exploration-script
  (:use #:cl #:alexandria
        #:script-utility #:priority-fifo
        #:GAMS-exploration))

(in-package #:GAMS-exploration-script)

(block nil
  (destructuring-bind (*lst-directory*
                       min-file max-file errors-file
                       initial-points-directory
                       sets-list stop-criteria-list strategies-list
                       model-name variable-list
                       *set-point-loader* *initial-point-loader*
                       solvers-list)
      (let ((args (mapcar #'get-argument '("--lsts" "--min" "--max"
                                           "--errors"
                                           "--init-points" "--sets"
                                           "--stops" "--strategies"
                                           "--gms" "--variables"
                                           "--current-sets"
                                           "--current-point"
                                           "--solvers"))))
        (when (or (some #'null (nthcdr 3 args))
                  (get-argument "--help" :boolean-p t))
          (format *error-output*
                  "Mandatory arguments:
--init-points
~Tdirectory in which initial points will be stored.
--sets
~Tlist of sets of indices to explore
--stops
~Tlist of stop criteria
--strategies
~Tlist of strategies
--gms
~TGAMS model
--variables
~Tlist of variables in the GAMS model
--current-sets
~Tfile in which the current set point will be stored
--current-point
~Tfile in which the current start point will be stored
--solvers
~Tlist of solvers directives
--help
~Tdisplay this help message then quit

Optional arguments:
--lsts
~Tdirectory in which the lst generated by GAMS will be stored
--min
~Tfile in which will be stored the minimal solution found so far
--max
~Tfile in which will be stored the maximal solution found so far
--errors
~Tfile in which the list of GAMS errors will be stored
Current args:~%~A" *arguments*)
          (return))
        args)
    (let* ((*sets*                   (parse-sets          sets-list))
           (stop-criteria            (parse-stop-criteria stop-criteria-list))
           (strategies               (parse-strategies    strategies-list))
           (*variables*              (parse-variable-list variable-list))
           (solvers                  (parse-solver-list   solvers-list)))
      (declare (ignore stop-criteria strategies))
      (let* ((min-result) (max-result))
        (loop named main-loop
           with feasible-point-found-p = t
           with todo = (make-priority-fifo (curry #'every #'<=) (make-set-point))

           for set-point = (priority-fifo-pop todo)

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
                                  (loop
                                     for set in *sets*

                                     for old-set-point = (let ((p (copy-hash-table set-point)))
                                                           (decf (gethash set p))
                                                           p)

                                     for previous-result-points =
                                       (let ((coord (set-point->result-coordinate old-set-point)))
                                         (unless (some #'minusp coord)
                                           (apply #'aref result-points coord)))

                                     append
                                       (let ((stage (cond
                                                      ((or (null set)
                                                           (= (gethash set set-point) 0)) :empty-set)
                                                      ((= (gethash set set-point) 1) :first-element)
                                                      (t :additional-element))))
                                         (generate-initial-points set-point stage
                                                                  set previous-result-points))))

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
                          (setq min-result result)
                          (when min-file
                            (with-open-file (file min-file :direction :output :if-exists :supersede)
                              (print-result-point result file))))
                        (when (or (null max-result)
                                  (> (result-point-objective-value result)
                                     (result-point-objective-value max-result)))
                          (setq max-result result)
                          (when max-file
                            (with-open-file (file max-file :direction :output :if-exists :supersede)
                              (print-result-point result file)))))))
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
             (priority-fifo-pushes todo
                                   (loop
                                      for set in *sets*

                                      for new-set-point = (let ((p (copy-hash-table set-point)))
                                                            (incf (gethash set p))
                                                            p)
                                      unless (>= (gethash set new-set-point)
                                                 (dynamic-set-max-size set))
                                      collect new-set-point))


           finally (return-from main-loop result-points))
        (format t "**** MINIMAL SOLUTION ****~%")
        (print-result-point min-result)
        (format t "**** MAXIMAL SOLUTION ****~%")
        (print-result-point max-result)
        (when *GAMS-errors*
          (format t "**** ERROR POINTS ****~%~%")
          (let ((text (apply #'concatenate 'string
                             (mapcar (lambda (error-point)
                                       (destructuring-bind (exit-code GAMS-model initial-point solvers) error-point
                                         (format nil "exit-code: ~A~%model: ~A~%solvers: ~A~%initial point: ~A {{~A}}~%~%"
                                                 exit-code GAMS-model solvers
                                                 (initial-point-file-name initial-point)
                                                 (initial-point-history   initial-point))))
                                     *GAMS-errors*))))
            (format t "~A" text)
            (when errors-file
              (with-open-file (file errors-file :direction :output :if-exists :supersede)
                (format file "~A" text)))))))))
