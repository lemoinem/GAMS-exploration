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

(cl:in-package #:GAMS-exploration)

(defun generate-set-element (set nth)
  "Generate the nth GAMS set element name."
  (declare (type dynamic-set set)
           (unsigned-byte nth))
  (the string
    (concatenate 'string
                 (string-downcase (dynamic-set-name set))
                 (write-to-string nth))))

(defun set-point->string (set-point)
  "Serializes a set-point as a string."
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
  "Generates the coordinates of this set-point's results in the results back store."
  (declare (type set-point set-point))
  (loop
     for set in *sets*
     collect (gethash set set-point)))

(defun set-point->result-known-dimensions (set-point)
  "Generates the dimensions of the results back store to store results up to this set-point."
  (declare (type set-point set-point))
  (loop
     for set in *sets*
     collect (1+ (or (dynamic-set-max-size set)
                     (gethash set set-point)))))

(defun generate-initial-points (set-point strategies result-points)
  "Apply the usable strategies to generate various initial points at this set-point."
  (declare (type set-point set-point))
  (let ((strategies (remove-if (compose #'not
                                        (rcurry #'strategy-applicable-p set-point))
                               strategies)))
    (loop
       for strategy in strategies

       for set = (strategy-step-set strategy)
       for %make-initial-point = (curry #'make-initial-point strategy set-point)

       for previous-set-point = (when set
                                  (let ((p (copy-hash-table set-point)))
                                    (decf (gethash (strategy-step-set strategy) set-point))
                                    p))

       for previous-result-points = (when set
                                      (remove-if (compose #'not #'feasible-point-p)
                                                 (aref (set-point->result-coordinate previous-set-point)
                                                       result-points)))

       when (eql (strategy-derivation strategy) :independent) collect (funcall %make-initial-point)
       when (eql (strategy-derivation strategy) :derived)     append  (mapcar %make-initial-point
                                                                              previous-result-points)
       when (eql (strategy-derivation strategy) :family)      append  (map-product
                                                                       %make-initial-point
                                                                       previous-result-points
                                                                       (iota (gethash set
                                                                                      previous-set-point)
                                                                             :start 1)))))

(defun initial-point-history (instance)
  "Returns this initial-point history.
The history is a slash-separated list of
 stategy, set-point, solver and set-element used to generate this initial point."
  (declare (type initial-point instance))
  (with-accessors ((strategy initial-point-strategy)) instance
    (with-accessors ((strategy-derivation strategy-derivation)) strategy
      (the string
        (apply #'concatenate 'string
               (let ((local-begining (list
                                      "/" (strategy-name strategy) "("
                                      (set-point->string (initial-point-set-point instance))
                                      ")")))
                 (if (eql strategy-derivation :independent)
                     local-begining
                     (with-accessors ((current-set  initial-point-set)
                                      (result-point initial-point-result-point))
                         instance
                       (append (list (initial-point-history (result-point-initial-point result-point)))
                               (when #1=(result-point-solver result-point)
                                     (list "," #1#))
                               local-begining
                               (list ","
                                     (generate-set-element current-set
                                                           (gethash current-set
                                                                    (initial-point-set-point instance))))
                               (when (eql strategy-derivation :family)
                                 (list ","
                                       (generate-set-element current-set
                                                             (initial-point-set-index instance)))))))))))))

(defun print-result-point (point &optional (stream *standard-output*))
  "Displays a result-point."
  (declare (type (or null result-point) point))
  (unless point
    (return-from print-result-point))
  (format stream "initial point: ~A~%"
          (initial-point-file-name (result-point-initial-point point)))
  (format stream "solver: ~A~%" (result-point-solver point))
  (format stream "status: ~A~%" (result-point-solver-status point))
  (format stream "iteration count: ~A~%" (result-point-solver-iteration point))
  (format stream "time: ~A~%" (result-point-solver-time point))
  (format stream "objective value: ~A~%~%" (result-point-objective-value point))
  (write-GAMS-point (result-point-point point) stream)
  (princ #\Newline)
  (values))
