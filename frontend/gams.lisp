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

(cl:in-package #:gams-dynamic-sets)

(defparameter *set-point-loader* ()
  "File containing the GAMS representation of the current set-point")

(defun load-set-point (set-point)
  "Loads this set-point in *SET-POINT-LOADER*."
  (declare (type set-point set-point))
  (with-open-file (stream *set-point-loader*
                          :direction :output
                          :if-exists :supersede)
    (maphash (lambda (set current-size)
               (let ((name (dynamic-set-name set)))
                 (format stream "$phantom ~A~%Set ~:*~A /~{~A~^, ~}/;~%~%"
                         name
                         (or (mapcar (curry #'generate-set-element set)
                                     (iota current-size :start 1))
                             (list name)))))
             set-point)))

(defun write-gams-point (point &optional (stream *standard-output*))
  "Writes the GAMS point using the GAMS syntax."
  (declare (type point point))
  (maphash (lambda (key val)
             (format stream "~A.l~@[(~{'~A'~^,~})~] = ~A~%"
                     (point-key-name key) (point-key-indices key)
                     val))
           point))

(defun write-initial-point (initial-point &optional (directory "initial-points/"))
  "Write the initial point GAMS file."
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
         (new-set-element (when current-set
                            (generate-set-element current-set
                                                  (gethash current-set
                                                           (initial-point-set-point initial-point)))))
         (old-set-element (when current-set
                            (generate-set-element current-set
                                                  (initial-point-set-index initial-point)))))
    (setf (initial-point-file-name initial-point) file)
    (with-accessors ((strategy initial-point-strategy)) initial-point
      (with-accessors ((strategy-derivation strategy-derivation)) strategy
        (with-open-file (stream file :direction :output :if-does-not-exist :create)
          (format stream "* ~A~%" history)
          (unless (eql strategy-derivation :independent)
            (write-gams-point stream)
            (princ #\Newline))
          (format stream "$batinclude ~A" (strategy-file-name strategy))
          (unless (eql strategy-derivation :independent)
            (format stream " ~A" old-set-element)
            (when (eql strategy-derivation :family)
              (format stream " ~A" new-set-element)))
          (princ #\Newline))))))

(defparameter *initial-point-loader* ""
  "File containing the GAMS representation of the current initial-point.")

(defun load-initial-point (initial-point)
  "Loads this initial-point in *INITIAL-POINT-LOADER*."
  (declare (type initial-point initial-point))
  (with-open-file (stream *initial-point-loader*
                          :direction :output
                          :if-exists :supersede)
    (format stream "$include ~A" (initial-point-file-name initial-point)))
  (values))

(defparameter *variables* ()
  "List of GAMS model variables.")

(defstruct (gams-variable
             (:constructor make-gams-variable (name dimension)))
  (name      nil :read-only t :type string)
  (dimension nil :read-only t :type integer))

(defun solve-gams-model (gams-model initial-point &optional solvers)
  "Runs GAMS on the model and returns the result point.
The initial-point must have been load and is used only to generate result point.
/!\\ This function is currently implemented only for SBCL /!\\"
  (declare (type initial-point initial-point))
  #+sbcl (let ((exit-code (sb-ext:process-exit-code
                           (sb-ext:run-program "gams" (list* gams-model solvers)
                                               :search t
                                               :output *standard-output*))))
           (unless (zerop exit-code)
             (error "gams exited with error code ~A (gams ~A ~{~A~^ ~})"
                    exit-code gams-model solvers)))
  #-sbcl (error "This script currently only supports SBCL")
  (parse-result-point (concatenate 'string
                                   (subseq gams-model 0
                                           (search ".gms" gams-model
                                                   :from-end t))
                                   ".lst")
                      initial-point))
