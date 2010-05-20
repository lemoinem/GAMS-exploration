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

(defparameter *set-point-loader* ()
  "File containing the GAMS representation of the current set-point")

(defun load-set-point (set-point)
  "Loads this set-point in *SET-POINT-LOADER*."
  (declare (type set-point set-point))
  (with-open-file (stream *set-point-loader*
                          :direction :output
                          :if-exists :supersede)
    (format stream "$phantom null~%")
    (maphash (lambda (set current-size)
               (let ((name (dynamic-set-name set)))
                 (format stream "Set ~A /~{~A~^, ~}/;~%~%"
                         name
                         (or (mapcar (curry #'generate-set-element set)
                                     (iota current-size :start 1))
                             '("null")))))
             set-point)))

(defun write-GAMS-point (point &optional (stream *standard-output*))
  "Writes the GAMS point using the GAMS syntax."
  (declare (type point point))
  (maphash (lambda (key val)
             (format stream "~A.l~@[(~{'~A'~^,~})~] = ~A;~%"
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
                                       (subseq history (1+ (position #\/ history :from-end t)))
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
            (write-GAMS-point (result-point-point
                               (initial-point-result-point initial-point))
                              stream)
            (princ #\Newline))
          (format stream "$batinclude ~A" (strategy-file-name strategy))
          (unless (eql strategy-derivation :independent)
            (format stream " ~A" old-set-element)
            (when (eql strategy-derivation :family)
              (format stream " ~A" new-set-element)))
          (princ #\Newline))))))

(defun initial-point-file-number (instance)
  (declare (type initial-point instance))
  (with-accessors ((file-name initial-point-file-name)) instance
    (when file-name
      (let ((base-name (subseq file-name (1+ (position #\/ file-name :from-end t)))))
        (parse-integer (subseq base-name 0 (position #\. base-name)))))))

(defparameter *initial-point-loader* ""
  "File containing the GAMS representation of the current initial-point.")

(defun load-initial-point (initial-point)
  "Loads this initial-point in *INITIAL-POINT-LOADER*."
  (declare (type initial-point initial-point))
  (with-open-file (stream *initial-point-loader*
                          :direction :output
                          :if-exists :supersede)
    (format stream "$include ~A~%" (initial-point-file-name initial-point)))
  (values))

(defparameter *variables* ()
  "List of GAMS model variables.")

(defstruct (GAMS-variable
             (:constructor make-GAMS-variable (name dimension)))
  (name      nil :read-only t :type string)
  (dimension nil :read-only t :type integer))

(defparameter *GAMS-errors* ()
  "List of errors GAMS returned.")

(defun store-GAMS-error (exit-code GAMS-model initial-point solvers)
  "Simplest GAMS error handler: store.
Logs any error GAMS returns and propagates it."
  (push (list exit-code GAMS-model initial-point solvers) *GAMS-errors*)
  (let ((solver-status (case exit-code
                         (7 11)
                         (t 13))))
    (values (make-result-point initial-point (format nil "~{~A~^ ~}" solvers) solver-status) t)))

(defun signal-GAMS-error (exit-code GAMS-model initial-point solvers)
  "Simplest GAMS error handler: signal.
Signals any error GAMS returns."
  (error "gams exited with error code ~A (gams ~A ~{~A~^ ~})"
         exit-code GAMS-model solvers))

(defun advanced-GAMS-error-handler (exit-code GAMS-model initial-point solvers)
  "GAMS error handler.
If GAMS returns a n°3 error (execution error, often due to arithmetic error)
             or a n°7 error (licensing error),
  stores it;
If GAMS returns any other error,
  signals it."
  (if (member exit-code '(3 7))
      (store-GAMS-error exit-code GAMS-model initial-point solvers)
      (signal-GAMS-error exit-code GAMS-model initial-point solvers)))

(defparameter *lst-directory* nil
  "Directory in which lst files are stored.")

(defun solve-GAMS-model (GAMS-model initial-point &optional solvers (GAMS-error-handler #'signal-GAMS-error))
  "Runs GAMS on the model and returns the result point.
The initial-point must have been load and is used only to generate result point.
/!\\ This function is currently implemented only for SBCL /!\\"
  (declare (type initial-point initial-point))
  (let ((result-file (when *lst-directory*
                       (concatenate 'string
                                    *lst-directory*
                                    (subseq GAMS-model 0
                                            (search ".gms" GAMS-model
                                                    :from-end t))
                                    ".lst"))))
    (unwind-protect
         (progn
           #+sbcl (let ((exit-code (sb-ext:process-exit-code
                                    (sb-ext:run-program "gams" (list* GAMS-model "lo=3" solvers)
                                                        :search t
                                                        :output *standard-output*))))
                    (unless (zerop exit-code)
                      (multiple-value-bind (return-value return-p)
                          (funcall GAMS-error-handler exit-code GAMS-model initial-point solvers)
                        (when return-p (return-from solve-GAMS-model return-value)))))
           #-sbcl (error "This script currently only supports SBCL")
           (parse-result-point result-file initial-point))
      (when (file-exists-p result-file)
        (let ((file (format nil "~A.~:[default~;~:*~{~A~^,~}~].~A"
                            (initial-point-file-number initial-point)
                            solvers result-file)))
          (if result-file
              (copy-file result-file file)
              (delete-file file)))))))
