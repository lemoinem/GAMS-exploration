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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (stop-criterion
               (:constructor make-stop-criterion
                             (name function)))
    "Criterion used to stop the growing of a set.
function must a callable object receiving 3 arguments."
    (name        nil :read-only t :type symbol)
    (function    nil :read-only t :type function)))

(defconstant +check-max-set-size-stop-criterion+
  (if (boundp '+check-max-set-size-stop-criterion+)
      (symbol-value '+check-max-set-size-stop-criterion+)
      (make-stop-criterion 'check-max-set-size
                           (lambda (set-point _)
                             (declare (ignore _)
                                      (type set-point set-point))
                             (some (lambda (set)
                                     (when (dynamic-set-max-size set)
                                       (> (dynamic-set-max-size set)
                                          (gethash set set-point))))
                                   *sets*)))))

(defparameter *stop-criteria*
  (list +check-max-set-size-stop-criterion+)
  "List of stop criteria.")

(defun clear-stop-criteria ()
  "Removes all stop criteria."
  (setq *stop-criteria*
        (list +check-max-set-size-stop-criterion+))
  (values))

(defun stop-criteria-reached-p (set-point result-points)
  (and (every (lambda (set)
                (declare (type dynamic-set set))
                (or (null (dynamic-set-min-size set))
                    (> (gethash set set-point)
                       (dynamic-set-min-size set))))
              *sets*)
       (some (lambda (stop-criterion)
               (declare (type stop-criterion stop-criterion))
               (funcall (stop-criterion-function stop-criterion)
                        set-point result-points))
             *stop-criteria*)))

(defmacro sets-max-size (&rest args)
  "Assignes various max sizes to dynamic sets.
args is a alist of pairs set-name/size."
  `(setf
    ,@(loop
         for (set size) on args by #'cddr
         append (list `(dynamic-set-max-size (find-set-from-name ',set)) size))))

(defmacro sets-min-size (&rest args)
  "Assignes various min sizes to dynamic sets.
args is a alist of pairs set-name/size."
  `(setf
    ,@(loop
         for (set size) on args by #'cddr
         append (list `(dynamic-set-min-size (find-set-from-name ',set)) size))))

(defmacro def-stop-criterion (name
                              (&optional (set-point-var     (gensym "set-point/"))
                                         (result-points-var (gensym "result-points/")))
                              &body body
                              &aux (args (list set-point-var result-points-var)))
  "Creates a new stop criterion.
set-point-var and result-points-var are arguments of the criterion.
They are always setted to, respectively:
- the current set-point;
- the result points back store.
These variables are read-only.
The behavior is undefined any of them is modified within the evaluation of the criterion."
  `(push (make-stop-criterion ',name
                              (lambda ,args
                                (declare (type set-point ,set-point-var)
                                         (ignorable ,@args))
                                ,@body))
         *stop-criteria*))
