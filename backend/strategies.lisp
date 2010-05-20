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

(deftype derivation ()
  "Allowed kinds of derivation strategy to generate a new initial point.
:independent means that no previous result point is required;
:derived means that the initial points is generated using a previous result point;
:family means that an initial point is generated for each previous element of the set
  by using a previous result point."
  '(member :independent :derived :family))

(defstruct (strategy
             (:constructor %make-strategy
                           (file-name derivation &optional step-set)))
  "Strategy to generate a new initial point."
  (file-name  nil :read-only t :type string)
  (step-set   nil :read-only t :type (or null dynamic-set))
  (derivation nil :read-only t :type derivation)
  (domain     (make-hash-table :test #'equalp :size (length *sets*))))

(defun strategy-name (instance)
  "Returns the name of a strategy."
  (declare (type strategy instance))
  (the (values string &optional) ;; http://www.sbcl.org/manual/#Implementation-Limitations
    (string-replace (strategy-file-name instance) "/" ":")))

(defun strategy-set-min-size (instance set)
  "Returns the minimum size required for the set so the strategy is applicable."
  (declare (type strategy instance)
           (type dynamic-set set))
  (the (or null unsigned-byte)
    (aref (gethash set (strategy-domain instance) #(nil)) 0)))

(defun (setf strategy-set-min-size) (new-size instance set)
  "Modifies the minimum size required for the set so the strategy is applicable."
  (declare (type (or null unsigned-byte) new-size)
           (type strategy instance)
           (type dynamic-set set))
  (multiple-value-bind (_ present)
      (gethash set (strategy-domain instance))
    (declare (ignore _))
    (unless present
      (setf (gethash set (strategy-domain instance)) #(nil nil))))
  (setf (aref (gethash set (strategy-domain instance)) 0) new-size))

(defun strategy-set-max-size (instance set)
  "Returns the maximum size required for the set so the strategy is applicable."
  (declare (type strategy instance)
           (type dynamic-set set))
  (the (or null unsigned-byte)
    (aref (gethash set (strategy-domain instance) #(0 0)) 1)))

(defun (setf strategy-set-max-size) (new-size instance set)
  "Modifies the maximum size required for the set so the strategy is applicable."
  (declare (type (or null unsigned-byte) new-size)
           (type strategy instance)
           (type dynamic-set set))
  (multiple-value-bind (_ present)
      (gethash set (strategy-domain instance))
    (declare (ignore _))
    (unless present
      (setf (gethash set (strategy-domain instance)) #(nil nil))))
  (setf (aref (gethash set (strategy-domain instance)) 1) new-size))

(defun strategy-applicable-p (instance set-point)
  "Returns whether the instance is applicable to this set-point."
  (declare (type strategy instance)
           (type set-point set-point))
  (every (lambda (set)
           (declare (type dynamic-set set))
           (let ((min (if (or (eq (strategy-derivation instance) :independent)
                              (not (equalp set (strategy-step-set instance))))
                          (or (strategy-set-min-size instance set) 0)
                          (max (or (strategy-set-min-size instance set) 1)
                               (1+ (dynamic-set-start-size set))))))
             (with-accessors ((max strategy-set-max-size)) instance
               (and (>= (gethash set set-point) min)
                    (not (null max))
                    (<= (gethash set set-point) max)))))
         *sets*))

(defun make-strategy (file-name derivation &optional step-set)
  "Create a new strategy to generate initial points.
Step-set must be not null if the derivation strategy is :derived or :family.
Step-set will be ignored if the derivation is :independent."
  (declare (type derivation derivation)
           (type (or null dynamic-set) step-set))
  (when (zerop (length file-name))
    (error "file name must be non empty"))
  (let ((step-set (when (member derivation '(:derived :family))
                    (or step-set
                        (error "Step-set must be not null if the derivation strategy is :derived or :family.")))))
    (%make-strategy file-name derivation step-set)))
