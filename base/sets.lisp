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

(defparameter *sets* ()
  "List of dynamics sets.")

(defstruct (dynamic-set
             (:constructor make-dynamic-set
                           (name &optional max-size (start-size 0) (min-size 1))))
  (name          nil                     :read-only t :type string)
  (max-size      nil                                  :type (or null integer))
  (min-size       1                                   :type integer)
  (start-size     0                      :read-only t :type integer)
  (stop-criteria nil                                  :type list))

(deftype set-name-descriptor ()
  "Dynamic set name descriptor."
  '(or string symbol))

(defun make-set-name (name)
  "Generates a set name (string) from a string or a symbol."
  (declare (type set-name-descriptor name))
  (if (and (symbolp name) (not (null name)))
      (symbol-name name)
      name))

(defun find-set-from-name (name)
  "Returns the set having this name."
  (declare (type (or null set-name-descriptor) name))
  (when name
    (the (or null dynamic-set)
      (find (make-set-name name) *sets* :key #'dynamic-set-name :test #'string-equal))))

(defun set-point-p (set-point)
  "Returns whether set-point is a instance of set-point.
A set-point is an hash-table having an unsigned-byte value assigned to each set
 and if this value is greater than or equal to the start-size of this set .
This definition does not prevent the hash-table from containing any other data."
  (declare (hash-table set-point))
  (every (lambda (set)
           (let ((value (gethash set set-point)))
             (and (typep value 'unsigned-byte)
                  (>= value (dynamic-set-start-size set)))))
         *sets*))

(deftype set-point ()
  "A set-point assignes a size to the dynamic sets of a GAMS model."
  '(satisfies set-point-p))

(defun make-set-point (&rest args)
  "Creates a new set-point."
  (let ((set-point (make-hash-table :test #'equalp :size (length *sets*))))
    (map nil (lambda (set)
               (setf (gethash set set-point)
                     (the integer (or (second
                                       (member (dynamic-set-name set) args
                                               :key #'make-set-name
                                               :test #'string-equal))
                                      (dynamic-set-start-size set)))))
         *sets*)
    (the set-point set-point)))
