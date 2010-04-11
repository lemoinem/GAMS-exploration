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

(cl:in-package #:GAMS-dynamic-sets)

(defun string-replace (str old new)
  "Replaces all occurences of old in str by new."
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

(defun string-nth-column (str nth)
  "Returns the nth column of a space-separated list."
  (declare (string str)
           (integer nth))
  (the string
    (nth (1- nth)
         (split-sequence #\Space str
                         :remove-empty-subseqs t
                         :count nth))))

(defun begins-with (start sequence &key (test #'eql))
  "Returns wether start is a subsequence of sequence starting at index 0."
  (declare (type sequence sequence))
  (let ((length (length start)))
    (when (>= (length sequence) length)
      (funcall test start (subseq sequence 0 length)))))
