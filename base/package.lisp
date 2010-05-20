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

(cl:defpackage #:GAMS-exploration
  (:use #:cl #:alexandria #:split-sequence #:parse-number)
  (:import-from #:ppcre #:register-groups-bind)
  (:shadowing-import-from #:fad #:list-directory #:copy-file #:file-exists-p)
  (:export

;;; UTILITIES
   #:string-replace #:string-nth-column
   #:seq-contains-only-p #:begins-with

;;; SETS
   #:*sets*
   #:dynamic-set #:make-dynamic-set #:copy-dynamic-set #:dynamic-set-p
   #:dynamic-set-name #:dynamic-set-max-size #:dynamic-set-min-size
   #:dynamic-set-start-size #:dynamic-set-stop-criteria #:dynamic-set-strategies

   #:set-name-descriptor #:make-set-name
   #:find-set-from-name

   #:set-point #:make-set-point #:set-point-p

;;; STRATEGIES
   #:derivation

   #:strategy #:make-stratey #:copy-strategy #:strategy-p
   #:strategy-file-name #:strategy-derivation #:strategy-step-set
   #:strategy-name #:strategy-set-min-size #:strategy-set-max-size
   #:strategy-applicable-p

;;; POINTS
   #:initial-point #:make-initial-point #:copy-initial-point #:initial-point-p
   #:initial-point-strategy #:initial-point-set-point #:initial-point-result-point
   #:initial-point-set-index #:initial-point-file-name #:initial-point-set

   #:point-key #:make-point-key #:copy-point-key #:point-key-p
   #:point-key-name #:point-key-indices

   #:point #:point-p

   #:result-point #:make-result-point #:copy-result-point #:result-point-p
   #:result-point-initial-point #:result-point-solver
   #:result-point-solver-status #:result-point-solver-iteration #:result-point-solver-time
   #:result-point-objective-value #:result-point-point

   #:erroneous-point-p #:feasible-point-p

;;; STOP-CRITERIA
   #:stop-criterion #:make-stop-criterion #:copy-stop-criterion #:stop-criterion-p
   #:stop-criterion-name #:stop-criterion-function #:stop-criteria-reached-p

   #:*stop-criteria*
   #:sets-max-size #:sets-min-size #:def-stop-criterion

;;; HELPERS
   #:generate-set-element

   #:set-point->string
   #:set-point->result-coordinate #:set-point->result-known-dimensions

   #:generate-initial-points
   #:initial-point-history

   #:print-result-point
   
;;; GAMS
   #:*set-point-loader* #:load-set-point

   #:write-GAMS-point

   #:write-initial-point #:initial-point-file-number
   #:*initial-point-loader* #:load-initial-point

   #:*variables*
   #:GAMS-variable #:make-GAMS-variable #:copy-GAMS-variable #:GAMS-variable-p
   #:GAMS-variable-name #:GAMS-variable-dimension

   #:*lst-directory*
   #:solve-GAMS-model
   #:*GAMS-errors* #:store-GAMS-error
   #:signal-GAMS-error #:advanced-GAMS-error-handler

;;; PARSERS
   #:parse-sets
   #:parse-variable-list
   #:parse-strategies
   #:parse-stop-criteria
   #:parse-result-point
   #:parse-solver-list
   ))
