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

(cl:defpackage #:gams-dynamic-sets
  (:use #:cl #:alexandria #:split-sequence #:parse-number)
  (:shadowing-import-from #:fad #:list-directory)
  (:export

   ;;; UTILITIES
   #:string-replace #:string-nth-column
   #:begins-with

   ;;; SETS
   #:*sets*
   #:dynamic-set #:make-dynamic-set #:copy-dynamic-set #:dynamic-set-p
   #:dynamic-set-name #:dynamic-set-max-size #:dynamic-set-stop-criteria

   #:set-name-descriptor #:make-set-name
   #:find-set-from-name

   #:set-point #:make-set-point #:set-point-p

   ;;; STRATEGIES
   #:derivation #:concret-stage #:abstract-stage #:stage

   #:strategy #:make-stratey #:copy-strategy #:strategy-p
   #:strategy-file-name #:strategy-derivation #:strategy-stage #:strategy-set
   #:strategy-name

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
   #:stop-criterion-name #:stop-criterion-function

   #:*independent-stop-criteria*
   #:sets-max-size #:sets-min-size #:def-stop-criterion

   ;;; HELPERS
   #:generate-set-element

   #:set-point->string
   #:set-point->result-coordinate #:set-point->result-known-dimensions
   #:generate-next-set-point

   #:strategies-stage-filtering

   #:generate-initial-points
   #:initial-point-history

   #:print-result-point
   
   ;;; GAMS
   #:*set-point-loader* #:load-set-point
   #:write-gams-point #:write-initial-point
   #:*initial-point-loader* #:load-initial-point

   #:*variables*
   #:gams-variable #:make-gams-variable #:copy-gams-variable #:gams-variable-p
   #:gams-variable-name #:gams-variable-dimension

   #:solve-gams-model

   ;;; PARSERS
   #:parse-sets
   #:parse-variable-list
   #:parse-strategies
   #:parse-stop-criteria
   #:parse-result-point
   #:parse-solver-list
   ))
