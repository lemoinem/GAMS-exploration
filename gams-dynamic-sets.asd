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

(defpackage #:GAMS-dynamic-sets-system
  (:use #:cl #:asdf))

(in-package #:GAMS-dynamic-sets-system)

(defsystem :GAMS-dynamic-sets
  :licence "3-clause BSD Licence"
  :description "GAMS driver to manipulate dynamic sets and provide customized stratgies to create new initial points."
  :depends-on ("alexandria" "cl-fad" "cl-ppcre" "split-sequence" "parse-number" "script-utility")
  :serial t
  :components
  ((:module "docs"
            :components
            ((:static-file "gams-dynamic-sets.tex")
             (:static-file "gams-dynamic-sets.pdf")))
   (:module "base"
            :serial t
            :components
            ((:file "package")
             (:file "utilities")
             (:file "sets")))
   (:module "backend"
            :components
            ((:file "stop-criteria")
             (:file "strategies")
             (:file "points" :depends-on ("strategies"))))
   (:module "frontend"
            :components
            ((:file "GAMS")
             (:file "helpers")))
   (:file "parsers")))
