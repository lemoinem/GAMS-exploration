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

(deftype derivation ()
  "Allowed kinds of derivation strategy to generate a new initial point.
:independent means that no previous result point is required;
:derived means that the initial points is generated using a previous result point;
:family means that an initial point is generated for each previous element of the set
  by using a previous result point."
  '(member :independent :derived :family))

;; replace :empty-set by :first-evaluation
(deftype concret-stage ()
  "Allowed states of a dynamic-set set-point."
  '(member :empty-set :first-element :additional-element))

(deftype abstract-stage ()
  "Wildcards for the state of a dynamic-set."
  '(member :always :non-empty-set))

(deftype stage ()
  "Allowed restriction of use of a initial point generation strategy."
  '(or concret-stage abstract-stage))

(defstruct (strategy
             (:constructor %make-strategy
                           (file-name derivation set stage)))
  "Strategy to generate a new initial point.
Some restrictions apply to the various slots:
- file-name must not be empty;
- if the derivation strategy is :derived or :family, set must not be null;
- if the derivation strategy is :derived or :family, stage must not be :empty-set or :always;
- if the derivation strategy is :family, stage must not be :first-element or :non-empty-set
  (i.e. must be :additional-element)."
  (file-name  nil :read-only t :type string)
  (derivation nil :read-only t :type derivation)
  (stage      nil :read-only t :type stage)
  (set        nil :read-only t :type (or null dynamic-set)))

(defun strategy-name (instance)
  "Returns the name of a strategy."
  (declare (type strategy instance))
  (the (values string &optional) ;; http://www.sbcl.org/manual/#Implementation-Limitations
    (string-replace (strategy-file-name instance) "/" ":")))

(defun make-strategy (file-name derivation &optional set-name stage)
  "Creates a new strategy to generate initial points.
Stage defaults value:
- if derivation is :independent, stage defaults to :always;
- if derivation is :derived,     stage defaults to :non-empty-set;
- if derivation is :family,      stage defaults to :additional-element"
  (declare (type derivation derivation)
           (type (or null stage) stage))
  (when (zerop (length file-name))
    (error "file name must be non empty"))
  (let ((stage (or stage
                   (ecase derivation
                     (:independent :always)
                     (:derived     :non-empty-set)
                     (:family      :additional-element))))
        (set   (find-set-from-name set-name)))
    (when (and (not (null set-name))
               (null set))
      (error "set-name must be an existing set name or null"))
    (when (member derivation '(:derived :family))
      (when (null set)
        (error "'derived' and 'family' derivations require a set name"))
      (when (member stage '(:empty-set :always))
        (error "'empty set' and 'always' stages are not allowed for 'derived' or 'family' derivations")))
    (when (and
           (eql derivation :family)
           (member stage '(:first-element :non-empty-set)))
      (error "'first element' and 'non-empty set' stages are not allowed for 'family' derivation"))
    (%make-strategy file-name derivation set stage)))
