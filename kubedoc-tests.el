;;; kubedoc-tests.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Dean Lindqvist Todevski
;;
;; Author: Dean Lindqvist Todevski <https://github.com/r0bobo>
;; Maintainer: Dean Lindqvist Todevski <dean.todevski@gmail.com>
;; Created: September 08, 2021
;; Modified: September 08, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/r0bobo/kubedoc-tests
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'cl-lib)

(load-file "kubedoc.el")

(defun kubedoc-tests-completion-function (_resource)
  ""
  "KIND:     ConfigMap
VERSION:  v1

DESCRIPTION:
     ConfigMap holds configuration data for pods to consume.

FIELDS:
   apiVersion	<string>
   binaryData	<map[string]string>
   data	<map[string]string>
   immutable	<boolean>
   kind	<string>
   metadata	<Object>
      annotations	<map[string]string>
      clusterName	<string>
      creationTimestamp	<string>
      deletionGracePeriodSeconds	<integer>
      deletionTimestamp	<string>
      finalizers	<[]string>
      generateName	<string>
      generation	<integer>
      labels	<map[string]string>
      managedFields	<[]Object>
         apiVersion	<string>
         fieldsType	<string>
         fieldsV1	<map[string]>
         manager	<string>
         operation	<string>
         time	<string>
      name	<string>
      namespace	<string>
      ownerReferences	<[]Object>
         apiVersion	<string>
         blockOwnerDeletion	<boolean>
         controller	<boolean>
         kind	<string>
         name	<string>
         uid	<string>
      resourceVersion	<string>
      selfLink	<string>
      uid	<string>
")

(defmacro kubedoc-tests-fixture (&rest body)
  `(unwind-protect
      (progn (setq kubedoc--field-completion-source-function #'kubedoc-tests-completion-function)
             ,@body)
    (setq kubedoc--field-completion-source-function nil)))

(ert-deftest kubedoc-tests--resorce-path-canonical ()
  (kubedoc-tests-fixture
   (should (string= (kubedoc--resource-path-canonical "configmaps" "metadata") "configmaps/metadata/"))
   (should (string= (kubedoc--resource-path-canonical "configmaps" "kind") "configmaps/kind"))))

(ert-deftest kubedoc-tests--field-completion-table ()
  (kubedoc-tests-fixture
   (should (seq-set-equal-p
            (kubedoc--field-completion-table "configmap")
            (reverse '("configmap/apiVersion"
                       "configmap/binaryData"
                       "configmap/data"
                       "configmap/immutable"
                       "configmap/kind"
                       "configmap/metadata/annotations"
                       "configmap/metadata/clusterName"
                       "configmap/metadata/creationTimestamp"
                       "configmap/metadata/deletionGracePeriodSeconds"
                       "configmap/metadata/deletionTimestamp"
                       "configmap/metadata/finalizers"
                       "configmap/metadata/generateName"
                       "configmap/metadata/generation"
                       "configmap/metadata/labels"
                       "configmap/metadata/managedFields/apiVersion"
                       "configmap/metadata/managedFields/fieldsType"
                       "configmap/metadata/managedFields/fieldsV1"
                       "configmap/metadata/managedFields/manager"
                       "configmap/metadata/managedFields/operation"
                       "configmap/metadata/managedFields/time"
                       "configmap/metadata/name"
                       "configmap/metadata/namespace"
                       "configmap/metadata/ownerReferences/apiVersion"
                       "configmap/metadata/ownerReferences/blockOwnerDeletion"
                       "configmap/metadata/ownerReferences/controller"
                       "configmap/metadata/ownerReferences/kind"
                       "configmap/metadata/ownerReferences/name"
                       "configmap/metadata/ownerReferences/uid"
                       "configmap/metadata/resourceVersion"
                       "configmap/metadata/selfLink"
                       "configmap/metadata/uid"))))))

(ert-deftest kubedoc-tests--completion-sort ()
  (should (equal
           (kubedoc--completion-sort '("kind" "status/" "metadata/" "apiVersion" "spec/"))
           '("metadata/" "spec/" "status/" "apiVersion" "kind"))))

(ert t)

(provide 'kubedoc-tests)
;;; kubedoc-tests.el ends here
