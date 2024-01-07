;;; kubedoc-tests.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021, 2024 Dean Lindqvist Todevski
;;
;;; Commentary:
;;
;;; Code:

(require 'cl-lib)

(load-file "kubedoc.el")

;; Make sure kubectl does not have valid config.
(setenv "KUBECONFIG" "/dev/null")

;; Disable caching during tests
(setq kubedoc-cache-enabled nil)

(defun kubedoc--field-completion-table-cached-mock (_)
  '("configmaps/metadata" "configmaps/metadata/name" "configmaps/kind"))

(defun kubedoc--resource-completion-table-cached-mock ()
  '("configmaps/" "pods/" "services/"))

(defun kubedoc-erts-test (file fun)
  ""
  (ert-test-erts-file
   file
   (lambda ()
     (let ((result (funcall fun)))
       (erase-buffer)
       (point-min)
       (insert (string-join result "\n"))
       (insert "\n")))))

(ert-deftest resorce-path-canonical ()
  (cl-letf (((symbol-function 'kubedoc--resource-completion-table-cached)
             #'kubedoc--resource-completion-table-cached-mock)
            ((symbol-function 'kubedoc--field-completion-table-cached)
             #'kubedoc--field-completion-table-cached-mock))
    (should (string=
             (kubedoc--resource-path-canonical "configmaps" "metadata")
             "configmaps/metadata/"))
    (should (string=
             (kubedoc--resource-path-canonical "configmaps" "kind")
             "configmaps/kind"))))

(ert-deftest completion-sort ()
  (should (equal
           (kubedoc--completion-sort '("kind" "status/" "metadata/" "apiVersion" "spec/"))
           '("metadata/" "spec/" "status/" "apiVersion" "kind"))))

(ert-deftest completion-table ()
  (cl-letf (((symbol-function 'kubedoc--resource-completion-table-cached)
             #'kubedoc--resource-completion-table-cached-mock)
            ((symbol-function 'kubedoc--field-completion-table-cached)
             #'kubedoc--field-completion-table-cached-mock))
    (should (equal
             (kubedoc--completion-table "configmaps/")
             '("metadata" "metadata/" "kind")))))

(ert-deftest parse-api-resources ()
  (with-temp-buffer
    (insert "configmaps
deployments.apps
nodes.metrics.k8s.io
pods
pods.metrics.k8s.io
services")
    (should
     (equal
      (kubedoc--parse-api-resources)
      '("configmaps/" "deployments.apps/" "pods/" "services/")))))

(ert-deftest parse-kubectl-explain-fields/openapiv3 ()
  (with-temp-buffer
    (insert kubectl-explain-openapiv3)
    (should
     (equal
      (kubedoc--parse-kubectl-explain-fields)
      kubectl-explain-fields-expected))))

(ert-deftest parse-kubectl-explain-fields/openapiv2 ()
  (with-temp-buffer
    (insert kubectl-explain-openapiv2)
    (should
     (equal
      (kubedoc--parse-kubectl-explain-fields)
      kubectl-explain-fields-expected))))


(defconst kubectl-explain-fields-expected
  '("apiVersion"
    "binaryData"
    "data"
    "immutable"
    "kind"
    "metadata/annotations"
    "metadata/creationTimestamp"
    "metadata/deletionGracePeriodSeconds"
    "metadata/deletionTimestamp"
    "metadata/finalizers"
    "metadata/generateName"
    "metadata/generation"
    "metadata/labels"
    "metadata/managedFields/apiVersion"
    "metadata/managedFields/fieldsType"
    "metadata/managedFields/fieldsV1"
    "metadata/managedFields/manager"
    "metadata/managedFields/operation"
    "metadata/managedFields/subresource"
    "metadata/managedFields/time"
    "metadata/name"
    "metadata/namespace"
    "metadata/ownerReferences/apiVersion"
    "metadata/ownerReferences/blockOwnerDeletion"
    "metadata/ownerReferences/controller"
    "metadata/ownerReferences/kind"
    "metadata/ownerReferences/name"
    "metadata/ownerReferences/uid"
    "metadata/resourceVersion"
    "metadata/selfLink"
    "metadata/uid"))

(defconst kubectl-explain-openapiv3
  "KIND:       ConfigMap
VERSION:    v1

DESCRIPTION:
    ConfigMap holds configuration data for pods to consume.

FIELDS:
  apiVersion	<string>
  binaryData	<map[string]string>
  data	<map[string]string>
  immutable	<boolean>
  kind	<string>
  metadata	<ObjectMeta>
    annotations	<map[string]string>
    creationTimestamp	<string>
    deletionGracePeriodSeconds	<integer>
    deletionTimestamp	<string>
    finalizers	<[]string>
    generateName	<string>
    generation	<integer>
    labels	<map[string]string>
    managedFields	<[]ManagedFieldsEntry>
      apiVersion	<string>
      fieldsType	<string>
      fieldsV1	<FieldsV1>
      manager	<string>
      operation	<string>
      subresource	<string>
      time	<string>
    name	<string>
    namespace	<string>
    ownerReferences	<[]OwnerReference>
      apiVersion	<string> -required-
      blockOwnerDeletion	<boolean>
      controller	<boolean>
      kind	<string> -required-
      name	<string> -required-
      uid	<string> -required-
    resourceVersion	<string>
    selfLink	<string>
    uid	<string>

")

(defconst kubectl-explain-openapiv2
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
         subresource	<string>
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

(provide 'kubedoc-tests)
;;; kubedoc-tests.el ends here
