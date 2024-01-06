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

(defconst kubedoc-tests--openapiv2-raw
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

(defconst kubedoc-tests--openapiv2-parsed
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

(defconst kubedoc-tests--openapiv3-raw
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

(ert-deftest kubedoc-tests--resorce-path-canonical/openapiv2 ()
  (cl-letf (((symbol-function 'kubedoc--kubectl-explain-resource) (lambda (_) kubedoc-tests--openapiv2-raw)))
    (should (string= (kubedoc--resource-path-canonical "configmaps" "metadata") "configmaps/metadata/"))
    (should (string= (kubedoc--resource-path-canonical "configmaps" "kind") "configmaps/kind"))))

(ert-deftest kubedoc-tests--resorce-path-canonical/openapiv3 ()
  (cl-letf (((symbol-function 'kubedoc--kubectl-explain-resource) (lambda (_) kubedoc-tests--openapiv3-raw)))
    (should (string= (kubedoc--resource-path-canonical "configmaps" "metadata") "configmaps/metadata/"))
    (should (string= (kubedoc--resource-path-canonical "configmaps" "kind") "configmaps/kind"))))

(ert-deftest kubedoc-tests--completion-sort ()
  (should (equal
           (kubedoc--completion-sort '("kind" "status/" "metadata/" "apiVersion" "spec/"))
           '("metadata/" "spec/" "status/" "apiVersion" "kind"))))

(provide 'kubedoc-tests)
;;; kubedoc-tests.el ends here
