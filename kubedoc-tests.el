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

(ert-deftest kubedoc-tests--resorce-path-canonical ()
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

(ert-deftest kubedoc-tests--completion-sort ()
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

(ert-deftest parse-kubectl-explain-fields ()
  (kubedoc-erts-test
   "tests/data-parse-kubectl-explain-fields.erts"
   #'kubedoc--parse-kubectl-explain-fields))

(ert-deftest parse-api-resources ()
  (kubedoc-erts-test
   "tests/data-parse-api-resources.erts"
   #'kubedoc--parse-api-resources))

(provide 'kubedoc-tests)
;;; kubedoc-tests.el ends here
