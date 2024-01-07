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

(ert-deftest kubedoc-tests--resorce-path-canonical ()
  (cl-letf (((symbol-function 'kubedoc--resource-completion-table-cached)
             (lambda () '("configmaps/"
                          "pods/"
                          "services/")))
            ((symbol-function 'kubedoc--field-completion-table-cached)
             (lambda (_) '("configmaps/metadata"
                           "configmaps/metadata/name"
                           "configmaps/kind"))))
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

(ert-deftest parse-kubectl-explain-fields-v2 ()
  (ert-test-erts-file
   "tests/data-parse-kubectl-explain-fields.erts"
   (lambda ()
     (let ((result (kubedoc--parse-kubectl-explain-fields-2)))
       (erase-buffer)
       (point-min)
       (insert (string-join result "\n"))
       (insert "\n")))))

(provide 'kubedoc-tests)
;;; kubedoc-tests.el ends here
