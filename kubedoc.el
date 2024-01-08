;;; kubedoc.el --- Kubernetes API Documentation -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021, 2024 Dean Lindqvist Todevski
;;
;; Author: Dean Lindqvist Todevski <https://github.com/r0bobo>
;; Maintainer: Dean Lindqvist Todevski
;; Keywords: docs help k8s kubernetes tools
;; Version: 1.0
;; Homepage: https://github.com/r0bobo/kubedoc.el/
;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; kubedoc.el provides Kubernetes API documentation in Emacs.

;;; Code:

;;;; Requirements

(require 'map)
(require 'seq)
(require 'subr-x)

;;;; Completion style

(add-to-list 'completion-category-overrides '(kubedoc (styles partial-completion)))

;;;; Declarations

(defgroup kubedoc nil
  "Kubernetes API documentation."
  :group 'tools)

(defcustom kubedoc-excluded-resources '("\\.metrics\\.k8s\\.io")
  "List of regexps for Kubernetes resources to ignore.
Some Aggregated APIs have no docs for example."
  :type 'sexp)

(defconst kubedoc--explain-field-regex
  "^\\([ \t]+\\)\\(\\w+\\)[ \t]+\\(<.*>+\\)[ \t]*\\(-required-\\)?$")

(defconst kubedoc-imenu-generic-expression
  `((nil ,kubedoc--explain-field-regex 2 kubedoc--imenu-goto-field)))

(defconst kubedoc-font-lock-defaults
  `(("^GROUP\\|^KIND\\|^VERSION\\|^RESOURCE\\|^DESCRIPTION\\|^FIELDS?" . font-lock-keyword-face)
    (,kubedoc--explain-field-regex 3 font-lock-comment-face)
    ("-required-" . font-lock-function-name-face)))

(defvar kubedoc-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (set-keymap-parent map (make-composed-keymap button-buffer-map special-mode-map))
    map))

(defvar kubedoc--field-completion-table-cache (make-hash-table :test 'equal))

(defvar kubedoc--resource-completion-table-cache nil)

(defvar kubedoc--current-context nil)

(defvar-local kubedoc--buffer-path nil)

(define-button-type 'kubedoc-field
  'follow-link t
  'help-echo "mouse-2, RET: display this section"
  'action #'kubedoc--field-button-function)


;;;; Commands

;;; UI

(defun kubedoc--imenu-goto-field (_name position)
  "Jump to correctly indented field POSITION."
  (goto-char (+ position 3)))

(defun kubedoc--highlight-field-links ()
  "Create field link buttons in current buffer."
  (while (re-search-forward kubedoc--explain-field-regex nil t)
    (make-button
     (match-beginning 2)
     (match-end 2)
     'type 'kubedoc-field
     'kubectl-section (match-string 2))))

(defun kubedoc--field-button-function (button)
  "Follow field link in BUTTON."
  (with-current-buffer (current-buffer)
    (let ((field (button-get button 'kubectl-section)))
      (apply #'kubedoc--view-resource (append kubedoc--buffer-path (list field))))))

;;; Helpers

(defun kubedoc--kubectl-command (&rest args)
  "Run kubectl with ARGS and write output to current buffer."
  (when kubedoc--current-context
    (push (concat "--context=" kubedoc--current-context) args))
  (let ((stderr (make-temp-file "emacs-kubedoc-kubectl-stderr-"))
        (kubectl (executable-find "kubectl")))
    (unwind-protect
        (when-let* ((returncode (apply #'call-process kubectl nil `(t ,stderr) nil args))
                    ((/= returncode 0))
                    (errmsg (with-temp-buffer
                              (insert-file-contents stderr)
                              (buffer-string))))
          (user-error (format "kubedoc.el kubectl error: %s" errmsg)))
      (delete-file stderr))))

;;; Parse functions

(defun kubedoc--parse-api-resources ()
  "Read output of `kubectl api-resources' from buffer and return as list.
Excludes resouces that match regexps in `kubedoc-excluded-resources'."
  (let ((excluded-resource-p (lambda (elt)
                               (seq-every-p
                                (lambda (re)
                                  (not (string-match-p re elt))) kubedoc-excluded-resources))))
    (thread-last
      (split-string (buffer-string) nil t)
      (seq-filter excluded-resource-p)
      (seq-map (apply-partially #'format "%s/")))))

(defun kubedoc--parse-kubectl-explain-fields ()
  "Parse resource document from current buffer.
Buffer should contain output from `kubectl explain --recursive'.
Result is list of full paths for current resource.
Supports both OpenAPI v2 and v3 schema."
  (let ((result '())
        (path '())
        (base -1))
    (goto-char (point-min))
    (while (not (eobp))
      (let ((line (thing-at-point 'line)))
        (when (string-match kubedoc--explain-field-regex line)
          (let* ((depth (current-indentation))
                 (field (match-string 2 line)))
            ;; Set base indention length from first iteration
            (when (= base -1)
              (setq base depth))
            (let ((position (/ (- depth base) base)))
              ;; Drop all previous path elements that are
              ;; deeper than the current.
              (setq path (reverse (seq-subseq (reverse path) 0 position)))
              (push field path)
              (push (string-join (reverse path) "/") result)))))
      (forward-line 1))
    ;; Filter result so that every result
    ;; ends with the left field of each hierarchy.
    ;; If the result contains '("kind" "metadata" "metadata/labels")
    ;; the result should be '("kind" "metadata/labels")
    (let ((full-path-p (lambda (elt)
                         (thread-last
                           result
                           (seq-some (apply-partially #'string-prefix-p (concat elt "/")))
                           (not)))))
      (thread-last
        result
        (seq-filter full-path-p)
        (reverse)))))

(defun kubedoc--kubectl-contexts ()
  "List available kubectl contexts."
  (with-temp-buffer
    (kubedoc--kubectl-command "config" "get-contexts" "--output=name")
    (seq-sort #'string< (split-string (buffer-string) nil t))))

;;; Completion

(defun kubedoc--resource-completion-table ()
  "Completion candidate list for Kubernetes resources in the cluster."
  (with-temp-buffer
    (kubedoc--kubectl-command "api-resources" "--output" "name")
    (kubedoc--parse-api-resources)))

(defun kubedoc--resource-completion-table-cached ()
  "Cached completion candidate list for Kubernetes resources in the cluster."
  (if-let ((cached kubedoc--resource-completion-table-cache))
      cached
    (setq kubedoc--resource-completion-table-cache (kubedoc--resource-completion-table))))

(defun kubedoc--field-completion-table (resource)
  "Completion candidate list for all fields of Kubernetes RESOURCE."
  (with-temp-buffer
    (kubedoc--kubectl-command "explain" "--recursive" resource)
    (thread-last
      (kubedoc--parse-kubectl-explain-fields)
      (seq-map (apply-partially #'format "%s/%s" resource)))))

(defun kubedoc--field-completion-table-cached (resource)
  "Cached Completion candidate list for all fields of Kubernetes RESOURCE."
  (if-let (cached (map-elt kubedoc--field-completion-table-cache resource))
      cached
    (map-put! kubedoc--field-completion-table-cache resource (kubedoc--field-completion-table resource))))

(defun kubedoc--completion-table (path)
  "Completion candidate list for given Kubernetes resource and field PATH.
PATH is a filesystem style path such as pods/spec/containers."
  (let* ((string-parts (split-string path "/+"))
         (resource (car string-parts)))
    (if (= (length string-parts) 1)
        (seq-filter
         (lambda (e)
           (string-prefix-p path e))
         (kubedoc--resource-completion-table-cached))
      (seq-uniq
       (mapcar
        (lambda (e)
          (let* ((trimmed (string-remove-prefix (file-name-directory path) e))
                 (end (or (string-match-p "/" trimmed) (- (length trimmed) 1))))
            (substring trimmed 0 (+ end 1))))
        (seq-filter
         (lambda (e)
           (string-prefix-p path e))
         (kubedoc--field-completion-table-cached resource)))
       #'string-equal))))

(defun kubedoc--completion-sort (collection)
  "Completion candidate list sorting of COLLECTION.
Sorts alphabetically with parent fields on top."
  (seq-sort
   (lambda (a b)
     (let ((a-parent-p (string= (substring a -1) "/"))
           (b-parent-p (string= (substring b -1) "/")))
       (if (equal a-parent-p b-parent-p)
           (string< a b)
         a-parent-p)))
   collection))

(defun kubedoc--completion-function (string pred action)
  "Completion for Kubernetes API resources.
See argument STRING PRED ACTION descriptions in command `try-completion'."
  (cond
   ((eq action 'metadata)
    '(metadata (category . kubedoc)
      (display-sort-function . kubedoc--completion-sort)))
   ((eq (car-safe action) 'boundaries)
    (let* ((start (length (file-name-directory string)))
           (end (string-match-p "/" (cdr action))))
      `(boundaries ,start . ,end)))
   (t
    (let ((candidate
           (if (string-equal string "")
               string
             (string-remove-prefix (or (file-name-directory string) "") string))))
      (complete-with-action action (kubedoc--completion-table string) candidate pred)))))

(defun kubedoc--view-resource (resource &rest field)
  "Display Kubernetes api documentation for RESOURCE and optionally FIELD."
  (let* ((path (append (list resource) field))
         (prev-buffer (current-buffer))
         (buffer (concat "*Kubernetes Docs <" (string-join path "/") ">*")))
    (unless (get-buffer buffer)
      (with-current-buffer (get-buffer-create buffer)
        (kubedoc--kubectl-command "explain" (string-join path "."))
        (when field
          (goto-char (point-max))
          (insert "\n")
          (insert-text-button "Navigate up" 'action (lambda (_button) (kubedoc-up)))
          (insert "  ")
          (insert-text-button "Navigate to top" 'action (lambda (_button) (kubedoc-top)))
          (goto-char (point-min)))
        (untabify (point-min) (point-max))
        (set-buffer-modified-p nil)
        (kubedoc-mode)
        (setq-local kubedoc--buffer-path path)))
    (if (with-current-buffer prev-buffer (equal major-mode 'kubedoc-mode))
        (pop-to-buffer-same-window buffer)
      (pop-to-buffer buffer))))

(defun kubedoc--resource-path-canonical (resource &rest field)
  "Return canonical path for Kubernetes RESOURCE and optionally FIELD.
Paths are suffixed with a `/' if they contain any child fields."
  (let* ((path (string-join (append (list resource) field) "/"))
         (path-trailing-slash (concat path "/")))
    (if (kubedoc--completion-table path-trailing-slash) path-trailing-slash path)))


;;;; Interactive commands

;;;###autoload
(defun kubedoc ()
  "Show Kubernetes API documentation.
Uses current context."
  (interactive)
  (let* ((current-path (if-let ((current kubedoc--buffer-path))
                           (apply #'kubedoc--resource-path-canonical current)
                         ""))
         (path (completing-read
                "Kubernetes resource: " #'kubedoc--completion-function
                nil nil current-path)))
    (apply #'kubedoc--view-resource (split-string (string-trim-right path "/+") "/"))))

;;;###autoload
(defun kubedoc-for-context ()
  "Show Kubernetes API documentation.
Prompts for changing current context."
  (interactive)
  (kubedoc-set-context)
  (kubedoc))

(defun kubedoc-up ()
  "Navigate to parent field of current Kubernetes resource."
  (interactive)
  (let ((parts (buffer-local-value 'kubedoc--buffer-path (current-buffer))))
    (when (> (length parts) 1)
      (apply #'kubedoc--view-resource (butlast kubedoc--buffer-path)))))

(defun kubedoc-top ()
  "Navigate to top of current Kubernetes resource."
  (interactive)
  (let ((parts (buffer-local-value 'kubedoc--buffer-path (current-buffer))))
    (when (> (length parts) 1)
      (apply #'kubedoc--view-resource (list (car kubedoc--buffer-path))))))

(defun kubedoc-set-context ()
  "Set current kubeconfig context."
  (interactive)
  (let* ((contexts (kubedoc--kubectl-contexts))
         (context (completing-read "Kubernetes context: " contexts)))
    ;; Invalidate cache if context is changed
    (unless (string= context kubedoc--current-context)
      (kubedoc-invalidate-cache))
    (setq kubedoc--current-context context)))

(defun kubedoc-invalidate-cache ()
  "Invalidate kubedoc completion cache."
  (interactive)
  (setq kubedoc--field-completion-table-cache (make-hash-table :test 'equal))
  (setq kubedoc--resource-completion-table-cache nil))


;;;; Mode
(define-derived-mode kubedoc-mode special-mode "kubedoc"
  "Major mode for displaying Kubernetes api documentation."
  (setq buffer-auto-save-file-name nil
        truncate-lines t
        font-lock-defaults '((kubedoc-font-lock-defaults))
        imenu-generic-expression kubedoc-imenu-generic-expression)
  (view-mode)
  (auto-fill-mode -1)
  (goto-char (point-min))
  (kubedoc--highlight-field-links)
  (goto-char (point-min)))


(provide 'kubedoc)
;;; kubedoc.el ends here
