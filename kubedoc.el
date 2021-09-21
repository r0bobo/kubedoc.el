;;; kubernets-docs.el --- Kubernetes API Documentation -*- lexical-binding: t; -*-
;;; Commentary:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

;;; Code:

(add-to-list 'completion-category-overrides '(kubedoc (styles partial-completion)))

;;;; Declarations

(defconst kubedoc-imenu-generic-expression
  '((nil "^   \\(\\w+\\)" 1 kubedoc--imenu-goto-field)))

(defconst kubedoc-font-lock-defaults
  `(("^KIND\\|^VERSION\\|^RESOURCE\\|^DESCRIPTION\\|^FIELDS?" . font-lock-keyword-face)
    ("-required-" . font-lock-function-name-face)
    ("\\(<.+>\\)\\( -required-\\)?$" 1 font-lock-comment-face)
    ))

(defvar kubedoc-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (set-keymap-parent map (make-composed-keymap button-buffer-map special-mode-map))
    map))

(defvar kubedoc--field-completion-table-cache nil)

(defvar-local kubedoc--buffer-path nil)

(defvar kubedoc--field-completion-source-function nil)

(define-button-type 'kubedoc-field
  'follow-link t
  'help-echo "mouse-2, RET: display this section"
  'action #'kubedoc--field-button-function)


;;;; Functions

(defun kubedoc--imenu-goto-field (_name position)
  (goto-char (+ position 3)))

(defun kubedoc--highlight-field-links ()
  "Create field links buttons in current buffer."
  (while (re-search-forward "^   \\(\\w+\\)" nil t)
    (make-button
     (match-beginning 1)
     (match-end 1)
     'type 'kubedoc-field
     'kubectl-section (match-string 1))))

(defun kubedoc--field-button-function (button)
  (with-current-buffer (current-buffer)
    (let ((field (button-get button 'kubectl-section)))
      (apply #'kubedoc--view-resource (append kubedoc--buffer-path (list field))))))

(defun kubedoc--resource-completion-table ()
  (mapcar
   (lambda (e) (concat e "/"))
   (split-string
    (shell-command-to-string
     "kubectl api-resources --cached --output name") nil t)))

(defun kubedoc--default-field-completion-source-function (resource)
  (shell-command-to-string
   (concat "kubectl explain --recursive " (shell-quote-argument resource))))

(defun kubedoc--field-completion-table (resource)
  (let* ((res '())
         (path '())
         (prev-indent 0)
         (explain-output-lines
          (split-string
           (funcall (or kubedoc--field-completion-source-function 'kubedoc--default-field-completion-source-function) resource)
           "\\\n" t))
         (explain-output-lines-trimmed (seq-drop-while (lambda (e) (not (string-match "^FIELDS" e))) explain-output-lines)))

    (dolist (item explain-output-lines-trimmed)
      (unless (string-match "^FIELDS" item)
        (string-match "^\\( +\\)\\(\\w+\\) ?" item)

        (let ((indent (/ (length (match-string 1 item)) 3))
              (match (match-string 2 item)))

          (unless (> indent prev-indent)
            (push (string-join (reverse path) "/") res))

          (cond
           ((< indent prev-indent)
            (setq path (nthcdr (+ 1 (- prev-indent indent)) path)))
           ((= indent prev-indent)
            (setq path (nthcdr 1 path))))

          (push match path)

          (when (string= item (car (reverse explain-output-lines)))
            (push (string-join (reverse path) "/") res))

          (setq prev-indent indent))))
    (mapcar (lambda (e) (concat resource "/" e)) res)))

(defun kubedoc--field-completion-table-cached (resource)
  (let ((cached (cdr (assoc resource kubedoc--field-completion-table-cache))))
    (if (null cached)
        (let ((all (kubedoc--field-completion-table resource)))
          (cl-pushnew `(,resource . ,all) kubedoc--field-completion-table-cache)
          all)
      cached)))

(defun kubedoc--completion-table (path)
  (let* ((string-parts (split-string path "/+"))
         (resource (car string-parts)))
    (if (= (length string-parts) 1)
        (seq-filter
         (lambda (e)
           (string-prefix-p path e))
         (kubedoc--resource-completion-table))

      (seq-uniq
       (mapcar
        (lambda (e)
          (let* ((trimmed (string-remove-prefix (file-name-directory path) e))
                 (end (or (string-match-p "/" trimmed) (- (length trimmed) 1))))
            (substring trimmed 0 (+ end 1))
            )
          )
        (seq-filter
         (lambda (e)
           (string-prefix-p path e))
         (kubedoc--field-completion-table-cached resource)))
       #'string-equal)
      )))

(defun kubedoc--completion-sort (collection)
  (seq-sort
   (lambda (a b)
     (let ((a-parent-p (string= (substring a -1) "/"))
           (b-parent-p (string= (substring b -1) "/")))
       (if (equal a-parent-p b-parent-p)
           (string< a b)
         a-parent-p)))
   collection))

(defun kubedoc--completion-function (string pred action)
  "Completion for Kubernetes API resources."
  (cond
   ;; ((eq action 'metadata) nil)
   ((eq action 'metadata)
    '(metadata (category . kubedoc)
               (display-sort-function . kubedoc--completion-sort)))

   ;; completion-boundaries
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
  ""
  (let* ((path (append (list resource) field))
         (prev-buffer (current-buffer))
         (buffer (concat "*Kubernetes Docs <" (string-join path "/") ">*")))
    (unless (get-buffer buffer)
      (with-current-buffer (get-buffer-create buffer)
        (shell-command (concat "kubectl explain " (shell-quote-argument (string-join path "."))) buffer)

        (when field
          (goto-char (point-max))
          (insert "\n")
          (insert-text-button "Navigate up" 'action (lambda (_button) (kubedoc-up)))
          (insert "  ")
          (insert-text-button "Navigate to top" 'action (lambda (_button) (kubedoc-top)))
          (goto-char (point-min)))

        (untabify (point-min) (point-max))
        (kubedoc-mode)
        (setq-local kubedoc--buffer-path path)))

    (if (with-current-buffer prev-buffer (equal major-mode 'kubedoc-mode))
        (pop-to-buffer-same-window buffer)
      (pop-to-buffer buffer))))

(defun kubedoc--resource-path-canonical (resource &rest field)
  ""
  (let* ((path (string-join (append (list resource) field) "/"))
         (path-trailing-slash (concat path "/")))
    (if (kubedoc--completion-table path-trailing-slash) path-trailing-slash path)))

;;;; Interactive functions

;;;###autoload
(defun kubedoc ()
  "Kubernetes API documentation for (RESOURCE)."
  (interactive)
  (let* ((current-path (if-let ((current kubedoc--buffer-path))
                           (apply #'kubedoc--resource-path-canonical current)
                         ""))
         (path (completing-read
                "Kubernetes resource: " #'kubedoc--completion-function
                nil nil current-path)))
    (apply #'kubedoc--view-resource (split-string (string-trim-right path "/+") "/"))))


(defun kubedoc-up ()
  "Navigate to the parent field of the current resource."
  (interactive)
  (let ((parts (buffer-local-value 'kubedoc--buffer-path (current-buffer))))
    (when (> (length parts) 1)
      (apply #'kubedoc--view-resource (butlast kubedoc--buffer-path)))))

(defun kubedoc-top ()
  "Navigate to the top of the current resource."
  (interactive)
  (let ((parts (buffer-local-value 'kubedoc--buffer-path (current-buffer))))
    (when (> (length parts) 1)
      (apply #'kubedoc--view-resource (list (car kubedoc--buffer-path))))))

(defun kubedoc-invalidate-cache ()
  "Invalidate kubedoc completion cache."
  (interactive)
  (setq kubedoc--field-completion-table-cache nil))

;;;; Mode
(define-derived-mode kubedoc-mode special-mode "kubedoc"
  ""

  (setq buffer-auto-save-file-name nil
        truncate-lines t
        buffer-read-only t
        font-lock-defaults '((kubedoc-font-lock-defaults))
        imenu-generic-expression kubedoc-imenu-generic-expression)
  (buffer-disable-undo)
  (auto-fill-mode -1)
  (kubedoc--highlight-field-links)
  (goto-char (point-min)))

(provide 'kubedoc)
;;; kubedoc.el ends here
