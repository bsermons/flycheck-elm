;;; flycheck-elm.el --- Flycheck support for the elm language
;;; Commentary:
;;; Usage: (eval-after-load 'flycheck
;;;          '(add-hook 'flycheck-mode-hook #'flycheck-elm-setup))

;;; Code:

(require 'json)
(require 'flycheck)

(defun flycheck-elm-decode-elm-error (error checker buffer)
  (let* ((region (assoc 'region error))
         (start (assoc 'start region))
         (start-col (cdr (assoc 'column start)))
         (start-line (cdr (assoc 'line start))))
    (flycheck-error-new
     :checker checker
     :buffer buffer
     :filename (cdr (assoc 'file error))
     :line start-line
     :column start-col
     :message (cdr (assoc 'overview error))
     :level (flycheck-elm-decode-type error))))

(defun flycheck-elm-decode-type (error)
  (let ((type (cdr (assoc 'type error))))
    (pcase type
      (`"warning" 'warning)
      (`"error" 'error)
      (_ 'warning))))

(defun flycheck-elm-read-json (str)
  (condition-case nil
      (json-read-from-string str)
    (error nil)))

(defun flycheck-elm-parse-error-data (data)
  (let* ((json-array-type 'list)
         (adata (split-string data "\n")))
    (mapcar 'car (mapcar 'flycheck-elm-read-json adata))))

(defun flycheck-elm-parse-errors (output checker buffer)
  "Decode elm json output errors."
  (let* ((data (flycheck-elm-parse-error-data output))
         (errors (mapcar (lambda (x) (flycheck-elm-decode-elm-error x checker buffer)) data)))
    errors))

(flycheck-define-checker elm
  "A syntax checker for elm-mode using the json output from elm-make"
  :command ("elm-make" "--report=json" source)
  :error-parser flycheck-elm-parse-errors
  :modes elm-mode)

;;;###autoload
(defun flycheck-elm-setup ()
  "Setup Flycheck elm."
  (interactive)
  (add-to-list 'flycheck-checkers 'elm))

(provide 'flycheck-elm)
;;; flycheck-elm.el ends here
