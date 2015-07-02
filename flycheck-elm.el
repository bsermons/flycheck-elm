;; [
;;     {
;;         "details": "",
;;         "file": "Sample.elm",
;;         "overview": "Cannot find variable `text`",
;;         "region": {
;;             "end": {
;;                 "column": 12,
;;                 "line": 3
;;             },
;;             "start": {
;;                 "column": 8,
;;                 "line": 3
;;             }
;;         },
;;         "subregion": null,
;;         "suggestions": [],
;;         "tag": "NAMING ERROR",
;;         "type": "error"
;;     }
;; ]

(require 'json)
(require 'dash)
(require 'flycheck)

(setq sdata  "[{\"subregion\":null,\"suggestions\":[],\"details\":\"\",\"region\":{\"end\":{\"column\":12,\"line\":3},\"start\":{\"column\":8,\"line\":3}},\"type\":\"error\",\"file\":\"Sample.elm\",\"tag\":\"NAMING ERROR\",\"overview\":\"Cannot find variable `text`\"}]")

(flycheck-ad)

(defun elm-error-to-flycheck-error (error)
  (let* ((region (assoc 'region error))
         (start (assoc 'start region))
         (start-col (cdr (assoc 'column start)))
         (start-line (cdr (assoc 'line start))))
    '(:line start-line :column start-col :message overview :level 'error)))

(defun elm-parse-errors (sdata)
  "Parse elm json output errors"
  (let* ((json-array-type 'list)
         (data (json-read-from-string sdata)))
    (-map 'elm-error-to-flycheck-error data)))

(elm-parse-errors sdata)




;; (flycheck-define-checker elm
;;   "A syntax checker for elm-mode using the json output from elm-make"
;;   :command ("elm-make" "--report=json" source)
;;   :error-parser elm-parse-error-json
;;   :modes (elm-mode))
