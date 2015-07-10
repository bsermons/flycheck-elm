;;; flycheck-elm.el --- Flycheck support for the elm language

;; Copyright (c) 2015 Brian Sermons

;; Author: Brian Sermons
;; Package-Requires: ((flycheck "0.24") (emacs "24.4"))
;; URL: https://github.com/bsermons/flycheck-elm

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

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
      (_ 'unknown))))

(defun flycheck-elm-read-json (str)
  (condition-case nil
      (json-read-from-string str)
    (error nil)))

(defun flycheck-elm-parse-error-data (data)
  (let* ((json-array-type 'list)
         (mapdata (mapcar
                   'flycheck-elm-read-json
                   (split-string data "\n"))))
    (append (car mapdata) (car (cdr mapdata)))))

(defun flycheck-elm-parse-errors (output checker buffer)
  "Decode elm json output errors."
  (let* ((data (flycheck-elm-parse-error-data output))
         (errors (mapcar
                  (lambda (x) (flycheck-elm-decode-elm-error x checker buffer))
                  data)))
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
