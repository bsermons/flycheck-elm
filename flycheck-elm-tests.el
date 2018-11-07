;;; flycheck-elm-tests.el --- Flycheck support for the elm language

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
;; Expected error format
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

;;; Code:

(require 'flycheck-elm)
(require 'json)

(ert-deftest test-parse-warning ()
  "Test that the warning types are parsed correctly"
  (let* ((json-array-type 'list)
         (data (json-read-from-string "{\"type\":\"warning\"}")))
    (should (equal
             (flycheck-elm-decode-type data)
             'warning))))

(ert-deftest test-parse-error ()
  "Test that the error types are parsed correctly"
  (let* ((json-array-type 'list)
         (data (json-read-from-string "{\"type\":\"error\"}")))
    (should  (equal
              (flycheck-elm-decode-type data)
              'error))))

(defun parse-with-pref (input pref)
  (let ((flycheck-elm-reporting-mode pref))
    (with-temp-buffer
      (flycheck-elm-parse-errors input nil (current-buffer)))))

(ert-deftest test-parse-multiple-errors-and-warning ()
  "Test that output containing errors and warnings get decoded successfully."
  (let ((parsed (flycheck-elm-parse-errors error+warnings nil nil)))
    (should (= 8 (length parsed)))))

(ert-deftest test-filter-by-error ()
  "Test that we can filter out by certain error types."
  (let ((parsed (parse-with-pref error+warnings 'all)))
    (should (= 1 (length (flycheck-elm-filter-by-type 'error parsed))))
    (should (= 7 (length (flycheck-elm-filter-by-type 'warning parsed))))))

(ert-deftest test-parse-user-preference-all ()
  "Test user preference to show all errors and warnings."
  (should (= 8 (length (parse-with-pref error+warnings 'all))))
  (should (= 7 (length (parse-with-pref warnings-only 'all)))))

(ert-deftest test-parse-user-preference-errors-only ()
  "Test user preference to show errors only."
  (should (= 1 (length (parse-with-pref error+warnings 'errors-only))))
  (should (= 0 (length (parse-with-pref warnings-only 'errors-only)))))

(ert-deftest test-parse-user-preference-warnings-after-errors ()
  "Test user preference to show warnings after errors."
  (should (= 1 (length (parse-with-pref error+warnings 'warn-after-errors))))
  (should (= 7 (length (parse-with-pref warnings-only 'warn-after-errors)))))

(ert-deftest test-parse-no-errors ()
  "Test that empty output is interpreted as no errors"
  (should (eq nil (parse-with-pref no-errors 'all))))

(ert-deftest test-parse-new-compile-errors ()
  "Test parsing of Elm 0.19 compilation errors."
  (let ((parsed (parse-with-pref new-compile-errors 'all)))
    (should (= 1 (length parsed)))
    (let ((err (car parsed)))
      (should (eq 'error (flycheck-error-level err)))
      (should (string= "src/Main.elm" (flycheck-error-filename err)))
      (should (eq 10 (flycheck-error-line err)))
      (should (eq 23 (flycheck-error-column err)))
      (should (string= "BAD IMPORT: The `Time` module does not expose `very`:

10| import Time exposing (very)
                          ^^^^
These names seem close though:

    here
    now
    toDay
    toYear" (flycheck-error-message err)))
      )))

(ert-deftest test-parse-new-top-level-errors ()
  "Test parsing of Elm 0.19 top-level make errors."
  (let ((parsed (parse-with-pref unknown-import-error 'all)))
    (should (= 1 (length parsed)))
    (let ((err (car parsed)))
      (should (eq 'error (flycheck-error-level err)))
      (should (string= "src/Common/Dates.elm" (flycheck-error-filename err)))
      (should (eq 1 (flycheck-error-line err)))
      (should (eq 0 (flycheck-error-column err)))
      (should (string= "UNKNOWN IMPORT: The Common.Dates module has a bad import:

    import Date.Extra.Format

I cannot find that module! Is there a typo in the module name?

The \"source-directories\" field of your elm.json tells me to only look in the src
directory, but it is not there. Maybe it is in a package that is not installed
yet?" (flycheck-error-message err)))
      )))

;; Example errors

(setq single-error  "[{\"subregion\":null,\"suggestions\":[],\"details\":\"\",\"region\":{\"end\":{\"column\":12,\"line\":3},\"start\":{\"column\":8,\"line\":3}},\"type\":\"error\",\"file\":\"Sample.elm\",\"tag\":\"NAMING ERROR\",\"overview\":\"Cannot find variable `text`\"}]")
(setq no-errors "\n")

(setq error+warnings "[{\"details\":\"\",\"region\":{\"end\":{\"column\":40,\"line\":4},\"start\":{\"column\":1,\"line\":4}},\"type\":\"warning\",\"file\":\"g:/projects/test/scripts/Website.elm\",\"tag\":\"unused import\",\"overview\":\"Module `Html.Attributes` is unused.\"},{\"details\":\"The type annotation you want looks something like this:\\n\\n    init :\\n        { login :\\n              { errors : List a\\n              , login : String\\n              , loginDisabled : Bool\\n              , password : String\\n              , response : Maybe b\\n              , signinAttempts : number\\n              }\\n        }\",\"region\":{\"end\":{\"column\":6,\"line\":28},\"start\":{\"column\":1,\"line\":26}},\"type\":\"warning\",\"file\":\"g:/projects/test/scripts/Website.elm\",\"tag\":\"missing type annotation\",\"overview\":\"Top-level value `init` does not have a type annotation.\"},{\"details\":\"The type annotation you want looks something like this:\\n\\n    update :\\n        Action\\n        -> { a | login : LoginViewModel } -> { a | login : LoginViewModel }\",\"region\":{\"end\":{\"column\":60,\"line\":34},\"start\":{\"column\":1,\"line\":31}},\"type\":\"warning\",\"file\":\"g:/projects/test/scripts/Website.elm\",\"tag\":\"missing type annotation\",\"overview\":\"Top-level value `update` does not have a type annotation.\"},{\"details\":\"The type annotation you want looks something like this:\\n\\n    loginSuccessHandler : LoginResponse -> Task a ()\",\"region\":{\"end\":{\"column\":60,\"line\":40},\"start\":{\"column\":1,\"line\":37}},\"type\":\"warning\",\"file\":\"g:/projects/test/scripts/Website.elm\",\"tag\":\"missing type annotation\",\"overview\":\"Top-level value `loginSuccessHandler` does not have a type annotation.\"},{\"details\":\"The type annotation you want looks something like this:\\n\\n    view :\\n        (a, b)\\n        -> { c |\\n               login :\\n                   { errors : List (String, String)\\n                   , login : String\\n                   , loginDisabled : Bool\\n                   , password : String\\n                   , signinAttempts : Int\\n                   , response : Maybe LoginResponse\\n                   }\\n               }\\n        -> Html\",\"region\":{\"end\":{\"column\":51,\"line\":46},\"start\":{\"column\":1,\"line\":43}},\"type\":\"warning\",\"file\":\"g:/projects/test/scripts/Website.elm\",\"tag\":\"missing type annotation\",\"overview\":\"Top-level value `view` does not have a type annotation.\"},{\"details\":\"The type annotation you want looks something like this:\\n\\n    model : Signal { login : LoginViewModel }\",\"region\":{\"end\":{\"column\":34,\"line\":50},\"start\":{\"column\":1,\"line\":48}},\"type\":\"warning\",\"file\":\"g:/projects/test/scripts/Website.elm\",\"tag\":\"missing type annotation\",\"overview\":\"Top-level value `model` does not have a type annotation.\"},{\"details\":\"The type annotation you want looks something like this:\\n\\n    main : a -> b -> Signal Html\",\"region\":{\"end\":{\"column\":45,\"line\":53},\"start\":{\"column\":1,\"line\":52}},\"type\":\"warning\",\"file\":\"g:/projects/test/scripts/Website.elm\",\"tag\":\"missing type annotation\",\"overview\":\"Top-level value `main` does not have a type annotation.\"}]
[{\"subregion\":null,\"details\":\"I need an Element, Html, (Signal Element), or (Signal Html) so I can render it\\non screen, but you gave me:\\n\\n    a -> b -> Signal Html\",\"region\":{\"end\":{\"column\":45,\"line\":53},\"start\":{\"column\":1,\"line\":52}},\"type\":\"error\",\"file\":\"g:/projects/test/scripts/Website.elm\",\"tag\":\"BAD MAIN TYPE\",\"overview\":\"The 'main' value has an unsupported type.\"}]")

(setq warnings-only "[{\"details\":\"\",\"region\":{\"end\":{\"column\":40,\"line\":4},\"start\":{\"column\":1,\"line\":4}},\"type\":\"warning\",\"file\":\"g:/projects/test/scripts/Website.elm\",\"tag\":\"unused import\",\"overview\":\"Module `Html.Attributes` is unused.\"},{\"details\":\"The type annotation you want looks something like this:\\n\\n    init :\\n        { login :\\n              { errors : List a\\n              , login : String\\n              , loginDisabled : Bool\\n              , password : String\\n              , response : Maybe b\\n              , signinAttempts : number\\n              }\\n        }\",\"region\":{\"end\":{\"column\":6,\"line\":28},\"start\":{\"column\":1,\"line\":26}},\"type\":\"warning\",\"file\":\"g:/projects/test/scripts/Website.elm\",\"tag\":\"missing type annotation\",\"overview\":\"Top-level value `init` does not have a type annotation.\"},{\"details\":\"The type annotation you want looks something like this:\\n\\n    update :\\n        Action\\n        -> { a | login : LoginViewModel } -> { a | login : LoginViewModel }\",\"region\":{\"end\":{\"column\":60,\"line\":34},\"start\":{\"column\":1,\"line\":31}},\"type\":\"warning\",\"file\":\"g:/projects/test/scripts/Website.elm\",\"tag\":\"missing type annotation\",\"overview\":\"Top-level value `update` does not have a type annotation.\"},{\"details\":\"The type annotation you want looks something like this:\\n\\n    loginSuccessHandler : LoginResponse -> Task a ()\",\"region\":{\"end\":{\"column\":60,\"line\":40},\"start\":{\"column\":1,\"line\":37}},\"type\":\"warning\",\"file\":\"g:/projects/test/scripts/Website.elm\",\"tag\":\"missing type annotation\",\"overview\":\"Top-level value `loginSuccessHandler` does not have a type annotation.\"},{\"details\":\"The type annotation you want looks something like this:\\n\\n    view :\\n        (a, b)\\n        -> { c |\\n               login :\\n                   { errors : List (String, String)\\n                   , login : String\\n                   , loginDisabled : Bool\\n                   , password : String\\n                   , signinAttempts : Int\\n                   , response : Maybe LoginResponse\\n                   }\\n               }\\n        -> Html\",\"region\":{\"end\":{\"column\":51,\"line\":46},\"start\":{\"column\":1,\"line\":43}},\"type\":\"warning\",\"file\":\"g:/projects/test/scripts/Website.elm\",\"tag\":\"missing type annotation\",\"overview\":\"Top-level value `view` does not have a type annotation.\"},{\"details\":\"The type annotation you want looks something like this:\\n\\n    model : Signal { login : LoginViewModel }\",\"region\":{\"end\":{\"column\":34,\"line\":50},\"start\":{\"column\":1,\"line\":48}},\"type\":\"warning\",\"file\":\"g:/projects/test/scripts/Website.elm\",\"tag\":\"missing type annotation\",\"overview\":\"Top-level value `model` does not have a type annotation.\"},{\"details\":\"The type annotation you want looks something like this:\\n\\n    main : a -> b -> Signal Html\",\"region\":{\"end\":{\"column\":45,\"line\":53},\"start\":{\"column\":1,\"line\":52}},\"type\":\"warning\",\"file\":\"g:/projects/test/scripts/Website.elm\",\"tag\":\"missing type annotation\",\"overview\":\"Top-level value `main` does not have a type annotation.\"}]")

(setq new-compile-errors "{\"type\":\"compile-errors\",\"errors\":[{\"path\":\"src/Main.elm\",\"name\":\"Main\",\"problems\":[{\"title\":\"BAD IMPORT\",\"region\":{\"start\":{\"line\":10,\"column\":23},\"end\":{\"line\":10,\"column\":27}},\"message\":[\"The `Time` module does not expose `very`:\\n\\n10| import Time exposing (very)\\n                          \",{\"bold\":false,\"underline\":false,\"color\":\"red\",\"string\":\"^^^^\"},\"\\nThese names seem close though:\\n\\n    \",{\"bold\":false,\"underline\":false,\"color\":\"yellow\",\"string\":\"here\"},\"\\n    \",{\"bold\":false,\"underline\":false,\"color\":\"yellow\",\"string\":\"now\"},\"\\n    \",{\"bold\":false,\"underline\":false,\"color\":\"yellow\",\"string\":\"toDay\"},\"\\n    \",{\"bold\":false,\"underline\":false,\"color\":\"yellow\",\"string\":\"toYear\"},\"\"]}]}]}")

(setq unknown-import-error "{\"type\":\"error\",\"path\":\"src/Common/Dates.elm\",\"title\":\"UNKNOWN IMPORT\",\"message\":[\"The Common.Dates module has a bad import:\\n\\n    \",{\"bold\":false,\"underline\":false,\"color\":\"RED\",\"string\":\"import Date.Extra.Format\"},\"\\n\\nI cannot find that module! Is there a typo in the module name?\\n\\nThe \\\"source-directories\\\" field of your elm.json tells me to only look in the src\\ndirectory, but it is not there. Maybe it is in a package that is not installed\\nyet?\"]}")

;;; flycheck-elm-tests.el ends here
