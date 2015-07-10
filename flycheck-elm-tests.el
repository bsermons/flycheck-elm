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

(ert-deftest test-parse-multiple-errors-and-warning ()
  "Test that output containing errors and warnings get decoded successfully."
  (let ((parsed (flycheck-elm-parse-error-data error+warnings)))
    (should (= 8 (length parsed)))))

(ert-deftest test-filter-by-error ()
  "Test that we can filter out by certain error types."
  (let ((parsed (flycheck-elm-parse-error-data error+warnings)))
    (should (= 1 (length (flycheck-elm-filter-by-type 'error parsed))))
    (should (= 7 (length (flycheck-elm-filter-by-type 'warning parsed))))))

(ert-deftest test-parse-user-preference-all ()
  "Test user preference to show all errors and warnings."
  (let ((all (flycheck-elm-parse-error-data error+warnings))
        (warnings (flycheck-elm-parse-error-data warnings-only)))
    (should (= 8 (length (flycheck-elm-filter-by-preference all 'all))))
    (should (= 7 (length (flycheck-elm-filter-by-preference warnings 'all))))))

(ert-deftest test-parse-user-preference-errors-only ()
  "Test user preference to show errors only."
  (let ((all (flycheck-elm-parse-error-data error+warnings))
        (warnings (flycheck-elm-parse-error-data warnings-only)))
    (should (= 1 (length (flycheck-elm-filter-by-preference all 'errors-only))))
    (should (= 0 (length (flycheck-elm-filter-by-preference warnings 'errors-only))))))

(ert-deftest test-parse-user-preference-warnings-after-errors ()
  "Test user preference to show warnings after errors."
  (let ((all (flycheck-elm-parse-error-data error+warnings))
        (warnings (flycheck-elm-parse-error-data warnings-only)))
    (should (= 1 (length (flycheck-elm-filter-by-preference all 'warn-after-errors))))
    (should (= 7 (length (flycheck-elm-filter-by-preference warnings 'warn-after-errors))))))

;; Example errors

(setq single-error  "[{\"subregion\":null,\"suggestions\":[],\"details\":\"\",\"region\":{\"end\":{\"column\":12,\"line\":3},\"start\":{\"column\":8,\"line\":3}},\"type\":\"error\",\"file\":\"Sample.elm\",\"tag\":\"NAMING ERROR\",\"overview\":\"Cannot find variable `text`\"}]")

(setq error+warnings "[{\"details\":\"\",\"region\":{\"end\":{\"column\":40,\"line\":4},\"start\":{\"column\":1,\"line\":4}},\"type\":\"warning\",\"file\":\"g:/projects/test/scripts/Website.elm\",\"tag\":\"unused import\",\"overview\":\"Module `Html.Attributes` is unused.\"},{\"details\":\"The type annotation you want looks something like this:\\n\\n    init :\\n        { login :\\n              { errors : List a\\n              , login : String\\n              , loginDisabled : Bool\\n              , password : String\\n              , response : Maybe b\\n              , signinAttempts : number\\n              }\\n        }\",\"region\":{\"end\":{\"column\":6,\"line\":28},\"start\":{\"column\":1,\"line\":26}},\"type\":\"warning\",\"file\":\"g:/projects/test/scripts/Website.elm\",\"tag\":\"missing type annotation\",\"overview\":\"Top-level value `init` does not have a type annotation.\"},{\"details\":\"The type annotation you want looks something like this:\\n\\n    update :\\n        Action\\n        -> { a | login : LoginViewModel } -> { a | login : LoginViewModel }\",\"region\":{\"end\":{\"column\":60,\"line\":34},\"start\":{\"column\":1,\"line\":31}},\"type\":\"warning\",\"file\":\"g:/projects/test/scripts/Website.elm\",\"tag\":\"missing type annotation\",\"overview\":\"Top-level value `update` does not have a type annotation.\"},{\"details\":\"The type annotation you want looks something like this:\\n\\n    loginSuccessHandler : LoginResponse -> Task a ()\",\"region\":{\"end\":{\"column\":60,\"line\":40},\"start\":{\"column\":1,\"line\":37}},\"type\":\"warning\",\"file\":\"g:/projects/test/scripts/Website.elm\",\"tag\":\"missing type annotation\",\"overview\":\"Top-level value `loginSuccessHandler` does not have a type annotation.\"},{\"details\":\"The type annotation you want looks something like this:\\n\\n    view :\\n        (a, b)\\n        -> { c |\\n               login :\\n                   { errors : List (String, String)\\n                   , login : String\\n                   , loginDisabled : Bool\\n                   , password : String\\n                   , signinAttempts : Int\\n                   , response : Maybe LoginResponse\\n                   }\\n               }\\n        -> Html\",\"region\":{\"end\":{\"column\":51,\"line\":46},\"start\":{\"column\":1,\"line\":43}},\"type\":\"warning\",\"file\":\"g:/projects/test/scripts/Website.elm\",\"tag\":\"missing type annotation\",\"overview\":\"Top-level value `view` does not have a type annotation.\"},{\"details\":\"The type annotation you want looks something like this:\\n\\n    model : Signal { login : LoginViewModel }\",\"region\":{\"end\":{\"column\":34,\"line\":50},\"start\":{\"column\":1,\"line\":48}},\"type\":\"warning\",\"file\":\"g:/projects/test/scripts/Website.elm\",\"tag\":\"missing type annotation\",\"overview\":\"Top-level value `model` does not have a type annotation.\"},{\"details\":\"The type annotation you want looks something like this:\\n\\n    main : a -> b -> Signal Html\",\"region\":{\"end\":{\"column\":45,\"line\":53},\"start\":{\"column\":1,\"line\":52}},\"type\":\"warning\",\"file\":\"g:/projects/test/scripts/Website.elm\",\"tag\":\"missing type annotation\",\"overview\":\"Top-level value `main` does not have a type annotation.\"}]
[{\"subregion\":null,\"details\":\"I need an Element, Html, (Signal Element), or (Signal Html) so I can render it\\non screen, but you gave me:\\n\\n    a -> b -> Signal Html\",\"region\":{\"end\":{\"column\":45,\"line\":53},\"start\":{\"column\":1,\"line\":52}},\"type\":\"error\",\"file\":\"g:/projects/test/scripts/Website.elm\",\"tag\":\"BAD MAIN TYPE\",\"overview\":\"The 'main' value has an unsupported type.\"}]")

(setq warnings-only "[{\"details\":\"\",\"region\":{\"end\":{\"column\":40,\"line\":4},\"start\":{\"column\":1,\"line\":4}},\"type\":\"warning\",\"file\":\"g:/projects/test/scripts/Website.elm\",\"tag\":\"unused import\",\"overview\":\"Module `Html.Attributes` is unused.\"},{\"details\":\"The type annotation you want looks something like this:\\n\\n    init :\\n        { login :\\n              { errors : List a\\n              , login : String\\n              , loginDisabled : Bool\\n              , password : String\\n              , response : Maybe b\\n              , signinAttempts : number\\n              }\\n        }\",\"region\":{\"end\":{\"column\":6,\"line\":28},\"start\":{\"column\":1,\"line\":26}},\"type\":\"warning\",\"file\":\"g:/projects/test/scripts/Website.elm\",\"tag\":\"missing type annotation\",\"overview\":\"Top-level value `init` does not have a type annotation.\"},{\"details\":\"The type annotation you want looks something like this:\\n\\n    update :\\n        Action\\n        -> { a | login : LoginViewModel } -> { a | login : LoginViewModel }\",\"region\":{\"end\":{\"column\":60,\"line\":34},\"start\":{\"column\":1,\"line\":31}},\"type\":\"warning\",\"file\":\"g:/projects/test/scripts/Website.elm\",\"tag\":\"missing type annotation\",\"overview\":\"Top-level value `update` does not have a type annotation.\"},{\"details\":\"The type annotation you want looks something like this:\\n\\n    loginSuccessHandler : LoginResponse -> Task a ()\",\"region\":{\"end\":{\"column\":60,\"line\":40},\"start\":{\"column\":1,\"line\":37}},\"type\":\"warning\",\"file\":\"g:/projects/test/scripts/Website.elm\",\"tag\":\"missing type annotation\",\"overview\":\"Top-level value `loginSuccessHandler` does not have a type annotation.\"},{\"details\":\"The type annotation you want looks something like this:\\n\\n    view :\\n        (a, b)\\n        -> { c |\\n               login :\\n                   { errors : List (String, String)\\n                   , login : String\\n                   , loginDisabled : Bool\\n                   , password : String\\n                   , signinAttempts : Int\\n                   , response : Maybe LoginResponse\\n                   }\\n               }\\n        -> Html\",\"region\":{\"end\":{\"column\":51,\"line\":46},\"start\":{\"column\":1,\"line\":43}},\"type\":\"warning\",\"file\":\"g:/projects/test/scripts/Website.elm\",\"tag\":\"missing type annotation\",\"overview\":\"Top-level value `view` does not have a type annotation.\"},{\"details\":\"The type annotation you want looks something like this:\\n\\n    model : Signal { login : LoginViewModel }\",\"region\":{\"end\":{\"column\":34,\"line\":50},\"start\":{\"column\":1,\"line\":48}},\"type\":\"warning\",\"file\":\"g:/projects/test/scripts/Website.elm\",\"tag\":\"missing type annotation\",\"overview\":\"Top-level value `model` does not have a type annotation.\"},{\"details\":\"The type annotation you want looks something like this:\\n\\n    main : a -> b -> Signal Html\",\"region\":{\"end\":{\"column\":45,\"line\":53},\"start\":{\"column\":1,\"line\":52}},\"type\":\"warning\",\"file\":\"g:/projects/test/scripts/Website.elm\",\"tag\":\"missing type annotation\",\"overview\":\"Top-level value `main` does not have a type annotation.\"}]")

;;; flycheck-elm-tests.el ends here
