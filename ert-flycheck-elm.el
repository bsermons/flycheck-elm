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

(setq sdata  "[{\"subregion\":null,\"suggestions\":[],\"details\":\"\",\"region\":{\"end\":{\"column\":12,\"line\":3},\"start\":{\"column\":8,\"line\":3}},\"type\":\"error\",\"file\":\"Sample.elm\",\"tag\":\"NAMING ERROR\",\"overview\":\"Cannot find variable `text`\"}]")
(setq mdata "[{\"details\":\"\",\"region\":{\"end\":{\"column\":40,\"line\":4},\"start\":{\"column\":1,\"line\":4}},\"type\":\"warning\",\"file\":\"g:/projects/gymfuturistic/routine_engine/trunk/FuturisticLabs.RoutineEngine.WebService/scripts/Website.elm\",\"tag\":\"unused import\",\"overview\":\"Module `Html.Attributes` is unused.\"},{\"details\":\"The type annotation you want looks something like this:\\n\\n    init :\\n        { login :\\n              { errors : List a\\n              , login : String\\n              , loginDisabled : Bool\\n              , password : String\\n              , response : Maybe b\\n              , signinAttempts : number\\n              }\\n        }\",\"region\":{\"end\":{\"column\":6,\"line\":28},\"start\":{\"column\":1,\"line\":26}},\"type\":\"warning\",\"file\":\"g:/projects/gymfuturistic/routine_engine/trunk/FuturisticLabs.RoutineEngine.WebService/scripts/Website.elm\",\"tag\":\"missing type annotation\",\"overview\":\"Top-level value `init` does not have a type annotation.\"},{\"details\":\"The type annotation you want looks something like this:\\n\\n    update :\\n        Action\\n        -> { a | login : LoginViewModel } -> { a | login : LoginViewModel }\",\"region\":{\"end\":{\"column\":60,\"line\":34},\"start\":{\"column\":1,\"line\":31}},\"type\":\"warning\",\"file\":\"g:/projects/gymfuturistic/routine_engine/trunk/FuturisticLabs.RoutineEngine.WebService/scripts/Website.elm\",\"tag\":\"missing type annotation\",\"overview\":\"Top-level value `update` does not have a type annotation.\"},{\"details\":\"The type annotation you want looks something like this:\\n\\n    loginSuccessHandler : LoginResponse -> Task a ()\",\"region\":{\"end\":{\"column\":60,\"line\":40},\"start\":{\"column\":1,\"line\":37}},\"type\":\"warning\",\"file\":\"g:/projects/gymfuturistic/routine_engine/trunk/FuturisticLabs.RoutineEngine.WebService/scripts/Website.elm\",\"tag\":\"missing type annotation\",\"overview\":\"Top-level value `loginSuccessHandler` does not have a type annotation.\"},{\"details\":\"The type annotation you want looks something like this:\\n\\n    view :\\n        (a, b)\\n        -> { c |\\n               login :\\n                   { errors : List (String, String)\\n                   , login : String\\n                   , loginDisabled : Bool\\n                   , password : String\\n                   , signinAttempts : Int\\n                   , response : Maybe LoginResponse\\n                   }\\n               }\\n        -> Html\",\"region\":{\"end\":{\"column\":51,\"line\":46},\"start\":{\"column\":1,\"line\":43}},\"type\":\"warning\",\"file\":\"g:/projects/gymfuturistic/routine_engine/trunk/FuturisticLabs.RoutineEngine.WebService/scripts/Website.elm\",\"tag\":\"missing type annotation\",\"overview\":\"Top-level value `view` does not have a type annotation.\"},{\"details\":\"The type annotation you want looks something like this:\\n\\n    model : Signal { login : LoginViewModel }\",\"region\":{\"end\":{\"column\":34,\"line\":50},\"start\":{\"column\":1,\"line\":48}},\"type\":\"warning\",\"file\":\"g:/projects/gymfuturistic/routine_engine/trunk/FuturisticLabs.RoutineEngine.WebService/scripts/Website.elm\",\"tag\":\"missing type annotation\",\"overview\":\"Top-level value `model` does not have a type annotation.\"},{\"details\":\"The type annotation you want looks something like this:\\n\\n    main : a -> b -> Signal Html\",\"region\":{\"end\":{\"column\":45,\"line\":53},\"start\":{\"column\":1,\"line\":52}},\"type\":\"warning\",\"file\":\"g:/projects/gymfuturistic/routine_engine/trunk/FuturisticLabs.RoutineEngine.WebService/scripts/Website.elm\",\"tag\":\"missing type annotation\",\"overview\":\"Top-level value `main` does not have a type annotation.\"}]
[{\"subregion\":null,\"details\":\"I need an Element, Html, (Signal Element), or (Signal Html) so I can render it\\non screen, but you gave me:\\n\\n    a -> b -> Signal Html\",\"region\":{\"end\":{\"column\":45,\"line\":53},\"start\":{\"column\":1,\"line\":52}},\"type\":\"error\",\"file\":\"g:/projects/gymfuturistic/routine_engine/trunk/FuturisticLabs.RoutineEngine.WebService/scripts/Website.elm\",\"tag\":\"BAD MAIN TYPE\",\"overview\":\"The 'main' value has an unsupported type.\"}]")

(defun ert-test-parse-warning ()
  (let* ((json-array-type 'list)
         (data (json-read-from-string "{\"type\":\"warning\"}")))
    (flycheck-elm-decode-type data)))

(defun ert-test-parse-error ()
  (let* ((json-array-type 'list)
         (data (json-read-from-string "{\"type\":\"error\"}")))
    (flycheck-elm-decode-type data)))

(ert-test-parse-error)
(elm-parse-error-data mdata)
