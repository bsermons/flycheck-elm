;;; flycheck-elm.el --- Flycheck support for the elm language

;; Copyright (c) 2015 Brian Sermons

;; Author: Brian Sermons
;; Package-Requires: ((flycheck "0.29-cvs") (emacs "24.4"))
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

;; Enable this checker automatically by adding code like the following
;; to your startup files:

;;     (eval-after-load 'flycheck
;;       '(flycheck-elm-setup))

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'flycheck)

(defgroup flycheck-elm nil
  "Elm support for Flycheck."
  :prefix "flycheck-elm-"
  :group 'flycheck
  :link '(url-link :tag "Github" "https://github.com/bsermons/flycheck-elm"))

(defcustom flycheck-elm-reporting-mode 'all
  "*Types of messages to show."
  :type '(choice
          (const :tag "Show warnings and errors." all)
          (const :tag "Show only errors." errors-only)
          (const :tag "Show warnings only if no errors occur." warn-after-errors))
  :group 'flycheck-elm)

(defun flycheck-elm-decode-elm-error (error checker buffer)
  (let* ((region (assoc 'region error))
         (tag (concat "[" (cdr (assoc 'tag error)) "]"))
         (overview (cdr (assoc 'overview error)))
         (details (cdr (assoc 'details error)))
         (start (assoc 'start region))
         (start-col (cdr (assoc 'column start)))
         (start-line (cdr (assoc 'line start))))
    (flycheck-error-new
     :checker checker
     :buffer buffer
     :filename (cdr (assoc 'file error))
     :line start-line
     :column start-col
     :message (mapconcat 'identity (list tag overview details) "\n")
     :level (flycheck-elm-decode-type error))))

(defun flycheck-elm-decode-type (error)
  (let ((type (cdr (assoc 'type error))))
    (pcase type
      (`"warning" 'warning)
      (`"error" 'error)
      (_ 'unknown))))

(defun flycheck-elm-read-json (str)
  "Read json from STR, use elisp lists rather than vectors."
  (let ((json-array-type 'list))
    (condition-case nil
        (json-read-from-string str)
      (error nil))))

(defun flycheck-elm-parse-error-data (data)
  (let* ((json-array-type 'list)
         (mapdata (mapcar
                   'flycheck-elm-read-json
                   (split-string data "\n"))))
    (append (car mapdata) (car (cdr mapdata)))))

(defun flycheck-elm-parse-errors (output checker buffer)
  "Decode elm json OUTPUT errors from elm-make CHECKER in BUFFER.
As of Elm 0.17.1, there are a few edge cases in which elm-make does not
output json even with the --report=json flag.  When the OUTPUT is json
`flycheck-elm-read-json' returns non-nil with the parsed data.  If it fails
we process OUTPUT as a regular Elm error message."
  (let ((errors (flycheck-elm-read-json output)))
    (cond
     ((not (null errors))
      (mapcar (lambda (x) (flycheck-elm-decode-elm-error x checker buffer))
                (flycheck-elm-filter-by-preference errors)))
     ((flycheck-elm-plain-error-check output)
      (flycheck-elm-plain-decode-error output checker buffer))
     (t '()))))

(defun flycheck-elm-filter-by-preference (lst &optional pref)
  "Filter the lst by user preference."
  (let ((errors (flycheck-elm-filter-by-type 'error lst)))
    (or pref (set 'pref flycheck-elm-reporting-mode))
    (pcase pref
      (`errors-only errors)
      (`warn-after-errors
       (pcase (length errors)
         (0 (flycheck-elm-filter-by-type 'warning lst))
         (_ errors)))
      (_  lst))))

(defun flycheck-elm-filter-by-type (type lst)
  "Return a new LIST of errors of type TYPE."
  (cl-remove-if-not
   (lambda (x)(equal (flycheck-elm-decode-type x) type))
   lst))

(defun flycheck-elm-package-json-directory (&optional checker)
  "Find the directory in which CHECKER should run \"elm-make\"."
  (locate-dominating-file default-directory "elm-package.json"))

(defvar flycheck-elm-plain-regex-line-number "^\\([0-9]*\\)|"
  "Line starting with one or more digits, flowed by a pipe character.")

(defvar flycheck-elm-plain-regex-columns "^[ ]*\\(\\^+\\)"
  "Line starting with any number of spaces and one or more carets.")

(defvar flycheck-elm-plain-regex-tag "^-- \\([A-Z]* [A-Z]*\\|[A-Z]*\\) -+"
  "Match title line for SYNTAX ERRORS, and possibly other types.")

(defun flycheck-elm-plain-columns ()
  "Grab match data for columns.
Uses previously completed `flycheck-elm-plain-regex-columns' match."
  (cons (1+ (match-beginning 1))
        (match-end 1)))

(defun flycheck-elm-plain-error-check (output)
  "Check for elm-make output for success.
If elm-make exits without errors its OUTPUT is a string starting
with \"Successfully generated\"."
  (not (string-match "^Successfully generated" output)))

(defun flycheck-elm-plain-line-number (line)
  "Grab match data for line number in LINE.
Uses previously completed `flycheck-elm-plain-regex-row' match."
  (flycheck-string-to-number-safe (match-string 1 line)))

(defun flycheck-elm-plain-tag (line)
    "Grab match data for error tag from LINE.
Uses previously completed `flycheck-elm-plain-regex-tag' match."
    (match-string 1 line))

(defun flycheck-elm-plain-message (lines &optional l c m)
  "Build flycheck error properties from LINES.
Construct the properties line number 'L, column number C andthe message M."
  (let ((line (car lines))
        (rest (cdr lines)))
    (cond
     ;; Return an alist. If line number not found set it to zero
     ((null lines)
      (list (cons 'line (or l 0))
            (cons 'column (or c 0))
            (cons 'message m)))
     ;; If title line, grab title
     ((string-match flycheck-elm-plain-regex-tag line)
      (let ((tag (flycheck-elm-plain-tag line)))
        (flycheck-elm-plain-message rest l c (concat "[" tag "]" m))))
     ;; If column indicating line, grab column data
     ((string-match flycheck-elm-plain-regex-columns line)
      (let ((columns (flycheck-elm-plain-columns)))
        (flycheck-elm-plain-message rest l (car columns) m)))
     ;; Capture the line number if the message includes it
     ((string-match flycheck-elm-plain-regex-line-number line)
      (let ((row (flycheck-elm-plain-line-number line)))
        (flycheck-elm-plain-message rest row c m)))
     ;; Otherwise concat current line onto the message with a newline between
     (t
      (flycheck-elm-plain-message rest l c (concat m "\n" line))))))

(defun flycheck-elm-plain-decode-error (output checker buffer)
  "Decode elm regular OUTPUT errors from CHECKER in BUFFER.
This is an edge case for handling some odd output from elm-make.
In some situtions elm-make does not output json errors, even when
provided the --report=json flag."
  ;; Should this type of message go through `flycheck-elm-filter-by-preference'?
  ;; It seems when elm-make emits this type of error, it emits it and stops
  ;; checking the rest of the file. So there is only one message, which needs to
  ;; be fixed before elm-make can be used to detect the remaining errors.
  (let ((data (flycheck-elm-plain-message (split-string output "\n+"))))
    (list (flycheck-error-new
           :checker checker
           :buffer buffer
           :filename (buffer-file-name buffer)
           :line (cdr (assoc 'line data))
           :column (cdr (assoc 'column data))
           :message (cdr (assoc 'message data))
           :level 'error))))

(flycheck-def-option-var flycheck-elm-output-file nil elm
  "The output file to compile to when performing syntax checking.

The value of this variable is either nil, or a string with the
path to the desired compilation output file.

If nil, flycheck-elm will compile to `/dev/null' so as to not
interfere with your project files. Elm-make has special logic
to handle /dev/null, hence the use of /dev/null instead of `null-device' even
on Windows.
See commit: https://github.com/elm-lang/elm-make/commit/ddcd4980fac9127c91c1de373c310155de9fa558

If a string is provided, the flycheck-elm will compile your code
to the given file each time it performs syntax checking. This can
be set to any file with a .js or .html extension. Please note
that the contents of this file will be overwritten every time
flycheck-elm successfully compiles your Elm code."
  :type '(string))


(flycheck-def-option-var flycheck-elm-main-file nil elm
  "A main elm file for flycheck-elm to compile instead of individual files.

The value of this variable is either nil, in which case
flycheck-elm will compile individual files when checking them, or
a string with the path to the main elm file within your
project. The main elm file is the .elm file which contains a
\"main\" function, for example: \"Main.elm\")."
  :type '(string))

(flycheck-define-checker elm
  "A syntax checker for elm-mode using the json output from elm-make"
  :command ("elm-make" "--report=json"
            (eval (or flycheck-elm-main-file buffer-file-name))
            (eval (concat  "--output=" (or flycheck-elm-output-file "/dev/null"))))
  :error-parser flycheck-elm-parse-errors
  :working-directory flycheck-elm-package-json-directory
  :predicate flycheck-elm-package-json-directory
  :modes elm-mode)

;;;###autoload
(defun flycheck-elm-setup ()
  "Setup Flycheck elm."
  (interactive)
  (add-to-list 'flycheck-checkers 'elm))

(provide 'flycheck-elm)
;;; flycheck-elm.el ends here
