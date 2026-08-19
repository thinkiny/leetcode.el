;;; leetcode.el --- An leetcode client           -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2019  Wang Kai

;; Author: Wang Kai <kaiwkx@gmail.com>
;; Keywords: extensions, tools
;; URL: https://github.com/kaiwk/leetcode.el
;; Package-Requires: ((emacs "28.1") (s "1.13.0") (aio "1.0") (log4e "0.3.3"))
;; Version: 0.1.27

;; This program is free software; you can redistribute it and/or modify
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

;; leetcode.el is an unofficial LeetCode client.
;;
;; Now it implements several API:
;; - Check problems list
;; - Try testcase
;; - Submit code
;;
;; Since most HTTP requests works asynchronously, it won't block Emacs.
;;
;;; Code:
(eval-when-compile
  (require 'let-alist))

(require 'json)
(require 'shr)
(require 'seq)
(require 'subr-x)
(require 'mm-url)
(require 'cl-lib)

(require 's)
(require 'aio)
(require 'log4e)

(log4e:deflogger "leetcode" "%t [%l] %m" "%H:%M:%S" '((fatal . "fatal")
                                                      (error . "error")
                                                      (warn  . "warn")
                                                      (info  . "info")
                                                      (debug . "debug")
                                                      (trace . "trace")))
(setq log4e--log-buffer-leetcode "*leetcode-log*")

;;;###autoload
(defun leetcode-toggle-debug ()
  "Toggle debug."
  (interactive)
  (if (leetcode--log-debugging-p)
      (progn
        (leetcode--log-set-level 'info)
        (leetcode--log-disable-debugging)
        (message "leetcode disable debug"))
    (progn
      (leetcode--log-set-level 'debug)
      (leetcode--log-enable-debugging)
      (message "leetcode enable debug"))))

(defun leetcode--install-my-cookie ()
  "Install leetcode dependencies."
  (let ((async-shell-command-display-buffer t)
        (pipx (executable-find "pipx"))
        (python3 (executable-find "python3"))
        (python (executable-find "python")))
    (async-shell-command
     (if pipx
         (format "%s install my_cookies" pipx)
       (format "%s -m venv --clear %s && %s/bin/pip3 install my_cookies"
               (or python3 python "python") ; require python environment
               leetcode-python-environment leetcode-python-environment))
     (get-buffer-create "*leetcode-install*"))))

(defun leetcode--my-cookies-path ()
  "Find the path to the my_cookies executable."
  (or (executable-find (format "%s/bin/my_cookies" leetcode-python-environment))
      (executable-find "my_cookies")))

(defun leetcode--check-deps ()
  "Check if all dependencies installed."
  (if (leetcode--my-cookies-path)
      t
    (leetcode--install-my-cookie)
    nil))

(defgroup leetcode nil
  "A Leetcode client."
  :prefix 'leetcode-
  :group 'tools)

(defcustom leetcode-prefer-tag-display t
  "Whether to display tags by default in the *leetcode* buffer."
  :group 'leetcode
  :type 'boolean)

(defcustom leetcode-prefer-language "python3"
  "LeetCode programming language.
c, cpp, csharp, golang, java, javascript, typescript, kotlin, php, python,
python3, ruby, rust, scala, swift."
  :group 'leetcode
  :type 'string)

(defcustom leetcode-prefer-sql "mysql"
  "LeetCode sql implementation.
mysql, mssql, oraclesql."
  :group 'leetcode
  :type 'string)

(defcustom leetcode-directory "~/leetcode"
  "Directory to save solutions."
  :group 'leetcode
  :type 'string)

(defcustom leetcode-save-solutions nil
  "If it's t, save leetcode solutions to `leetcode-directory'."
  :group 'leetcode
  :type 'boolean)

(defcustom leetcode-buffer-header
  '(("golang" . "package main\n\n"))
  "Alist mapping LeetCode langSlug to header text prepended to a fresh solve buffer.

The header is inserted once, at the top of the buffer, only when the buffer
is empty (first open of a problem).  Reopen under `leetcode-save-solutions'
skips re-insertion since the buffer is non-empty, so there is no duplication.

LeetCode strips common headers (e.g. `package main') server-side on
submit/try, so no client-side stripping is performed on send."
  :group 'leetcode
  :type '(alist :key-type string :value-type string))

(defcustom leetcode-random-filter '("medium" "all" nil)
  "Last used `leetcode-random' filter, as (DIFFICULTY STATUS TAGS).
DIFFICULTY is \"easy\"/\"medium\"/\"hard\"/\"all\", STATUS is
\"unsolved\"/\"all\", TAGS is a list of tag strings or nil.

Persisted through Customize so the previous filter is reused on
next Emacs start."
  :group 'leetcode
  :type '(list (string :tag "Difficulty")
               (string :tag "Status")
               (repeat string)))

(defvar leetcode--random-total-problems 4200
  "Initial guess of the total LeetCode problem count, updated from
the first random fetch's totalLength and used to derive the random
page range.")

(defvar leetcode--random-page-size 25
  "Number of problems per random page fetch.")

(defcustom leetcode-focus t
  "When execute `leetcode', always delete other windows."
  :group 'leetcode
  :type 'boolean)

(defcustom leetcode-python-environment (file-name-concat user-emacs-directory "leetcode-env")
  "The path to the isolated python virtual-environment to use."
  :group 'leetcode
  :type 'directory)

(defcustom leetcode-cache-file (file-name-concat user-emacs-directory "leetcode-problems-cache.json")
  "File caching the problem list between sessions.
Metadata only (id/title/slug/status/acceptance/difficulty/paid/tags);
problem content, snippets and testcases are always fetched live."
  :group 'leetcode
  :type 'file)

(cl-defstruct leetcode-user
  "A LeetCode User.
The object with following attributes:
:username   String
:id         Int
:is-premium Boolean {t|nil}"
  username id is-premium)

(cl-defstruct leetcode-snippet
  "A code snippet.
:lang String
:lang-slug String
:code String

We need both :lang and :lang-slug, because some programming
languages name conversion is not 'a-b-c' <=> 'aBC'.

For example: :lang 'C++' and :lang-slug 'cpp', :lang 'C#' and
:lang-slug 'csharp'."
  lang lang-slug code)

(cl-defstruct leetcode-problem
  "A single LeetCode problem.
:status     String
:id         String
:backend-id String
:title      String
:title-slug String
:acceptance String
:difficulty String {Easy,Medium,Hard}
:paid-only  Boolean {t|nil}
:likes      Number
:dislikes   Number
:tags       List
:content    String
:snippets   List {leetcode-snippet}
:testcases  List {String}

'id' is frontend id in LeetCode. We almost always use frontend id
in 'leetcode.el'."
  status id backend-id title title-slug acceptance
  difficulty paid-only likes dislikes tags content
  snippets testcases)

(cl-defstruct leetcode-problems
  "All LeetCode problems, the problems can filtered by tag.
:num      Number
:tag      String
:problems List[leetcode--problems]
:has-more Boolean"
  num tag problems has-more)

(defvar leetcode--user (make-leetcode-user)
  "A User object.")

(defvar leetcode--problems (make-leetcode-problems)
  "Problems object with a list of `leetcode-problem'.")

(defvar leetcode--all-tags nil
  "All problems tags.")

(defvar leetcode--problem-titles nil
  "Problem titles that have been open in solving layout.")

(defvar-local leetcode--problem-id nil
  "Buffer-local problem id in a LeetCode code buffer.")

(defvar leetcode--display-tags leetcode-prefer-tag-display
  "(Internal) Whether tags are displayed the *leetcode* buffer.")

(defcustom leetcode-display-paid nil
  "Whether paid problems are displayed in the *leetcode* buffer.
Also gates `leetcode-random': when nil, random picks free problems only."
  :group 'leetcode
  :type 'boolean)

(defvar leetcode--lang leetcode-prefer-language
  "LeetCode programming language or sql for current problem internally.
Default is programming language.")

(defvar leetcode--description-window nil
  "(Internal) Holds the reference to description window.")

(defvar leetcode--testcase-window nil
  "(Internal) Holds the reference to testcase window.")

(defvar leetcode--result-window nil
  "(Internal) Holds the reference to result window.")

(defconst leetcode--lang-suffixes
  '(("c" . ".c") ("cpp" . ".cpp") ("csharp" . ".cs")
    ("dart" . ".dart") ("elixir" . ".ex") ("erlang" . ".erl")
    ("golang" . ".go") ("java" . ".java") ("javascript" . ".js")
    ("kotlin" . ".kt") ("php" . ".php") ("python" . ".py") ("python3" . ".py")
    ("racket" . ".rkt") ("ruby" . ".rb") ("rust" . ".rs")
    ("scala" . ".scala") ("swift" . ".swift") ("typescript" . ".ts")
    ("mysql" . ".sql") ("mssql" . ".sql") ("oraclesql" . ".sql"))
  "A map of language slug name to LeetCode programming language suffix.
c, cpp, csharp, golang, java, javascript, typescript, kotlin, php, python,
python3, ruby, rust, scala, swift, mysql, mssql, oraclesql.")

(defconst leetcode--code-start "// code_start"
  "Code start mark in LeetCode description.")
(defconst leetcode--code-end "// code_end"
  "Code end mark in LeetCode description.")

(defvar leetcode--filter-regex nil "Filter rows by regex.")
(defvar leetcode--filter-tag nil "Filter rows by tag.")
(defvar leetcode--filter-difficulty nil
  "Filter rows by difficulty, it can be \"easy\", \"medium\" and \"hard\".")

(defconst leetcode--all-difficulties '("Easy" "Medium" "Hard"))
(defconst leetcode--paid "•" "Paid mark.")
(defconst leetcode--checkmark "✓" "Checkmark for accepted problem.")
(defconst leetcode--buffer-name             "*leetcode*")

(defface leetcode-paid-face
  '((t (:foreground "gold")))
  "Face for `leetcode--paid'."
  :group 'leetcode)

(defface leetcode-checkmark-face
  '((t (:foreground "#5CB85C")))
  "Face for `leetcode--checkmark'."
  :group 'leetcode)

(defface leetcode-easy-face
  '((t (:foreground "#5CB85C")))
  "Face for easy problems."
  :group 'leetcode)

(defface leetcode-medium-face
  '((t (:foreground "#F0AD4E")))
  "Face for medium problems."
  :group 'leetcode)

(defface leetcode-hard-face
  '((t (:foreground "#D9534E")))
  "Face for hard problems."
  :group 'leetcode)

(defface leetcode-accepted-face
  '((t (:foreground "#228b22")))
  "Face for submission accepted."
  :group 'leetcode)

(defface leetcode-error-face
  '((t (:foreground "#dc143c")))
  "Face for submission compile error, runtime error and TLE."
  :group 'leetcode)

;;; Login
;; URL
(defconst leetcode--domain    "leetcode.com")
(defconst leetcode--url-base  "https://leetcode.com")
(defconst leetcode--url-login (concat leetcode--url-base "/accounts/login"))

;; Cookie key name
(defconst leetcode--cookie-csrftoken "csrftoken")
(defconst leetcode--cookie-session "LEETCODE_SESSION")

;; Header
(defconst leetcode--User-Agent       '("User-Agent" .
                                       "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.12; rv:66.0) Gecko/20100101 Firefox/66.0"))
(defconst leetcode--X-Requested-With '("X-Requested-With" . "XMLHttpRequest"))
(defconst leetcode--X-CSRFToken      "X-CSRFToken")
(defconst leetcode--Content-Type     '("Content-Type" . "application/json"))

;; API URL
(defconst leetcode--url-api                 (concat leetcode--url-base "/api"))
(defconst leetcode--url-graphql             (concat leetcode--url-base "/graphql"))
(defconst leetcode--url-all-problems        (concat leetcode--url-api "/problems/all/"))
(defconst leetcode--url-all-tags            (concat leetcode--url-base "/problems/api/tags"))
(defconst leetcode--url-daily-challenge
  (concat
   "query questionOfToday { activeDailyCodingChallengeQuestion {"
   " link question { status title titleSlug qid: questionFrontendId } } }"))
;; submit
(defconst leetcode--url-submit              (concat leetcode--url-base "/problems/%s/submit/"))
(defconst leetcode--url-problems-submission (concat leetcode--url-base "/problems/%s/submissions/"))
(defconst leetcode--url-check-submission    (concat leetcode--url-base "/submissions/detail/%s/check/"))
;; try testcase
(defconst leetcode--url-try                 (concat leetcode--url-base "/problems/%s/interpret_solution/"))
(defconst leetcode--url-problems            (concat leetcode--url-base "/problems/%s/"))

(defconst leetcode--graphql-global-data "
query globalData {
  userStatus { userId username isPremium activeSessionId isSignedIn }
}")

;; graphql.el doesn't support `:as' keyword, so let's use the raw graphQL string.
(defconst leetcode--graphql-problemset-question-list-v2 "
query problemsetQuestionListV2($filters: QuestionFilterInput, $limit: Int, $searchKeyword: String, $skip: Int, $sortBy: QuestionSortByInput, $categorySlug: String) {
  problemsetQuestionListV2(
    filters: $filters
    limit: $limit
    searchKeyword: $searchKeyword
    skip: $skip
    sortBy: $sortBy
    categorySlug: $categorySlug
  ) {
    questions {
      id
      titleSlug
      title
      translatedTitle
      questionFrontendId
      paidOnly
      difficulty
      topicTags {
        name
        slug
        nameTranslated
      }
      status
      isInMyFavorites
      frequency
      acRate
      contestPoint
    }
    totalLength
    finishedLength
    hasMore
  }
}")

(defconst leetcode--graphql-question-page
  ;; The list query with a distinct operation name; the root field must
  ;; stay `problemsetQuestionListV2' — only the operation declaration is
  ;; renamed (operationName must match a declaration in the document).
  (string-replace "query problemsetQuestionListV2("
                  "query questionPage("
                  leetcode--graphql-problemset-question-list-v2))

(defconst leetcode--graphql-question-title "
query questionTitle($titleSlug: String!) {
  question(titleSlug: $titleSlug) { questionId questionFrontendId title titleSlug
                                    isPaidOnly difficulty likes dislikes categoryTitle } }")

(defconst leetcode--graphql-question-content "
query questionContent($titleSlug: String!) {
  question(titleSlug: $titleSlug) { content mysqlSchemas dataSchemas } }")

(defconst leetcode--graphql-question-editor-data "
query questionEditorData($titleSlug: String!) {
  question(titleSlug: $titleSlug) {
    questionId
    questionFrontendId
    codeSnippets { lang langSlug code }
    envInfo
    enableRunCode
    hasFrontendPreview
    frontendPreviews
  }
}")

(defconst leetcode--graphql-question-hints "
query questionHints($titleSlug: String!) {
  question(titleSlug: $titleSlug) { hints } }")

(defconst leetcode--graphql-console-panel-config "
query consolePanelConfig($titleSlug: String!) {
  question(titleSlug: $titleSlug) {
    questionId
    questionFrontendId
    questionTitle
    enableDebugger
    enableRunCode
    enableSubmit
    enableTestMode
    exampleTestcaseList
    metaData
  }
}")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun leetcode--insert-code-start-marker ()
  "Insert code start marker."
  (when (or (string= leetcode--lang "c")
            (string= leetcode--lang "cpp"))
    (insert (format "\n\n%s\n\n" leetcode--code-start))))

(defun leetcode--insert-code-end-marker ()
  "Insert code end marker."
  (when (or (string= leetcode--lang "c")
            (string= leetcode--lang "cpp"))
    (insert (format "\n\n%s\n\n" leetcode--code-end))))

(defun leetcode--referer (value)
  "It will return an alist as the HTTP Referer Header.
VALUE should be the referer."
  (cons "Referer" value))

(defun leetcode--cookie-get-all ()
  "Get leetcode session with `my_cookies'. You can install it with pip."
  (let* ((my-cookies (leetcode--my-cookies-path))
         (my-cookies-output (shell-command-to-string (leetcode--my-cookies-path)))
         (cookies-list (seq-filter (lambda (s) (not (string-empty-p s)))
                                   (s-split "\n" my-cookies-output 'OMIT-NULLS)))
         (cookies-pairs (seq-map (lambda (s) (s-split-up-to " " s 1 'OMIT-NULLS)) cookies-list)))
    cookies-pairs))

(defun leetcode--cookie-get (cookie-key)
  "Get LeetCode cookie value by COOKIE-KEY."
  (if-let* ((cookie (seq-find
                     (lambda (item)
                       (string= (aref item 1) cookie-key))
                     (url-cookie-retrieve leetcode--domain "/" t))))
      (aref cookie 2)))

(defun leetcode--maybe-csrf-token ()
  "Return LeetCode CSRF token if it exists, otherwise return nil."
  (leetcode--cookie-get leetcode--cookie-csrftoken))

(defun leetcode--maybe-session ()
  "Return LeetCode session if it exists, otherwise return nil."
  (leetcode--cookie-get leetcode--cookie-session))

(aio-defun leetcode--csrf-token ()
  "Return csrf token."
  (unless (leetcode--maybe-csrf-token)
    (aio-await (leetcode--login))
    (aio-await (leetcode--login)))
  (leetcode--maybe-csrf-token))

(defun leetcode--login-p ()
  "Whether user is login."
  (let ((username (leetcode-user-username leetcode--user)))
    (and username
         (not (string-empty-p username))
         (leetcode--maybe-session))))

(defun leetcode--slugify-title (title)
  "Make TITLE a slug title.
Such as 'Two Sum' will be converted to 'two-sum'. 'Pow(x, n)' will be 'powx-n'"
  (let* ((str1 (replace-regexp-in-string "[\s-]+" "-" (downcase title)))
         (res (replace-regexp-in-string "[(),']" "" str1)))
    res))

(defun leetcode--replace-in-buffer (regex to)
  "Replace string matched REGEX in `current-buffer' to TO."
  (with-current-buffer (current-buffer)
    (save-excursion
      (goto-char (point-min))
      (save-match-data
        (while (re-search-forward regex (point-max) t)
          (replace-match to))))))

(defun leetcode--problem-link (title)
  "Generate problem link from TITLE."
  (concat leetcode--url-base "/problems/" (leetcode--slugify-title title)))

(defun leetcode--stringify-difficulty (difficulty)
  "Add font-lock to DIFFICULTY."
  (pcase (downcase difficulty)
    ("easy" (leetcode--add-font-lock "Easy" 'leetcode-easy-face))
    ("medium" (leetcode--add-font-lock "Medium" 'leetcode-medium-face))
    ("hard" (leetcode--add-font-lock "Hard" 'leetcode-hard-face))))

(defun leetcode--add-font-lock (str face)
  "Add font-lock FACE to STR."
  (prog1 str
    (put-text-property 0 (length str) 'font-lock-face face str)))

(defun leetcode--detail-buffer-name (problem-id)
  "Detail buffer name with PROBLEM-ID."
  (format "*leetcode-detail-%s*" problem-id))

(defun leetcode--testcase-buffer-name (problem-id)
  "Testcase buffer name with PROBLEM-ID."
  (format "*leetcode-testcase-%s*" problem-id))

(defun leetcode--result-buffer-name (problem-id)
  "Result buffer name with PROBLEM-ID."
  (format "*leetcode-result-%s*" problem-id))

(defun leetcode--maybe-focus ()
  "Delete other windows, keep only *leetcode* buffer."
  (if leetcode-focus (delete-other-windows)))

(defun leetcode--parse-buffer (buffer)
  "Parse BUFFER content from json to alist."
  (with-current-buffer buffer
    (goto-char url-http-end-of-headers)
    (json-read)))

(aio-defun leetcode--common-extra-headers ()
  "Common extra headers for `url-request-extra-headers'."
  `(,leetcode--User-Agent ,leetcode--Content-Type
                          ,(cons leetcode--X-CSRFToken (aio-await (leetcode--csrf-token)))))

(defun leetcode--buffer-content (buf)
  "Get content without text properties of BUF."
  (with-current-buffer buf
    (buffer-substring-no-properties
     (point-min) (point-max))))

(defun leetcode--format-testcase-with-number (testcase number)
  "Return TESTCASE prefixed with a \"case NUMBER: \" label line."
  (concat "case " (number-to-string number) ":\n" testcase))

(defun leetcode--testcase-buffer-data (problem-id)
  "Get testcases buffer content of PROBLEM-ID as one argument per line.
Case-number label lines (\"case N:\") added on fill are stripped."
  (string-join
   (seq-remove (lambda (line) (string-match-p "\\`case [0-9]+:*\\'" line))
               (split-string
                (leetcode--buffer-content (get-buffer (leetcode--testcase-buffer-name problem-id)))
                "\n" t))
   "\n"))

(defun leetcode--code-buffer-data ()
  "Get code buffer content, that is, the `current-buffer'."
  (let ((code (leetcode--buffer-content (current-buffer)))
        (pattern (concat leetcode--code-start "\\([\0-\377]*?\\)" leetcode--code-end)))
    (if (string-match pattern code)
        (match-string 1 code)
      code)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; LeetCode API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun leetcode--graphql-payload (operation query &optional vars)
  "Construct GraphQL request payload with OPERATION, QUERY or maybe VARS."
  (json-encode
   (let ((ret `(("operationName" . ,operation)
                ("query" . ,query))))
     (if vars `(,@ret ("variables" . ,vars)) ret))))


(defmacro leetcode--define-graphql (query-name args &rest body)
  "Define LeetCode GraphQL queries.
Define a function with name of 'leetcode--fetch-<QUERY-NAME>',
and the ARGS will be the arguments of the defined function. BODY
will be executed when query successfully.

GraphQL request is defined with 'leetcode--graphql-<QUERY-NAME>'.
In the GraqhQL request body, operation name is lower camel case
of QUERY-NAME."
  (declare (indent defun) (doc-string 3))
  ;; `s-lower-camel-case' splits on `[:word:]', which is syntax-table
  ;; dependent; pin the standard syntax table so loading in a buffer where
  ;; `-' has word syntax doesn't corrupt the operation name.
  (let* ((camel (lambda (sym)
                  (with-syntax-table (standard-syntax-table)
                    (s-lower-camel-case (symbol-name sym)))))
         (variables (mapcar (lambda (arg) `(cons ,(funcall camel arg) ,arg)) args)))
    `(aio-defun ,(intern (concat "leetcode--fetch-" (symbol-name query-name))) ,args
       (let* ((graphql-operation-name ,(funcall camel query-name))
              (graphql-body ,(intern (concat "leetcode--graphql-" (symbol-name query-name))))
              (payload (leetcode--graphql-payload graphql-operation-name
                                                  graphql-body
                                                  (list ,@variables)))
              (url-request-method "POST")
              (url-request-extra-headers `(,leetcode--User-Agent ,leetcode--Content-Type))
              (url-request-data payload)
              (response (aio-await (aio-url-retrieve leetcode--url-graphql)))
              (response-status (car response))
              (response-buffer (cdr response)))
         (if-let* ((error (plist-get response-status :error)))
             (progn
               (switch-to-buffer response-buffer))
           (let-alist (with-current-buffer response-buffer (goto-char url-http-end-of-headers) (json-read))
             ,@body))))))

(leetcode--define-graphql global-data ()
  (setf (leetcode-user-id leetcode--user) .data.userStatus.userId)
  (setf (leetcode-user-username leetcode--user) .data.userStatus.username)
  (setf (leetcode-user-is-premium leetcode--user) .data.userStatus.isPremium))

(defun leetcode--parse-question-list-page (data)
  "Parse DATA, the `problemsetQuestionListV2' response alist.
Return (PROBLEMS . TOTAL-LENGTH), where PROBLEMS is a fresh list
of `leetcode-problem' in response order."
  (let ((problems)
        (questions (cdr (assq 'questions data))))
    (dotimes (i (length questions))
      (let-alist (aref questions i)
        (push (make-leetcode-problem
               :status     .status
               :id         .questionFrontendId
               :title      .title
               :title-slug .titleSlug
               :acceptance (format "%.1f%%" (* .acRate 100))
               :difficulty .difficulty
               :paid-only  (eq .paidOnly t)
               :tags       (seq-reduce (lambda (tags tag)
                                         (let-alist tag
                                           (push .slug tags)))
                                       .topicTags '()))
              problems)))
    (cons (nreverse problems) (cdr (assq 'totalLength data)))))

(leetcode--define-graphql problemset-question-list-v2 (category-slug skip limit filters search-keyword sort-by)
  (let* ((page (leetcode--parse-question-list-page .data.problemsetQuestionListV2))
         (problems (car page))
         (page-problems-len (length problems)))
    (leetcode--debug "length: %s, total: %s" page-problems-len (cdr page))
    (dolist (problem problems)
      (setq leetcode--all-tags (append leetcode--all-tags (leetcode-problem-tags problem))))
    (setf (leetcode-problems-problems leetcode--problems) (append (leetcode-problems-problems leetcode--problems) problems)
          (leetcode-problems-num leetcode--problems) (+ (leetcode-problems-num leetcode--problems) page-problems-len)
          (leetcode-problems-tag leetcode--problems) "all"
          (leetcode-problems-has-more leetcode--problems) .data.problemsetQuestionListV2.hasMore)
    ;; problem tags
    (delete-dups leetcode--all-tags)))

(leetcode--define-graphql question-page (category-slug skip limit filters search-keyword sort-by)
  ;; Non-mutating twin of `problemset-question-list-v2': random pages must
  ;; not corrupt the problem list's pagination state.
  (leetcode--parse-question-list-page .data.problemsetQuestionListV2))

(defun leetcode--problem-to-cache-alist (problem)
  "Serialize PROBLEM's list metadata to an alist for the cache file."
  `((status . ,(leetcode-problem-status problem))
    (id . ,(leetcode-problem-id problem))
    (title . ,(leetcode-problem-title problem))
    (titleSlug . ,(leetcode-problem-title-slug problem))
    (acceptance . ,(leetcode-problem-acceptance problem))
    (difficulty . ,(leetcode-problem-difficulty problem))
    (paidOnly . ,(leetcode-problem-paid-only problem))
    (tags . ,(vconcat (leetcode-problem-tags problem)))))

(defun leetcode--cache-alist-to-problem (entry)
  "Rebuild a `leetcode-problem' from a cache ENTRY alist."
  (let-alist entry
    (make-leetcode-problem
     :status .status
     :id .id
     :title .title
     :title-slug .titleSlug
     :acceptance .acceptance
     :difficulty .difficulty
     :paid-only .paidOnly
     :tags (append .tags '()))))

(defun leetcode--save-problems-cache ()
  "Write the current problem list to `leetcode-cache-file'.
Deduplicated by problem id, keeping the first occurrence."
  (when leetcode-cache-file
    (let* ((problems (leetcode-problems-problems leetcode--problems))
           (seen ())
           (deduped (seq-filter (lambda (p)
                                  (let ((id (leetcode-problem-id p)))
                                    (if (member id seen)
                                        nil
                                      (push id seen) t)))
                                problems)))
      (with-temp-file leetcode-cache-file
        (insert (json-encode
                 `((version . 1)
                   (hasMore . ,(leetcode-problems-has-more leetcode--problems))
                   (problems . ,(vconcat
                                 (mapcar #'leetcode--problem-to-cache-alist
                                         deduped))))))))))

(defun leetcode--load-problems-cache ()
  "Populate `leetcode--problems' from `leetcode-cache-file'.
Return t when the cache was usable, nil on missing/corrupt/old-version
files."
  (when (and leetcode-cache-file (file-exists-p leetcode-cache-file))
    (condition-case nil
        (let-alist (with-temp-buffer
                     (insert-file-contents leetcode-cache-file)
                     (goto-char (point-min))
                     (json-read))
          (when (eq .version 1)
            (let ((problems (mapcar #'leetcode--cache-alist-to-problem
                                    (append .problems '()))))
              (setf (leetcode-problems-problems leetcode--problems) problems
                    (leetcode-problems-num leetcode--problems) (length problems)
                    (leetcode-problems-tag leetcode--problems) "all"
                    (leetcode-problems-has-more leetcode--problems) .hasMore)
              (setq leetcode--all-tags
                    (delete-dups
                     (apply #'append (mapcar #'leetcode-problem-tags problems))))
              t)))
      (error nil))))

(leetcode--define-graphql question-content (title-slug)
  (let ((problem (leetcode--get-problem title-slug)))
    (if problem
        (progn
          (setf (leetcode-problem-content problem) .data.question.content)
          problem)
      (user-error "LeetCode problem not found: %s" title-slug))))

(leetcode--define-graphql question-title (title-slug)
  (let ((problem (leetcode--get-problem title-slug)))
    (if problem
        (progn
          (setf (leetcode-problem-likes problem) .data.question.likes)
          (setf (leetcode-problem-dislikes problem) .data.question.dislikes)
          problem)
      (user-error "LeetCode problem not found: %s" title-slug))))

(leetcode--define-graphql console-panel-config (title-slug)
  (let ((id .data.question.questionFrontendId)
        (testcases (append .data.question.exampleTestcaseList nil))
        (problem (leetcode--get-problem title-slug)))
    (if problem
        (progn
          (setf (leetcode-problem-testcases problem) testcases)
          problem)
      (user-error "LeetCode problem not found: %s" title-slug))))

(leetcode--define-graphql question-editor-data (title-slug)
  (let ((id .data.question.questionFrontendId)
        (problem (leetcode--get-problem title-slug))
        (snippets (seq-map (lambda (snippet-alist)
                             (let-alist snippet-alist
                               (make-leetcode-snippet
                                :lang      .lang
                                :lang-slug .langSlug
                                :code      .code)))
                           .data.question.codeSnippets)))
    (if problem
        (progn
          (setf (leetcode-problem-snippets problem) snippets)
          (setf (leetcode-problem-backend-id problem) .data.question.questionId)
          problem)
      (user-error "LeetCode problem not found: %s" title-slug))))

(defalias 'leetcode--fetch-user-status (symbol-function 'leetcode--fetch-global-data))
(defalias 'leetcode--fetch-question-list (symbol-function 'leetcode--fetch-problemset-question-list-v2))
(defalias 'leetcode--fetch-question-testcases (symbol-function 'leetcode--fetch-console-panel-config))
(defalias 'leetcode--fetch-question-snippets (symbol-function 'leetcode--fetch-question-editor-data))

(aio-defun leetcode--ensure-question-title (problem)
  (if (and (leetcode-problem-dislikes problem)
           (leetcode-problem-likes problem))
      problem
    (aio-await (leetcode--fetch-question-title
                (leetcode-problem-title-slug problem)))))

(aio-defun leetcode--ensure-question-content (problem)
  (if (leetcode-problem-content problem)
      problem
    (aio-await (leetcode--fetch-question-content
                (leetcode-problem-title-slug problem)))))

(aio-defun leetcode--ensure-question-snippets (problem)
  (if (leetcode-problem-snippets problem)
      problem
    (aio-await (leetcode--fetch-question-snippets
                (leetcode-problem-title-slug problem)))))

(aio-defun leetcode--ensure-question-testcases (problem)
  (if (leetcode-problem-testcases problem)
      problem
    (aio-await (leetcode--fetch-question-testcases
                (leetcode-problem-title-slug problem)))))

(aio-defun leetcode--api-interpret-solution  (problem)
  "Fetch PROBLEM interpret_id."
  (let* ((title-slug (leetcode-problem-title-slug problem))
         (problem-id (leetcode-problem-id problem))
         (backend-id (leetcode-problem-backend-id problem))
         (payload (json-encode `((data_input . ,(leetcode--testcase-buffer-data problem-id))
                                 (lang . ,leetcode--lang)
                                 (question_id . ,backend-id)
                                 (typed_code . ,(leetcode--code-buffer-data)))))
         (url-request-method "POST")
         (url-request-extra-headers `(,@(aio-await (leetcode--common-extra-headers))
                                      ,(leetcode--referer (format leetcode--url-problems title-slug))))
         (url-request-data payload)
         (response (aio-await (aio-url-retrieve (format leetcode--url-try title-slug))))
         (response-status (car response))
         (response-buffer (cdr response)))
    (if-let* ((error-info (plist-get response-status :error)))
        (progn
          (switch-to-buffer response-buffer)
          (leetcode--warn "LeetCode interpret problem ERROR: %S" error-info))
      (let-alist (with-current-buffer response-buffer (goto-char url-http-end-of-headers) (json-read))
        .interpret_id))))

(aio-defun leetcode--api-submit (backend-id slug-title code)
  "Submit CODE for problem which has BACKEND-ID and SLUG-TITLE."
  (message "LeetCode submit slug-title: %s, backend-id: %s" slug-title backend-id)
  (let* ((url-request-method "POST")
         (url-request-extra-headers `(,@(aio-await (leetcode--common-extra-headers))
                                      ,(leetcode--referer (format leetcode--url-problems-submission slug-title))))
         (url-request-data
          (json-encode `((lang . ,leetcode--lang)
                         (question_id . ,backend-id)
                         (typed_code . ,code)))))
    (aio-await (aio-url-retrieve (format leetcode--url-submit slug-title)))))

(aio-defun leetcode--api-check-submission (interpret-id problem on-success)
  "Poll the submission INTERPRET-ID until it succeeds or retries run out.
On success, call ON-SUCCESS with the problem id and result alist."
  (message "LeetCode check submission: %s"
           (format leetcode--url-check-submission interpret-id))
  (let* ((title-slug (leetcode-problem-title-slug problem))
         (result 'retry))
    (catch 'done
      (dotimes (_ 20)                        ; 20 polls x 0.5s sleep + request latency
        (let* ((url-request-method "GET")
               (url-request-extra-headers
                `(,@(aio-await (leetcode--common-extra-headers))
                  ,(leetcode--referer (format leetcode--url-problems title-slug))))
               (response (aio-await (aio-url-retrieve
                                     (format leetcode--url-check-submission interpret-id))))
               (response-buffer (cdr response)))
          (if (plist-get (car response) :error)
              (progn
                (switch-to-buffer response-buffer)
                (leetcode--warn "LeetCode check submission ERROR: %S"
                                (plist-get (car response) :error))
                (throw 'done nil))
            (setq result (condition-case nil
                             (leetcode--parse-buffer response-buffer)
                           (error 'retry)))
            (kill-buffer response-buffer)))
        (when (equal (alist-get 'state result) "SUCCESS")
          (throw 'done (funcall on-success
                                (leetcode-problem-id problem) result)))
        (aio-await (aio-sleep 0.5)))
      (leetcode--warn "LeetCode check submission timeout."))))


(aio-defun leetcode--login ()
  "We are not login actually, we are retrieving LeetCode login session
from local browser. It also cleans LeetCode cookies in `url-cookie-file'."
  (ignore-errors (url-cookie-delete-cookies leetcode--domain))
  (let* ((leetcode-cookie (leetcode--cookie-get-all)))
    (cl-loop for (key value) in leetcode-cookie
             do (url-cookie-store key value nil leetcode--domain "/" t)))
  ;; After login, we should have our user data already.
  (message "LeetCode fetching user data...")
  (aio-await (leetcode--fetch-user-status)))

(defun leetcode--problems-rows ()
  "Generate tabulated list rows from `leetcode--problems'.
Return a list of rows, each row is a vector:
\([<checkmark> <position> <title> <acceptance> <difficulty>] ...)"
  (let ((problems (leetcode-problems-problems leetcode--problems))
        rows)
    (dolist (p problems (reverse rows))
      (if (or leetcode-display-paid (not (leetcode-problem-paid-only p)))
          (let* ((p-status (if (equal (leetcode-problem-status p) "SOLVED")
                               (leetcode--add-font-lock leetcode--checkmark 'leetcode-checkmark-face)
                             (string-width leetcode--checkmark)
                             "  "))
                 (p-id (leetcode-problem-id p))
                 (p-title (concat
                           (leetcode-problem-title p)
                           " "
                           (if (leetcode-problem-paid-only p)
                               (leetcode--add-font-lock leetcode--paid 'leetcode-paid-face)
                             " ")))
                 (p-acceptance (leetcode-problem-acceptance p))
                 (p-difficulty (leetcode--stringify-difficulty (leetcode-problem-difficulty p)))
                 (p-tags (if leetcode--display-tags (string-join (leetcode-problem-tags p) ", ") ""))
                 (single-row (vector p-status p-id p-title p-acceptance p-difficulty p-tags)))
            (setq rows (cons single-row rows)))))))

(defun leetcode--filter (rows)
  "Filter ROWS by `leetcode--filter-regex', `leetcode--filter-tag' and `leetcode--filter-difficulty'."
  (seq-filter
   (lambda (row)
     (and
      (if leetcode--filter-regex
          (let ((title (aref row 2)))
            (string-match-p leetcode--filter-regex title))
        t)
      (if leetcode--filter-tag
          (let ((tags (leetcode-problem-tags (leetcode--get-problem-by-id (aref row 1)))))
            (member leetcode--filter-tag tags))
        t)
      (if leetcode--filter-difficulty
          (let ((difficulty (leetcode-problem-difficulty (leetcode--get-problem-by-id (aref row 1)))))
            (string-equal-ignore-case difficulty leetcode--filter-difficulty))
        t)))
   rows))


(defun leetcode--random-filter-to-string (filter)
  "Serialize FILTER, a (DIFFICULTY STATUS TAGS) list, to a prompt string.
Tags are comma-joined and omitted when nil."
  (let ((difficulty (nth 0 filter))
        (status (nth 1 filter))
        (tags (nth 2 filter)))
    (string-join (if tags
                     (list difficulty status (string-join tags ","))
                   (list difficulty status))
                 " ")))

(defun leetcode--random-parse-difficulty (token)
  "Canonicalize difficulty TOKEN to lowercase, or nil if unrecognized."
  (when token
    (let ((candidates (append leetcode--all-difficulties '("all"))))
      (downcase (seq-find (lambda (cand) (string-equal-ignore-case cand token))
                          candidates)))))

(defun leetcode--random-parse-filter (input)
  "Parse INPUT, a \"[difficulty] [unsolved|all] [tag,tag,...]\" line.
Return a (DIFFICULTY STATUS TAGS) list.  Missing or unrecognized
tokens fall back to the previous filter
\(`leetcode-random-filter')."
  (let* ((prev leetcode-random-filter)
         (tokens (split-string input nil t))
         (difficulty (leetcode--random-parse-difficulty (nth 0 tokens)))
         (status (when (nth 1 tokens)
                   (let ((down (downcase (nth 1 tokens))))
                     (when (member down '("unsolved" "all"))
                       down))))
         (tag-tokens (seq-mapcat (lambda (token)
                                   (split-string token "," t))
                                 (nthcdr 2 tokens))))
    (list (or difficulty (nth 0 prev))
          (or status (nth 1 prev))
          (if (null (nthcdr 2 tokens))
              (nth 2 prev)
            tag-tokens))))

(defun leetcode--random-filter-problems (problems difficulty status tags)
  "Return members of PROBLEMS matching DIFFICULTY, STATUS and TAGS (OR semantics).
DIFFICULTY and STATUS are lowercase strings (\"all\" means no
restriction)."
  (seq-filter
   (lambda (p)
     (and (or leetcode-display-paid (not (leetcode-problem-paid-only p)))
          (or (string= difficulty "all")
              (string-equal-ignore-case (leetcode-problem-difficulty p) difficulty))
          (or (string= status "all")
              (not (equal (leetcode-problem-status p) "SOLVED")))
          (or (null tags)
              (seq-some (lambda (tag) (member tag (leetcode-problem-tags p))) tags))))
   problems))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; User Command ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun leetcode-reset-filter-and-refresh ()
  "Reset filter."
  (interactive)
  (setq leetcode--filter-regex nil)
  (setq leetcode--filter-tag nil)
  (setq leetcode--filter-difficulty nil)
  (leetcode-refresh))

(defun leetcode-set-filter-regex (regex)
  "Set `leetcode--filter-regex' as REGEX and refresh."
  (interactive "sSearch: ")
  (setq leetcode--filter-regex regex)
  (leetcode-refresh))

(defun leetcode-set-filter-tag ()
  "Set `leetcode--filter-tag' from `leetcode--all-tags' and refresh."
  (interactive)
  (setq leetcode--filter-tag
        (completing-read "Tags: " leetcode--all-tags))
  (leetcode-refresh))

(defun leetcode-set-prefer-language ()
  "Set `leetcode-prefer-language' from `leetcode--lang-suffixes' and refresh."
  (interactive)
  (setq leetcode-prefer-language
        (completing-read "Language: " leetcode--lang-suffixes))
  (leetcode-refresh))

(defun leetcode-set-filter-difficulty ()
  "Set `leetcode--filter-difficulty' from `leetcode--all-difficulties' and refresh."
  (interactive)
  (setq leetcode--filter-difficulty
        (completing-read "Difficulty: " leetcode--all-difficulties))
  (leetcode-refresh))

(defun leetcode-toggle-tag-display ()
  "Toggle `leetcode--display-tags` and refresh."
  (interactive)
  (setq leetcode--display-tags (not leetcode--display-tags))
  (leetcode-refresh))

(defun leetcode-toggle-paid-display ()
  "Toggle `leetcode-display-paid' and refresh."
  (interactive)
  (setq leetcode-display-paid (not leetcode-display-paid))
  (leetcode-refresh))

(defun leetcode--make-tabulated-headers (header-names rows)
  "Calculate headers width.
Column width calculated by picking the max width of every cell
under that column and the HEADER-NAMES. HEADER-NAMES are a list
of header name, ROWS are a list of vector, each vector is one
row."
  (let ((widths
         (seq-reduce
          (lambda (acc row)
            (cl-mapcar
             (lambda (a col) (max a (length col)))
             acc
             (append row '())))
          rows
          (seq-map #'length header-names))))
    (vconcat
     (cl-mapcar
      (lambda (col size) (list col size nil))
      header-names widths))))

(aio-defun leetcode--load-more ()
  "Load more problems."
  (aio-await (leetcode--fetch-question-list "all-code-essentials"
                                            (leetcode-problems-num leetcode--problems)
                                            100
                                            '((filterCombineType . "ALL"))
                                            ""
                                            '((sortField . "CUSTOM")
                                              (sortOrder . "ASCENDING"))))
  (leetcode--save-problems-cache)
  (leetcode-refresh))

(defvar leetcode--load-more-button-fn
  (lambda () (interactive) (aio-wait-for (leetcode--load-more)))
  "Load more button action.")

(defvar leetcode--load-more-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map (kbd "RET") leetcode--load-more-button-fn)
      (define-key map [mouse-1] leetcode--load-more-button-fn))))

(defun leetcode-refresh ()
  "Make `tabulated-list-entries'."
  (interactive)
  (let* ((header-names (append '(" " "#" "Problem" "Acceptance" "Difficulty")
                               (if leetcode--display-tags '("Tags"))))
         (rows (leetcode--filter (leetcode--problems-rows)))
         (headers (leetcode--make-tabulated-headers header-names rows)))
    (with-current-buffer (get-buffer-create leetcode--buffer-name)
      (leetcode--problems-mode)
      (setq tabulated-list-format headers)
      (setq tabulated-list-entries
            (append
             (cl-mapcar
              (lambda (i x) (list i x))
              (number-sequence 0 (1- (length rows)))
              rows)
             (if (leetcode-problems-has-more leetcode--problems)
                 `((:load-more
                    ["" ""
                     ,(propertize "[Load More]"
                                  'face 'button
                                  'keymap leetcode--load-more-map
                                  'help-echo "Click to load more"
                                  'mouse-face 'highlight)
                     "" "" ""])))))

      (tabulated-list-init-header)
      (tabulated-list-print t))))

(aio-defun leetcode-refresh-fetch ()
  "Refresh problems and update `tabulated-list-entries'.
Fetch one more page on top of the loaded problems, so the list grows
a page per refresh."
  (interactive)
  (message "LeetCode refreshing question list...")
  (setf (leetcode-problems-has-more leetcode--problems) t)
  ;; max page limit is 100
  (aio-await (leetcode--fetch-question-list "all-code-essentials"
                                            (leetcode-problems-num leetcode--problems)
                                            100
                                            '((filterCombineType . "ALL"))
                                            ""
                                            '((sortField . "CUSTOM")
                                              (sortOrder . "ASCENDING"))))
  (leetcode--save-problems-cache)
  (setq leetcode--display-tags leetcode-prefer-tag-display)
  (leetcode-reset-filter-and-refresh))

(aio-defun leetcode--ensure-login (&optional force)
  (when (or force (not (leetcode--login-p)))
    (aio-await (leetcode--login)) ; It's weird that somehow we have to login twice to be real login...
    (aio-await (leetcode--login))))

;;;###autoload(autoload 'leetcode "leetcode" nil t)
(aio-defun leetcode ()
  "Start Leetcode."
  (interactive)
  (when (leetcode--check-deps)
    (if (get-buffer leetcode--buffer-name)
        (switch-to-buffer leetcode--buffer-name)
      (aio-await (leetcode--ensure-login))
      (unless (leetcode--load-problems-cache)
        (aio-await (leetcode-refresh-fetch)))
      (switch-to-buffer leetcode--buffer-name)
      (leetcode-refresh))
    (leetcode--maybe-focus)))

;;;###autoload(autoload 'leetcode-daily "leetcode" nil t)
(aio-defun leetcode-daily ()
  "Open the daily challenge."
  (interactive)
  (aio-await (leetcode--ensure-login))
  (let* ((url-request-method "POST")
         (url-request-extra-headers `(,@(aio-await (leetcode--common-extra-headers))
                                      ,(leetcode--referer leetcode--url-login)))
         (url-request-data
          (json-encode
           `((operationName . "questionOfToday")
             (query . ,leetcode--url-daily-challenge)))))
    (with-current-buffer (url-retrieve-synchronously leetcode--url-graphql)
      (goto-char url-http-end-of-headers)
      (let-alist (json-read)
        (let ((qid .data.activeDailyCodingChallengeQuestion.question.qid))
          (leetcode-show-problem qid))))))

(aio-defun leetcode--random-page (skip)
  "Fetch problem page at SKIP without touching list state.
Update `leetcode--random-total-problems' from the response's
totalLength and return the page's problems."
  (let ((page (aio-await (leetcode--fetch-question-page "all-code-essentials"
                                                        skip leetcode--random-page-size
                                                        '((filterCombineType . "ALL"))
                                                        ""
                                                        '((sortField . "CUSTOM")
                                                          (sortOrder . "ASCENDING"))))))
    (setq leetcode--random-total-problems (cdr page))
    (car page)))

(aio-defun leetcode--fetch-question-by-id (problem-id)
  "Fetch the problem with frontend id PROBLEM-ID and register it in
`leetcode--problems'.  For problems not in the loaded list, e.g. an
id typed into `leetcode-show-problem'.  A numeric search keyword
matches the frontend id; the page is then exact-matched to skip
number-bearing titles.  Return the problem, or `user-error' when the
id matches nothing."
  (let* ((page (aio-await (leetcode--fetch-question-page "all-code-essentials"
                                                         0 5
                                                         '((filterCombineType . "ALL"))
                                                         problem-id
                                                         '((sortField . "CUSTOM")
                                                           (sortOrder . "ASCENDING")))))
         (problem (seq-find (lambda (p) (equal (leetcode-problem-id p) problem-id))
                            (car page))))
    (if problem
        (progn
          (cl-pushnew problem (leetcode-problems-problems leetcode--problems)
                      :test (lambda (a b) (equal (leetcode-problem-id a)
                                                 (leetcode-problem-id b))))
          (leetcode--save-problems-cache)
          problem)
      (user-error "LeetCode problem not found: %s" problem-id))))

(aio-defun leetcode--random-pick (difficulty status tags)
  "Return a random problem matching DIFFICULTY, STATUS and TAGS, or nil.
Tries up to 4 random pages of the problem set, filtering each
locally."
  (let ((tried-skips ())
        (pick nil))
    (while (and (null pick)
                (< (length tried-skips) 4)
                ;; A stale-high cache can overestimate the page count; stop
                ;; once every pickable page has been tried.
                (< (length tried-skips)
                   (ceiling leetcode--random-total-problems leetcode--random-page-size)))
      (let* ((page-size leetcode--random-page-size)
             (page-count (ceiling leetcode--random-total-problems page-size))
             (skip (* page-size (random page-count))))
        (unless (memq skip tried-skips)
          (push skip tried-skips)
          (let* ((problems (aio-await (leetcode--random-page skip)))
                 (candidates (leetcode--random-filter-problems problems difficulty status tags)))
            (when candidates
              (setq pick (nth (random (length candidates)) candidates)))))))
    pick))

;;;###autoload(autoload 'leetcode-random "leetcode" nil t)
(aio-defun leetcode-random (&optional arg)
  "Pick a random problem matching a filter and start solving it.

With plain \\[universal-argument] ARG, prompt once for the filter:
\"[difficulty] [unsolved|all] [tag,tag,...]\" — e.g.
\"medium unsolved array,hash-table\".  Bare RET keeps the previous
filter.  Otherwise pick immediately using `leetcode-random-filter'.  Tags are OR semantics.

Picks a random page of the problem set (page count derived from
`leetcode--random-total-problems', seeded by the first response),
filters it locally, then picks within the page; retries other
random pages when the filter matches nothing.

The picked problem opens in the coding layout with the code buffer
selected; the problem description is not shown."
  (interactive "P")
  (let* ((filter (if arg
                     (leetcode--random-parse-filter
                      (read-from-minibuffer
                       "Random filter [difficulty unsolved|all tag,tag]: "
                       (leetcode--random-filter-to-string leetcode-random-filter)))
                   leetcode-random-filter))
         (pick (progn
                 (aio-await (leetcode--ensure-login))
                 (aio-await (leetcode--random-pick (nth 0 filter) (nth 1 filter) (nth 2 filter))))))
    (if (null pick)
        (message "No problems match: %s %s %s"
                 (nth 0 filter) (nth 1 filter) (string-join (nth 2 filter) ","))
      (unless (equal filter leetcode-random-filter)
        (customize-save-variable 'leetcode-random-filter filter))
      ;; Downstream lookups (`leetcode--get-problem-by-id') require the
      ;; problem to be registered in `leetcode--problems'.
      (cl-pushnew pick (leetcode-problems-problems leetcode--problems)
                  :test (lambda (a b) (equal (leetcode-problem-id a)
                                             (leetcode-problem-id b))))
      (let ((problem-id (leetcode-problem-id pick)))
        (leetcode--cleanup-other-problems problem-id)
        (leetcode-show-problem problem-id)))))

(aio-defun leetcode-try ()
  "Asynchronously test the code using customized testcase."
  (interactive)
  (leetcode-restore-layout)
  (aio-await (leetcode--ensure-login t))
  (let* ((problem (leetcode--get-problem-by-id leetcode--problem-id))
         (interpret-id (aio-await (leetcode--api-interpret-solution problem))))
    (aio-await (leetcode--api-check-submission interpret-id problem #'leetcode--show-testcases-result))))

(aio-defun leetcode-submit ()
  "Asynchronously submit the code and show result."
  (interactive)
  (leetcode-restore-layout)
  (aio-await (leetcode--ensure-login t))
  (let* ((problem (leetcode--get-problem-by-id leetcode--problem-id))
         (slug-title (leetcode-problem-title-slug problem))
         (backend-id (leetcode-problem-backend-id problem))
         (code (leetcode--code-buffer-data))
         (response (aio-await (leetcode--api-submit backend-id slug-title code)))
         (response-status (car response))
         (response-buffer (cdr response)))
    (if-let* ((error-info (plist-get response-status :error)))
        (switch-to-buffer response-buffer)
      (let* ((resp (leetcode--parse-buffer response-buffer))
             (submission-id (number-to-string (alist-get 'submission_id resp))))
        (aio-await (leetcode--api-check-submission submission-id problem #'leetcode--show-submission-result))))))

(defun leetcode--solving-window-layout ()
  "Specify layout for solving problem.
+---------------+----------------+
|               |                |
|               |     Detail     |
|               |                |
|               +----------------+
|     Code      |   Customize    |
|               |   Testcases    |
|               +----------------+
|               |Submit/Testcases|
|               |    Result      |
+---------------+----------------+"
  (delete-other-windows)
  (setq leetcode--description-window (split-window-horizontally))
  (other-window 1)
  (setq leetcode--testcase-window (split-window-below))
  (other-window 1)
  (setq leetcode--result-window (split-window-below))
  (other-window -1)
  (other-window -1))

(defun leetcode--display-result (buffer &optional alist)
  "Display function for LeetCode result.
BUFFER is used to show LeetCode result. ALIST is a combined alist
specified in `display-buffer-alist'."
  (let ((window (window-next-sibling
                 (window-next-sibling
                  (window-top-child
                   (window-next-sibling
                    (window-left-child
                     (frame-root-window))))))))
    (set-window-buffer window buffer)
    window))

(defun leetcode--display-testcase (buffer &optional alist)
  "Display function for LeetCode testcase.
BUFFER is used to show LeetCode testcase. ALIST is a combined
alist specified in `display-buffer-alist'."
  (let ((window (window-next-sibling
                 (window-top-child
                  (window-next-sibling
                   (window-left-child
                    (frame-root-window)))))))
    (set-window-buffer window buffer)
    window))

(defun leetcode--display-detail (buffer &optional alist)
  "Display function for LeetCode detail.
BUFFER is used to show LeetCode detail. ALIST is a combined alist
specified in `display-buffer-alist'."
  (let ((window (window-top-child
                 (window-next-sibling
                  (window-left-child
                   (frame-root-window))))))
    (set-window-buffer window buffer)
    window))

(defun leetcode--display-code (buffer &optional alist)
  "Display function for LeetCode code.
BUFFER is the one to show LeetCode code. ALIST is a combined
alist specified in `display-buffer-alist'."
  (let ((window (window-left-child (frame-root-window))))
    (set-window-buffer window buffer)
    window))

(defun leetcode--show-testcases-result (problem-id result)
  "Show testcases RESULT by PROBLEM-ID."
  (let-alist result
    ;; The result buffer may be gone if the problem was cleaned up while
    ;; the request was in flight.
    (when-let* ((buf (get-buffer (leetcode--result-buffer-name problem-id))))
      (with-current-buffer buf
        (erase-buffer)
        (goto-char (point-max))
        (cond
         ((eq .status_code 10)
          (if (equal .code_answer .expected_code_answer)
              (insert (leetcode--add-font-lock "PASS: " 'leetcode-accepted-face))
            (insert (leetcode--add-font-lock "FAIL: " 'leetcode-error-face)))
          (insert "\n\n")
          ;; Code Answer
          (insert "Code Answer:")
          (dotimes (i (length .code_answer))
            (insert (format "\n%s" (aref .code_answer i))))
          (insert "\n")
          ;; Expected
          (insert "Expected Code Answer:")
          (dotimes (i (length .expected_code_answer))
            (insert (format "\n%s" (aref .expected_code_answer i))))
          (insert "\n")
          ;; Std output
          (when (seq-find (lambda (s) (not (string-empty-p s))) .std_output_list)
            (insert "Std Output:\n")
            (dotimes (i (length .std_output_list))
              (when (and (aref .std_output_list i)
                         (not (string-empty-p (aref .std_output_list i))))
                (insert (leetcode--format-testcase-with-number
                         (aref .std_output_list i) i))))))
         ((or (eq .status_code 12) (eq .status_code 14))
          (insert (format "Status: %s\n\n"
                          (leetcode--add-font-lock
                           (format "%s (%s/%s)" .status_msg .total_correct .total_testcases)
                           'leetcode-error-face)))
          (insert (format "Test Case: \n%s\n\n" .last_testcase))
          (insert (format "Expected Answer: %s\n\n" .expected_output))
          (unless (string-empty-p .std_output)
            (insert (format "Stdout: \n%s\n" .std_output))))
         ((eq .status_code 15)
          (insert (leetcode--add-font-lock .status_msg 'leetcode-error-face))
          (insert "\n\n")
          (insert .full_runtime_error))
         ((eq .status_code 20)
          (insert (leetcode--add-font-lock .status_msg 'leetcode-error-face))
          (insert "\n\n")
          (insert .full_compile_error)))))))

(defun leetcode--show-submission-result (problem-id result)
  "Show error info in `leetcode--result-buffer-name' by PROBLEM-ID.
Error info comes from RESULT.

STATUS_CODE has following possible value:

- 10: Accepted
- 11: Wrong Anwser
- 12: Memory Limit Exceeded
- 13: Output Limit Exceeded
- 14: Time Limit Exceeded
- 15: Runtime Error.  full_runtime_error
- 20: Compile Error.  full_compile_error"
  (let-alist result
    (with-current-buffer (get-buffer-create (leetcode--result-buffer-name problem-id))
      (erase-buffer)
      (font-lock-mode +1)
      (cond
       ((eq .status_code 10)
        (insert (format "Status: %s\n"
                        (leetcode--add-font-lock
                         (format "%s (%s/%s)" .status_msg .total_correct .total_testcases)
                         'leetcode-accepted-face)))
        (insert (format "Runtime: %s, faster than %.2f%% of %s submissions.\n"
                        .status_runtime .runtime_percentile .pretty_lang))
        (insert (format "Memory Usage: %s, less than %.2f%% of %s submissions."
                        .status_memory .memory_percentile .pretty_lang)))
       ((eq .status_code 11)
        (insert (format "Status: %s\n"
                        (leetcode--add-font-lock
                         (format "%s (%s/%s)" .status_msg .total_correct .total_testcases)
                         'leetcode-error-face)))
        (insert (format "Test Case: \n%s\n" .input))
        (insert (format "Answer: %s\n" .code_output))
        (insert (format "Expected Answer: %s\n" .expected_output))
        (unless (string-empty-p .std_output)
          (insert (format "Stdout: \n%s\n" .std_output))))
       ((eq .status_code 12)
        (insert (format "Status: %s" (leetcode--add-font-lock .status_msg 'leetcode-error-face)))
        (insert (format "\n%s / %s testcases passed\n" .total_correct .total_testcases))
        (insert (format "Last Test Case: %s\n" .last_testcase)))
       ((eq .status_code 13)
        (insert (format "Status: %s" (leetcode--add-font-lock .status_msg 'leetcode-error-face))))
       ((eq .status_code 14)
        (insert (format "Status: %s" (leetcode--add-font-lock .status_msg 'leetcode-error-face)))
        (insert (format "\n%s / %s testcases passed\n" .total_correct .total_testcases))
        (insert (format "Last Test Case: %s\n" .last_testcase)))
       ((eq .status_code 15)
        (insert (format "Status: %s" (leetcode--add-font-lock .status_msg 'leetcode-error-face)))
        (insert (format "\n%s / %s testcases passed\n" .total_correct .total_testcases))
        (insert (format "Last Test Case: %s\n" .last_testcase))
        (insert "\n")
        (insert (format .full_runtime_error)))
       ((eq .status_code 20)
        (insert (format "Status: %s" (leetcode--add-font-lock .status_msg 'leetcode-error-face)))
        (insert "\n")
        (insert (format .full_compile_error))))
      (display-buffer (current-buffer)
                      '((display-buffer-reuse-window
                         leetcode--display-result)
                        (reusable-frames . visible))))))

(defun leetcode--show-problem (problem)
  "Show the detail of PROBLEM.
Use `shr-render-buffer' to render problem detail. This action
will show the detail in other window and jump to it."
  (let* ((problem-id (leetcode-problem-id problem))
         (title (leetcode-problem-title problem))
         (difficulty (leetcode-problem-difficulty problem))
         (likes (leetcode-problem-likes problem))
         (dislikes (leetcode-problem-dislikes problem))
         (content (leetcode-problem-content problem))
         (buf-name (leetcode--detail-buffer-name problem-id))
         (html-margin "&nbsp;&nbsp;&nbsp;&nbsp;"))
    (leetcode--debug "select title: %s" title)
    ;; Kill defail buffer if exists, we'll re-create a new one.
    (when (get-buffer buf-name) (kill-buffer buf-name))
    ;; Render question with `shr'.
    (with-temp-buffer
      (insert (concat "<h1>" problem-id ". " title "</h1>"))
      (insert (concat (capitalize difficulty) html-margin
                      "likes: " (number-to-string likes) html-margin
                      "dislikes: " (number-to-string dislikes)))
      ;; Sometimes LeetCode don't have a '<p>' at the outermost...
      (insert "<p>" content "</p>")
      (leetcode--replace-in-buffer "" "")
      ;; NOTE: shr.el can't render "https://xxxx.png", so we use "http"
      (leetcode--replace-in-buffer "https" "http")
      (shr-render-buffer (current-buffer)))

    ;; `shr-render-buffer' will put the result in buffer *html*.
    (with-current-buffer "*html*"
      (save-match-data
        (re-search-forward "dislikes: .*" nil t)
        (insert (make-string 4 ?\s))
        (insert-text-button "Solve it"
                            'action (lambda (btn) (leetcode--start-coding problem))
                            'help-echo "Solve the problem.")
        (insert (make-string 4 ?\s))
        (insert-text-button "Link"
                            'action (lambda (btn) (browse-url (leetcode--problem-link title)))
                            'help-echo "Open the problem in browser.")
        (insert (make-string 4 ?\s))
        (insert-text-button "Solution"
                            'action (lambda (btn) (browse-url (concat (leetcode--problem-link title) "/solution")))
                            'help-echo "Open the problem solution page in browser."))
      (rename-buffer buf-name)
      (leetcode--problem-detail-mode)
      (switch-to-buffer (current-buffer))
      (search-backward "Solve it"))
    (leetcode--maybe-focus)))

(aio-defun leetcode-show-problem (problem-id)
  "Show the detail of problem with id PROBLEM-ID.
Get problem by id and use `shr-render-buffer' to render problem
detail. This action will show the detail in other window and jump
to it."
  (interactive (list (read-string "Show problem by problem id: "
                                  (leetcode--get-current-problem-id))))
  (let* ((problem (or (leetcode--get-problem-by-id problem-id)
                      (aio-await (leetcode--fetch-question-by-id problem-id))))
         (problem-with-title (aio-await (leetcode--ensure-question-title problem)))
         (problem-with-content (aio-await (leetcode--ensure-question-content problem)))
         (problem-with-testcases (aio-await (leetcode--ensure-question-testcases problem)))
         (problem-with-snippets (aio-await (leetcode--ensure-question-snippets problem))))
    (leetcode--show-problem problem-with-snippets)))

(defun leetcode-show-problem-by-slug (slug-title)
  "Show the detail of problem with SLUG-TITLE.
This function will work after first run
\\[execute-extended-command] leetcode. Get problem by id and use
`shr-render-buffer' to render problem detail. This action will
show the detail in other window and jump to it.

It can be used in org-link elisp:(leetcode-show-problem-by-slug \"3sum\")."
  (interactive (list (read-string "Show problem by problem id: "
                                  (leetcode--get-current-problem-id))))
  (let* ((problem (leetcode--get-problem slug-title))
         (problem-id (leetcode-problem-id problem)))
    (leetcode-show-problem problem-id)))

(defun leetcode-show-current-problem ()
  "Show current problem's detail.
Call `leetcode-show-problem' on the current problem id. This
action will show the detail in other window and jump to it."
  (interactive)
  (leetcode-show-problem (leetcode--get-current-problem-id)))

(aio-defun leetcode-view-problem (problem-id)
  "View problem by PROBLEM-ID while staying in `LC Problems' window.
Similar with `leetcode-show-problem', but instead of jumping to
the detail window, this action will jump back in `LC Problems'."
  (interactive (list (read-string "View problem by problem id: "
                                  (leetcode--get-current-problem-id))))
  (aio-await (leetcode-show-problem problem-id))
  (leetcode--jump-to-window-by-buffer-name leetcode--buffer-name))

(defun leetcode-view-current-problem ()
  "View current problem while staying in `LC Problems' window.
Similar with `leetcode-show-current-problem', but instead of
jumping to the detail window, this action will jump back in `LC
Problems'."
  (interactive)
  (leetcode-view-problem (leetcode--get-current-problem-id)))

(defun leetcode-show-problem-in-browser (problem-id)
  "Open the problem with id PROBLEM-ID in browser."
  (interactive (list (read-string "Show in browser by problem id: "
                                  (leetcode--get-current-problem-id))))
  (let* ((problem (leetcode--get-problem-by-id problem-id))
         (title (leetcode-problem-title problem))
         (link (leetcode--problem-link title)))
    (leetcode--debug "open in browser: %s" link)
    (browse-url link)))

(defun leetcode-show-current-problem-in-browser ()
  "Open the current problem in browser.
Call `leetcode-show-problem-in-browser' on the current problem id."
  (interactive)
  (leetcode-show-problem-in-browser (leetcode--get-current-problem-id)))

(aio-defun leetcode-solve-problem (problem-id)
  "Start coding the problem with id PROBLEM-ID."
  (interactive (list (read-string "Solve the problem with id: "
                                  (leetcode--get-current-problem-id))))
  (aio-await (leetcode-show-problem problem-id))
  (leetcode--start-coding (leetcode--get-problem-by-id problem-id)))

(defun leetcode-solve-current-problem ()
  "Start coding the current problem.
Call `leetcode-solve-problem' on the current problem id."
  (interactive)
  (leetcode-solve-problem (leetcode--get-current-problem-id)))

(defun leetcode--jump-to-window-by-buffer-name (buffer-name)
  "Jump to window by BUFFER-NAME."
  (select-window (get-buffer-window buffer-name)))

(defun leetcode--kill-buff-and-delete-window (buf)
  "Kill BUF and delete its window."
  (when buf
    (delete-windows-on buf t)
    (kill-buffer buf)))

(defun leetcode--cleanup-problem-by-id (problem-id)
  "Kill the code/detail/result/testcase buffers of problem with id PROBLEM-ID.
Save a modified solution file buffer before killing it; remove the
problem from `leetcode--problem-titles'."
  (let* ((problem (leetcode--get-problem-by-id problem-id))
         (title (when problem (leetcode-problem-title problem))))
    ;; Code buffer first: it holds the user's solution; save it before killing.
    ;; Skip when the code buffer is the one being killed (hook re-entry).
    (let ((code-buffer (when title (get-buffer (leetcode--get-code-buffer-name title)))))
      (when (and code-buffer (not (eq code-buffer (current-buffer))))
        (with-current-buffer code-buffer
          (when (and buffer-file-name (buffer-modified-p))
            (save-buffer)))
        (leetcode--kill-buff-and-delete-window code-buffer)))
    (leetcode--kill-buff-and-delete-window
     (get-buffer (leetcode--detail-buffer-name problem-id)))
    (leetcode--kill-buff-and-delete-window
     (get-buffer (leetcode--result-buffer-name problem-id)))
    (leetcode--kill-buff-and-delete-window
     (get-buffer (leetcode--testcase-buffer-name problem-id)))
    (setq leetcode--problem-titles (remove title leetcode--problem-titles))))

(defun leetcode--cleanup-other-problems (problem-id)
  "Clean up buffers of open problems other than PROBLEM-ID."
  (dolist (title leetcode--problem-titles)
    (let ((other (leetcode--get-problem (leetcode--slugify-title title))))
      (when (and other (not (equal (leetcode-problem-id other) problem-id)))
        (leetcode--cleanup-problem-by-id (leetcode-problem-id other))))))

(defun leetcode--cleanup-on-kill ()
  "Clean up the current problem when its code buffer is killed."
  (when leetcode--problem-id
    (leetcode--cleanup-problem-by-id leetcode--problem-id)))

(defun leetcode-quit ()
  "Close and delete leetcode related buffers and windows."
  (interactive)
  (leetcode--kill-buff-and-delete-window (get-buffer leetcode--buffer-name))
  (mapc (lambda (title)
          (leetcode--kill-buff-and-delete-window
           (get-buffer (leetcode--get-code-buffer-name title)))
          (let* ((slug-title (leetcode--slugify-title title))
                 (problem (leetcode--get-problem slug-title))
                 (problem-id (leetcode-problem-id problem)))
            (leetcode--kill-buff-and-delete-window (get-buffer (leetcode--detail-buffer-name problem-id)))
            (leetcode--kill-buff-and-delete-window (get-buffer (leetcode--result-buffer-name problem-id)))
            (leetcode--kill-buff-and-delete-window (get-buffer (leetcode--testcase-buffer-name problem-id)))))
        leetcode--problem-titles)
  (setq leetcode--problem-titles '()))

(defun leetcode--set-lang (snippets)
  "Set `leetcode--lang' based on langSlug in SNIPPETS."
  (let ((has-lang (lambda (lang)
                    (seq-find (lambda (s)
                                (equal (leetcode-snippet-lang-slug s) lang))
                              snippets))))
    (setq leetcode--lang
          (cond
           ;; if there is a mysql snippet, we use `leetcode-prefer-sql'.
           ((funcall has-lang leetcode-prefer-sql) leetcode-prefer-sql)
           ((funcall has-lang leetcode-prefer-language) leetcode-prefer-language)
           ;; the preferred language is not offered (e.g. shell/bash-only
           ;; problems); fall back to the first available snippet's language.
           ((car snippets) (leetcode-snippet-lang-slug (car snippets)))
           (t leetcode-prefer-language)))))

(defun leetcode--get-code-buffer-name (title)
  "Get code buffer name by TITLE and `leetcode-prefer-language'."
  (let* ((suffix (assoc-default
                  leetcode--lang
                  leetcode--lang-suffixes))
         (slug-title (leetcode--slugify-title title))
         (title-with-suffix (concat slug-title suffix)))
    (if leetcode-save-solutions
        (format "%s_%s" (leetcode--get-problem-id slug-title) title-with-suffix)
      title-with-suffix)))

(defun leetcode--get-code-buffer (buf-name)
  "Get code buffer by BUF-NAME."
  (if (not leetcode-save-solutions)
      (get-buffer-create buf-name)
    (unless (file-directory-p leetcode-directory)
      (make-directory leetcode-directory))
    (find-file-noselect
     (concat (file-name-as-directory leetcode-directory)
             buf-name))))

(defun leetcode--get-problem (slug-title)
  "Get problem from `leetcode--problems' by SLUG-TITLE."
  (seq-find (lambda (p)
              (equal slug-title (leetcode-problem-title-slug p)))
            (leetcode-problems-problems leetcode--problems)))

(defun leetcode--get-problem-by-id (id)
  "Get problem from `leetcode--problems' by ID."
  (seq-find (lambda (p)
              (equal id (leetcode-problem-id p)))
            (leetcode-problems-problems leetcode--problems)))

(defun leetcode--get-problem-id (slug-title)
  "Get problem id by SLUG-TITLE."
  (let ((problem (leetcode--get-problem slug-title)))
    (leetcode-problem-id problem)))

(defun leetcode--get-current-problem-id ()
  "Get id of the current problem, or nil outside the problems list."
  (if (derived-mode-p 'leetcode--problems-mode)
      (aref (tabulated-list-get-entry) 1)
    leetcode--problem-id))

(defun leetcode--start-coding (problem)
  "Create a buffer for coding PROBLEM.
The buffer will be not associated with any file.  It will choose
major mode by `leetcode-prefer-language'and `auto-mode-alist'."
  (let* ((title (leetcode-problem-title problem))
         (slug-title (leetcode-problem-title-slug problem))
         (problem-id (leetcode-problem-id problem))
         (snippets (leetcode-problem-snippets problem))
         (testcases (leetcode-problem-testcases problem))
         (testcase-buf-name (leetcode--testcase-buffer-name problem-id))
         (result-buf-name (leetcode--result-buffer-name problem-id)))

    ;; Clean up the previous problem's buffers by default.
    (leetcode--cleanup-other-problems problem-id)

    ;; Record windows opened for later cleanup.
    (unless (member title leetcode--problem-titles)
      (push title leetcode--problem-titles))

    (leetcode--solving-window-layout)

    ;; Set current programming language.
    (leetcode--set-lang snippets)

    ;; Setup code buffer
    (let* ((code-buf-name (leetcode--get-code-buffer-name title))
           (code-buf (leetcode--get-code-buffer code-buf-name))
           (suffix (assoc-default leetcode--lang leetcode--lang-suffixes)))
      (with-current-buffer code-buf
        (when (= (buffer-size code-buf) 0)
          (let* ((snippet (seq-find (lambda (s)
                                      (equal (leetcode-snippet-lang-slug s) leetcode--lang))
                                    snippets))
                 (template-code (leetcode-snippet-code snippet)))
            (let ((header (alist-get leetcode--lang leetcode-buffer-header nil nil #'equal)))
              (when header
                (insert header)))
            (leetcode--insert-code-start-marker)
            (insert template-code)
            (leetcode--insert-code-end-marker)
            (leetcode--replace-in-buffer "" "")))
        (funcall (assoc-default suffix auto-mode-alist #'string-match-p))
        (leetcode-solution-mode t)
        ;; After the major mode's `kill-all-local-variables'.
        (setq-local leetcode--problem-id problem-id))

      (display-buffer code-buf
                      '((display-buffer-reuse-window
                         leetcode--display-code)
                        (reusable-frames . visible))))

    ;; Setup testcase buffer
    (with-current-buffer (get-buffer-create testcase-buf-name)
      (erase-buffer)
      (insert (s-join "\n"
                      (seq-map-indexed (lambda (testcase index)
                                         (leetcode--format-testcase-with-number testcase index))
                                       testcases)))
      (set-window-buffer leetcode--testcase-window (current-buffer)))
    (with-current-buffer (get-buffer-create result-buf-name)
      (erase-buffer)
      (set-window-buffer leetcode--result-window (current-buffer)))))

(aio-defun leetcode-restore-layout ()
  "This command should be run in LeetCode code buffer.
It will restore the layout based on current buffer's problem id."
  (interactive)
  (let* ((problem-id leetcode--problem-id)
         (desc-buf (get-buffer (leetcode--detail-buffer-name problem-id)))
         (testcase-buf (get-buffer-create (leetcode--testcase-buffer-name problem-id)))
         (result-buf (get-buffer-create (leetcode--result-buffer-name problem-id))))
    (leetcode--solving-window-layout)
    (unless desc-buf
      (aio-await (leetcode-show-problem problem-id)))
    (with-current-buffer result-buf
      (erase-buffer)
      (insert "Waiting for result..."))
    (display-buffer desc-buf
                    '((display-buffer-reuse-window
                       leetcode--display-detail)
                      (reusable-frames . visible)))
    (display-buffer testcase-buf
                    '((display-buffer-reuse-window
                       leetcode--display-testcase)
                      (reusable-frames . visible)))
    (display-buffer result-buf
                    '((display-buffer-reuse-window
                       leetcode--display-result)
                      (reusable-frames . visible)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Problems Mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar leetcode--problems-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (suppress-keymap map)
      (define-key map (kbd "RET") #'leetcode-show-current-problem)
      (define-key map (kbd "TAB") #'leetcode-view-current-problem)
      (define-key map "o" #'leetcode-show-current-problem)
      (define-key map "O" #'leetcode-show-problem)
      (define-key map "v" #'leetcode-view-current-problem)
      (define-key map "V" #'leetcode-view-problem)
      (define-key map "b" #'leetcode-show-current-problem-in-browser)
      (define-key map "B" #'leetcode-show-problem-in-browser)
      (define-key map "c" #'leetcode-solve-current-problem)
      (define-key map "C" #'leetcode-solve-problem)
      (define-key map "s" #'leetcode-set-filter-regex)
      (define-key map "L" #'leetcode-set-prefer-language)
      (define-key map "t" #'leetcode-set-filter-tag)
      (define-key map "T" #'leetcode-toggle-tag-display)
      (define-key map "P" #'leetcode-toggle-paid-display)
      (define-key map "d" #'leetcode-set-filter-difficulty)
      (define-key map "g" #'leetcode-refresh)
      (define-key map "G" #'leetcode-refresh-fetch)
      (define-key map "R" #'leetcode-random)
      (define-key map "r" #'leetcode-reset-filter-and-refresh)
      (define-key map "q" #'quit-window)))
  "Keymap for `leetcode--problems-mode'.")

(define-derived-mode leetcode--problems-mode
  tabulated-list-mode "LC Problems"
  "Major mode for browsing a list of problems."
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook #'leetcode-refresh nil t)
  :group 'leetcode
  :keymap leetcode--problems-mode-map)

(defun leetcode--set-evil-local-map (map)
  "Set `evil-normal-state-local-map' to MAP."
  (when (featurep 'evil)
    (define-key map "h" nil)
    (define-key map "v" nil)
    (define-key map "V" nil)
    (define-key map "b" nil)
    (define-key map "B" nil)
    (define-key map "g" nil)
    (define-key map "G" nil)
    (define-key map "z" #'leetcode-refresh)
    (define-key map "Z" #'leetcode-refresh-fetch)
    (setq evil-normal-state-local-map map)))

(add-hook 'leetcode--problems-mode-hook #'hl-line-mode)
(add-hook 'leetcode--problems-mode-hook
          (lambda () (leetcode--set-evil-local-map leetcode--problems-mode-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Detail Mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar leetcode--problem-detail-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (suppress-keymap map)
      (define-key map (kbd "C-c C-r") #'leetcode-random)
      (define-key map "q" #'quit-window)))
  "Keymap for `leetcode--problem-detail-mode'.")

(define-derived-mode leetcode--problem-detail-mode
  special-mode "LC Detail"
  "Major mode for display problem detail."
  :group 'leetcode
  :keymap leetcode--problem-detail-mode-map)

(add-hook 'leetcode--problem-detail-mode-hook
          (lambda () (leetcode--set-evil-local-map leetcode--problem-detail-mode-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Solution Mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar leetcode-solution-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map (kbd "C-c C-t") #'leetcode-try)
      (define-key map (kbd "C-c C-s") #'leetcode-submit)
      (define-key map (kbd "C-c C-r") #'leetcode-random)
      (define-key map (kbd "C-c C-l") #'leetcode-restore-layout)
      (define-key map (kbd "C-c C-o") #'leetcode-show-problem-in-browser)))
  "Keymap for `leetcode-solution-mode'.")

(define-minor-mode leetcode-solution-mode
  "Minor mode to provide shortcut and hooks."
  :require 'leetcode
  :lighter " LC-Solution"
  :group 'leetcode
  :keymap leetcode-solution-mode-map
  (if leetcode-solution-mode
      (add-hook 'kill-buffer-hook #'leetcode--cleanup-on-kill nil t)
    (remove-hook 'kill-buffer-hook #'leetcode--cleanup-on-kill t)))

(provide 'leetcode)
;;; leetcode.el ends here
