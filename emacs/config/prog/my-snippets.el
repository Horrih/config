;;; yasnippet : Snippets insertion
(use-package yasnippet
  :demand
  :config
  (yas-global-mode)
  (diminish 'yas-minor-mode))

;;; yasnippet - python docstring
;;;; Main functions
(defun my/yas-python-docstring()
  "Generate the appropriate doxygen docstring according to location (file, class, function)."
  (cond ((eq (line-number-at-pos) 1) (my/yas-python-file-docstring))
        ((my/yas-python-prev-line-is-class) (my/yas-python-class-docstring))
        (t (my/yas-python-function-docstring))))

(defun my/yas-python-class-docstring()
  "Build the python docstring for a class."
  "\"\"\"${1:Class summary here}

${2:Class details}
\"\"\"
$0")

(defun my/yas-python-file-docstring()
  "Build the python docstring for a file."
  "\"\"\"$1\n\n$2\"\"\"")

(defun my/yas-python-function-docstring()
  "Build the python docstring for a function, google style."
  (let* ((count 1)
         (args (my/yas-python-arg-names))
         (return-type (my/yas-python-fun-ret)))
    (format
     "\"\"\"${1:Function %s}

%s
%s\"\"\"$0"
     (my/yas-python-fun-name)

     ;; Display arguments if not nil
     (if args
         (concat "Args\n" (mapconcat
           (lambda(arg) (format "    %s: ${%d:Parameter %s}" arg (cl-incf count) arg))
           args "\n"))
       "")

     ;; Display ret type if not nil
      (if (and return-type (not (string= return-type "None")))
          (format "
Returns:
    ${%d:Returned value (%s)}\n" (cl-incf count) return-type)
        ""))))

;;;; Helper functions
(defun my/yas-python-fun-name()
  "Extracts the function name."
  (save-excursion
    (search-backward "def ")
    (search-forward "def ")
    (let ((begin (point)))
      (search-forward "(")
      (backward-char)
      (buffer-substring-no-properties begin (point)))))

(defun my/yas-python-arg-names()
  "Extracts a list of the argument names."
  (save-excursion
    (search-backward "(")
    (forward-char)
    (let ((beg (point)))
      (search-forward ")")
      (backward-char)
      (my/yas-python-extract-args (buffer-substring-no-properties beg (point))))))

(defun my/yas-python-extract-args(args-str)
  "Extract the function argument names from a string containing all arguments."
  (let* (;; Remove default values
         (args-no-default-value (replace-regexp-in-string "\s?=[^,]*" "" args-str))

         ;; Remove brackets in type hints (because they can contain commas)
         (args-tmp args-no-default-value)
         (prev-length 0)
         (args-no-brackets
          (progn
            (while (/= prev-length (length args-tmp))
              (setq prev-length (length args-tmp))
              (setq args-tmp (replace-regexp-in-string "\\[[^\\[]*?\\]" "" args-tmp)))
              args-tmp))

         ;; Remove the remaining type annotations
         (args-no-type (replace-regexp-in-string ":[^,)]*" "" args-no-brackets))

         ;; Transform into list
         (args-list (mapcar #'string-trim (split-string args-no-type "," t)))

         ;; Remove the self argument for methods
         (args-no-self (seq-filter (lambda(str) (not (string= str "self"))) args-list)))
    args-no-self))

(defun my/yas-python-fun-ret()
  "Returns the return type, i.e the string between next line and the last whitespace before '('"
  (save-excursion
    (search-backward "def ")
    (search-forward ")")
    (let* ((beg (point))
           (type-hint
            (progn
              (search-forward ":" nil t)
              (backward-char)
              (buffer-substring-no-properties beg (point))))
           (ret-type-no-arrow (replace-regexp-in-string "\s*->\s*" "" type-hint))
           (ret-type (if (string-empty-p type-hint) "None" ret-type-no-arrow)))
      (string-trim ret-type))))

(defun my/yas-python-prev-line-is-class()
  "Return t if next inline is a class or struct definition."
  (save-excursion
    (forward-line -1)
    (beginning-of-line)
    (looking-at "\\s-*class ")))



;;; yasnippet - c++ docstring
;;;; Main docstring functions
(defun my/yas-c++-doxygen-docstring()
  "Generate the appropriate doxygen docstring according to location (file, class, function)."
  (cond ((eq (line-number-at-pos) 1) (my/yas-c++-doxygen-file-docstring))
        ((my/yas-c++-next-line-is-class) (my/yas-c++-doxygen-class-docstring))
        (t (my/yas-c++-doxygen-function-docstring))))

(defun my/yas-c++-doxygen-class-docstring()
  "Return the docstring to use for a class or struct."
  "/** @brief $1
*
* $2
*/")

(defun my/yas-c++-doxygen-file-docstring()
  "Return the docstring to use for a file."
  (format "/** @file %s
*
* $1
*/
" (buffer-name)))

(defun my/yas-c++-doxygen-function-docstring()
  (let* ((count 1)
         (args (my/yas-c++-arg-names))
         (return-type (my/yas-c++-fun-ret)))
     (format "/** @brief ${1:Function %s}
*
%s
%s*/"
      (my/yas-c++-fun-name)
      (mapconcat
       (lambda(arg) (format "* @param %s ${%d:Parameter %s}" arg (cl-incf count) arg))
       args "\n")
      (if (and return-type (not (string-equal "void" return-type)))
          (format "* @return ${%d:Returned value}\n" (cl-incf count))
        ""))))

;;;; Helper methods
(defun my/yas-c++-fun-name()
  "Extracts the function name."
  (save-excursion
    (search-forward "(")
    (backward-char)
    (let ((end (point)))
      (re-search-backward "[ \n]" nil t)
      (forward-char)
      (buffer-substring-no-properties (point) end))))

(defun my/yas-c++-fun-ret()
  "Returns the return type, i.e the string between next line and the last whitespace before '('"
  (save-excursion
    (let ((beg (point)))
      (search-forward "(")
      (re-search-backward "[ \n]" nil t)
      (string-trim (buffer-substring-no-properties beg (point))))))

(defun my/yas-c++-next-line-is-class()
  "Return t if next inline is a class or struct definition."
  (save-excursion
    (forward-line 1)
    (beginning-of-line)
    (or (looking-at "\\s-*class")
        (looking-at "\\s-*struct"))))

(defun my/yas-c++-arg-names()
  "Extracts a list of the argument names."
  (save-excursion
    (search-forward "(")
    (let ((beg (point)))
      (search-forward ")")
      (backward-char)
      (my/yas-c++-extract-args (buffer-substring-no-properties beg (point))))))

(defun my/yas-c++-extract-args(args-str)
  "Extract the function argument names from a string containing all arguments."
  (let* (;; Remove optional arguments
         (args-no-default-value (replace-regexp-in-string "\s?=[^,]*" "" args-str))

         ;; Remove types, start by removing everything inside template brackets
         (args-tmp args-no-default-value)
         (prev-length 0)
         (args-no-<>
          (progn
            (while (/= prev-length (length args-tmp))
              (setq prev-length (length args-tmp))
              (setq args-tmp (replace-regexp-in-string "<[^<]*?>" "" args-tmp)))
            args-tmp))

         ;; Remove the base type remaining, e.g int/double/const std::vector&
         (args-no-type (replace-regexp-in-string "\\([^(,]* \\)\\([^ ]*\\),?" "" args-no-<> t nil 1))
         ;; Transform the argument names string into a list of string
         (args-list (mapcar #'string-trim (split-string args-no-type "," t))))
    args-list))
