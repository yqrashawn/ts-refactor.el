;;; ts-refactor.el --- js2-refactor for ts -*- lexical-binding: t; -*-

;; Copyright © 2025-present yqrashawn

;; Author: yqrashawn <namy.19@gmail.com>
;; Keywords: typescript tsx languages refactoring

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a collection of small refactoring functions to further the idea of a
;; TypeScript IDE in Emacs that refactored from js2-refactor.

;; doomemacs setup example
;;
;; (use-package! ts-refactor
;;   :hook (typescript-ts-mode tsx-ts-mode)
;;   :config
;;   (when (modulep! :editor evil +everywhere)
;;     (add-hook 'ts-refactor-mode-hook #'evil-normalize-keymaps)
;;     (let ((ts-refactor-mode-map (evil-get-auxiliary-keymap ts-refactor-mode-map 'normal t t)))
;;       (ts-refactor-add-keybindings-with-prefix (format "%s r" doom-localleader-key)))))

;;; Code:
(require 'dash)
(require 'typescript-ts-mode)

(defvar ts-refactor-mode-map
  (make-sparse-keymap)
  "Keymap for ts-refactor.")

(defvar ts-refactor-keybinding-prefix
  nil
  "Store keybinding prefix used by ts-refactor.")

;;;###autoload
(define-minor-mode ts-refactor-mode
  "Minor mode providing JavaScript refactorings."
  :lighter " tsr"
  :keymap ts-refactor-mode-map)

;;; Settings:
(defgroup ts-refactor nil
  "Minor mode providing TypeScript refactorings."
  :group 'tools)

;;; Impls:
;;;; log
;;;###autoload
(defun ts-refactor-log-this (arg)
  "Log the node at point, adding a 'console.log()' statement.
With a prefix argument ARG, use JSON pretty-printing for logging."
  (interactive "P")
  (if (not (treesit-parser-p (car (treesit-parser-list))))
      (user-error "Buffer has no tree-sitter parser")
    (let* ((log-info (ts-refactor--figure-out-what-to-log-where))
           (stmt (car log-info))
           (pos (cdr log-info)))
      (save-excursion
        (goto-char pos)
        (when (looking-at "[;{]")
          (forward-char 1))
        (newline-and-indent)
        (if arg
            (progn (insert "console.log(\"" stmt " = \");")
                   (newline-and-indent)
                   (insert "console.dir(" stmt ", { depth:null, colors: true });"))
          (insert "console.log(\"" stmt " = \", " stmt ");"))))))

;;;###autoload
(defun ts-refactor-debug-this ()
  "Debug the node at point, adding a 'debug()' statement."
  (interactive)
  (if (not (treesit-parser-p (car (treesit-parser-list))))
      (user-error "Buffer has no tree-sitter parser")
    (let* ((log-info (ts-refactor--figure-out-what-to-log-where))
           (stmt (car log-info))
           (pos (cdr log-info)))
      (save-excursion
        (goto-char pos)
        (when (looking-at "[;{]")
          (forward-char 1))
        (newline-and-indent)
        (insert "console.debug(\"" stmt " = \", " stmt ");")))))

(defun ts-refactor--figure-out-what-to-log-where ()
  "Return a dotted pair containing the statement to log and the
position where the log should be inserted."
  (if (use-region-p)
      ;; If region is active, log the region content
      (cons (buffer-substring (region-beginning) (region-end))
            (ts-refactor--find-suitable-log-position))

    (let* ((node (treesit-node-at (point)))
           (node-type (treesit-node-type node))
           (parent (treesit-node-parent node)))

      (cond
       ;; Variable identifier
       ((string= node-type "identifier")
        (cons (treesit-node-text node)
              (ts-refactor--find-suitable-log-position)))

       ;; Property access (obj.prop)
       ((and parent (string= (treesit-node-type parent) "member_expression"))
        (cons (treesit-node-text parent)
              (ts-refactor--find-suitable-log-position)))

       ;; Function parameters
       ((and parent (string= (treesit-node-type parent)
                             (if (string= (treesit-language-at (point)) "tsx")
                                 "function_expression" "function")))
        (let ((func-name (treesit-node-text
                          (treesit-node-child-by-field-name parent "name"))))
          (cons func-name
                (treesit-node-start
                 (treesit-node-child-by-field-name parent "body")))))

       ;; For object properties/methods
       ((and parent (member (treesit-node-type parent)
                            '("public_field_definition" "method_definition")))
        (let ((prop-name (treesit-node-text
                          (treesit-node-child-by-field-name parent "name"))))
          (cons prop-name
                (ts-refactor--find-suitable-log-position))))

       ;; Default: try to use the node text
       (t
        (cons (or (treesit-node-text node t) "unknown")
              (ts-refactor--find-suitable-log-position)))))))

(defun ts-refactor--find-suitable-log-position ()
  "Find a suitable position to insert the log statement."
  (let* ((node (treesit-node-at (point)))
         (stmt-node (ts-refactor--get-parent-statement node)))
    (if stmt-node
        (treesit-node-end stmt-node)
      ;; Fallback to end of line if no statement node found
      (save-excursion (end-of-line) (point)))))

(defun ts-refactor--get-parent-statement (node)
  "Get the parent statement node of NODE."
  (let ((current node)
        (statement-types '("expression_statement"
                           "variable_declaration"
                           "lexical_declaration"
                           "if_statement"
                           "for_statement"
                           "while_statement"
                           "return_statement"
                           "block")))
    (while (and current
                (not (member (treesit-node-type current) statement-types)))
      (setq current (treesit-node-parent current)))
    current))

;;;; move
;;;###autoload
(defun ts-refactor-move-line-down ()
  "Move the current line down one line.
Make sure commas are placed correctly when moving a line up or
down in an object or array literal."
  (interactive)
  (if (and (ts-refactor--current-line-is-a-list-item)
           (ts-refactor--next-line-is-a-list-item))
      (ts-refactor--move-line-down-as-list-item)
    (ts-refactor--move-line-down))
  (indent-according-to-mode))

;;;###autoload
(defun ts-refactor-move-line-up ()
  "Move the current line up one line.
Make sure commas are placed correctly when moving a line up or
down in an object or array literal."
  (interactive)
  (if (and (ts-refactor--current-line-is-a-list-item)
           (ts-refactor--previous-line-is-a-list-item))
      (ts-refactor--move-line-up-as-list-item)
    (ts-refactor--move-line-up))
  (indent-according-to-mode))

(defun ts-refactor--move-line-down ()
  "Move the current line down one line."
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun ts-refactor--move-line-up ()
  "Move the current line up one line."
  (let ((col (current-column)))
    (transpose-lines 1)
    (forward-line -2)
    (move-to-column col)))

(defun ts-refactor--current-line-is-a-list-item ()
  "Return whether the current line is part of an array or object literal."
  (save-excursion
    (back-to-indentation)
    (let* ((node (treesit-node-at (point)))
           (parent (treesit-node-parent node)))
      (and parent
           (or (string= (treesit-node-type parent) "array")
               (string= (treesit-node-type parent) "object"))))))

(defun ts-refactor--next-line-is-a-list-item ()
  "Return whether the next line is part of an array or object literal."
  (save-excursion
    (forward-line)
    (ts-refactor--current-line-is-a-list-item)))

(defun ts-refactor--previous-line-is-a-list-item ()
  "Return whether the previous line is part of an array or object literal."
  (save-excursion
    (forward-line -1)
    (ts-refactor--current-line-is-a-list-item)))

(defun ts-refactor--current-line-has-comma ()
  "Return whether the current line ends with a comma."
  (save-excursion
    (end-of-line)
    (skip-chars-backward " \t")
    (eq (char-before) ?,)))

(defun ts-refactor--previous-line-has-comma ()
  "Return whether the previous line ends with a comma."
  (save-excursion
    (forward-line -1)
    (ts-refactor--current-line-has-comma)))

(defun ts-refactor--move-line-down-as-list-item ()
  "Move the current line containing a list literal down one line, and also move the comma."
  (ts-refactor--move-line-down)
  (when (not (ts-refactor--previous-line-has-comma))
    (save-excursion
      (end-of-line)
      (delete-char -1)                  ; delete comma from current line
      (forward-line -1)
      (end-of-line)
      (insert ","))))

(defun ts-refactor--move-line-up-as-list-item ()
  "Move the current line containing a list literal up one line, and also move the comma."
  (ts-refactor--move-line-up)
  (when (not (ts-refactor--current-line-has-comma))
    (save-excursion
      (end-of-line)
      (insert ",")
      (forward-line)
      (end-of-line)
      (skip-chars-backward " \t")
      (when (eq (char-before) ?,)
        (delete-char -1)))))

;;;; string
;;;###autoload
(defun ts-refactor-string-to-template ()
  "Convert the string at point into a template string."
  (interactive)
  (if (not (treesit-parser-p (car (treesit-parser-list))))
      (user-error "Buffer has no tree-sitter parser")
    (let* ((node (treesit-node-at (point)))
           (string-node (ts-refactor--get-string-node node)))
      (if (not string-node)
          (user-error "Not on a string")
        ;; Check if it's already a template string
        (let* ((start (treesit-node-start string-node))
               (end (treesit-node-end string-node))
               (string-type (treesit-node-type string-node)))
          (when (string= string-type "string")
            (save-excursion
              ;; Check the first character to determine the delimiter
              (goto-char start)
              (when (memq (char-after) '(?\" ?'))
                ;; Replace the string delimiters with backticks
                (let ((original-text (treesit-node-text string-node))
                      (delimiter (char-to-string (char-after))))
                  ;; Replace the end delimiter first
                  (goto-char (1- end))
                  (delete-char 1)
                  (insert "`")
                  ;; Replace the start delimiter
                  (goto-char start)
                  (delete-char 1)
                  (insert "`")
                  ;; Escape any existing backticks in the string
                  (let ((content-start (1+ start))
                        (content-end (1- end)))
                    (save-restriction
                      (narrow-to-region content-start content-end)
                      (goto-char (point-min))
                      (while (search-forward "`" nil t)
                        (replace-match "\\`")))))))))))))

(defun ts-refactor--get-string-node (node)
  "Get the string node at or containing NODE."
  (if (and node (string= (treesit-node-type node) "string"))
      node
    (let ((parent node))
      (while (and parent
                  (not (string= (treesit-node-type parent) "string")))
        (setq parent (treesit-node-parent parent)))
      parent)))

;;;###autoload
(defun ts-refactor-split-string ()
  "Split the string node at point.  If the string is already split, join it instead."
  (interactive)
  (if (not (treesit-parser-p (car (treesit-parser-list))))
      (user-error "Buffer has no tree-sitter parser")
    (let* ((node (treesit-node-at (point)))
           (string-node (ts-refactor--get-string-node node)))
      (if (not string-node)
          (user-error "Not inside a string")
        (let* ((delimiter (ts-refactor--string-delimiter string-node)))
          ;; Check if we're looking at a string concatenation
          (if (looking-at (regexp-quote (format "%s + %s" delimiter delimiter)))
              ;; Join string
              (delete-char (length (format "%s + %s" delimiter delimiter)))
            ;; Split string
            (insert (format "%s + %s" delimiter delimiter))))))))

(defun ts-refactor--string-delimiter (node)
  "Return the delimiter character of the string NODE.
It can be a single or double quote."
  (save-excursion
    (goto-char (treesit-node-start node))
    (char-to-string (char-after))))

;;;; functions
;;;###autoload
(defun ts-refactor-toggle-function-async ()
  "Toggle the innermost function from sync to async."
  (interactive)
  (save-excursion
    (ts-refactor--find-closest-function)
    (if (looking-at "async[[:space:]\n]+")
        (delete-region (match-beginning 0) (match-end 0))
      (insert "async "))))

(defun ts-refactor--find-closest-function ()
  (when (not (ts-refactor--function-start-p))
    (let* ((fn (ts-refactor--closest #'ts-refactor--is-function-p)))
      (goto-char (treesit-node-start fn)))))

(defun ts-refactor--function-start-p ()
  (let* ((fn (ts-refactor--closest #'ts-refactor--is-function-p)))
    (and fn
         (= (treesit-node-start fn) (point)))))

(defun ts-refactor--closest (predicate)
  "Find closest node satisfying PREDICATE."
  (let ((node (treesit-node-at (point)))
        (result nil))
    (while (and node (not (funcall predicate node)))
      (setq node (treesit-node-parent node))
      (when (funcall predicate node)
        (setq result node)))
    result))

(defun ts-refactor--is-function-p (node)
  "Check if NODE is any kind of function."
  (and node
       (member (treesit-node-type node)
               '("function" "function_declaration" "arrow_function"
                 "function_expression" "method_definition"))))

;;;###autoload
(defun ts-refactor-toggle-arrow-function ()
  "Toggle between arrow function and function expression."
  (interactive)
  (let* ((node (treesit-node-at (point)))
         (function-node (ts-refactor--closest
                         (lambda (n)
                           (member (treesit-node-type n)
                                   '("arrow_function" "function_expression"))))))

    (unless function-node
      (user-error "No arrow function or function expression at point"))

    (save-excursion
      (if (string= (treesit-node-type function-node) "arrow_function")
          (ts-refactor--transform-arrow-to-function function-node)
        (ts-refactor--transform-function-to-arrow function-node)))))

(defun ts-refactor--transform-arrow-to-function (arrow-node)
  "Transform ARROW-NODE from an arrow function to a function expression."
  (let* ((beg (treesit-node-start arrow-node))
         (end (treesit-node-end arrow-node))
         (arrow-text (treesit-node-text arrow-node))
         (params-node (treesit-node-child-by-field-name arrow-node "parameters"))
         (body-node (treesit-node-child-by-field-name arrow-node "body"))
         (is-expression-body (not (string= (treesit-node-type body-node) "statement_block")))
         (params-text (treesit-node-text params-node))
         (body-text (treesit-node-text body-node)))

    (delete-region beg end)
    (goto-char beg)

    (insert "function")
    (insert params-text)

    ;; If the arrow function had an expression body (no curly braces),
    ;; add a return statement
    (when is-expression-body
      (insert " return ")
      (insert body-text)
      (insert "; "))

    (unless is-expression-body
      (insert body-text))

    (unless (string-match-p "}[[:space:]]*$" body-text)
      (insert "}"))))

(defun ts-refactor--transform-function-to-arrow (function-node)
  "Transform FUNCTION-NODE from a function expression to an arrow function."
  (let* ((beg (treesit-node-start function-node))
         (end (treesit-node-end function-node))
         (params-node (treesit-node-child-by-field-name function-node "parameters"))
         (body-node (treesit-node-child-by-field-name function-node "body"))
         (params-text (treesit-node-text params-node))
         (single-param (and
                        (string-match-p "^\\s-*(\\([^,)]+\\))\\s-*$" params-text)
                        (not (string-match-p "[{}]" params-text))))
         (body-content (buffer-substring
                        (+ (treesit-node-start body-node) 1)
                        (- (treesit-node-end body-node) 1)))
         (single-return (ts-refactor--has-single-return-p body-node)))

    ;; Delete the existing function
    (delete-region beg end)
    (goto-char beg)

    ;; Insert the arrow function
    ;; Remove parentheses for single parameter with no default value
    (if single-param
        (insert (replace-regexp-in-string "[()]" "" params-text))
      (insert params-text))

    (insert " => ")

    ;; Handle the body transformation
    (if single-return
        ;; Get the return expression and omit the curly braces
        (let ((return-expr (ts-refactor--get-return-expression body-node)))
          (insert return-expr))
      ;; Keep the full body with curly braces
      (insert "{" body-content "}"))))

(defun ts-refactor--has-single-return-p (body-node)
  "Check if BODY-NODE has a single return statement and nothing else."
  (let* ((statements '())
         (has-single-return nil))
    (treesit-search-subtree body-node "statement")
    (when (= (length statements) 1)
      (setq has-single-return
            (string= (treesit-node-type (car statements)) "return_statement")))
    has-single-return))

(defun ts-refactor--get-return-expression (body-node)
  "Extract the expression from the return statement in BODY-NODE."
  (let* ((return-node (car (treesit-search-subtree
                            body-node "return_statement"
                            (lambda (n) t))))
         (expr-node (treesit-node-child-by-field-name return-node "argument")))
    (when expr-node
      (string-trim (treesit-node-text expr-node)))))

;;; Keybindings:
(defun ts-refactor--add-keybindings (key-fn)
  "Add ts-refactor refactoring keybindings to `typescript-ts-mode-map' using KEY-FN to create each keybinding."
  (define-key ts-refactor-mode-map (funcall key-fn "ss") #'ts-refactor-split-string)
  (define-key ts-refactor-mode-map (funcall key-fn "st") #'ts-refactor-string-to-template)
  (define-key ts-refactor-mode-map (funcall key-fn "ta") #'ts-refactor-toggle-arrow-function-and-expression)
  (define-key ts-refactor-mode-map (funcall key-fn "ts") #'ts-refactor-toggle-function-async)
  (define-key ts-refactor-mode-map (funcall key-fn "lt") #'ts-refactor-log-this)
  (define-key ts-refactor-mode-map (funcall key-fn "dt") #'ts-refactor-debug-this)
  (define-key ts-refactor-mode-map (kbd "<C-S-down>") #'ts-refactor-move-line-down)
  (define-key ts-refactor-mode-map (kbd "<C-S-up>") #'ts-refactor-move-line-up))

(defun ts-refactor--key-pairs-with-prefix (prefix keys)
  (read-kbd-macro (concat prefix " " keys)))

;;;###autoload
(defun ts-refactor-add-keybindings-with-prefix (prefix)
  "Add ts-refactor keybindings using the prefix PREFIX."
  (setq ts-refactor-keybinding-prefix (read-kbd-macro prefix))
  (ts-refactor--add-keybindings (-partial #'ts-refactor--key-pairs-with-prefix prefix)))

(provide 'ts-refactor)
;;; ts-refactor.el ends here
