(defvar slime-js-remote-history nil
  "History list for JS remote names.")

(defun slime-js-read-remote-index (&optional prompt)
  (let* ((completion-ignore-case nil)
         (remotes (slime-eval '(js:list-remotes)))
         (remote-names
          (loop for remote in remotes
                collect (replace-regexp-in-string
                         "^:" ""
                         (concat (symbol-name (second remote))
                                 "/" (third remote)))))
         (prompt (or prompt "Remote: "))
         (p (or (position
                 (completing-read prompt (slime-bogus-completion-alist remote-names)
                                  nil nil nil
                                  'slime-remote-history nil)
                 remote-names :test #'equal)
                (error "bad remote name"))))
    (first (elt remotes p))))

(defun slime-js-select-remote (n)
  "Select JS remote by number"
  (interactive (list (slime-js-read-remote-index)))
  (slime-eval-async `(js:select-remote ,n nil)))

(defun slime-js-sticky-select-remote (n)
  "Select JS remote by number in sticky mode"
  (interactive (list (slime-js-read-remote-index)))
  (slime-eval-async `(js:select-remote ,n t)))

(defslime-repl-shortcut slime-repl-js-select-remote ("select-remote")
  (:handler 'slime-js-select-remote)
  (:one-liner "Select JS remote."))

(defslime-repl-shortcut slime-repl-js-sticky-select-remote ("sticky-select-remote")
  (:handler 'slime-js-sticky-select-remote)
  (:one-liner "Select JS remote in sticky mode."))

;; FIXME: should add an rpc command for browser-only eval

(defun slime-js-eval (str &optional cont)
  (slime-eval-async `(swank:interactive-eval ,str) cont))

(defun slime-js-reload ()
  (interactive)
  (slime-js-eval "SwankJS.reload()"
    #'(lambda (v)
        (message "Reloading the page"))))

(defun slime-js-refresh-css ()
  (interactive)
  (slime-js-eval "SwankJS.refreshCSS()"
    #'(lambda (v)
        (message "Refreshing CSS"))))

(defun slime-js-start-of-toplevel-form ()
  (interactive)
  (when js2-mode-buffer-dirty-p
    (js2-mode-wait-for-parse #'slime-js-start-of-toplevel-form))
  (js2-forward-sws)
  (if (= (point) (point-max))
      (js2-mode-forward-sexp -1)
    (let ((node (js2-node-at-point)))
      (when (or (null node)
                (js2-ast-root-p node))
        (error "cannot locate any toplevel form"))
      (while (and (js2-node-parent node)
                  (not (js2-ast-root-p (js2-node-parent node))))
        (setf node (js2-node-parent node)))
      (goto-char (js2-node-abs-pos node))
      (js2-forward-sws)))
  (point))

(defun slime-js-end-of-toplevel-form ()
  (interactive)
  (js2-forward-sws)
  (let ((node (js2-node-at-point)))
    (unless (or (null node) (js2-ast-root-p node))
      (while (and (js2-node-parent node)
                  (not (js2-ast-root-p (js2-node-parent node))))
        (setf node (js2-node-parent node)))
      (goto-char (js2-node-abs-end node)))
    (point)))

;; FIXME: this breaks if // comment directly precedes the function
(defun slime-js-send-defun ()
  (interactive)
  (save-excursion
    (lexical-let ((start (slime-js-start-of-toplevel-form))
                  (end (slime-js-end-of-toplevel-form)))
      ;; FIXME: use slime-eval-region
      (slime-js-eval
       (buffer-substring-no-properties start end)
       #'(lambda (v)
           (save-excursion
             (goto-char start)
             (let ((sent-func "<...>"))
               (when (looking-at "[ \t]*\\([^ \t\n{}][^\n{}]*\\)")
                 (setf sent-func (match-string 1)))
               (message "Sent: %s" sent-func))))))))

(define-minor-mode slime-js-minor-mode
  "Toggle slime-js minor mode
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode."
  nil
  " slime-js"
  '(("\C-\M-x"  . slime-js-send-defun)
    ("\C-c\C-c" . slime-js-send-defun)
    ;; ("\C-c\C-r" . slime-eval-region)
    ("\C-c\C-z" . slime-switch-to-output-buffer)))

;; TBD: dabbrev in repl:
;; DABBREV--GOTO-START-OF-ABBREV function skips over REPL prompt
;; because it has property 'intangible' and (forward-char -1) doesn't do
;; what is expected at the propmpt edge. Must redefine this function
;; or define and advice for it.
;; TBD: lost continuations (pipelined request ...) - maybe when closing page
(provide 'slime-js)
