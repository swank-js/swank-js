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

;; TBD: sticky-select-remote (the 'sticky' effect is cancelled by select-remote)

;; TBD: dabbrev in repl:
;; DABBREV--GOTO-START-OF-ABBREV function skips over REPL prompt
;; because it has property 'intangible' and (forward-char -1) doesn't do
;; what is expected at the propmpt edge. Must redefine this function
;; or define and advice for it.

(provide 'slime-js)
