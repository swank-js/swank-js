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

(defslime-repl-shortcut slime-repl-js-select-remote ("select-remote")
  (:handler 'slime-js-select-remote)
  (:one-liner "Select JS remote."))

;; TBD: dabbrev in repl?
(provide 'slime-js)
