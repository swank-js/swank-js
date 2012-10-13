;;; slime-js.el --- Slime extension for swank-js.

;; Author: Ivan Shvedunov
;; Adapted-by: Irakli Gozalishvili
;; URL: http://github.com/gozala/slime-js
;; Version: 0.0.1
;; Keywords: js, languages, lisp, slime
;; Package-Requires: ((slime-repl "20100404") (slime "20100404"))

;;; Licence: 
;; Copyright (c) 2010 Ivan Shvedunov. All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; * Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;;
;; * Redistributions in binary form must reproduce the above
;; copyright notice, this list of conditions and the following
;; disclaimer in the documentation and/or other materials
;; provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(defgroup slime-js nil "Slime Extension for Swank.js"
  :group 'slime-js)

(defcustom  slime-js-swank-command "npm"
  "Command for running the swank-js server from node.js"
  :type 'string
  :group 'slime-js)

(defcustom slime-js-swank-args '("run" "swank")
  "Command arguments for running the swank-js server from node.js.
Note that file paths need to be complete file paths, i.e. ~ to /home/you or /Uesrs/you.
If you are using npm, then you probably want this to have 2 values: \"run\" \"swank\".
If you want swank-js to run on a differnet port, add it as the third element to this list."
  :type '(repeat (string :tag "Arg"))
  :group 'slime-js)

(define-slime-contrib slime-js
  "Emacs-side support for Swank-JS."
  (:authors "Ivan Shvedunov")
  (:license "X11-style")
  (:slime-dependencies slime-repl)
  (:on-load
   (add-hook 'slime-event-hooks 'slime-js-event-hook-function))
  (:on-unload
   (remove-hook 'slime-event-hooks 'slime-js-event-hook-function)))

(defun slime-js-repl-update-package ()
  (let ((name (slime-current-package)))
    (with-current-buffer (slime-output-buffer)
      (let ((previouse-point (- (point) slime-repl-input-start-mark)))
        (setf (slime-lisp-package) name
              (slime-lisp-package-prompt-string) name
              slime-buffer-package name)
        (slime-repl-insert-prompt)
        (when (plusp previouse-point)
          (goto-char (+ previouse-point slime-repl-input-start-mark)))))))

(defvar slime-js-swank-buffer 'nil)

; Just the bare-simplest thing that can be done for now.
(defun slime-js-run-swank ()
  "Runs the swank side of the equation."
  (interactive)
  (setq slime-js-swank-buffer (apply #'make-comint "swank-js"  (expand-file-name slime-js-swank-command) nil slime-js-swank-args)))

(defun slime-js-event-hook-function (event)
  (when (equal "JS" (slime-lisp-implementation-type))
    (destructure-case event
      ((:new-package package prompt)
       (let ((buffer (slime-connection-output-buffer)))
         (setf (slime-lisp-package) package)
         (setf (slime-lisp-package-prompt-string) prompt)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (setq slime-buffer-package package)
             (slime-js-repl-update-package)
             (save-excursion
               (goto-char (marker-position slime-repl-prompt-start-mark))
               (slime-mark-output-start))))
         t))
      (t nil))))

(defvar slime-js-remote-history nil
  "History list for JS remote names.")

(defun slime-js-browse-test ()
  "Interactive funciton to open the test page locally"
  (interactive)
  (browse-url "http://localhost:8009/client/test.html"))

(defun slime-js-read-remote-index (&optional prompt)
  (let* ((completion-ignore-case nil)
         (remotes (slime-eval '(js:list-remotes)))
         (remote-names
          (loop for remote in remotes
                collect (concat (third remote)
                                "/"
                                (replace-regexp-in-string
                                 "^:" ""(symbol-name (second remote))))))
         (prompt (or prompt "Remote: "))
         (p (or (position
                 (completing-read prompt (slime-bogus-completion-alist remote-names)
                                  nil nil nil
                                  'slime-js-remote-history nil)
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

(defun slime-js-sticky-select-remote (n)
  "Select JS remote by number in sticky mode"
  (interactive (list (slime-js-read-remote-index)))
  (slime-eval-async `(js:select-remote ,n t)))

(defslime-repl-shortcut slime-repl-js-sticky-select-remote ("sticky-select-remote")
  (:handler 'slime-js-sticky-select-remote)
  (:one-liner "Select JS remote in sticky mode."))

(defun slime-js-set-target-url (url)
  "Set target URL for the proxy"
  (interactive "sTarget URL: ")
  (slime-eval-async `(js:set-target-url ,url)))

(defslime-repl-shortcut slime-repl-js-set-target-url ("target-url")
  (:handler 'slime-js-set-target-url)
  (:one-liner "Select target URL for the swank-js proxy"))

(defun slime-js-set-slime-version (url)
  "Set SLIME version for swank-js"
  (interactive "sVersion: ")
  (slime-eval-async `(js:set-slime-version ,url)))

(defslime-repl-shortcut slime-repl-js-set-slime-version ("js-slime-version")
  (:handler 'slime-js-set-slime-version)
  (:one-liner "Set SLIME version for swank-js"))

(defvar slime-js-require-history nil
  "History list for slime-js ,require command (`slime-js-require')")

(defvar slime-js-available-modules nil
  "List of core Node modules, plus any in node_modules directories.

Dynamically bound in `slime-js-read-module-name'.")

(defvar slime-js-node-core-modules
  '("assert" "buffer" "buffer_ieee754" "child_process" "cluster" "console"
    "constants" "crypto" "dgram" "dns" "domain" "events" "freelist" "fs" "http"
    "https" "module" "net" "os" "path" "punycode" "querystring" "readline"
    "repl" "stream" "string_decoder" "sys" "timers" "tls" "tty" "url" "util"
    "vm" "zlib")
  "List of modules in the Node.JS core.")

(defun slime-js-require (module)
  "Load a node.js module via require(), with completion on module names.

Treats module names that begin with `.' or `/' as filenames for
completion purposes, resolving relative paths with respect to the
directory where swank-js was started.  Otherwise, attempts to
complete the module name by searching the list of core Node
modules, plus installed modules in relevant node_modules
directories. See `slime-js-node-core-modules'.

Core modules and those loaded from node_modules directories are
assigned to a variable of the same name. Modules loaded from
filenames are assigned to a variable with the same name as the
file, minus any extension."
  (interactive (list (slime-js-read-module-name)))
  (let* ((var-name
          (replace-regexp-in-string
           "[^A-Za-z0-9_]" "_"
           (file-name-sans-extension
            (file-name-nondirectory module))))
         (js (format "%s = require('%s');" var-name module)))
    (message js)
    (slime-js-eval js)))

(defun slime-js-read-module-name ()
  "Read the name of a node.js module with minibuffer completion."  
  (let ((slime-js-available-modules
         (append
          slime-js-node-core-modules
          (slime-js-local-module-list))))
    (completing-read "Module: "
                     (completion-table-dynamic
                      #'slime-js-complete-module-name)
                     nil nil "" 'slime-js-require-history)))

(defun slime-js-complete-module-name (module)
  "Dynamic completion function for reading Node.JS module names"
  (cond ((or (string= module ".") (string= module ".."))
         nil)

        ((string-match-p "^[./]" module)
         (let ((directory (file-name-directory module)))
           (mapcar
            #'(lambda (file) (list (concat directory file)))
            (remove-if
             (lambda (file) (string-match-p "^\\." file))
             (file-name-all-completions
              (file-name-nondirectory module)
              (expand-file-name directory (slime-js-module-root)))))))

        (t
         (all-completions module slime-js-available-modules))))

(defun slime-js-local-module-list ()
  "Return a list of modules installed in node_modules/ directories.

Looks for node_modules/ directories by starting in `slime-js-root'
and moving up the filesystem to the root directory."
  ;; TODO: Cache these two values?
  (let ((root-dir (slime-js-module-root))
        (node-module-dirs (slime-js-node-module-dirs))
        (modules '()))
    (dolist (dir node-module-dirs)
      (when (file-directory-p dir)
        (setq modules
              (cons
               (directory-files dir nil "^[^.]")
               modules))))
    (apply #'append modules)))

(defun slime-js-node-module-dirs ()
  "Return the list of node_modules directories the node.js process will search."
  (cdr (slime-eval '(js:list-module-paths))))

(defun slime-js-module-root ()
  "Return the absolute directory where node.js resolves relative filenames."
  ;; js:module-filename returns the directory + "/repl"
  (file-name-directory (cadr (slime-eval '(js:module-filename)))))

(defslime-repl-shortcut slime-repl-js-require ("require")
  (:handler 'slime-js-require)
  (:one-liner "Require a Node.JS module"))


;; FIXME: should add an rpc command for browser-only eval

(defun slime-js-eval (str &optional cont)
  (slime-eval-async `(swank:interactive-eval ,str) cont))

(defun slime-js-reload ()
  (interactive)
  (slime-js-eval "SwankJS.reload()"
    #'(lambda (v)
        (message "Reloading the page"))))

(defun slime-js-refresh-css ()
  "If the current buffer points to a CSS file then the browser
will reload it. Otherwise it will reload all linked stylesheets"
  (interactive)
  (slime-js-eval
   (format "SwankJS.refreshCSS('%s')"
           (replace-regexp-in-string
            "(')" "\\\\\\1"
            (if (string-match "\\.css$" (buffer-file-name))
                (replace-regexp-in-string
                 "^.*/" "" (buffer-file-name))
              "")))
    #'(lambda (v)
        (message "Refreshing CSS"))))

(defun slime-js-make-js-string (string)
  "escapes the string so that it can be used as a string in js"
  (concat "\"" (replace-regexp-in-string "\n" "\\n" string nil t) "\""))

(defun slime-js-buffer-or-region-string ()
  (let ((start (if (region-active-p) (region-beginning) (point-min)))
         (end (if (region-active-p) (region-end) (point-max))))
    (buffer-substring-no-properties start end)))

(defun slime-js-embed-css (&optional arg)
  "send an active region or the whole buffer string to the browser
and embed it in a style element"
  (interactive "P")
  (let ((command (if arg "removeEmbeddedCSS" "embedCSS"))
        (param (if arg "" (slime-js-make-js-string
                           (slime-js-buffer-or-region-string)))))
    (slime-js-eval
     (format "SwankJS.%s(%s)" command param)
     #'(lambda (v) (message "Embedding CSS")))))

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
      (slime-flash-region start end)
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
