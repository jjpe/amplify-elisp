;; amplify-elisp.el --- Elisp bindings for Amplify

;;; Commentary:

;;; Code:

(defvar amplify-elisp/root-directory (file-name-directory load-file-name)
  "The amplify-elisp.el root directory.")

(defun amplify-elisp/path (&rest subpath-specs)
  "Calculate the absolute path out of of SUBPATH-SPECS, then return it.
The subpath formed by concatenating the SUBPATH-SPECS will be relative
to `amplify-elisp/root-directory'.  SUBPATH-SPECS must be some mix of strings and
symbols.  So for example, (amplify-elisp/path \"foo/\" \"bar/\") will work, as
will (amplify-elisp/path \"foo/\" 'bar/). Note that the path separators are
explicitly included."
  (let ((spec-names (mapcar (lambda (x) (if (symbolp x) (symbol-name x) x))
                            subpath-specs)))
    (apply #'concat amplify-elisp/root-directory spec-names)))

(defun amplify-elisp/subproc-path (&rest subpath-specs)
  "Return the path to a file located in one of the sub-processes."
  (apply #'amplify-elisp/path "subproc/" subpath-specs))



(defvar amplify-elisp/current-version "0.10.3"
  "The current semantic version of the Emacs module for Amplify.")

(defvar amplify-elisp/releases-dir
  (amplify-elisp/subproc-path "module/")
  "The directory in which all Amplify Emacs module releases are stored.")

(defvar amplify-elisp/current-release-dir
  (concat  amplify-elisp/releases-dir  amplify-elisp/current-version  "/")
  "The directory in which the current Amplify Emacs module release is stored.
This is based on the semantic version stored in `amplify-elisp/current-version'.")





;; Internal utility functions
(defun amplify-elisp/log (fmt-string &rest args)
  "Log a message.
FMT-STRING: A string that can contain string format arguments.
ARGS: Any arguments to FMT-STRING."
  (message "[amplify-elisp] %s" (format fmt-string args)))

(defun amplify-elisp/download-resource (url file-name)
  "Download a resource from URL to FILE-NAME."
  (condition-case nil
      (url-copy-file url file-name)
    (error ; file-already-exists
     (amplify-elisp/log "Using cached resource @ %s" file-name))))

(defun amplify-elisp/query-latest-release ()
  "Query GitHub for the latest Amplify release information."
  (amplify-elisp/fetch-latest-release "amplify-elisp"))

(cl-defun amplify-elisp/fetch-latest-release (project &key author)
  "Query GitHub for the latest release information for a PROJECT by AUTHOR.
AUTHOR is a keyword argument that can be omitted, and defaults to \"jjpe\".
This function uses the GitHub REST API v3. "
  (let* ((author (or author "jjpe"))
         (url (format "https://api.github.com/repos/%s/%s/releases/latest"
                      author project)))
    (with-current-buffer (url-retrieve-synchronously url)
      (->> (json-read)
           (assoc 'tag_name)
           cdr))))



(defun amplify-elisp/module-download-release (semver)
  "Download an Amplify module release with a specific SEMVER, e.g. \"0.9.6\".
Specifically the following is downloaded:
  * libamplify_module-SEMVER-osx.so, the main module library.
  * libamplify_module-SEMVER-osx-dbg.so, a version
    of libamplify_module-SEMVER-osx.so with debug symbols.
Files that already exist won't be downloaded again."
  (let* ((new-dir-path (concat amplify-elisp/releases-dir semver "/"))
         (url-base "https://github.com/jjpe/amplify-elisp/releases/download/")
         (module-url (concat url-base semver "/libamplify_module-" semver "-osx.so"))
         (module-lib (concat new-dir-path "libamplify_module-" semver "-osx.so"))
         (module-dbg-url (concat url-base semver "/libamplify_module-" semver "-osx-dbg.so"))
         (module-dbg-lib (concat new-dir-path "libamplify_module-" semver "-osx-dbg.so")))
    (unless (file-exists-p (amplify-elisp/subproc-path))
      (make-directory (amplify-elisp/subproc-path)))
    (unless (file-exists-p amplify-elisp/releases-dir)
      (make-directory amplify-elisp/releases-dir))
    (unless (file-exists-p new-dir-path)
      (make-directory new-dir-path))
    (amplify-elisp/download-resource module-url module-lib)
    (amplify-elisp/download-resource module-dbg-url module-dbg-lib)
    (set-file-modes module-lib #o755)
    (set-file-modes module-dbg-lib #o755)))


(defun amplify-elisp/switch-version (semver)
  "Download and switch to Amplify SEMVER version e.g. \"0.10.3\".
This explicitly does not stop or start any processes, that must be done separately."
  ;; TODO: persistence of new SEMVER, especially when Amplify was upgraded.
  (amplify-elisp/module-download-release semver)
  (setq amplify-elisp/current-version      semver)
  (setq amplify-elisp/current-release-dir
        (concat  amplify-elisp/releases-dir  semver  "/")))




(require 'cl-macs) ;; For early return functionality in cl-defun
(amplify-elisp/switch-version amplify-elisp/current-version)
(require 'amplify-module
         (->> (concat "libamplify_module-" amplify-elisp/current-version "-osx-dbg.so")
              (amplify-elisp/subproc-path "module/" amplify-elisp/current-version "/")))
(message "[amplify-elisp] Loaded libamplify_module.so")



(cl-defun amplify-elisp/msg (&key process request-number kind origin
                           contents regions language ast)
  "Easily create a new Msg.  "
  (let* ((msg (amplify-elisp/msg-new)))
    (unless (stringp process)
      (error "Expected a string for 'process'"))
    (unless (integerp request-number)
      (error "Expected an integer for 'request-number'"))
    (unless (stringp kind)
      (error "Expected a string for 'kind'"))
    (unless (listp regions)
      (error "Expected a list of (start . end) conses for 'regions'"))
    (amplify-elisp/msg-set-process        msg process)
    (amplify-elisp/msg-set-request-number msg request-number)
    (amplify-elisp/msg-set-kind           msg kind)
    (amplify-elisp/msg-set-origin         msg origin)
    (cond ((not contents) nil)
          ((stringp contents)
           (amplify-elisp/msg-set-contents msg (amplify-elisp/contents-new-text contents)))
          ((listp contents)
           (amplify-elisp/msg-set-contents msg (apply #'amplify-elisp/contents-new-entries contents)))
          ((user-ptrp contents)
           (amplify-elisp/msg-set-contents msg contents))
          (t (error "Expected a string, a list of strings, or a contents object")))
    (dolist (region regions)
      (cond ((user-ptrp region)
             (amplify-elisp/msg-add-region region))
            ((listp region)
             (let* ((begin (car region))
                    (end (cdr region))
                    (region (amplify-elisp/region-new begin end)))
               (amplify-elisp/msg-add-region msg region)))
            (t (error "Invalid regions value: %s" regions))))
    (cond ((not language) nil)
          ((stringp language)
           (amplify-elisp/msg-set-language msg (amplify-elisp/language-new language)))
          ((user-ptrp language)
           (amplify-elisp/msg-set-language msg language))
          (t (error "Expected either a string or a language object")))
    (cond ((not ast) nil)
          ((listp ast)
           ;; TODO:
           ;; (amplify-elisp/msg-set-ast msg .... )
           (error "Using plist ASTs as values here is not yet implemented"))
          ((user-ptrp ast)
           (amplify-elisp/msg-set-ast msg ast))
          (t (error "Expected either a string or a language object")))
    msg))

(cl-defun amplify-elisp/msg-plistify (msg)
  "Turn a msg object into a property list."
  (list :process (amplify-elisp/msg-get-process msg)
        :request-number (amplify-elisp/msg-get-request-number msg)
        :kind (amplify-elisp/msg-get-kind msg)
        :origin (amplify-elisp/msg-get-origin msg)
        :contents (amplify-elisp/msg-get-contents msg)
        :regions (amplify-elisp/msg-get-regions msg)
        :language (->> (amplify-elisp/msg-get-language msg)
                       (amplify-elisp/language-get-name))
        :ast      (->> (amplify-elisp/msg-get-ast msg)
                       (amplify-elisp/ast-plistify))))


(cl-defun amplify-elisp/ast (name &key data children)
  "Easily create a new AST with :data, :children, or both."
  (let* ((name (cond ((stringp name) name)
                     ((symbolp name) (symbol-name name))
                     (t (error "Expected a symbol or a string value for name, got %s"
                               (type-of name)))))
         (ast (amplify-elisp/ast-new name)))
    (when (stringp data)
      (amplify-elisp/ast-set-data ast data))
    (when (and data (not (stringp data)))
      (amplify-elisp/ast-set-data ast (format "%s" data)))
    (unless (listp children)
      (error "Expected either nil or a list of child AST nodes"))
    (dolist (child children)
      (unless (user-ptrp child)
        (error "Each child must be an AST node"))
      (amplify-elisp/ast-add-child ast child))
    ast))

(defun amplify-elisp/msg-get-regions (msg)
  "Return all regions in MSG as a list of (:begin BEGIN :end END) plists."
  (let ((num-regions (amplify-elisp/msg-count-regions msg)))
    (cl-loop for region-index
             from 0 to (1- num-regions)
             collect (let* ((region (amplify-elisp/msg-get-region msg region-index)))
                       (list :begin (amplify-elisp/region-get-begin region)
                             :end   (amplify-elisp/region-get-end region))))))


(defun amplify-elisp/ast-get-children (ast)
  "Return the children of AST as a list."
  (let ((num-children (amplify-elisp/ast-count-children ast)))
    (loop for idx from 0 to (1- num-children)
          collect (amplify-elisp/ast-get-child ast idx))))

(cl-defun amplify-elisp/report (&key action process request-number
                              duration-nanos command)
  "Create a new report."
  (let* ((report (amplify-elisp/report-new)))
    (when (stringp action)
      (amplify-elisp/report-set-action report action))
    (when (stringp process)
      (amplify-elisp/report-set-process report process))
    (when (integerp request-number)
      (amplify-elisp/report-set-request-number report request-number))
    (when (integerp duration-nanos)
      (amplify-elisp/report-set-duration-nanos report duration-nanos))
    (when (stringp command)
      (amplify-elisp/report-set-command report command))
    report))

(cl-defun amplify-elisp/ast-plistify (ast)
  ""
  (unless ast
    (return-from amplify-elisp/ast-plistify))
  (let* ((name     (make-symbol (amplify-elisp/ast-get-name ast)))
         (data     (amplify-elisp/ast-get-data ast))
         (children (amplify-elisp/ast-get-children ast))
         (result   (list :name name)))
    (when (and (stringp data) (> (length data) 0))
      (setq result (merge 'list result `(:data ,data) 'eq)))
    (when (and children (listp children))
      (let ((kiddos (loop for child in children
                          collect (amplify-elisp/ast-plistify child))))
        (setq result (merge 'list  result  (list :children `,kiddos)  'eq))))
    result))

(cl-defun amplify-elisp/ast-stringify (ast &key (indent-level 0) (indent-token "  "))
  "Return a string representation of an AST.  There are some keyword args:
INDENT-LEVEL: The level of indentation
INDENT-TOKEN: The indentation token.  Defaults to 2 spaces."
  (let* ((parent-indentation (amplify-elisp/repeat-string indent-token indent-level))
         (child-indentation (amplify-elisp/repeat-string indent-token (1+ indent-level)))
         (name     (amplify-elisp/ast-get-name ast))
         (data     (amplify-elisp/ast-get-data ast))
         (children (amplify-elisp/ast-get-children ast)))
    (->> (cond ((and  (> (length data) 0)  (not children))
                (concat data ")"))
               ((and  (> (length data) 0)  children  (listp children))
                (concat "\n"  child-indentation  data   ",\n"
                        (->> (loop for child in children
                                   collect (amplify-elisp/ast-stringify child
                                                                 :indent-level (1+ indent-level)
                                                                 :indent-token indent-token)
                                   collect ",\n")
                             (apply #'concat))
                        parent-indentation ")"))
               ((and  (or (not data) (= (length data) 0))  children  (listp children))
                (concat "\n"
                        (->> (loop for child in children
                                   collect (amplify-elisp/ast-stringify child
                                                                 :indent-level (1+ indent-level)
                                                                 :indent-token indent-token)
                                   collect ",\n")
                             (apply #'concat))
                        parent-indentation ")"))
               (t (error "[amplify-elisp/ast-stringify] Illegal state")))
         (concat  parent-indentation  name  "("))))


(defun amplify-elisp/repeat-string (string times)
  "Return a new string that is equal to STRING repeated a number of TIMES."
  (apply #'concat (loop for x from 1 to times
                        collect string)))
















(provide 'amplify-elisp)
;;; amplify-elisp.el ends here

;;  LocalWords:  ast stringify
