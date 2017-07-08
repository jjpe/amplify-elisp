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



(defvar amplify-elisp/current-version "0.13.5"
  "The current semantic version of the Amplify Emacs module.")

(defvar amplify-elisp/detected-os
  (pcase system-type
    ('darwin       "osx")
    ('gnu/linux    "linux")
    ;; TODO: Windows support
    (_ (error "Operating system '%s' is not supported" system-type)))
  "A tag associated with the detected operating system.")




(require 'cl-macs) ;; For early return functionality in cl-defun
(require 'amplify-module (->> (concat "libamplify_module-"
                                      amplify-elisp/current-version
                                      "-"
                                      amplify-elisp/detected-os
                                      "-dbg.so")
                              (amplify-elisp/path)))



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
  (when (user-ptrp msg)
    (list :process (amplify-elisp/msg-get-process msg)
          :request-number (amplify-elisp/msg-get-request-number msg)
          :kind (amplify-elisp/msg-get-kind msg)
          :origin (amplify-elisp/msg-get-origin msg)
          :contents (amplify-elisp/msg-get-contents msg)
          :regions (amplify-elisp/msg-get-regions msg)
          :language (->> (amplify-elisp/msg-get-language msg)
                         (amplify-elisp/language-get-name))
          :ast      (->> (amplify-elisp/msg-get-ast msg)
                         (amplify-elisp/ast-plistify)))))


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
  "Return the children of AST as a list of user-ptrs, each pointing to an Ast."
  (let ((num-children (amplify-elisp/ast-count-children ast)))
    (loop for idx from 0 to (1- num-children)
          collect (amplify-elisp/ast-get-child ast idx))))

(cl-defun amplify-elisp/ast-plistify (ast)
  "Make a plist out of an AST user ptr."
  (when (user-ptrp ast)
    (let* ((name     (make-symbol (amplify-elisp/ast-get-name ast)))
           (data     (amplify-elisp/ast-get-data ast))
           (children (amplify-elisp/ast-get-children ast))
           (result   nil))
      (unless (symbolp name)
        (error "Expected a symbol value for name, got: %s" name))
      ;; (unless (and (stringp name) (> (length name) 0))
      ;;   (error "Expected a string value for name, got: %s" name))
      (setq result (plist-put result :name name))
      (when (and (stringp data) (> (length data) 0))
        (setq result (plist-put result :data data)))
      (when (and children (listp children))
        (->> (loop for child in children
                   collect (amplify-elisp/ast-plistify child))
             (plist-put result :children)
             (setq result)))
      result)))

(cl-defun amplify-elisp/ast-stringify (ast &key (indent-level 0)
                                                (indent-token "  "))
  "Return a string representation of an AST user-ptr or an AST plist.
These are the supported keyword args:
INDENT-LEVEL: The level of indentation
INDENT-TOKEN: The indentation token.  Defaults to 2 spaces."
  (let* ((parent-indentation (amplify-elisp/repeat-string indent-token indent-level))
         (child-indentation (amplify-elisp/repeat-string indent-token (1+ indent-level)))
         (name     (cond ((user-ptrp ast)        (amplify-elisp/ast-get-name ast))
                         ((and ast (listp ast))  (symbol-name (plist-get ast :name)))
                         (t  (error "Unknown AST type \"%s\"" (type-of ast)))))
         (data     (cond ((user-ptrp ast)        (amplify-elisp/ast-get-data ast))
                         ((and ast (listp ast))  (plist-get ast :data))
                         (t  (error "Unknown AST type \"%s\"" (type-of ast)))))
         (children (cond ((user-ptrp ast)        (amplify-elisp/ast-get-children ast))
                         ((and ast (listp ast))  (plist-get ast :children))
                         (t  (error "Unknown AST type \"%s\"" (type-of ast))))))
    (->> (cond ((and  (> (length data) 0)  (not children))
                ;; data & no children
                (concat data ")"))
               ((and  (> (length data) 0)  children  (listp children))
                ;; data & children
                (concat "\n"  child-indentation  data   ",\n"
                        (->> (loop for child in children
                                   collect (amplify-elisp/ast-stringify child
                                                                 :indent-level (1+ indent-level)
                                                                 :indent-token indent-token)
                                   collect ",\n")
                             (apply #'concat))
                        parent-indentation ")"))
               ((and  (or (not data) (zerop (length data)))  children  (listp children))
                ;; no data & children
                (concat "\n"
                        (->> (loop for child in children
                                   collect (amplify-elisp/ast-stringify child
                                                                 :indent-level (1+ indent-level)
                                                                 :indent-token indent-token)
                                   collect ",\n")
                             (apply #'concat))
                        parent-indentation ")"))
               ((and  (or (not data) (zerop (length data)))  (not children))
                ;; no data & no children
                ")")
               (t (error "[amplify-elisp/ast-stringify] Illegal state")))
         (concat  parent-indentation  name  "("))))


(defun amplify-elisp/repeat-string (string times)
  "Return a new string that is equal to STRING repeated a number of TIMES."
  (apply #'concat (loop for x from 1 to times
                        collect string)))


(provide 'amplify-elisp)
;;; amplify-elisp.el ends here

;;  LocalWords:  ast stringify
