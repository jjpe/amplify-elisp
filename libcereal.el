;; libcereal.el --- Elisp bindings for libcereal

;;; Commentary:

;;; Code:



;; (setenv "ZMQ_LIB_NAME" "zmq") ;; TODO:
;; (setenv "ZMQ_LIB_PATH" "/home/j/repos/spoofax-mode/module.rs/lib/zeromq4-x")
;; env::set_var("ZMQ_LIB_NAME", "zmq");
;; env::set_var("ZMQ_LIB_PATH", "/home/j/repos/spoofax-mode/module.rs/lib/zeromq4-x");


(defvar cereal/root-directory (file-name-directory load-file-name)
  "The cereal.el root directory.")

(defun cereal/subpath (&rest subpath-specs)
  "Calculate the absolute path out of of SUBPATH-SPECS, then return it.
The subpath formed by concatenating the SUBPATH-SPECS will be relative
to `cereal/root-directory'.  SUBPATH-SPECS must be some mix of strings and
symbols.  So for example, (cereal/subpath \"foo/\" \"bar/\") will work, as
will (cereal/subpath \"foo/\" 'bar/). Note that the path separators are
explicitly included."
  (let ((spec-names (mapcar (lambda (x) (if (symbolp x) (symbol-name x) x))
                            subpath-specs)))
    (apply #'concat cereal/root-directory spec-names)))


(require 'cl-macs) ;; For early return functionality in cl-defun
(require 'libcereal-module (cereal/subpath "module/target/liblibcereal_module.so"))
(message "[cereal] Loaded liblibcereal_module.so")



(cl-defun cereal/msg (process request-number kind
                      &key origin contents regions language ast)
  "Easily create a new Msg."
  (let* ((msg (cereal/msg-new)))
    (unless (stringp process)
      (error "Expected a string for 'process'"))
    (unless (integerp request-number)
      (error "Expected an integer for 'request-number'"))
    (unless (stringp kind)
      (error "Expected a string for 'kind'"))
    (unless (listp regions)
      (error "Expected a list for 'regions'"))
    (cereal/msg-set-process        msg process)
    (cereal/msg-set-request-number msg request-number)
    (cereal/msg-set-kind           msg kind)
    (cereal/msg-set-origin         msg origin)
    (cereal/msg-set-contents       msg contents)
    (dolist (region regions)
      (cond ((user-ptrp region)
             (cereal/msg-add-region region))
            ((listp region)
             (let ((begin (plist-get region :begin))
                   (end   (plist-get region :end)))
               (->> (cereal/region-new begin end)
                    (cereal/msg-add-region msg))))
            (t (error "[cereal/msg] Invalid regions value: %s" regions))))
    (cereal/msg-set-language       msg language)
    (cereal/msg-set-ast            msg ast)
    msg))


(cl-defun cereal/ast (name &key data children)
  "Easily create a new AST with :data, :children, or both."
  (let* ((ast (cereal/ast-new name)))
    (when (stringp data)
      (cereal/ast-set-data ast data))
    (when (and data (not (stringp data)))
      (cereal/ast-set-data ast (format "%s" data)))
    (unless (listp children)
      (error "Expected either nil or a list of child AST nodes"))
    (dolist (child children)
      (unless (user-ptrp child)
        (error "Each child must be an AST node"))
      (cereal/ast-add-child ast child))
    ast))

(defun cereal/msg-get-regions (msg)
  "Return all regions in MSG as a list of (:begin BEGIN :end END) plists."
  (let ((num-regions (cereal/msg-count-regions msg)))
    (cl-loop for region-index
             from 0 to (1- num-regions)
             collect (let* ((region (cereal/msg-get-region msg region-index)))
                       (list :begin (cereal/region-get-begin region)
                             :end   (cereal/region-get-end region))))))


(defun cereal/ast-get-children (ast)
  "Return the children of AST as a list."
  (let ((num-children (cereal/ast-count-children ast)))
    (loop for idx from 0 to (1- num-children)
          collect (cereal/ast-get-child ast idx))))

(cl-defun cereal/report (&key action process request-number
                              duration-nanos command)
  "Create a new report."
  (let* ((report (cereal/report-new)))
    (when (stringp action)
      (cereal/report-set-action report action))
    (when (stringp process)
      (cereal/report-set-process report process))
    (when (integerp request-number)
      (cereal/report-set-request-number report request-number))
    (when (integerp duration-nanos)
      (cereal/report-set-duration-nanos report duration-nanos))
    (when (stringp command)
      (cereal/report-set-command report command))
    report))

(cl-defun cereal/ast-plistify (ast)
  ""
  (unless ast
    (return-from cereal/ast-plistify))
  (let* ((name     (cereal/ast-get-name ast))
         (data     (cereal/ast-get-data ast))
         (children (cereal/ast-get-children ast))
         (result   (list :name name)))
    (when (and (stringp data) (> (length data) 0))
      (setq result (merge 'list result `(:data ,data) 'eq)))
    (when (and children (listp children))
      (let ((-children (loop for child in children
                             collect (cereal/ast-plistify child))))
        (setq result (merge 'list  result  (list :children `,-children)  'eq))))
    result))

(cl-defun cereal/ast-stringify (ast &key (indent-level 0) (indent-token "  "))
  "Return a string representation of an AST.  There are some keyword args:
INDENT-LEVEL: The level of indentation
INDENT-TOKEN: The indentation token.  Defaults to 2 spaces."
  (let* ((parent-indentation (cereal/repeat-string indent-token indent-level))
         (child-indentation (cereal/repeat-string indent-token (1+ indent-level)))
         (name     (cereal/ast-get-name ast))
         (data     (cereal/ast-get-data ast))
         (children (cereal/ast-get-children ast))
         (stuff ))
    (->> (cond ((and  (> (length data) 0)  (not children))
                (concat data ")"))
               ((and  (> (length data) 0)  children  (listp children))
                (concat "\n"  child-indentation  data   ",\n"
                        (->> (loop for child in children
                                   collect (cereal/ast-stringify child
                                                                 :indent-level (1+ indent-level)
                                                                 :indent-token indent-token)
                                   collect ",\n")
                             (apply #'concat))
                        parent-indentation ")"))
               ((and  (or (not data) (= (length data) 0))  children  (listp children))
                (concat "\n"
                        (->> (loop for child in children
                                   collect (cereal/ast-stringify child
                                                                 :indent-level (1+ indent-level)
                                                                 :indent-token indent-token)
                                   collect ",\n")
                             (apply #'concat))
                        parent-indentation ")"))
               (t (error "[cereal/ast-stringify] Illegal state")))
         (concat  parent-indentation  name  "("))))


(defun cereal/repeat-string (string times)
  "Return a new string that is equal to STRING repeated a number of TIMES."
  (apply #'concat (loop for x from 1 to times
                        collect string)))

(provide 'libcereal)
;;; libcereal.el ends here

;;  LocalWords:  liblibcereal ast stringify
