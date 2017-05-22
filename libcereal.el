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

(require 'libcereal-module (cereal/subpath "module/target/liblibcereal_module.so"))
(message "[cereal] Loaded liblibcereal_module.so")



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
      (unless (eq (type-of child) 'user-ptr)
        (error "Each child must be an AST node"))
      (cereal/ast-add-child ast child))
    ast))

(defun cereal/msg-get-regions (msg)
  "Return all regions in MSG as a list of (:begin BEGIN :end END) plists."
  (let ((num-regions (cereal/msg-count-regions msg)))
    (cl-loop for region-index
             from 0 to (1- num-regions)
             collect (let* ((region (cereal/msg-get-region msg region-index))
                            (begin  (cereal/region-get-begin region))
                            (end    (cereal/region-get-end region)))
                       `(:begin ,begin :end ,end)))))

(provide 'libcereal)
;;; libcereal.el ends here

;;  LocalWords:  liblibcereal
