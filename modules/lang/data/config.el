;;; lang/data/config.el -*- lexical-binding: t; -*-

;;; Avro
;; .avsc (schema) and .avpr (protocol) are JSON
(add-to-list 'auto-mode-alist '("\\.avsc\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.avpr\\'" . json-mode))

;; Avro IDL
(define-generic-mode 'avro-idl-mode
  '("//" ("/*" . "*/"))
  '("protocol" "record" "error" "enum" "union" "fixed" "import" "idl" "schema"
    "null" "void" "boolean" "int" "long" "float" "double" "bytes" "string"
    "array" "map" "throws" "oneway" "namespace" "order" "aliases" "doc"
    "date" "time_ms" "timestamp_ms" "decimal" "uuid")
  '(("@[A-Za-z_][A-Za-z0-9_]*" . font-lock-preprocessor-face)
    ("`[^`]+`" . font-lock-variable-name-face)
    ("\\b[A-Z][A-Za-z0-9_]*\\b" . font-lock-type-face))
  '("\\.avdl\\'")
  nil
  "Major mode for Apache Avro IDL files.")
