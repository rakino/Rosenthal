(define-module (rosenthal packages tree-sitter)
  #:use-module (guix packages))

(define tree-sitter-grammar
  (@@ (gnu packages tree-sitter) tree-sitter-grammar))

(define-public tree-sitter-yaml
  (let ((base
         (tree-sitter-grammar
          "yaml" "YAML"
          "1bimf5fq85wn8dwlk665w15n2bj37fma5rsfxrph3i9yb0lvzi3q"
          "0.5.0"
          #:repository-url "https://github.com/ikatyang/tree-sitter-yaml")))
    (package
      (inherit base)
      (arguments
       (append '(#:tests? #f)           ;FIXME
               (package-arguments base))))))
