(define-module (rosenthal packages tree-sitter)
  #:use-module (guix packages))

(define tree-sitter-grammar
  (@@ (gnu packages tree-sitter) tree-sitter-grammar))

;; <https://issues.guix.gnu.org/64430>
(define-public tree-sitter-cmake
  (tree-sitter-grammar
   "cmake" "CMake"
   "0i8vsvkkas1fnn35na68gq5xq2y3xvhv2bk9q6x8c1bzsqm6rcsa"
   "0.4.0"
   #:repository-url "https://github.com/uyha/tree-sitter-cmake"))
