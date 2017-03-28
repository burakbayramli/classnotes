(load "ortak.lisp")
(load "kat-engelli-da.lisp")

;;
;; Bu sekilde kat-kat arama, kat-engelli-kka islemini cagiriyor. Bu
;; cagirmayi yaparken, her seferinde yeni bir derinlik limiti veriyor. 
;; Yani, kat engeli 1 ile cagiriyoruz, sonuc bulursak guzel.
;; Bulamazsak, kat engeli 2 ile.. vs, vs. 
(defun gitgide-derinlesen-kka (s0 sg sons depth ARTIS-OLCUSU)
  
  (block B
   (if (gitgide-icin-kka s0 sg sons depth) ;call dfs directly to depth
       (return-from B t)) ;solution found if dfs is true, so return t

   (gitgide-derinlesen-kka       ;else, try again but more deeply!
    s0			     	;the same initial state
    sg			      	;the same old goal state
    sons		       	;the same old set of operators
    (+ depth ARTIS-OLCUSU)	     	;but now a deeper search!
    ARTIS-OLCUSU)    ;and increment again later if you don't succeed

   ) ;; ends block
   
  );ends defun

(defun gitgide-derinlesen-kka-disyuz (s0 sg sons depth ARTIS-OLCUSU)
  (setf kaydir-sayisi 0)
  (gitgide-derinlesen-kka s0 sg sons depth ARTIS-OLCUSU)
  (print "# of nodes")
  (print kaydir-sayisi)  
)

;;
;; tests
;;
(setq s0 '((1 NIL 3 1) (2 3 4 1) (5 5 4 1)(1 1 1 1)(0 1)))
(setq s1 '((1 3 4 1) (2 3 NIL 1) (5 5 4 1)(1 1 1 1)(1 2)))
(test "dfs iterative deepening"
      (not (eql (gitgide-derinlesen-kka-disyuz s0 s1 nil 1 1) 'fail)) t )


(print "OK. Gitgide Derinlesen DA Testleri Isliyor")
