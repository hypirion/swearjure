((->>
  ((->> (<++-> <+> <-> <++-> (+)) #() (-> [<++-> <+> <->]))
   (->> (({(= =) #([<> %&](+))} (< $ <>) <+>) <+> <-> <++-> <>) #()
        (-> [<+> <-> <++-> <>]))
   (->> ($ _ ? $ (+ ! (*))) #() (-> [$ _ ? !]))
   (->> ($ _ ? $ (- ! (*))) #() (-> [$ _ ? !])))
   #()
   (-> [$]))
 (#(* (* % %) (* % %)) (#(* (* % %) (* % %)) (+ (*) (*)))))

;; A mutually recursive function set. Semantically, it translates to this:

;; (let [goal 65536]
;;   (let [f2-b1 (fn [forward back f2-b1 val]
;;                 (if (> val goal)
;;                   val
;;                   (forward forward back f2-b1 val)))
;;         forward (fn [a b c val] (a b c (+ val 1)))
;;         backward (fn [a b c val] (a b c (- val 1)))]
;;     (f2-b1 forward backward f2-b1 0)))
