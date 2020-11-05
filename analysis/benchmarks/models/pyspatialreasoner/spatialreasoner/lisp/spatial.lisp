;;;;;  This program is called space-5 and was originally written in Allegro Common Lisp, version 2.0b1,
;;;;;  but has been amended to run in MCL Common Lisp 4.1 The only major difference is that
;;;;;  MCL 4.1 initializes arrays with zero's, whereas 2.0b1 initializes them with nil's (see
;;;;;  calls to make-array).
;;;;;  The program makes  spatial deductions using a compositional semantics and bu + bt parser.
;;;;;  The program constructs only one falsifying model. It does not allow
;;;;;  premises to assert that one item is in the same place
;;;;;  as another, but it does put items in the same place temporarily
;;;;;  in the course of searching for models that refute conclusions.

;;;;;  There are two ways to use the program.  1.  Write (test n problems) where
;;;;;  n is the number of a particular sort of problem, see below.  E.g.
;;;;;  (test 2 combo-problems) gives the program the second combo-problem in the
;;;;;  list below.   2.  Call interpret giving it a list of premises, e.g.
;;;;;  (interpret '((the square is behind the circle)(the triangle is behind the cross)
;;;;;  (the cross is on the left of the square)))


;;; Some sample proplems that require models to be combined
(setf combo-problems '(
(1 ((the square is behind the circle)
    (the cross is in front of the triangle)
    (the square is on the left of the cross)) )
(2 ((the circle is in front of the square)
    (the triangle is behind the cross)
    (the cross is on the right of the square)) )
(3 ((the square is behind the circle)
    (the triangle is behind the cross)
    (the cross is on the left of the square)) )
(4 ((the square is behind the circle)
    (the triangle is behind the cross)
    (the line is above the triangle)
    (the cross is on the left of the square)) ) ))

;; Problems with a deductive conclusion
(setf ded-problems '(
(1 ((the circle is on the right of the square)
    (the triangle is on the left of the circle)
    (the cross is in front of the triangle)
    (the line is in front of the circle)
    (the cross is on the left of the line)) )
(2 ((the cross is in front of the circle)
    (the circle is in front of the triangle)
    (the cross is in front of the triangle)) )
(3 ((the square is on the right of the circle)
    (the circle is on the right of the triangle)
    (the square is on the right of the triangle)) )
(4 ((the square is on the right of the circle)
    (the triangle is on the left of the circle)
    (the square is on the right of the triangle)) )
(5 ((the square is on the right of the circle)
    (the cross is in front of the triangle)
    (the triangle is on the left of the square)
    (the square is behind the line)
    (the line is on the right of the cross)) )
(6 ((the triangle is on the right of the square)
    (the circle is in front of the square)
    (the cross is on the left of the square)
    (the line is in front of the cross)
    (the line is on the right of the ell)
    (the star is in front of the ell)
    (the circle is on the left of the vee)
    (the ess is in front of the vee)
    (the star is on the left of the ess)) ) ))

;;; Spatially indeterminate problems
(setf indet-problems '(
(1 ((the circle is on the right of the square)
    (the triangle is on the left of the circle)
    (the cross is in front of the triangle)
    (the line is in front of the square)
    (the cross is on the left of the line)) )       ;;  true but neednt be
(2 ((the triangle is on the right of the square)
    (the circle is in front of the square)
    (the cross is on the left of the triangle)
    (the line is in front of the cross)
    (the line is on the right of the ell)
    (the star is in front of the ell)
    (the circle is on the left of the vee)
    (the ess is in front of the vee)
    (the star is on the right of the ess)) )        ;; false but neednt be
(3 ((the square is on the right of the circle)
    (the triangle is on the left of the square)
    (the triangle is on the right of the circle)) ) ;; false but neednt be
(4 ((the square is on the right of the circle)
    (the triangle is on the left of the square)
    (the cross is in front of the triangle)
    (the line is in front of the circle)
    (the cross is on the right of the line)) )      ;; false but neednt be
(5 ((the square is on the right of the circle)
    (the triangle is on the left of the square)
    (the cross is in front of the triangle)
    (the line is in front of the circle)
    (the triangle is on the right of the circle)) ) ;; false but neednt be
(6 ((the circle is on the right of the square)
    (the triangle is on the left of the circle)
    (the cross is in front of the triangle)
    (the line is in front of the square)
    (the cross is on the right of the line)) )      ;; false but neednt be
(7 ((the triangle is in front of the square)
    (the circle is on the right of the square)
    (the cross is behind the triangle)
    (the line is on the right of the cross)
    (the line is in front of the ell)
    (the star is on the right of the ell)
    (the circle is behind the vee)
    (the ess is on the right of the vee)
    (the star is in front of the ess)) )            ;; false but neednt be
(8 ((the triangle is on top of the square)
    (the circle is on the right of the square)
    (the cross is below the triangle)
    (the line is on the right of the cross)
    (the line is on top of the ell)
    (the star is on the right of the ell)
    (the circle is below the vee)
    (the ess is on the right of the vee)
    (the star is on top of the ess)) )              ;; false but neednt be
(9 ((the square is on the right of the triangle)
    (the circle is on the left of the square)
    (the circle is behind the star)
    (the ell is in front of the circle)
    (the line is in front of the triangle)
    (the vee is in front of the triangle)
    (the star is on the right of the vee)) )))   ;; false but neednt be

;;; Problems with inconsistent premises
(setf incons-problems '(
(1 ((the square is on the left of the circle)
    (the cross is in front of the square)
    (the triangle is on the right of the circle)
    (the triangle is behind the line)
    (the line is on the left of the cross)) )
(2 ((the square is in front of the circle)
    (the triangle is behind the circle)
    (the triangle is in front of the square)) )
(3 ((the triangle is on the right of the square)
    (the circle is in front of the square)
    (the cross is on the left of the square)
    (the line is in front of the cross)
    (the line is on the right of the ell)
    (the star is in front of the ell)
    (the circle is on the left of the vee)
    (the ess is in front of the vee)
    (the star is on the right of the ess)) )))

;;; TOP-LEVEL FUNCTIONS

(defvar *premises* nil)  ;; global variable to hold premises

;;; Applies interpret to nth item in probs
(defun test (n probs)
(cond((null probs) nil)
     ((eq (caar probs) n)(interpret (cadar probs)))
     (t (test n (cdr probs)))))

;;; interpret
;;;     parse OK rtns e.g. ((0 -1 0) ([]) (O))
;;;     decide
;;;     prin-lis  (prints premises)
;;;        prin-space |
;;;     prin-arr  (prints arrays)

;;; High-level function takes list of premises parses each one, and then
;;; applies decide to result, e.g. to '( ((1 0 0) ... ) ( (o) (v) )), where
;;; list of args can be used to search models, and so trigger startmodel, or
;;; ensure that one or both are added, or combine models, or verify. Each
;;; of these functions, including verify rtns a model, which is consd on
;;; list of models since find-itm in decide has removed the relevant mods
(defun interpret(prems)
(setf *premises* prems)      ;;;  a global for use by make-true & make-false
(prog(mods)
loop
(cond((null prems)(print-arr (car mods))(return mods))
     (t (prin-lis (car prems))
        (setf mods (cons (decide (parse (car prems)) mods) mods))
        (print-arr (car mods))(terpri)
        (setf prems (cdr prems))(go loop)))))

;;; decide        what fn to call depending on what is already in set of mods
;;;    relfn    |      retrieves relation from representation of premise's meaning
;;;    subjfn  |     retrieves subject
;;;    objfn   |      retrieves object
;;;    find-itm   retrieves subject or object in array rtns its co-ords & mod in which it occurs
;;;       finders
;;;          finds
;;;              eq-or-inc
;;;                 check-member |
;;;    extract |   removes this mod from the set of models
;;;    deci       calls one of seven functions depending on which models subj and obj are in
;;;      verify        checks whether relation holds between subj and obj
;;;         veri      checks relation
;;;         finders   initializes and calls:-
;;;           finds        to find item in array
;;;              eq-or-inc
;;;                 check-member |
;;;         objfn |
;;;         subjfn |
;;;         relfn |

;;;      make-false
;;;         remove-prem
;;;            parse [see above]
;;;         negate-prop
;;;            relfn |
;;;            convert |
;;;         make
;;;            conflict
;;;              parse [see above]
;;;              subjfn |
;;;              objfn |
;;;              finders
;;;                 finds        to find item in array
;;;                    eq-or-inc
;;;                       check-member |
;;;              verify [see above]
;;;            switch
;;;               relfn |
;;;               subjfn |
;;;               objfn |
;;;               finders
;;;                 finds        to find item in array
;;;                    eq-or-inc
;;;                       check-member |


;;;               find-rel-prop
;;;                  list-subtract
;;;                  normalize
;;;               swap
;;;                 remove-item
;;;                    rem-itm-lis |
;;;                 add-item
;;;               copy-shrink-arr
;;;                  shrink-arr
;;;                     dimensions
;;;                     list-ints ||
;;;                     shrink
;;;                        dimensions
;;;                        rem-n ||
;;;                  newds ||
;;;                  dimensions
;;;                  copy-shr-arr
;;;                     dimensions
;;;                     newcoords
;;;                        newco
;;;                           nitms ||
;;;               conflictprops
;;;                  verify
;;;               move
;;;               convert |
;;;            print-arr
;;;         verify
;;;         prin-lis
;;;            prin-space |
;;;      make-true
;;;        remove-prem
;;;        make
;;;        verify
;;;        prin-lis
;;;           prin-space |
;;;      combine
;;;         print-arr
;;;         dims-n-orig
;;;            ortho
;;;         copy-array
;;;      convert |
;;;      add-item
;;;         add-it
;;;           list-add ||
;;;           copy-array
;;;             copy-arr
;;;               update ||
;;;               find-neg-num
;;;                  posn-neg-num ||
;;;                  is-neg-num ||
;;;               list-add
;;;               dimensions ||
;;;           dimensions ||
;;;           outside
;;;             new-orig ||
;;;             out ||
;;;           update-co-ords ||
;;;      startmod
;;;         add-item (see above)


;;;      find-rel-prop
;;;      swap
;;;        remove-item
;;;           rem-itm-lis |
;;;        add-item
;;;      copy-shrink-arr
;;;      conflictprops
;;;      move
;;;        remove-item
;;;           rem-itm-lis |
;;;        add-item
;;;        newposn
;;;           compare-ints |
;;;        update-co-ords
;;;      convert |

;;; OK [2] decides what function to call depending on what is already in set of mods
;;; prop is a list, '( ((1 0 0) ... ) ( (o) (v) )) containing reln and its args
;;; find-itm rtns list containing dims and mod
;;; If subj & obj in mod, calls verify, if one of them is in mod calls add-item,
;;; and if neither in mod calls startmod
;;; Find-itm removes mod from list of models so decide adds
;;; the updated model or same model in case of verify or new model in case
;;; of startmod to mods which are var in interpret
(defun decide(prop mods)
(let ((rel (relfn prop)) (subj (subjfn prop)) (obj (objfn prop))(subj-co-mod nil)
     (obj-co-mod nil) (s-co nil) (o-co nil) (subj-mod nil) (obj-mod nil))
; (print 'rel-subj-obj)(princ (list rel subj obj))
(cond((setf subj-co-mod (find-itm subj mods))
           (setf s-co (car subj-co-mod) subj-mod (cadr subj-co-mod))))
(cond((setf obj-co-mod (find-itm obj mods))
           (setf o-co (car obj-co-mod) obj-mod (cadr obj-co-mod))))
(setf mods (extract subj-mod mods))
(setf mods (extract obj-mod mods))
; (print 's-co-and-o-co)(princ (list s-co o-co))
(deci prop s-co o-co rel subj obj subj-mod obj-mod)))

;;; OK [3] calls appropriate fn depending on whether s-co or o-co are already in mods
(defun deci(prop s-co o-co rel subj obj subj-mod obj-mod)
(cond( s-co
       (cond( o-co
              (cond((equal subj-mod obj-mod)(print 'verify)
                      (cond((verify prop subj-mod)(make-false prop subj-mod))
                           (t (make-true prop subj-mod))))
                   (t (print 'combine)
                      (combine rel s-co o-co subj-mod obj-mod))))
            (t (print 'add-obj)(add-item s-co (convert rel) obj subj-mod))))
     ( o-co  (print 'add-subj)(add-item o-co  rel subj obj-mod))
     ( t  (print 'startmod)(startmod rel subj obj))))

;;;;; BU + backtracking parser based on PNJL's
;;;;; music parser. We have modified it to put semantics
;;;;; on stack and to unreduce semantics in backtracking 21-4-89

;;;  OK PARSER derives from bu-parse. It takes a sentence  as input and
;;;  tries to parse it bottom up using backtracking.
;;;  To run the program type (parse '(the triangle is in front of the square)).
;;;  There is a cfgrammar and lexicon, named as such.
;;;  The program parses  according to cfgrammar and copes with both
;;;  syntactic and lexical ambiguities.

;;;  The sorts of sentences the grammar accepts are:
;;;  The triangle is in front of the square
;;;  using the relations: right of, left of, front of, behind, above, below

;;; Grammar for spatial relations
(defvar *rel-grammar* '(
 (((NP-sing) (PRED))     (S 1) (s-prop))
 (((NP-sing) (NEGPRED))  (S 2) (sneg-sem) )
 (((art-def) (N-sing))       (NP-sing 1) (npfun) )
 (((rel)) (reln 1) (npfun) )
 (((next-p)(to-p))       (reln 1) (npfun) )
 (((in-p)(rel front-p)(of-p))(reln 1) (npfun) )
 (((in-p)(art-def)(front-p)(of-p)) (reln 1) (npfun) )
 (((on-p)(art-def)(rel horiz)(of-p)) (reln 1) (npfun) )  ;; NB (rel horiz)
 (((on-p)(rel vert)(of-p))(reln 1)(npfun) )
 (((in-p)(art-def)(adj-same)(n-loc)(as-p)) (reln 1)(npfun) )
 (((in-p)(art-indef)(adj-different)(n-loc)(to-p)) (reln 1)(npfun) )
 (((in-p)(art-indef)(adj-different)(n-loc)(from-p)) (reln 1)(npfun) )
 (((V-cop)(reln)(NP-sing))(PRED 1) (pred) )
 (((V-cop)(NEG)(reln)(NP-sing))(NEGPRED 1) (neg-pred-sem)  )))

;;;  Lexicon for spatial relations
(defvar *rel-lexicon* '(
 ((a)   ((art-indef) (list 'dummy) ))
 ((the) ((art-def) (list 'dummy) ))
 ((not) ((neg) (list 'neg-semantics) ))
 ((of)  ((of-p) (list 'dummy) ))
 ((as)  ((as-p) (list 'dummy) ))
 ((is) ((V-cop)(list 'dummy) ))                   ;;redundant??
 ((in) ((in-p)(list 'dummy) ))
 ((next)  ((next-p)(list 'next-semantics) ))
 ((to)    ((to-p)(list 'dummy) ))
 ((from)  ((from-p)(list 'dummy) ))
 ((on) ((on-p)(list 'dummy) ))
 ((right) ((rel horiz) (list '(1 0 0)) ))  ;; complex syntactic cat
 ((left)  ((rel horiz) (list '(-1 0 0)) ))
 ((front) ((rel front-p) (list '(0 1 0)) ))
 ((behind) ((rel) (list '(0 -1 0)) ))
 ((above)  ((rel) (list '(0 0 -1)) ))
 ((top)    ((rel vert) (list '(0 0 -1)) ))
 ((below)  ((rel) (list '(0 0 1)) ))
 ((north)  ((rel) (list '(0 -1 0))))
 ((south)  ((rel) (list '(0 1 0))))
 ((east)   ((rel) (list '(1 0 0))))
 ((west)   ((rel) (list '(-1 0 0))))
 ((north-east) ((rel) (list '(1 -1 0))))
 ((south-east) ((rel) (list '(1 1 0))))
 ((south-west) ((rel) (list '(-1 1 0))))
 ((north-west) ((rel) (list '(-1 -1 0))))
 ((between) ((relat)(list 'between-semantics) ))
 ((among)  ((relat) (list 'among-semantics) ))
 ((beside) ((relat) (list 'beside-semantics) ))
 ((square) ((N-sing) (list '[]) ))
 ((triangle) ((N-sing) (list 'V) ))
 ((circle) ((N-sing) (list 'O) ))
 ((line) ((N-sing) (list 'I) ))
 ((cross) ((N-sing) (list '+) ))
 ((ell) ((N-sing) (list 'L) ))
 ((vee) ((N-sing) (list '^) ))
 ((star) ((N-sing) (list '*) ))
 ((ess) ((N-sing) (list 'S) )) ))

;;; 4 global variables
(defvar sen nil)
(defvar sen-shifted nil)
(defvar pstack nil)
(defvar history nil)

;;; parse
;;;    analyze
;;;       reduces-word
;;;          word |
;;;          lexical-category
;;;             legalcat
;;;                member-lis |
;;;       reduces-syn
;;;          syntax-rule
;;;             member-lis |
;;;             expand
;;;                lhs-of-rule
;;;                   rule-in-grammar |
;;;                rhs-of-rule |
;;;             rule-semantics
;;;                expand
;;;                   lhs-of-rule
;;;                      rule-in-grammar |
;;;                   rhs-of-rule |
;;;          rule-list
;;;             matchrule |
;;;             rhs-of-rule |
;;;          strip-sem |
;;;          compose
;;;             lhs-of-rule
;;;                rule-in-grammar |
;;;             sem-of-rule |
;;;             args-from-stack |
;;;             rhs-of-rule |
;;;             chop |
;;;       history-construction |
;;;    shift |
;;; backtrack

;;; OK [4] tries to parse sentence by calling reduces or shift, and if they fail it
;;; goes into backtrack mode. TMP is what is presently under examination.
;;; If it succeeds, it rtns e.g. '( ((1 0 0)) (([ ]) (V)) )
;;; for '(the square is on the right of the triangle)
(defun parse(sentence)
(prog ((sen sentence)(gram *rel-grammar*)(lex *rel-lexicon*))
(setf sen-shifted nil pstack nil history nil)
loop
; (pvalues)
; (print pstack)
(cond((and (equal (caar pstack) '(S))(null sen))(return (cadar pstack)))
     ((analyze gram lex nil) (go loop))
     ((shift)(go loop))
     ((backtrack gram lex) (go loop))
     (t (print '(parse fails)) nil))))

;;; OK [5] Normal PARSING tries to reduce word or syn-cat; also called by backtrack.
;;; LHS is nil except when there are ambiguities, in which case it is the
;;; alternative(s) already considered.
(defun analyze (gram lex lhs)
(let ((tmp nil))
(cond((setf tmp (or (reduces-word lex lhs)
                    (reduces-syn gram lhs)))
               (setf history (history-construction tmp))))))

;;; OK [6] if top of stack is word replaces it with syn cat, where lhs has a value
;;; then rtns next syn cat after it in lex entry for word, e.g. if lhs has a
;;; value such as  '((n-sing)(love-n-sem)) then rtns next syn cat after it in
;;; lex; tmp rtnd for history
;;; Puts word's semantics on stacks too
(defun reduces-word(lex lhs)
(let ((tmp nil))
(cond((null pstack) nil)
     ((and (word (car pstack) lex)
           (setf tmp (lexical-category (car pstack) lex lhs)))
                (setf tmp (list (car tmp)(eval (cadr tmp)))) ; evals lex semantics
                (setf pstack (cons tmp (cdr pstack)))
                tmp))))

;;; OK [7] If top of pstack is nonterminal and there is a legal rule for reducing it
;;; incl perhaps other symbols on stack, then symbol(s) on top of stack replaced
;;; by reduction -- next reduction after lhs  --  and new lhs of relevant rule
;;; rtned for history
(defun reduces-syn(gram lhs)
(let ((tmp nil))
(cond((setf tmp (syntax-rule (rule-list (strip-sem pstack) gram) lhs gram))
          ;; (print 'tmp-in-reduces)(princ tmp)(terpri)
           (compose tmp gram)))))

;;; OK [8] takes a complete rule such as '(((art-def)(n-sing)) (np-sing 1) (npfun))
;;; and extracts newsyn on lhs, (np-sing),newsemantics of rule, (npfun), and
;;; the list of semantic arguments from the pstack.  Then it applies fun to
;;; args, if possible, and puts newsyn + newsem (result of composition) on
;;; pstack in place of symbols that rule has reduced.
;;; Finally, rtns full lhs-of-rule inc rule number and newsem for history
(defun compose(rule gram)
(let* ((newsyn (list(car(lhs-of-rule rule gram))))
       (newsem (sem-of-rule rule))
       (args (list (args-from-stack (reverse (rhs-of-rule rule)) pstack))))
; (print newsyn)(print newsem)(print args)
(or (setf newsem (apply (car newsem) args))
    (print '(no-composition rule for const))(terpri))
(setf pstack (cons (list newsyn newsem)(chop (rhs-of-rule rule) pstack)))
(list (lhs-of-rule rule gram) newsem))) ;; for history

;;; OK [9] takes reverse rhs of syntactic rule and rtns a list of corresponding
;;; semantic elements from stak ready for the semantic fn to be applied to
;;; them by compose
(defun args-from-stack(rev-rhs stak)
(cond((null rev-rhs) nil)
     ((equal (car rev-rhs)(caar stak))
               (cons (cadar stak)(args-from-stack (cdr rev-rhs)(cdr stak))))))

;;; OK [10] shifts car.sen from sen to sen-shifted; & rtns it for history stack
;;; No semantics needed for this operation.
(defun shift ()
(cond((null sen) nil)
     (t (setf pstack (cons (list(car sen)) pstack))
        (setf history (cons (list (car sen)) history))
        (setf sen-shifted (cons (car sen) sen-shifted))
        (setf sen (cdr sen))
        (car sen-shifted))))

;;; parse
;;;    analyze
;;;       reduces-word
;;;          word |
;;;          lexical-category
;;;             legalcat
;;;                member-lis |
;;;       reduces-syn
;;;          syntax-rule
;;;             member-lis |
;;;             expand
;;;                lhs-of-rule
;;;                   rule-in-grammar |
;;;                rhs-of-rule |
;;;             rule-semantics
;;;                expand
;;;                   lhs-of-rule
;;;                      rule-in-grammar |
;;;                   rhs-of-rule |
;;;          rule-list
;;;             matchrule |
;;;             rhs-of-rule |
;;;          strip-sem |
;;;          compose
;;;             lhs-of-rule
;;;                rule-in-grammar |
;;;             sem-of-rule |
;;;             args-from-stack |
;;;             rhs-of-rule |
;;;             chop |
;;;       history-construction |
;;;    shift |
;;;    backtrack

;;;;; BACKTRACKING

;;;    backtrack
;;;       word |
;;;       unreduces
;;;          non-term
;;;             lhs-of-rule
;;;                rule-in-grammar |
;;;          unred
;;;             rhs-of-rule |
;;;             copy-history
;;;                word |
;;;             rewrite
;;;                lhs-of-rule
;;;                   rule-in-grammar |
;;;             drop-rule-no
;;;                drop-no |
;;;       analyze  [see above]
;;;       shift    [see above]
;;;       unshift |

;;; OK [11] when it fails, nec to call unshift to prevent endless loop
;;; if null history then nothing left to backtrack on
;;; if top of car stack is not a word then calls unreduces then
;;;  if analyze or shift works it rtns to normal parsing but if both fail
;;;    it continues to backtrack
;;; if top of stack is a word then unshifts it and continues backtracking
(defun backtrack (gram lex)
(prog ((lhs nil)) (print 'backtracking)
loop
(cond((null history)(return nil))
     ((not(word (car pstack) lex))
            (unreduces gram lex)
            (setf lhs (car history) history (cdr history))
            (prin1 'reanal-with-lhs)(print lhs)(pvalues)
            (cond ((analyze gram lex lhs)(print 'rtn-parsing)
                                       (return t))
                  ((shift)(print 'rtn-parsing)(return t))
                  (t (print 'reanal-fails)(go loop))))
     (t (unshift)(go loop))) ))

;;; OK [12] if top of history is non-term then replaces top of stack with rhs of
;;; grammatical rule + the composed semantics of its constituents
;;; otherwise top of history is a syn cat it replaces it
;;; with corresponding word, which is in cadr history
(defun unreduces(gram lex)
(print 'unreduces)
(cond((non-term (caar history) gram)
        (setf pstack (unred gram lex)))
     (t (setf pstack(cons(cadr history)(cdr pstack))))))

;;; OK [13] for unreducing a lhs symbol, such as (S 2), removes  said lhs
;;; from both stacks, and then put onto pstack the
;;; constituents from the history stack that match the rhs of the rule,
;;; obtained by rewrite of lhs, and
;;; ensuring that the appropriate semantics is put back on pstack.  One
;;; further wrinkle is it also removes any rule numbers from the
;;; initial syntactic list for each item on history stack before putting
;;; them on pstack
(defun unred (gram lex)
(print 'unred)
(let* ((lhs (caar history))
      (pstack (cdr pstack))
      (rev-rhs-rule (reverse (rhs-of-rule (rewrite lhs gram)))) ;;ok
      (update-top (copy-history rev-rhs-rule (cdr history) lex)))
(append (drop-rule-no update-top) pstack)))

;;; OK [14] moves word off stacks & back from sen-shifted to sen
(defun unshift()
(print 'unshift)
(setf sen (cons (caar history) sen))
(setf sen-shifted (cdr sen-shifted))
(setf pstack (cdr pstack))
(setf history (cdr history)))

;;; OK [15] takes reversed rhs constituents of a rule, looks for their mates on
;;; history and rtns a list of them, including semantics for them
(defun copy-history (rev-rhs hist lex)
(cond((null rev-rhs) nil)
     ((null hist) nil)
     ((and (not(word (car hist) lex))      ;;; otherwise caaar fails
           (equal (caar rev-rhs)(caaar hist)))
           (cons (car hist)(copy-history (cdr rev-rhs) hist lex)))
     (t (copy-history rev-rhs (cdr hist) lex))))

;;; OK [16] takes items obtained from history and drops rule number from syn part of
;;; each item preparatory to putting on pstack as part of unred
(defun drop-rule-no (lis)
(mapcar #'drop-no lis))

;;; OK [17] takes syn + sem item from history and drops rule number from syn part
;;; preparatory to putting it on pstack as part of unreduce
(defun drop-no (item)
(list (list (caar item))(cadr item)))

;;;;; LEXICAL CATEGORIES AND WORDS

;;; OK [18] rtns category of item in lexicon, allowing for ambiguity in lexicon
;;; see parameter lc.
;;; e.g. given parameters '(love) lexicon '((n-sing)(love-n-sem)) it rtns
;;; ((v-plur)(love-v-sem))
(defun lexical-category(item lex lc)
(cond((null lex) (print '(symbol not in lexicon))(princ item) nil)
     ((equal item (caar lex))(legalcat lc (cdar lex)))
     (t (lexical-category item (cdr lex) lc))))

;;; OK [19] takes lis and lexical category, lc, and returns next item in lis after
;;; lc or else if none, nil;  where null.lc rtns car.lis. In practice takes a
;;; lc and the rhs of the lexicon it comes from and retns next lc if any
 (defun legalcat (lc lis)
 (cond((null lc) (car lis))
      (t (car (member-lis lc lis))))) ;;;subst. member with member-lis

;;; OK [20] rtns true if item is word in lexicon that has not been analyzed,
;;; i.e. it has no attached syntactic category
(defun word (item lex)
(cond((null lex) nil)
     ((equal (caar lex) item) t)
     (t (word item (cdr lex)))))

;;; SYNTACTIC CATEGORIES AND RULES
;;; OK [21] rtns first of lisrules after item that matches lhs, i.e. a complete
;;; grammatical rule.  Because different symbols may yield same expansion,
;;; the fn must may able to go to next item in lisrules even if it has a
;;; different category label from the one in the current lhs.
;;; member-lis rtns next rule after one that matches, if none rtns nil
;;; Maybe one day revise expand to return whole of rule from gram?
(defun syntax-rule (lisrules lhs gram)
(cond((null lisrules) nil)
     ((null lhs) (car lisrules))
     (t (car (member-lis (list (expand lhs gram) lhs
             (rule-semantics lhs gram)) lisrules)))))

;;; OK [22] rtns a list of rules -- in complete form  --  whose expansions when
;;; reversed match the items at the top of the syn-stack,i.e. stack from
;;; which semantic part of item has been stripped, using matchrule
(defun rule-list (syn-stack gram)
(cond((null gram) nil)
     ((matchrule (reverse(rhs-of-rule (car gram))) syn-stack)
             (cons (car gram)(rule-list syn-stack (cdr gram))))
     (t (rule-list syn-stack (cdr gram)))))

;;; OK [23] matches reversed rhs-of-rule with syn-stack, prior to reduces
(defun matchrule (revrule syn-stack)
(cond((null revrule) t)
      ((null syn-stack) nil)
      ((equal (car revrule)(car syn-stack))
              (matchrule (cdr revrule)(cdr syn-stack)))))

;;; OK [24] works like member but copes with lists that are members of lists
;;; NB if lis1 is last item in lis2 then rtns cdr of lis2, i.e. nil, needed
;;; by functions that use it in backtracking
(defun member-lis (lis1 lis2)
(cond((null lis2) nil)
      ((equal lis1 (car lis2))(cdr lis2))
      (t (member-lis lis1 (cdr lis2)))))

;;; OK [25] rtns stack minus same number of items as in lis, i.e. those on the rhs
;;; of a rule prior to reducing them to its lhs
(defun chop (lis stack)
(cond((null lis) stack)
     (t (chop (cdr lis)(cdr stack)))))

 ;;; OK [26] prints values of main variables for diagnostic purposes
 (defun pvalues()
 (print 'sen)(princ sen)
 (print 'pstack)(princ pstack)
 (print 'history)(princ history)
 ;; (print 'sen-shifted)(princ sen-shifted)
(terpri))

;;; Other functions used in parsing

;;; OK [27] expand takes the lhs of a rule, (S 1) -> NP VP and rtns its rhs, e.g
;;; ((NP-SING)(VP-SING)).
(defun expand (lhs gram)
(cond ((null gram) (print '(reduction not in grammar)))
      ((equal lhs (lhs-of-rule (car gram) gram))(rhs-of-rule (car gram)))
      (t (expand lhs (cdr gram)))))

;;; Possible replacement for expand in normal parsing-- STILL to be done, 21-4-89
;;; OK [28] given lhs of rule, such as (vp-plur 3) rtns complete rule
(defun rewrite (lhs gram)
(cond((null gram)(print '(no rule in grammar for lhs)))
     ((equal lhs (lhs-of-rule (car gram) gram))(car gram))
     (t (rewrite lhs (cdr gram)))))

;;; OK [29] If symb is lhs of a rule, e.g. (S), it rtns t, otherwise nil
(defun non-term (symb gram)
(cond ((null gram) nil)
      ((equal symb (lhs-of-rule (car gram) gram)) t)
      (t (non-term symb (cdr gram)))))

;;; OK [30] takes contents of stack and strips the semantic interpretation from each
;;; item to leave a conventional syntactic stack
(defun strip-sem (stack)
(mapcar #'car stack))

;;; OK [31] given rule such as (S 1) -> (NP-sing)(VP-sing), it rtns its lhs, i.e
;;; (S 1) provided that rule is in the cfgrammar; otherwise obviously rtns nil
(defun lhs-of-rule (rule gram)
(cond ((equal rule (rule-in-grammar rule gram))
         (cadr rule))))

;;; OK [32] takes a rule and cfgrammar and rtns the rule or else prints '(rule not
;;; in grammar)
(defun rule-in-grammar (rule grammar)
(cond ((null grammar) (print '(rule not in grammar)))
      ((equal rule (car grammar)) rule)
      (t (rule-in-grammar rule (cdr grammar)))))

;;; OK [33] takes grammatical rule, and rtns its rhs
(defun rhs-of-rule (rule)
(car rule))

;;; OK [34] given grammatical rule, it rtns the semantic part of it
(defun sem-of-rule (rule)
(caddr rule))

;;; OK [35] rtns the semantic part of a gram rule given just its lhs, e.g. (S 2) rtns
;;; (S 2 SEMANTICS)
(defun rule-semantics (lhs gram)
(sem-of-rule (expand lhs gram)))

;;; NOT USED IN THE PROGRAM
;;; OK rtns list of lex entries for all words with same hd of syntactic category
;;; e.g. given cat = '(rel) will include entries of '(rel horiz)
(defun find-same-cats(cat lex)
(cond((null lex) nil)
     ((equal (car (syncat-of (car lex))) (car cat))
          (cons (car lex)(find-same-cats cat (cdr lex))))
     (t (find-same-cats cat (cdr lex)))))

;;; NOT USED IN THE PROGRAM
;;; OK given semantics rtns corresponding lexical entry
(defun lex-entry-fr(sem lex)
(cond((null lex) nil)
     ((equal (car (eval (semantics-of (car lex)))) sem) (car lex))
     (t (lex-entry-fr sem (cdr lex)))))

;;; NOT USED IN THE PROGRAM
;;; OK given lexical entry rtns its semantics
(defun semantics-of(lex-entry)
(cadadr lex-entry))

;;; NOT USED IN THE PROGRAM
;;; OK given a lexical entry rtns full list representing syntactic category
(defun syncat-of(lex-entry)
(caadr lex-entry))

;;;;; SEMANTICS

;;; Example of history stack at end of parse involving backtracking
;;; '(((S 1) (S-SEM)) ((PRED 1) (PRED-SEM)) ((NP-SING 1) (NP1-SEM)) ((N-SING)
;;; (TRIANGLE-SEMANTICS)) (TRIANGLE) ((ART) (DUMMY-SEMANTICS)) (THE) ((RELN 2)
;;; (REL N-SEM)) ((PREP) (DUMMY-SEMANTICS)) (OF) ((REL) (RIGHT-SEMANTICS))
;;; (RIGHT) ((V-COP) (AFF-SEMANTICS)) (IS) ((NP-SING 1) (NP1-SEM)) ((N-SING)
;;; (CIRCLE-SEMANTICS)) (CIRCLE) ((ART) (DUMMY-SEMANTICS)) (THE))
;;; To build a model of 'The square is right of the circle'
;;; Open an array;  insert [] at origin, move rightwards and insert o

;;; COMPOSITIONAL RULES

;;; OK [36] assembles rel, arg, arg in list, e.g. ((1 0 0)([])(V))
(defun s-prop(lis)
(list (caaar lis)(cadr lis)(caadar lis)))

;;; OK [37] rtns first non-dummy function from list of args
(defun npfun (lis)
(cond((null lis) nil)
     ((equal (car lis) '(dummy))(npfun (cdr lis)))
     (t (car lis))))

;;; OK [38] shifts arg into list behind reln
(defun pred (lis)
(list (cadr lis)(list (car lis))))

;;; OK [39] rtns the appropriate history both when tmp is an atom and when it is a
;;; list.
(defun history-construction (tmp)
(cond ((atom tmp)(setf history (cons (list tmp) history)))
      (t (setf history (cons tmp history)))))

;;; FINDING ITEMS IN ARRAYS

;;; find-item
;;;    finders
;;;       finds

;;; OK [40] rtns a list containing 1. list of co-ords of item, and 2. mod in which it
;;; occurs
(defun find-itm(itm mods)
(let ((cords nil))
(cond((null mods) nil)
     ((setf cords (finders itm (car mods))) (list cords (car mods)))
     (t (find-itm itm (cdr mods))))))

;;; OK [41] calls finds and rtns co-ords of item in arr, calling finds to do job
(defun finders (itm arr)
(finds itm arr 0 0 0 (dimensions arr)))

;;; OK [42] finds item in arr and rtns lis of its co-ords
(defun finds (itm arr c1 c2 c3 dims)
(cond((<= c3 (caddr dims))
  (cond((<= c2 (cadr dims))
      (cond((<= c1 (car dims))
            (cond((eq-or-inc itm (apply #'aref arr (list c1 c2 c3)))
                           (list c1 c2 c3))
                 (t (finds itm arr (+ 1 c1) c2 c3 dims))))
           (t (finds itm arr 0 (+ 1 c2) c3 dims))))
       (t (finds itm arr 0 0 (+ 1 c3) dims))))))

;;; OK [43] rtns t iff item equals cell-value or else itm is e.g. '(O) and cell-value
;;; is '(V O)
(defun eq-or-inc (itm cell-value)
(cond((equal itm cell-value) t)
     ((check-member (car itm) cell-value) t)))

;;;   finds
;;;      eq-or-inc
;;;         check-member |

;;;  STARTING MODELS and ADDING ITEMS

;;; (make-array '(2 2 2) :adjustable t))
;;; MCL 2.0b1 initials with NIL in each cell, but
;;; MCL 4.1 initializes with 0 in each cell, and so necessary
;;; to call (make-array '(1 1 1) :initial-element 'nil :adjustable t)

;;; OK [44] builds  smallest possible new model in which subj is rel'd to obj
;;; Initial contents are nil instead of 0, which this version of lisp automatically
;;; inserts
(defun startmod(rel subj obj)
(print 'startmod)
(let* (( mod (make-array '(1 1 1) :initial-element 'nil :adjustable t))
       (mod (add-item '(0 0 0) '(0 0 0) obj mod)) ; puts object at origin
       (mod (add-item '(0 0 0)  rel subj mod)))
mod))

;;; startmod -- starts initial model
;;;     add-item -- works for '(0 0 0) '(0 0 0) (*) array) but then hangs up

;;; OK [45] updates co-ords according to rel and then calls add-it
;;; checks co-ords for outside model, and if so, expands mod -- nec
;;; in case rel is '(0 0 0) but assigned co-ords that are outside (see
;;; add-it)
(defun add-item(co-ords rel item mod)
(let ((co-ords (update-co-ords co-ords rel))(newlis nil))
(cond((and (equal rel '(0 0 0))
           (setf newlis (outside co-ords (dimensions mod))))
            ;;  (print 'new-dims+new-orig)(princ newlis)
              (setf mod (copy-array mod nil (car newlis)(cadr newlis)))
              (setf co-ords (list-add co-ords (cadr newlis)))))
                             ;;adding neworig to old coords
(add-it co-ords rel item mod)))

;;; OK [46] Adds item and rtns the new mod.
;;; First test detects 'in same place' and item is added to whatever is at
;;; co-ords.  Second test detects that co-ords are outside then
;;; lis of new-dims+new-orig used to copy mod to new mod to which item
;;; is then added having changed co-ords by adding new-orig to them.
;;; Third test, where co-ords within, is for whether current cell is nil in
;;; which case item is added.  Finally, where cells already has item,
;;; update co-ords accord to rel and try again.
;;; As it is recursive and may increase co-ords, it needs to expand array
;;; sometimes.  Calling fn, add-item, tests for outside where rel = '(0 0 0)
(defun add-it (co-ords rel item mod)
(let ((newlis nil))
(cond((equal '(0 0 0) rel)
          (setf item (cons(car item)(apply #'aref mod co-ords)))
          (setf (apply #'aref mod co-ords) item)  mod)
     ((setf newlis (outside co-ords (dimensions mod)))
          (setf mod (copy-array mod nil (car newlis)(cadr newlis)))
          (add-it (list-add co-ords (cadr newlis)) rel item mod) mod) ;;add neworig to old coords
     ((null (apply #'aref mod co-ords))
          (setf (apply #'aref mod co-ords) item)  mod)
     (t (add-it (update-co-ords co-ords rel) rel item mod)))))

;;; OK [47] rtns nil if co-ords within dims, otherwise rtns a list of newdims and
;;; neworigin, e.g. (1 3 -2) (2 2 2) -> ((2 3 4) (0 0 2))
(defun outside(co-ords dims)
(let ((newds nil))
(cond((equal (setf newds (out co-ords dims)) dims) nil)
     (t (list (list-add newds '(1 1 1)) (new-orig co-ords))))))

;;; OK [48] recurses through the two lists, adjusting output if a co-ord is too
;;; big or too small, i.e. a minus quantity
(defun out(co-ords dims)
(cond((null co-ords) nil)
     ((> (car co-ords)(car dims))
       (cons (car co-ords)(out (cdr co-ords)(cdr dims))))
     ((< (car co-ords) 0)
       (cons (+ (abs (car co-ords))(car dims))(out (cdr co-ords)(cdr dims))))
     (t (cons(car dims)(out (cdr co-ords)(cdr dims))))))

;;; OK [49] sets co-ords of origin to nil or to abs value of negative co-ord
(defun new-orig(co-ords)
(cond((null co-ords) nil)
     ((>= (car co-ords) 0)(cons 0 (new-orig (cdr co-ords))))
     (t (cons (abs (car co-ords))(new-orig (cdr co-ords))))))

;;; OK [50] Given e.g. left = '(-1 0 0), right = '(1 0 0) etc, rtns co-ords (of old
;;; item updated to new position that satisfies reln;  but to be modified so
;;; that position of old co-ords is changed on only one dimension
(defun update-co-ords (co-ord reln)
(list-add co-ord reln))

;;; OK [51] adds the numbers in lis1 to their corresponding items in lis2
(defun list-add(lis1 lis2)
(cond((null lis1) nil)
     (t (cons (+ (car lis1)(car lis2))(list-add (cdr lis1)(cdr lis2))))))

;;; OK [52] substracts the numbers in lis2 from the corresponding items in lis1
;;; Called by find-rel-prop
(defun list-subtract(lis1 lis2)
(cond((null lis1) nil)
     (t (cons (- (car lis1)(car lis2))(list-subtract (cdr lis1)(cdr lis2))))))

;;; COMBINING SEPARATE ARRAYS

;;; OK [53] combines subj-mod and obj-mod into one in a way that satisfies rel
(defun combine(rel s-co o-co subj-mod obj-mod)
(print '(combine is called))
(print-arr subj-mod)(print-arr obj-mod)
(let* ((tmp (dims-n-orig rel (array-dimensions subj-mod)
                         (array-dimensions obj-mod) s-co o-co))
       (new-dims (mapcar #'car tmp))(new-s-orig (mapcar #'cadr tmp))
       (new-o-orig (mapcar #'caddr tmp))
       (outarr (copy-array subj-mod nil new-dims new-s-orig))
       (outarr (copy-array obj-mod outarr nil new-o-orig)) )
outarr ))

;;; OK [54] works out dims of new array into which to combine two arrays and their
;;; new origins within the array.  Rtns list of triplets '((5 3 0)(3 0 0) ...)
;;; each triplet = new d, component of new origin for s-array, and component
;;; of new origin for o-array
(defun dims-n-orig (rel s-dims o-dims s-co o-co)
(cond((null rel) nil)
     ((> (car rel) 0) (cons(list (+ (car s-dims)(car o-dims))(car o-dims) 0)
           (dims-n-orig (cdr rel)(cdr s-dims)(cdr o-dims)(cdr s-co)(cdr o-co))))
     ((< (car rel) 0) (cons(list (+ (car s-dims)(car o-dims)) 0 (car s-dims))
           (dims-n-orig (cdr rel)(cdr s-dims)(cdr o-dims)(cdr s-co)(cdr o-co))))
     (t  (cons(ortho (car s-co)(car o-co)(car s-dims)(car o-dims))
           (dims-n-orig (cdr rel)(cdr s-dims)(cdr o-dims)(cdr s-co)(cdr o-co))))))

;;; OK [55] called by dims-n-orig where a dimension is orthogonal to one specified in
;;; rel, i.e. value of digit in rel = 0. rtns list of 3 digits: new d for
;;; combined array, component of new s-orig in it, and component of new o-orig
;;; in it
(defun ortho (sd od arrs-d arro-d)
(cond((> sd od)(list (+ sd (- arro-d od)) 0 (- sd od)))
     ((< sd od)(list (+ od (- arrs-d sd)) (- od sd) 0 ))
     ((> arrs-d arro-d)(list arrs-d 0 0))
     (t (list arro-d 0 0))))

;;; VERIFYING PROPOSITIONS IN MODELS

;;; OK [56] rtns mod iff subj-co is rel'd to obj-co in mod, otherwise rtns nil
;;; if one or other itm is not in mod also rtns nil
(defun verify (prop mod)
(let* ((rel (relfn prop))(subj (subjfn prop))(obj (objfn prop))
       (subj-co (finders subj mod))(obj-co (finders obj mod)))
(veri rel subj-co obj-co mod)))

;;; OK [57] rtns model if rel holds between subj-coords and obj-coords in mod
;;; otherwise when scan falls off edge of mod rtns nil
(defun veri (rel subj-co obj-co mod)
(let ((obj-co (list-add rel obj-co)))
(cond((outside obj-co (dimensions mod)) nil)
     ((equal subj-co obj-co) mod)
     (t (veri rel subj-co obj-co mod)))))

;;; RECURSIVE REVISION OF MODELS

;;; OK [58] called by deci when a prop is false in a mod, it calls make to try to
;;; revize model to make prop true. If succeeds rtns new mod to interpret,
;;; otherwise it rtns nil.  Input prop is list, i.e. ((1 0 0)(V) (O))
;;; not really nec to verify prop in model rtn to allow for alternative
;;; model in which prop is still false
(defun make-true (prop mod)
 (print 'make-true-prop)(princ prop)
(let* ((prems (remove-prem prop *premises*)) ;; premises are global in interpret
       (newmod (make (list prop) (list prop) mod prems)) )
(cond((and (not(null newmod))(verify prop newmod))
        (terpri)(prin-lis '(Premise was previously possibly false)) newmod)
     (t (terpri)
        (prin-lis '(Premise is inconsistent with previous premises)) nil))))

;;; OK [59] In trying to falsify, say, the cross is on the left of the circle, the program constructs
;;; a representation of its negation, i.e. the cross is on the right of the circle;  it
;;; changes the two items around to satisfy this negation, and then tries to modify the rest
;;; of the model so that it accomodates both the previous premises and this change. If it
;;; succeeds, it has falsified the proposition, and so it rtns original mod with comment
;;; below;  if it fails, premise follows validly.  Removes premise = prop from premises
;;; because make deals with negated premise.  Whatever happens in modifying the model, the
;;; negated premise (the cross is on the right of the circle) must not be destroyed.  Hence,
;;; one can move either (or both) items, but not into positions that falsify the relation,
;;; and so the second list = negprop must be handed down to make to prevent its violation
(defun make-false (prop mod)
(print 'make-false-prop)(princ prop)
(let* ((prems (remove-prem prop *premises*))(negprop (negate-prop prop))
       (newmod (make (list negprop) (list negprop) mod prems)))
(cond((and (not (null newmod))(verify negprop newmod))
         (terpri)(prin-lis '(Premise was previously possibly true)) mod)
     (t
         (terpri)(prin-lis '(Premise follows validly from previous premises)) mod) )))

;;; OK [60] rtns new model if no prems conflict with it, and rtns nil if mod becomes nil
;;; otherwise calls switch to construct mod that makes true first prop in proplis (provided
;;; that it is consistent with all the props in fixprops, which is initially just the
;;; prop set by make-true or make-false, but each time a model is formed to make a hitherto
;;; conflicting prem hold, then the corresponding prop is added to fixprops -- see the
;;; recursive call that shifts car.prop-lis to head of fixprops
(defun make(prop-lis fixprops mod prems)
(print 'make-with-proplis)(princ prop-lis)
(cond((null prop-lis)
        (cond((setf prop-lis (conflict prems mod))(make prop-lis fixprops mod prems))
             (t mod)))
     ((null mod) nil)
     ((and (setf mod (switch (car prop-lis) fixprops mod))
           (not (print-arr mod)))
        (make (cdr prop-lis) (cons (car prop-lis) fixprops) mod prems))))

;;; OK [61] called by make-false to remove premise = prop from premises
(defun remove-prem(prop premises)
(cond((null premises) nil)
     ((equal prop (parse (car premises)))(remove-prem prop (cdr premises)))
     (t (cons (car premises)(remove-prem prop (cdr premises))))))

;;; OK [62] rtns list of props corresponding to those premises false in model, excl
;;; those containing referents that are not in the model
(defun conflict (premises mod)
(let ((prop nil) (subj nil)(obj nil))       ;; parse rtns nil if nil.car.premises
(cond((null premises) nil)
     ((and (setf prop (parse (car premises)) subj (subjfn prop) obj (objfn prop))
           (or (not(finders subj mod)) (not(finders obj mod))))
                  (conflict (cdr premises) mod))
     ((verify prop mod)(conflict (cdr premises) mod))
     (t (cons prop (conflict (cdr premises) mod))))))

;;; OK [63] rtns list of props that are false in mod
(defun conflictprops(props mod)
(cond((null props) nil)
     ((verify (car props) mod)(conflictprops (cdr props) mod))
     (t (cons (car props)(conflictprops (cdr props) mod)))))

;;; OK [64] if required-rel is converse of one in mod tries to swap subj and obj items, otherwise
;;; (or if swap yields model conflicting with item in fixprops) tries to
;;; move subj, and if that fails, tries to move obj.  Otherwise rtns nil.
(defun switch (newprop fixprops mod)
(let* ((rel (relfn newprop))(subj (subjfn newprop))(obj (objfn newprop))
       (s-co (finders subj mod))(o-co (finders obj mod))(newmod nil) )
(cond((and (equal (find-rel-prop s-co o-co)(convert rel))
           (setf newmod (copy-shrink-arr (swap subj s-co obj o-co mod)))
           (not (conflictprops fixprops newmod)) )     newmod)
     ((and (setf newmod (copy-shrink-arr (move subj s-co rel o-co mod)))
            (not (conflictprops fixprops newmod)) )     newmod)
     ((and (setf newmod (copy-shrink-arr (move obj o-co (convert rel) s-co mod)))
           (not (conflictprops fixprops newmod)) )     newmod))))

;;;   switch
;;;      relfn |
;;;      subjfn |
;;;      objfn |
;;;      finders
;;;      find-rel-prop
;;;      swap
;;;        remove-item
;;;           rem-itm-lis |
;;;        add-item
;;;      copy-shrink-arr
;;;      conflictprops
;;;      move
;;;        remove-item
;;;           rem-itm-lis |
;;;        add-item
;;;        newposn
;;;           compare-ints |
;;;        update-co-ords
;;;      convert |



;;; OK [65] finds the difference in co-ords between s-co and o-co and then
;;; normalizes to express the semantics of the rel between them
(defun find-rel-prop(s-co o-co)
(let ((vector (list-subtract s-co o-co)))
(normalize vector)))

;;; OK [66] takes a vector and reduces each entry to 1, -1, or 0
(defun normalize(vec)
(mapcar #'(lambda (x)
(cond((> x 0) 1)
     ((< x 0) -1)
     ((= x 0) 0))) vec))

;;; OK [67] moves the item at it-co into rel with other-coords in model
;;; uses update-co-ords and rel of '(0 0 0) so as to put item into
;;; cell even if cell already occupied, i.e. only the co-ord concerning
;;; rel can be changed.  Rtns model
(defun move(item item-co rel other-co mod)
(let* ((mod (remove-item item item-co mod))
       (mod (add-item (newposn item-co rel other-co
              (update-co-ords other-co rel)) '(0 0 0) item mod)))
mod))

;;; OK [68] calls compare-ints to work out co-ords to which item is to be moved
;;; The motivation is to allow an item normally to move only along one dimension
(defun newposn(item-co rel other-co co-ords)
(cond((null item-co) nil)
     (t (cons (compare-ints (car item-co)(car rel)(car other-co)(car co-ords))
              (newposn (cdr item-co)(cdr rel)(cdr other-co)(cdr co-ords))))))

;;; OK [69] constructs new integer in co-ords of new position
;;; if integer of rel is 1 rtns int of item-co-ords if > int in other-cords
;;; but if integer of rel is -1 rtns int of item-co-ords if < int in other-cords
;;; otherwise rtns int from updated co-ords inc case where integer of rel is 0
(defun compare-ints(item-co-int rel-int other-co-int co-ords-int)
(cond((and (> rel-int 0)(> item-co-int other-co-int)) item-co-int)
     ((and (< rel-int 0)(< item-co-int other-co-int)) item-co-int)
     (t co-ords-int)))

;;; OK [70] swaps positions of subj and obj in mod, takes care to remove subj from
;;; its cell in case in shares it with other items, likewise obj
;;; rtns mod
(defun swap(subj s-co obj o-co mod)
(let* ((mod (remove-item subj s-co mod))
       (mod (remove-item obj o-co mod))
       (mod (add-item s-co '(0 0 0) obj mod))
       (mod (add-item o-co '(0 0 0) subj mod)))
mod))

;;; OK [71] removes item from contents at co-ords of mod, i.e. replaces contents
;;; minus item.  Copies array so that subsequent setf does not destroy original
;;; rtns revised mod
(defun remove-item(item co-ords mod)
(let* ((cell-lis (rem-itm-lis (car item) (apply #'aref mod co-ords)))
      (mod (copy-array mod nil nil '(0 0 0))))
(setf (apply #'aref mod co-ords) cell-lis)
mod))

;;; NEGATION OF PROPS

;;; OK [72] negates prop, i.e. neg of '(1 0 0) is (-1 0 0) but NOT (0 0 0)
(defun negate-prop (prop)
(let ((rel (relfn prop))(args (cdr prop)))
(cons (convert rel) args)))

;;; FUNCTIONS RETRIEVING ELEMENTS OF PROP

;;; [73]
(defun relfn(prop)
(car prop))

;;; [74]
(defun subjfn(prop)
(cadr prop))

;;; [75]
(defun objfn(prop)
(caddr prop))


;;;;;  ARRAYS for representing space

;;; To get content from cell 0 0 0 in array, arr1, use
;;;  (aref arr1 0 0 0) or  (apply #'aref arr1 li) where li = '(0 0 0).
;;; To put 'a into cell 0 0 0 in an array, arr1, use
;;; (setf (aref arr1 0 0 0) 'a) or
;;; (setf (apply #'aref arr1 li) 'a), where li = '(0 0 0)

;;; A test array
 (setf arr2
 (make-array '(2 2 2) :adjustable t))

 (setf (aref arr2 0 0 0) '(0 0 0))
 (setf (aref arr2 1 0 0) '(1 0 0))
 (setf (aref arr2 0 1 0) '(0 1 0))   ;;; yields 000 100
 (setf (aref arr2 1 1 0) '(1 1 0))   ;;;        010 110
 (setf (aref arr2 0 0 1) '(0 0 1))   ;;;
 (setf (aref arr2 1 0 1) '(1 0 1))   ;;;        001 101
 (setf (aref arr2 0 1 1) '(0 1 1))   ;;;        011 111
 (setf (aref arr2 1 1 1) '(1 1 1))

(setf arr3
 (make-array '(2 2 2) :adjustable t))

 (setf (aref arr3 0 0 0) nil)
 (setf (aref arr3 1 0 0) '(1 0 0))
 (setf (aref arr3 0 1 0) nil)        ;;; yields . 100
 (setf (aref arr3 1 1 0) '(1 1 0))   ;;;        . 110
 (setf (aref arr3 0 0 1) nil)        ;;;
 (setf (aref arr3 1 0 1) '(1 0 1))   ;;;        . 101
 (setf (aref arr3 0 1 1) nil)        ;;;        . 111
 (setf (aref arr3 1 1 1) '(1 1 1))

(setf arr4
 (make-array '(2 2 2) :adjustable t))

 (setf (aref arr4 0 0 0) '(0 0 0))
 (setf (aref arr4 1 0 0) nil)
 (setf (aref arr4 0 1 0) '(0 1 0))   ;;; yields 000 .
 (setf (aref arr4 1 1 0) nil)        ;;;        010 .
 (setf (aref arr4 0 0 1) '(0 0 1))   ;;;
 (setf (aref arr4 1 0 1) nil)        ;;;        001 .
 (setf (aref arr4 0 1 1) '(0 1 1))   ;;;        011 .
 (setf (aref arr4 1 1 1) nil)

;;; to adjust an array, which must contain :adjustable t, as above,
;;; (setf arr3 (adjust-array arr2 '(3 3 3))) ;; adjust-array is destructive

;;; COPYING ARRAYS

;;; OK [76] copies inarr to outarr.  If null.outarr makes one with newdims, and
;;; if null.newdims, makes exact copy.   Neworigin specifies origin of inarr
;;; within outarr. NB newdims must be larger than those of original inarr!
;;; If new-origin is '(0 0 0) then inarr located at origin of outarr, otherwise
;;; it can be located elsewhere by setting value of new-origin, say,to '(1 0 0)
;;; which shifts array to right by 1. NB if one subsequently setf's a new value
;;; in a cell of outarr, then inarr is not changed, and likewise subsequent
;;; change to inarr has no effect on outarr
(defun copy-array (inarr outarr newdims new-origin)
(cond((null outarr)
        (cond((null newdims)(setf newdims (array-dimensions inarr))))
        (setf outarr (make-array newdims :initial-element 'nil))))
;; (print-arr inarr)
(copy-arr inarr outarr (dimensions inarr) new-origin)
;; (print-arr outarr)
outarr)

;;; OK [77] called by copy-array, it changes outarr destructively, and rtns nil
;;; It can copy INARR into OUTARR at new-origin,or to (0 0 0) in outarr.
;;; Dims are of the inarr and so may be smaller than those of outarr
;;; remaining cells will be left null
(defun copy-arr (inarr outarr dims new-origin)
(let ((it nil)(outdims nil)(posn nil))
(cond((not (find-neg-num dims))   ;; neg num means dim has been reduced < 0
        (setf it (apply #'aref inarr dims))
        (setf outdims (list-add dims new-origin))
        (setf (apply #'aref outarr outdims) it)
        (copy-arr inarr outarr (cons (- (car dims) 1)(cdr dims)) new-origin))
     ((< (setf posn (find-neg-num dims))(- (length dims) 1))
        (copy-arr inarr outarr (update posn dims (dimensions inarr)) new-origin)))))

;;; OK [78] rtns list of no's of items on each dimension counting from 0
(defun dimensions (arr)
(let ((dims (array-dimensions arr))) ;; creates list e.g. '(2 3 4)
(mapcar #'(lambda(x) (- x 1)) dims)))  ;;; subtracts 1 from each to make
                                      ;;; suitable for manipulating arrs

;;; SHRINKING ARRAYS, i.e. removing empty rows or columns

;;; [79] Fn that controls shrink of arr to newarr by eliminating empty row, col, or plane slices
;;; It workouts dlist of d's to eliminate; makes new arr using newd's computed by newds
;;; and calls copy-shr-arr to copy oldarr to newarr working out new coords
;;; if no empty slices as shown by conditional test, merely rtns unaltered oldarr
(defun copy-shrink-arr(oldarr)
(cond((null oldarr) nil)  ;; incase oldarr is nil
(t (let* ((dlists (shrink-arr oldarr))
          (newarr (newds oldarr dlists))
          (oldims (dimensions oldarr)))
  ;   (terpri)
  ;   (print-arr oldarr)
     (cond((and(null (car dlists))(null (cadr dlists))(null (caddr dlists))) oldarr)
          (t (copy-shr-arr oldarr (car oldims)(cadr oldims)(caddr oldims) dlists newarr)))))))

;;; [80] works thru old arr copying it to newarr, working out newcoords to eliminate empty
;;; slices
(defun copy-shr-arr(oldarr d1 d2 d3 dlists newarr)
; (print 'ds)(princ (list d1 d2 d3))(print 'dlists)(princ dlists)(print 'newarr)(print-arr newarr)
(let* ((cell nil)(coords nil)(dims (dimensions oldarr))(od1 (car dims))(od2 (cadr dims)))
(cond((>= d3 0) ;; works through set of 2d planes
    (cond((>= d2 0) ;; works through each plane
        (cond((>= d1 0) ;; works through each row
                  (setf cell (apply #'aref oldarr (list d1 d2 d3)))
                  (cond((setf coords (newcoords d1 d2 d3 dlists))
                              ;  (print 'coords)(princ coords)
                                (setf (apply #'aref newarr coords) cell)))
                  (copy-shr-arr oldarr (- d1 1) d2 d3  dlists newarr))
             (t (copy-shr-arr oldarr od1 (- d2 1) d3 dlists newarr))))
         (t (copy-shr-arr oldarr od1 od2 (- d3 1) dlists newarr))))
     (t newarr))))

;;; OK [81] rtns a list of three lists each containing the d's equal to empty column slices,
;;; row slices, and plane slices.  First generates for each a list equal to all d's, e.g.
;;; '(3 2 1 0) (1 0) (0) and then calls shrink, which whenever it detects a non-null
;;; cell in arr removes the three corresponding co-ords from these three lists
;;; The array should then be copied with empty column(s) deleted from it
(defun shrink-arr(arr)
(let* ((dims (dimensions arr))(d1 (car dims))(d2 (cadr dims))(d3 (caddr dims))
       (cols (list-ints d1))(rows (list-ints d2))(plas (list-ints d3)) )
; (print (list cols rows plas))
(shrink d1 d2 d3 arr cols rows plas)
))

;;; OK [82] returns the list of d's equal to empty column slices, row slices, and plane slices
(defun shrink(d1 d2 d3 arr cols rows plas)
(let* ((dims (dimensions arr))(ori-d1 (car dims))(ori-d2 (cadr dims)))
(cond((and (null cols)(null rows)(null plas))(list nil nil nil))
     ((>= d3 0)  ;; works through set of 2d planes
      (cond((>= d2 0) ;; works through each plane
        (cond((>= d1 0)  ;; works through each row
          (cond((null (apply #'aref arr (list d1 d2 d3)))
                              (shrink (- d1 1) d2 d3 arr cols rows plas))
               (t (shrink (- d1 1) d2 d3 arr (rem-n d1 cols)(rem-n d2 rows)(rem-n d3 plas)))))
             (t  (shrink ori-d1 (- d2 1) d3 arr cols rows plas))))
           (t (shrink ori-d1 ori-d2 (- d3 1) arr cols rows plas))))
     (t (list cols rows plas)))))

;;; OK [83] calls shrink-arr to rtn list of lists of empty ds and dims of oldarr and works out
;;; dims for new arr
(defun newds (oldarr dlists)
(let* ((oldims (array-dimensions oldarr))
       (d1 (- (car oldims)(length (car dlists))))
       (d2 (- (cadr oldims)(length (cadr dlists))))
       (d3 (- (caddr oldims)(length (caddr dlists))))
       (newarr (make-array (list d1 d2 d3) :initial-element 'nil)) )
newarr))

;;; OK [84] rtns new set of coords for an item in order to shrink array, but if one of the
;;; co-ords is nil, because oldco = di of empty slice, then returns nil
(defun newcoords(old1 old2 old3 dlists)
(let ((d1 (newco old1 (car dlists)))(d2 (newco old2 (cadr dlists)))
      (d3 (newco old3 (caddr dlists))))
(cond((or (null d1)(null d2)(null d3)) nil)
     (t (list d1 d2 d3)))))

;;;  OK [85] takes one co-ordinate of item in oldarr and checks its relation to items in colis, which
;;;  is list of d's for that dimension that are empty;  if oldco is in this list, then
;;;  nothing is going to be copied to newarr;  if oldco is smaller than all d's in colis,
;;;  i.e. n = 0, then same co-ord is used in copying to newarr;  otherwise, new co-ord
;;;  equal oldco minus n, i.e. number of smaller items in colis
(defun newco(oldco colis)
(let ((n (nitms oldco colis)))
(cond((member oldco colis)  nil)  ;; signifies that cell is part of slice to be removed
     ((= n 0) oldco)  ;; oldco-ord is < than any item on dlis, thus has same value in newarr
     ((> n 0)(- oldco n)))))

;;; OK [86] rtns the number of items in colis that co is larger than, e.g. 3 '(5 2 1) => 2
(defun nitms (co colis)
(cond((null colis) 0)
     ((> co (car colis))(+ 1 (nitms co (cdr colis))))
     (t (nitms co (cdr colis)))))

;;; OK [87] makes a list of integers from n down to 0
(defun list-ints(n)
(cond((= n 0) (list 0))
     (t (cons n (list-ints (- n 1))))))

;;; OK [88] removes all intances of number, n, from lis
(defun rem-n (n lis)
(cond((null lis) nil)
     ((eq n (car lis))(rem-n n (cdr lis)))
     (t (cons (car lis)(rem-n n (cdr lis))))))

;;; NOT USED IN PROGRAM
;;; OK rtns t iff lis1 and lis2 contain same items regardless of order, but '(b a) is
;;; not equal to '(b a a), i.e. exact numbers of tokens must be same
(defun eq-sets(lis1 lis2)
(cond((equal lis1 lis2) t)
     ((check-member (car lis1) lis2)(eq-sets (cdr lis1)(rem-one-lis (car lis1) lis2)))))

;;; PRINTING ARRAYS
;;; print-arr.
;;;    prin-plane.
;;;      len-in-cell. ||
;;;      positn. ||
;;;      prin-space. ||
;;;      prin-cell. ||
;;;    max-len.
;;;      replac. ||
;;;      positn. ||
;;;      compare.
;;;       len-in-cell. ||
;;;    len-lis.
;;;       len-in-cell. || (see above)

;;; OK [89] Prints arrays, where d1 is left-right, d2 is front-behind, d3 is up-down
;;; and each 2D array is horizontal slice starting from bottom
(defun print-arr (arr)
(terpri)
(cond((null arr) nil)
     (t (let* ((dims (dimensions arr)) ;; rtns '(2 3 4) subtracting 1 from each
               (vector (reverse (len-lis dims arr))) ;;; rtns max len needed for each column
               ; (princ 'vector)(print vector)(terpri)
               (vector (max-len 0 0 0 dims arr vector))) ;; checks width for each column
               ; (princ 'revised-vector)(print vector)(terpri)
              (prin-plane 0 0 0 dims arr vector)))))

;;; OK [90] prints 3d array: first d is left-right, second d is down, and third
;;; yields separate planes
(defun prin-plane (c1 c2 c3 dims arr vector)
(let ((itms nil))
(cond((<= c3 (caddr dims))
  (cond((<= c2 (cadr dims))
      (cond((<= c1 (car dims))
            (setf itms (apply #'aref arr (list c1 c2 c3)))
            (prin-cell itms)
            (prin-space (- (positn c1 vector)(len-in-cell itms)))
            (prin-plane (+ 1 c1) c2 c3 dims arr vector))
           (t (terpri)(prin-plane 0 (+ 1 c2) c3 dims arr vector))))
       (t (terpri)(terpri)(prin-plane 0 0 (+ 1 c3) dims arr vector)))))))

;;; OK [91] rtns a reversed-list of lengths of each item in first row of array
;;; reversed to correct order by print-arr
(defun len-lis (dims arr)
(cond((< (car dims) 0) nil)
     (t (cons (len-in-cell (apply #'aref arr dims))
              (len-lis (cons (- (car dims) 1)(cdr dims)) arr)))))

;;; OK [92] rtns number of chars and spaces in cell, which is a list such as
;;; '(aa bbb c)
;;; NB (string(princ-to-string x)) converts value of x, such as 'a1, to
;;; a string so that its length can be measured
(defun len-in-cell(cell)
(cond((null cell) 1)   ;; 1 for extra parenthesis
     (t (+ (+ 1 (length(string(princ-to-string (car cell))))) ;; 1 for space
           (len-in-cell (cdr cell))))))

;;; OK [93] given vector of lengths of first row in arr, works out maximum no of
;;; items in lists in each cell and returns revised vector
(defun max-len (c1 c2 c3 dims arr vector)
(let ((it nil)(ve nil))
(cond((<= c3 (caddr dims))
  (cond((<= c2 (cadr dims))
      (cond((<= c1 (car dims))
         (cond((compare (setf it (apply #'aref arr (list c1 c2 c3)))
                        (setf ve (positn c1 vector))) ;; ve doesn't seem to be used!
                 ; (princ 'item)(princ it)(terpri)
                 ; (princ 'vect)(princ ve)(terpri)
                  (max-len (+ 1 c1) c2 c3 dims arr
                           (replac (len-in-cell it) c1 vector)))
              (t (max-len (+ 1 c1) c2 c3 dims arr vector))))
           (t (max-len 0 (+ 1 c2) c3 dims arr vector))))
       (t (max-len 0 0 (+ 1 c3) dims arr vector))))
     (t vector))))

;;; LOW-LEVEL FNS FOR ARRAYS
;;; OK [94] replaces ith member (from 0) of lis by item, if ith > len of lis then
;;; replaces last member of lis
(defun replac (item ith lis)
(cond((< ith 0) lis)
     ((> ith 0) (cons (car lis)(replac item (- ith 1)(cdr lis))))
     (t (cons item (replac item (- ith 1)(cdr lis))))))

;;; OK [95] rtns member of lis in ith positn, starting from 0
(defun positn (ith lis)
(cond((< ith 0) nil)
     ((= ith 0) (car lis))
     (t (positn (- ith 1)(cdr lis)))))

;;;OK [96] compares length of item with no. in vect
(defun compare (item vect)
(> (len-in-cell item) vect))

;;; NOT USED IN PROGRAM
;;; OK removes one token of itm from lis
(defun rem-one-lis(itm lis)
(cond((null lis) nil)
     ((equal (car lis) itm)(cdr lis))
     (t (cons (car lis)(rem-one-lis itm (cdr lis))))))

;;; OK [97] finds neg number in dims and restores it to the value in original-dims
;;; and subtracts one from the number after it
;;; e.g. (update 1 '(3 -1 2 3) 4) -> '(3 4 1 3)
(defun update(posn dims original-dims)
(cond((null dims) nil)
     ((= posn 0)
        (cons (car original-dims) (cons (- (cadr dims) 1)
              (update (- posn 2)(cddr dims) original-dims))))
     (t (cons (car dims)(update (- posn 1)(cdr dims) (cdr original-dims))))))

;; OK [98] rtns position, from 0 to n, of negative number in list; if none rtns nil
(defun find-neg-num(lis)
(cond((is-neg-num lis)(posn-neg-num lis))))

;;; OK [99] detects presence of negative number in lis
(defun is-neg-num(lis)
(cond((null lis) nil)
     ((< (car lis) 0) t)
     (t (is-neg-num (cdr lis)))))

;;; OK [100] given there is a negative number in list, rtns its posn from 0 to n
(defun posn-neg-num(lis)
(cond((null lis) -1)
     ((< (car lis) 0) 0)
     (t (+ 1 (find-neg-num (cdr lis))))))

;;;;;  GENERAL LOW-LEVEL FUNCTIONS

;;; OK [101] rtns models minus mod
(defun extract (mod models)
(cond((null models) nil)
     ((equal mod (car models))(extract mod (cdr models)))
     (t (cons (car models)(extract mod (cdr models))))))

;;; OK [102] switches pos nums to neg, and vice versa, leaving 0's unchanged
(defun convert(rel)
(cond((null rel) nil)
     ((>= (car rel) 0)(cons (- (car rel))(convert (cdr rel))))
     ((< (car rel) 0)(cons (abs (car rel))(convert (cdr rel))))))

;;; OK [103] rtns t iff item is in lis
(defun check-member (item lis)
(cond((null lis) nil)
     ((equal item (car lis)) t)
   ;  ((equal 'o (caar lis))(equal item (cdar lis))) ;; so b1 matches o b1
     (t (check-member item (cdr lis)))))

;;; NOT USED IN PROGRAM
;;; OK removes each member of lis1 from lis2
(defun rem-lis-lis(lis1 lis2)
(cond((null lis1) lis2)
     ( t (rem-itm-lis (car lis1)(rem-lis-lis (cdr lis1) lis2)))))

;;; OK [104] removes all occurrences of item from lis
(defun rem-itm-lis(it lis)
(apply #'append (mapcar #'(lambda(x)
(cond((equal x it) nil)
     (t (list x)))) lis)))

;;;;; PRINTING FUNCTIONS

;;; [105]
(defun prin-cell(lis)
(cond((null lis)(princ "."))
     (t (princ lis))))

;;; OK [106] prints item or "." after last item in lis
(defun prin-lis (lis)
(cond((null (cdr lis))(princ (car lis))(princ "."))
     (t (princ (car lis))(prin-space 1)(prin-lis (cdr lis)))))

;;; OK [107] prints n spaces
(defun prin-space(n)
(cond((= n 0)(princ " "))
     (t (princ " ")(prin-space (- n 1)))))

;;; END OF FILE





