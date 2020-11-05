#-------------------------------------------------------------------------------
# Name:        Spatial Parser
# Purpose:     Parser for spatial premises, based on LISP code developed by
#              PN Johnson-Laird and R.Byrne as part of their 1991 book
#              'Deduction' and their 1989 paper 'Spatial Reasoning'.
#
# Author:      Ashwath Sampath
# Based on: http://mentalmodels.princeton.edu/programs/space-6.lisp
# Created:     22-04-2018
# Copyright:   (c) Ashwath Sampath 2018
#-------------------------------------------------------------------------------
""" Parser for spatial premises, based on LISP code developed by
PN Johnson-Laird and R.Byrne as part of their 1991 book 'Deduction'
and their 1989 paper 'Spatial Reasoning'.  """

import copy
from . import parse_helper as helper

class Stack:
    """ An auxiliary class to implement a stack data structure, with push,
    pop and other operations. """
    def __init__(self):
        self.stack = []

    def push(self, item):
        """" Pushes item into the top (1st element) of the list items."""
        self.stack.insert(0, item)

    def pop(self):
        """" Pops an item fron the end (top) of the list items."""
        return self.stack.pop(0)

    def top(self):
        """" Returns the last element of the list items. """
        return self.stack[0]

    def nth_from_top(self, nth):
        """ Returns the nth element from the top of the stack: 0: 1st ele. """
        return self.stack[nth]

    def is_empty(self):
        """ Returns True if the list items is empty, false otherwise. """
        return self.stack == []

    def size(self):
        """ Returns the size of the list stack. """
        return len(self.stack)

    def rest_of_stack(self, index):
        """ Returns all elements after the element at index 'index' in stack,
        till the end of the stack. """
        return self.stack[index+1:]

class SpatialParser():
    """ This class contains functions to parse a spatial reasoning premise.
    such as 'The circle is in front of the square' """

    def __init__(self):
        """ Global variables for cfgrammar and lexicon, sen and the
        stacks (history and pstack) defined here."""
        self.rel_grammar = [
            [['NP-sing', 'PRED'], [['S', 1], 's_prop']],
            [['NP-sing', 'NEGPRED'], [['S', 2], 's_neg_sem']],
            [['art-def', 'N-sing'], [['NP-sing', 1], 'npfun']],
            [['rel'], [['reln', 1], 'npfun']],
            [['next-p', 'to-p'], [['reln', 1], 'npfun']],
            [['in-p', 'rel front-p', 'of-p'], [['reln', 1], 'npfun']],
            [['in-p', 'art-def', 'front-p', 'of-p'], [['reln', 1], 'npfun']],
            [['on-p', 'art-def', 'rel horiz', 'of-p'], [['reln', 1], 'npfun']],
            [['on-p', 'rel vert', 'of-p'], [['reln', 1], 'npfun']],
            [['in-p', 'art-def', 'adj-same', 'n-loc', 'as-p'],
             [['reln', 1], 'npfun']],
            [['in-p', 'art-indef', 'adj-different', 'n-loc', 'to-p'],
             [['reln', 1], 'npfun']],
            [['in-p', 'art-indef', 'adj-different', 'n-loc', 'from-p'],
             [['reln', 1], 'npfun']],
            [['V-cop', 'reln', 'NP-sing'], [['PRED', 1], 'pred']],
            [['V-cop', 'NEG', 'reln', 'NP-sing'], [['NEGPRED', 1],
                                                   'neg_pred_sem']]
        ]

        # [] is a dummy semantic value.
        self.rel_lexicon = {
            'a': ['art-indef', []], 'the': ['art-def', []],
            'not': ['neg', ['neg-semantics']], 'of': ['of-p', []],
            'as': ['as-p', []], 'is': ['V-cop', []], 'in': ['in-p', []],
            'next': ['next-p', ['next-semantics']], 'to': ['to-p', []],
            'from': ['from-p', []], 'on': ['on-p', []],
            'right': ['rel horiz', [1, 0, 0]],
            'left': ['rel horiz', [-1, 0, 0]],
            'front': ['rel front-p', [0, 1, 0]], 'behind': ['rel', [0, -1, 0]],
            'above': ['rel', [0, 0, -1]], 'top': ['rel vert', [0, 0, -1]],
            'below': ['rel', [0, 0, 1]],
            'beside': ['relat', ['beside-semantics']],
            'between': ['relat', ['between-semantics']],
            'among': ['relat', ['among-semantics']],
            'square': ['N-sing', ['[]']], 'triangle': ['N-sing', ['v']],
            'circle': ['N-sing', ['O']], 'line': ['N-sing', ['I']],
            'cross': ['N-sing', ['+']], 'ell': ['N-sing', ['L']],
            'vee': ['N-sing', ['^']], 'star': ['N-sing', ['*']],
            'ess':['N-sing', ['S']]
        }

        # Create a dict of semantic functions to avoid using eval to run
        # the semantic functions in the lhs of rel-grammar.
        # Note: I have retained neg_pred_sem and s_neg_sem semantic functions
        # in rel_grammar even though they aren't used simply because they
        # were present in the Lisp code.
        self.semantic_functions = {
            'npfun': helper.npfun,
            'pred': helper.pred,
            's_prop': helper.s_prop
        }
        # Global variables: sen is the list version of a (string) sentence
        # such as 'THE CIRCLE IS IN FRONT OF THE SQUARE'.
        self.sen = []
        self.sen_shifted = []
        self.history = Stack()
        self.pstack = Stack()

    def parse(self, sentence):
        """ This function tries to parse sentence by calling reduces or shift,
        and if they fail, it goes into backtrack mode. TMP is what is
        presently under examination. If it succeeds, it returns an intensional
        representation of the sentence (see Goodwin & Johnson Laird 2005:
        'Reasoning about Relations' for more details). For e.g., it returns
        [[1 0 0], ['[]'], ['V']] for the following premise:
        '(the square is on the right of the triangle) """

        # For 'generate all models, just return a list of an empty list.
        if sentence.lower().startswith("generate"):
            return [[]]
        # Make local copies of lexicon and grammar, as they will be modified
        lex = self.rel_lexicon
        gram = self.rel_grammar
        # reset pstack, sen-shifted and history to [] or "" each time parse
        # is called
        self.sen_shifted = []
        # Convert sen to a list for easier updates in shift.
        self.sen = sentence.split()
        self.pstack = Stack()
        self.history = Stack()
        # If the premise is "What is the relation between the x and the y",
        # it's a conclusion generation premise. No need of calling analyze
        # or shift, just get the semantics of the 2 tokens, and return them
        # with a dummy 'rel' component. E.g: [[], ['O'], ['[]']]
        if sentence.lower().startswith("what is the relation between"):
            return self.get_semantics_2_tokens(lex)
        while True:
            # When all the rules have been reduced, the syntactic category
            # on pstack is a sentence, represented by S.
            # The pstack is of the form  [['S', [[0, 1, 0], ['O'], ['[]']]]]
            # In this case, [[0, 1, 0], ['O'], ['[]']] is returned
            if not self.pstack.is_empty():
                if self.pstack.stack[0][0] == 'S' and self.sen == []:
                    return self.pstack.stack[0][1]
            # Analyze reduces rhs of grammar to lhs (non-terminals to
            # non-terminals) or terminals in lexicon to non-terminals (which
            # are part of the grammar),
            if self.analyze(gram, lex, None):
                continue
            # Shift a new token from sentence (self.sen) onto pstack
            if self.shift() is not None:
                continue
            # Backtrack is only called when both analyze and shift return None
            if self.backtrack(gram, lex):
                continue
            # Analyze, shift and backtrack failed
            print("Parse failed!")
            return None

    def get_semantics_2_tokens(self, lex):
        """ This function is used for generation premises. It gets the
        semantics of the 2 tokens in premises which start with 'What is the
        relation between'. It operates on self.sen, the list version of the
        original premise 'sentence' defined in parse(), and returns an
        intensional representation of the form [[], ['O'], ['[]']], where
        there is a dummy rel term followed by the semantics of the 2 tokens."""
        # self.sen = ["what", "is", "the", "relation", "between", "the", "x",
        #             "and", "the", "y"]
        subj = self.sen[6]
        obj = self.sen[9]
        subj_repr = lex[subj][1]
        obj_repr = lex[obj][1]
        return [[], subj_repr, obj_repr]

    def analyze(self, gram, lex, lhs):
        """ Normal PARSING tries to reduce word or syn-cat; also called by
        backtrack. LHS is None except when there are ambiguities (i.e., when
        called by backtrack. """
        # If reduces_word returns not None, its value is stored in tmp
        # regardless of what reduces_syn returns.
        tmp = self.reduces_word(lex, lhs)
        if tmp is None:
            tmp2 = self.reduces_syn(gram, lhs)
            if tmp2 is None:
                return False
            # Grammar term added to stack : tmp2 is not None
            self.history_construction(tmp2)
            return True
        # Lexical element added to stack: tmp is not None
        self.history_construction(tmp)
        return True

    def reduces_word(self, lex, lhs):
        """ If top of stack is word, replaces it with the appropriate
        syn cat from the lexicon. If lhs as a value, it returns next syn cat
        after it in lex; tmp is returned for history. The word's semantics
        are put on stacks too."""
        tmp = []
        # Return None if pstack is empty
        if self.pstack.is_empty():
            return None
        # Get the syntactic category of the word at the top of the stack
        # if it exists in lexicon. Otherwise, None is returned.
        tmp = helper.lexical_category(self.pstack.top(), lex, lhs)
        # Check if the word at the top of pstack is in lexicon and if
        # lexical_category returns the attached syntactic category.
        if helper.word(self.pstack.top(), lex) and tmp != None:
            # Pop out the LHS of lex (key pushed by shift()), and push
            # its corresponding syntactic category
            self.pstack.pop()
            self.pstack.push(tmp)
            return tmp
        return None

    def reduces_syn(self, gram, lhs):
        """ If top of pstack is nonterminal and there is a legal rule for
        reducing it including perhaps other symbols on stack, then symbol(s)
        on top of stack are replaced by reduction -- next reduction after lhs
         -- and new lhs of relevant rule is returned for history """
        if self.pstack.is_empty():
            return None
        list_of_rules = helper.rule_list(self.strip_sem(), gram)
        tmp = helper.syntax_rule(list_of_rules, lhs, gram)
        if tmp == []:
            return None
        for_hist = self.compose(tmp, gram)
        return for_hist

    def strip_sem(self):
        """ This function takes contents of the global pstack and strips the
        semantic interpretation from each item to leave a
        conventional syntactic stack. """
        return [ele[0] for ele in self.pstack.stack]

    def compose(self, rule, gram):
        """ This function takes a complete rule such as [['art-def', 'N-sing'],
        [['NP-sing',1], 'npfun']] and extracts newsyn from the LHS of the rule
        (NP-sing), newsem for the rule (npfun), and the list of semantic
        args from pstack. Then it applies the semantic function to args, if
        poss, and puts newsyn+newsem (result of composition) on pstack in
        place of symbols that rule has reduced. Finally, it returns the full
        lhs of rule including rule number and newsem for history construction
        """
        # Reduce RHS to LHS, pop RHS from stack, push LHS. Reduced acc to rule
        newsyn = helper.lhs_of_rule(rule, gram)[0]
        newsem = helper.sem_of_rule(rule)
        rhs = helper.rhs_of_rule(rule)
        # A deep copy of rhs is necessary: we need to only reverse the copy,
        # otherwise the original rule in gram will get modified.
        revrhs = copy.deepcopy(rhs)
        revrhs.reverse()
        # Get the corresponding semantic terms to the terms in revrhs from
        # the stack (args only has semantics)
        args = self.args_from_stack(revrhs) # Not passing pstack like in lisp
        sem_func = self.semantic_functions[newsem]
        #sem_func = eval('self.' + newsem)
        newsem = sem_func(args)
        if newsem == [] or newsem is None:
            print("No composition rule")
        # Remove the terms corresponding to the RHS in pstack, as RHS is
        # reduced to the term on the lhs.
        self.chop(helper.rhs_of_rule(rule))
        self.pstack.push([newsyn, newsem])
        # Return full lhs (non-terminal + rule no.) for history construction
        return [helper.lhs_of_rule(rule, gram), newsem]

    def args_from_stack(self, revrhs):
        """ This func takes the reverse rhs of the syntactic rule and returns
        a list of corresponding semantic elements from the global pstack,
        ready for the semantic function to be applied to them by compose."""
        semantics = []
        i = 0
        for i, syn in enumerate(revrhs):
            if syn == self.pstack.nth_from_top(i)[0]:
                semantics.append(self.pstack.nth_from_top(i)[1])
        return semantics

    def chop(self, lis):
        """ This function returns the stack minus same no. of items as in lis,
        i.e. those on the rhs of the rule prior to reducing  them to its lhs.
        """
        for _ in range(len(lis)):
            self.pstack.pop()
        # Nothing needs to be returned, it chops the global stack.

    def history_construction(self, tmp):
        """ This func. returns the appropriate history, both when tmp is an
        atom and when it is a list. """
        self.history.push(tmp)

    def shift(self):
        """ This function shifts the first word in sen to sen-shifted &
        returns it for history stack. No semantics needed for this operation.
        Note: sen and sen-shifted are lists made from the (string) premise"""
        if self.sen == []:
            return None
        # Pop the next word in the sentence, i.e. first element in sen
        next_word = self.sen.pop(0)

        self.pstack.push(next_word)
        self.sen_shifted.insert(0, next_word) # CHECK! Insert at end or start?
        self.history.push(next_word)
        return next_word

    def backtrack(self, gram, lex):
        """ When backtrack fails, necessary to call unshift to prevent
        endless loop. If history is [], then nothing is left to backtrack
        on. If top of stack is not a word, then it calls unreduces.
        Then if analyze or shift works it rtns to normal parsing but if
        both fail, it continues to backtrack. If top of stack is a word,
        then it unshifts it and continues backtracking. """
        if self.history.is_empty():
            return False
        if not helper.word(self.pstack.top(), lex):
            self.unreduces(gram, lex)
            lhs = self.history.pop()
            print("Reanalyze with lhs = {}".format(lhs))
            if self.analyze(gram, lex, lhs):

                print("Return to parsing.")
                return True
            if self.shift() is not None:
                print("Return to parsing.")
                return True
            print("Reanalysis failed")
            return self.backtrack(gram, lex)
        self.unshift()
        return self.backtrack(gram, lex)

    def unreduces(self, gram, lex):
        """ If top of history is a non-terminal, then it replaces top of
        pstack with rhs of grammatical rule + the composed semantics of
        its constituents. Otherwise, if top of history is a syn cat, it
        replaces it with the corresponding word from history."""
        if helper.non_term(self.history.top(), gram):
            self.unred(gram, lex)
        else:
            self.pstack.pop()
            self.pstack.push(self.history.nth_from_top(1))

    def unred(self, gram, lex):
        """ Function to unreduce an lhs symbol, such as (S,2). It removes
        the lhs from both stacks, and then pushes into pstack the
        constituents from the history stack that match the rhs of the rule,
        obtained by rewrite of lhs, while  ensuring that the appropriate
        semantics are put back on pstack. It also removes any rule numbers
        from the initial syntactic list for each item on the history stack
        before putting them on pstack"""
        lhs = self.history.top()
        self.pstack.pop()
##        # Get the whole rule corresponding to the lhs.
##        rule = helper.rewrite(lhs, gram)
##        rhs = helper.rhs_of_rule(rule)
        rhs = helper.expand(lhs, gram)
        revrhsrule = copy.deepcopy(rhs)
        revrhsrule.reverse()
        # Get al the elements except the first element in history.
        history = self.history.rest_of_stack(0)
        # Get elements from history which correspond to the syncats in
        # revrhsrule
        update_top = helper.copy_history(revrhsrule, history, lex)
        to_push = helper.drop_rule_no(update_top, lex)
        for pushed in reversed(to_push):
            self.pstack.push(pushed)

    def unshift(self):
        """ This function moves word off stacks & back from
        sen-shifted to sen. """
        print("Unshift from stacks")
        last_inserted = self.history.pop()
        self.pstack.pop()
        # Insert at beginning of sentence list
        self.sen.insert(0, last_inserted)
        # Pop first element from sen_shifted as it is in the reverse order
        self.sen_shifted.pop(0)

    def generate_conclusion(self, prop):
        """ Function which takes a proposition (intensional representation of
        a premise) like [[-1,0,0],['v'],['[]']], and returns the actual
        premise itself (which is not available)."""
        rel = prop[0]
        sub = prop[1]
        obj = prop[2]
        direction = [key for key, val in self.rel_lexicon.items() if
                     val[1] == rel]
        # direction is a list, get the string. For [0,0,-1], which can be
        # either 'on top of' or above', choose 'above'.
        direction = direction[0]
        token1 = [key for key, val in self.rel_lexicon.items() if
                  val[1] == sub][0]
        token2 = [key for key, val in self.rel_lexicon.items() if
                  val[1] == obj][0]
        if direction in ['right', 'left']:
            conclusion = "the {} is on the {} of the {}".format(token1,
                                                                direction,
                                                                token2)
        elif direction == 'front':
            conclusion = "the {} is in front of the {}".format(token1, token2)
        else:
            # behind, above and below
            conclusion = "the {} is {} the {}".format(token1, direction,
                                                      token2)
        return conclusion
