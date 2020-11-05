#-------------------------------------------------------------------------------
# Name:        Spatial Parser Helper functions
# Purpose:     A suite of functions which are used by the SpatialParser
#              class.
#
# Author:      Ashwath Sampath
# Based on: http://mentalmodels.princeton.edu/programs/space-6.lisp
# Created:     01-05-2018
# Copyright:   (c) Ashwath Sampath 2018
#-------------------------------------------------------------------------------
""" Module of functions used by the SpatialParser class in
spatial_parser.py. Based on LISP code developed by
PN Johnson-Laird and R.Byrne as part of their 1991 book
'Deduction' and their 1989 paper 'Spatial Reasoning'. """

import copy

def syntax_rule(lisrules, lhs, gram):
    """  SYNTACTIC CATEGORIES AND RULES
    This func. returns first of lisrules after item that matches lhs,
    i.e. a complete grammatical rule. Normally (when not called by
    backtractk), it just returns the first (only) rule in the lisrules list."""
    if lisrules == []:
        return []
    if lhs is None:
        return lisrules[0]
    # lhs is not none
    rhs = expand(lhs, gram)
    semantics = rule_semantics(lhs, gram)
    lis1 = [rhs, [lhs, semantics]]
    # Return the first rule after lis1 in lisrules. If lis1 is the last
    # rule of lisrules, member_lis returns [].
    result = member_lis(lis1, lisrules)[0]
    return result

def member_lis(lis1, lis2):
    """ If lis1 is last item in lis2, it returns the rest of lis2."""
    found_at = -1
    if lis1 is None or lis1 == []:
        return []
    for index, rule in enumerate(lis2):
        if lis1 == rule:
            found_at = index
            break
    # lis1 found at last pos in lis2, return [] as nothing is
    #lis2 after this.
    if found_at == len(lis2) - 1:
        return []
    # Return sub-lists after the index found_at, i.e return all
    # the elements in lis2 after element lis1.
    return lis2[found_at+1:]

def rule_list(syn_stack, gram):
    """ This function returns a list of rules (in complete form) whose
    expansions when reversed match the items at the top of the syn-stack
    (stack with semantic items stripped off), using matchrule. """
    list_of_rules = []
    for rule in gram:
        # A deep copy of rhs is necessary: we need to only reverse the copy,
        # otherwise the original rule in gram gets modified.
        rhs = rhs_of_rule(rule)
        revrhs = copy.deepcopy(rhs)
        revrhs.reverse()
        if match_rule(revrhs, syn_stack):
            list_of_rules.append(rule)
    return list_of_rules

def match_rule(revrule, syn_stack):
    """ This function matches reversed rhs of rule with syn-stack.
    It returns True if there is a match, false if there isn't. """
    if len(syn_stack) < len(revrule):
        return False
    for i, term in enumerate(revrule):
        if term != syn_stack[i]:
            return False
    return True

def lexical_category(item, lex, lexcat):
    """ This funtion returns category of item in lexicon, allowing
    for ambiguity in lexicon (through parameter lexcat). If the
    item doesn't exist in the lexicon, it returns None"""

    # if item is not a word (i.e. a terminal symbol), it will be a
    # list -> we can't get a lexical category.
    if isinstance(item, list):
        return None
    if item in lex:
        # E.g. lex[item] = ['art-indef', []]
        return legal_cat(lexcat, lex[item])
    print("symbol '{}' not in lexicon".format(item))
    return None

def legal_cat(lexcat, lis):
    """ This function takes lis and lexical category, lexcat, and
    returns next item in lis after lexcat or else if none, None.
    In practice, it takes a lexcat and the rhs of the
    lexicon it comes from and returns next lexcat if any """
    if lexcat is None:
        return lis
    # Otherwise, return 1st item after lexcat in lis.
    after_lexcat = member_lis(lexcat, [lis])
    if after_lexcat == []:
        # Lexcat is the last term of lus
        return None
    # Return next item after lexcat
    return after_lexcat[0]

def word(item, lex):
    """This function returns true if item is word in lexicon that has
    not been analyzed, i.e. it has no attached syntactic category"""
    # If item is a key in lex, return True
    if isinstance(item, list):
        return False
    if item in lex:
        return True
    return False

def sem_of_rule(rule):
    """ Given a grammatical rule, this function returns the semantic
    part of it. """
    return rule[1][1]

def rule_semantics(lhs, gram):
    """ Returns the semantic part of a given rule given its lhs.
    Eg. ['S',2] returns [['S', 2], 's_neg_sem']]"""
    for rule in gram:
        if lhs_of_rule(rule, gram) == lhs[0]:
            return sem_of_rule(rule)
    return None                                # CHECK

def lhs_of_rule(rule, gram):
    """ Given a rule such as (S 1) -> (NP-sing)(VP-sing), it
    returns its lhs, i.e (S 1) provided that rule is in the cfgrammar;
    otherwise it returns None. This func corresponds to functions
    lhs_of_rule and ruleInGrammar in the lisp code.  """
    if rule in gram:
        return rule[1][0]
    print("Rule not in grammar")
    return None

def rhs_of_rule(rule):
    """ This function takes a grammatical rule, and returns its RHS """
    return rule[0]

def rewrite(lhs, gram):
    """ Given lhs of the rule (e.g. ['NP-Sing', 1] , this function returns
    the complete rule"""
    for rule in gram:
        if lhs[0] == lhs_of_rule(rule, gram):
            return rule
    print("No rule in grammar for lhs = {}".format(lhs))
    return []

def non_term(symb, gram):
    """ Checks if symb is a non-terminal. If symb is lhs of a rule,
    e.g. 'S', this function returns True. Otherwise, it returns False."""
    # Check for word
    if not isinstance(symb, list):
        return False
    # Check for syn cat.
    if not isinstance(symb[0], list):
        return False
    for rule in gram:
        # lhs_of_rule returns lhs, for e.g. ['NP-sing', 1]
        if lhs_of_rule(rule, gram) == symb[0]:
            return True
    # symb not a non-terminal.
    return False

def expand(lhs, gram):
    """ Takes the lhs of a rule (S 1) -> NP VP, and returns its rhs."""
    for rule in gram:
        if lhs[0] == lhs_of_rule(rule, gram):
            return rhs_of_rule(rule)
    print("Reduction not in grammar")
    return []

def npfun(lis):
    """ Function which returns the first non [] item in lis  """
    for item in lis:
        if item != []:
            # Item will be a list
            return item
    return None

def pred(lis):
    """ This function moves the list representing a relation (first element
    of the list) AFTER relational term. """
    # Remove all dummy semantic elements.
    lis = [ele for ele in lis if ele != []]
    # Put the relational predicate in front of the token
    lis[0], lis[1] = lis[1], lis[0]

    return lis

def s_prop(lis):
    """ This function assmembles rel, arg1, arg2 together in a list.
    E.g. When lis is [[[1,0,0],['V']],['[]']], it returns
    [[1,0,0],['[]'],['V']] for the premise 'the square is to the
    right of the triangle'. """
    # Switch the order of the tokens we have the PRED part in one list
    # element (relation plus last token) and the NP-SING part (1st token
    # in the premise) in 2nd list element. Add them to a new list with
    # the order [relation, first-token, last-token].
    return [lis[0][0], lis[1], lis[0][1]]

def drop_rule_no(lis, lex):
    """ This func. takes items obtained from history, drops rule no. from
    syn part of each item => ready to push into pstack as part of unred"""
    # There are 3 types of elements in history, words, rhs in
    # gram/ term in lexicon (e.g. [V-cop', []] and Lhs in gram
    # (e.g. [['NP-sing', 1], ['O']]. We need to drop the rule no. from
    # the 3rd type -- lhs in gram.
    rule_number_absent = []
    for ele in lis:
        # words on history will not have rule no.s
        if word(ele, lex):
            rule_number_absent.append(ele)
            continue
        # No rule no.s in this type of element. [V-cop', []]
        if not isinstance(ele[0], list):
            rule_number_absent.append(ele)
            continue
        # pstack requires entries of the form ['NP-sing', ['O']] for
        #  [['NP-sing', 1], ['O']]
        tmp = [ele[0][0], ele[1]]
        rule_number_absent.append(tmp)
    return rule_number_absent

def copy_history(revrhs, hist, lex):
    """ This func. takes reversed rhs constituents of a rule, looks for
    their mates in history and returns a list of them, including their
    semantics. """
    rhs_in_history = []
    for syncat in revrhs:
        for element in hist:
            # If word is in history, indexing it will give an error
            if word(element, lex):
                continue
            # Check if syncats in rhs match a lexicon entry in history
            # E.g. revrhs = ['of-p', 'rel front-p', 'in-p'],
            # and history has ['of-p', []]
            # rhs of rule/lex element in history
            if element[0] == syncat:
                rhs_in_history.append(element)
                continue
            # lhs of rule in history, separate if needed as previous if
            # will have index out of bounds.
            if element[0][0] == syncat:
                rhs_in_history.append(element)
    return rhs_in_history
