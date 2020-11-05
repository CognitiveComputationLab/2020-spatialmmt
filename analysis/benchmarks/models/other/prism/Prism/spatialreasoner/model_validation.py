#-------------------------------------------------------------------------------
# Name:        Spatial Reasoning Model Validation
# Purpose:     Module of functions which perform model validation and
#              variation. When a conclusion premise is reached, the conclusion
#              has to be verified in the mental model. If it is true in the
#              mental model, we look for alternative models where it is false.
#              If it is false in the mental model, we look for alternative
#              models in which it is true.
#
# Author:      Ashwath Sampath
# Based on: http://mentalmodels.princeton.edu/programs/space-6.lisp
# Created:     26-05-2018
# Copyright:   (c) Ashwath Sampath 2018
#-------------------------------------------------------------------------------
"""Module of functions which perform model validation and
variation. When a conclusion premise is reached, the conclusion
has to be verified in the mental model. If it is true in the
mental model, we look for alternative models where it is false.
If it is false in the mental model, we look for alternative
models in which it is true. """
import copy
from . import spatial_array
from . import utilities
from . import model_builder


def verify_model(prop, mod):
    """ It returns mod iff subj is related to obj according to rel,
    otherwise it returns None. Calls recursive func verify, which
    inspects the subject and object coordinates, and verfies if rel
    holds between the coordinates"""
    rel = utilities.relfn(prop)
    subj = utilities.subjfn(prop)
    obj = utilities.objfn(prop)
    subj_coords = utilities.finds(subj, mod)
    obj_coords = utilities.finds(obj, mod)
    return verify(rel, subj_coords, obj_coords, mod)

def verify(rel, subj_coords, obj_coords, mod):
    """ It returns mod if rel holds between subj_coords and obj_coords
    in mod (i.e. subj_coord can be reached by adding rel to obj_coord
    a certain number of times). Otherwise, it returns None. """
    obj_coords = utilities.list_add(rel, obj_coords)
    dims_mod = spatial_array.dimensions(mod)
    if utilities.outside(obj_coords, dims_mod) is not None:
        # obj_coords is outside dims_mod, this might occur, for e.g.,
        # when addition of rel to obj_mod has caused it to become
        # more extreme than subj_mod (and dims).
        return None
    if subj_coords == obj_coords:
        # If obj reaches subj after a certain no. of steps
        return mod
    # Recursive call with new obj_coords
    return verify(rel, subj_coords, obj_coords, mod)

def make_true(prop, mod, premises):
    """ Called by call_appropriate_func when a prop is false in a mod, it
    calls make to try to revise model to make prop true. If it succeeds,
    it returns new mod, otherwise it returns None.  Input prop
    is a list like [[1 0 0), ['V'], ['O']]. """
    # print("Attempting to make the model true.")
    # Remove premise which corresponds to prop, keep others in prems.
    prems = remove_prems(prop, premises)
    newmod = make([prop], [prop], mod, prems)
    # If make has managed to create a new model in which prop (prems)
    # is now true, the mod is returned
    if newmod is not None and verify_model(prop, newmod) is not None:
        #print("Premise was previously possibly false, but can also be true")
        return newmod
    #print("Premise is inconsistent with previous premises")
    return mod

def make_false(prop, mod, premises):
    """ In trying to falsify, say, the cross is on the left of the
    circle, the program constructs a representation of its negation,
    i.e. the cross is on the right of the circle;  it changes the two
    items around to satisfy this negation, and then tries to modify
    the rest of the model so that it accomodates both the previous
    premises and this change. If it succeeds, it has falsified the
    proposition, and so it returns original mod with comment 'Prem was
    previously possibly true'. If it fails, premise follows validly.
    Removes premise = prop from premises because func make deals with
    negated premise.  Whatever happens in modifying the model, the
    negated premise (the cross is on the right of the circle) must not
    be destroyed.  Hence, one can move either (or both) items, but not
    into positions that falsify the relation, and so the second list =
    negprop must be handed down to make to prevent its violation"""
    #print("Attempting to falsify the model.")
    # Remove premise which corresponds to prop, keep others in prems.
    prems = remove_prems(prop, premises)
    # Get the opposite relation to the one in prop, along with subj & obj
    negprop = negate_prop(prop)
    newmod = make([negprop], [negprop], mod, prems)
    if newmod is not None and verify_model(negprop, newmod) is not None:
        #print("Premise was previously possibly true, but can also be false")
        return newmod
    # print("Premise follows validly from previous premises")
    return mod

def make(prop_list, fix_props, mod, prems):
    """ Function returns new model if no prems conflict with it, and
    returns None if mod becomes None (in switch). Otherwise, it calls
    switch to construct mod that makes first prop in prop_list true
    (provided that it is consistent with all the props in fixprops
    (which is initially just the prop list sent by make-true or make-false).
    But each time a model is formed to make a hitherto conflicting
    premISE hold, then the corresponding prop is added to fix_props. The
    recursive call shifts prop_list[0] to of fix_props[0]"""
    # Control reaches here after items in mod have been switched to match
    # with negprop.
    if prop_list == []:
        # After switch (swapping 2 tokens in conclusion), get list of props
        # that conflict now (due to the swap).
        prop_list = conflict(prems, mod)
        if prop_list != []:
            return make(prop_list, fix_props, mod, prems)
        return mod
    if mod is None:
        return None
    mod = switch(prop_list[0], fix_props, mod)
    # Switch returns None if swap yields a model conflicting with item
    # in fix_props, or if move failed.
    if mod != None:
        # Move first prop_list element to fix_props
        fix_props.insert(0, prop_list[0])
        return make(prop_list[1:], fix_props, mod, prems)
    # mod has become None as switch was not able to swap subj and obj,
    # move subj or move obj.
    return None

def switch(newprop, fixprops, mod):
    """ If rel is converse of the one in mod, this func tries to
    swap subj and obj items, otherwise (or if swap yields model
    conflicting with item in fixprops), it tries to move subj, and if
    that fails, it tries to move obj. Otherwise returns None. """
    rel = utilities.relfn(newprop)
    obj = utilities.objfn(newprop)
    subj = utilities.subjfn(newprop)
    subj_coord = utilities.finds(subj, mod)
    obj_coord = utilities.finds(obj, mod)
    # Semantics between subj and obj in mod is opposite of current rel:
        # we need to swap subj and obj in mod.
    newmod = swap(subj, subj_coord, obj, obj_coord, mod)
    newmod = spaces_to_none(newmod)
    newmod = spatial_array.copy_shrink_array(newmod)
    # Check if the relation between subject and object is opposite to
    # rel in newprop, and if none of the props in fixprop are false in
    # the model produced by swapping subj  and obj. If so, return newmod.
    # Note: conflict props returns [] if there are no conflicting props
    if find_rel_prop(subj_coord, obj_coord) == utilities.convert(rel) and \
       conflict_props(fixprops, newmod) == [] and newmod != {}:
        return newmod
    # After swapping the subject and object in the conclusion premise, there
    # are some conflicting props (premises) . In each of these premises, move
    # subj from subj_coord to new coordinates. If there is no conflict, return
    # the resulting model. Otherwise, try to move object to new position.
    newmod = move(subj, subj_coord, rel, obj_coord, mod)
    newmod = spaces_to_none(newmod)
    newmod = spatial_array.copy_shrink_array(newmod)
    if conflict_props(fixprops, newmod) == [] and newmod != {}:
        return newmod
    # Move object from obj_coord to new coordinates. If there is no
    # conflict, return the resulting model. Otherwise, return None.
    obj_rel = utilities.convert(rel)
    newmod = move(obj, obj_coord, obj_rel, subj_coord, mod)
    newmod = spaces_to_none(newmod)
    newmod = spatial_array.copy_shrink_array(newmod)
    if conflict_props(fixprops, newmod) == [] and newmod != {}:
        return newmod
    return None

def spaces_to_none(mod):
    """Converts any spaces found in the values of the dict mod, and replaces
    them with None"""
    tmpmod = copy.deepcopy(mod)
    for index, value in tmpmod.items():
        if value == "":
            mod[index] = None
    return mod

def swap(subj, subj_coord, obj, obj_coord, mod):
    """ Swaps positions of subj and obj in mod, takes care to remove subj
    from its cell in case in shares it with other items, does the same
    for obj. Returns mod"""
    # We need a deep copy of mod as it will be changed in this function,
    # and changes will be applied to the calling func (as dict is mutable)
    new_mod = copy.deepcopy(mod)
    # Remove the subject from subject coord, object from object_coord
    new_mod = remove_item(subj, subj_coord, new_mod)
    new_mod = remove_item(obj, obj_coord, new_mod)
    # Add obj at subj_coord and subj at obj_coord
    new_mod = model_builder.add_item(subj_coord, (0, 0, 0), obj, new_mod)
    new_mod = model_builder.add_item(obj_coord, (0, 0, 0), subj, new_mod)
    return new_mod

def remove_item(item, coords, mod):
    """  removes item from contents at coords of mod, i.e. replaces
    contents minus item. Returns revised mod"""
    cell_list = rem_item_from_list(item, mod[coords])
    mod[coords] = cell_list
    return mod

def rem_item_from_list(item, string):
    """ Removes all occurrences of token from string. If no occurrences of
    items are in string, nothing is removed."""
    return string.replace(item, "")

def move(item, item_coords, rel, other_coords, mod):
    """ Moves the item at item_coords into rel with other_coords in model.
    Uses update_coords and rel of (0,0,0) so as to put item into cell
    even if cell is already occupied. Note: this is called on subject/obj
    of each of the conflicting props (premises) AFTER swapping the subject and
    object of the conclusion premise. Finally, it returns the updated model.
    """
    # Deep copy needed as mod is mutable.
    new_mod = copy.deepcopy(mod)
    # remove the item at item coord
    new_mod = remove_item(item, item_coords, new_mod)
    updated_coords = utilities.update_coords(other_coords, rel)
    new_pos = new_position(item_coords, rel, other_coords, updated_coords)
    new_mod = model_builder.add_item(new_pos, (0, 0, 0), item, new_mod)
    return new_mod

def new_position(item_coords, rel, other_coords, coords):
    """ Calls compare_ints to work out co-ords to which item is to be moved
    to. The motivation is to allow an item normally to move only along one
    dimension."""
    new_pos = []
    for i, it_coord in enumerate(item_coords):
        new_pos.append(compare_ints(it_coord, rel[i],
                                    other_coords[i], coords[i]))
    return tuple(new_pos)

def compare_ints(item_coords_int, rel_int, other_coords_int, coords_int):
    """ Constructs new integer in coords of new position. If integer of
    rel is 1, it returns int of item_coords IF it is greater than other_cords.
    But if integer of rel is -1, it returns int of item_coords IF it is
    less than int of other-cords. Otherwise when int of rel is 0, it returns
    int from updated co-ords in the case where integer of rel is 0. """
    if rel_int > 0 and item_coords_int > other_coords_int:
        return item_coords_int
    if rel_int < 0 and item_coords_int < other_coords_int:
        return item_coords_int
    return coords_int

def find_rel_prop(subj_coords, obj_coords):
    """ Finds the difference in co-ords between subj coord and obj coord
    and then normalizes to express the semantics of the rel between them.
    E.g. If subj_coord = (0,2,0), obj_coord = (0,0,0), it returns (0,1,0)
    (subj is in front of the object). """
    vector = list_subtract(subj_coords, obj_coords)
    return tuple(normalize(vector))

def list_subtract(lis1, lis2):
    """ Subtracts numbers in lis2 from the corresponding numbers in
    lis1 and returns the resulting tuple. Called by find_rel_prop"""
    result = (lis1[i] - lis2[i] for i in range(len(lis1)))
    return tuple(result)

def normalize(vector):
    """ Takes a vector and reduces each entry to 1, -1, or 0.
    Returns a tuple"""
    return tuple([1 if ele > 0 else -1 if ele < 0 else 0 for ele in vector])

def negate_prop(prop):
    """ Negates the rel part of a proposition, for e.g. neg of [1,0,0],
    i.e. to the right of, is [-1,0,0] (to the left of). """
    opposite_rel = utilities.convert(utilities.relfn(prop))
    # Change only relation in prop, don't touch subj or obj.
    prop[0] = list(opposite_rel)
    return prop

def remove_prems(prop, premises):
    """ Removes parse(premise) = prop (proposition) from list of premises."""
    return [prem for prem in premises if prem[0] != prop]

def conflict(premises, mod):
    """ Returns list of props corresponding to those premises false in model,
    excluding those containing referents that are not in the model. """
    false_props = []
    for premise in premises:
        # premise is a list of containing the premise, premise[0]: string.
        prop = premise
        subj = utilities.subjfn(prop)
        obj = utilities.objfn(prop)
        # If either subject or object is not found, we can't tell if there
        # is a conflict.
        if utilities.finds(subj, mod) is None or \
           utilities.finds(obj, mod) is None:
            continue
        # Check if rel between subj and obj in current prop holds in mod
        if verify_model(prop, mod) is None:
            # verify_model returns None when the rel between subj and obj in
            # prop is invalid in mod, i.e. the premise is false in mod.
            false_props.append(prop)
        # If verify_model returned a list (not None), go to the next premise
        # (end of loop)
    return false_props

def conflict_props(props, mod):
    """ Returns list of props (list of props) that are false in mod."""
    return [prop for prop in props if verify_model(prop, mod) is None]
