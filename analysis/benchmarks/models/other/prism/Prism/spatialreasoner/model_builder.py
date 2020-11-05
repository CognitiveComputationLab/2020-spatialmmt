#-------------------------------------------------------------------------------
# Name:        Spatial Reasoning Model Builder
# Purpose:     Module of functions which create new models and add
#              items to existing models.
#
# Author:      Ashwath Sampath
# Based on: http://mentalmodels.princeton.edu/programs/space-6.lisp
# Created:     26-05-2018
# Copyright:   (c) Ashwath Sampath 2018
#-------------------------------------------------------------------------------
"""Module of functions which create new models or add items to existing
models. """
import copy
from . import utilities
from . import spatial_array

def start_mod(rel, subj, obj):
    """ This function builds the smallest possible model in which
    subject is related to object by relation 'rel'. """

    # Create mod: a dict to hole spatial values
    # mod[0, 0, 0] = ['[]'] inserts a square into coords (0, 0, 0).
    # Note: keys are tuples representing 3d coordiantes
    mod = {}
    # Insert dummy value None into origin of mods
    mod[0, 0, 0] = None
    # Add object at current origin (0, 0, 0)
    mod = add_item((0, 0, 0), (0, 0, 0), obj, mod)
    # Add subj to mod according to rel with obj.
    mod = add_item((0, 0, 0), rel, subj, mod)
    return mod

def add_item(coords, rel, item, mod):
    """ This function updates coordinates (coords) according to rel
    and then calls add_it. It checks if coordinates are outside the
    model, and if so, expands mod. This is necessary in case rel is
    (0, 0, 0), but assigned coords are outside. """

    coords = utilities.update_coords(coords, rel)
    # Dimensions: 0-based.
    dims_mod = spatial_array.dimensions(mod)
    # Check if new token will fit into the existing array, or if it needs
    # expansion. If it needs expansion, get the new dimensions & new origin.
    # newlis = (newdims, neworigin)
    newlis = utilities.outside(coords, dims_mod)
    if rel == (0, 0, 0) and newlis is not None:
        mod = spatial_array.copy_array(mod, {}, newlis[0], newlis[1])
        coords = utilities.list_add(coords, newlis[1])
    return add_it(coords, rel, item, mod)

def add_it(coords, rel, item, mod):
    """ This func. adds item and returns the new model (mod). The first
    test detects 'in same place' and item is added to whatever is at
    coords.  Second test detects when coords are outside, then list of
    new_dims + new_orig is used to copy mod to new mod. item is added to
    this, having changed coords by adding new_orig to them. Third test:
    used to check if mod[coords], i.e. cell at current coords, is empty.
    In this case, item is added.  Finally, where cells already has item,
    update coords according to rel and try again. As it is recursive and
    may increase co-ords, it needs to expand array sometimes. """
    if rel == (0, 0, 0):
        # Get what is currently present in mod at coordinates coords.
        current = mod.get(coords)
        # Concatenate item to what is already present at the given
        # coordinates in mod (item and current are strings).
        item = item + current if current is not None else item
        mod[coords] = item
        return mod
    dims_mod = spatial_array.dimensions(mod)
    # If coords is not within dims_mod, adjust origin and coords suitably.
    # outside returns ((new_coords),(new_origin)) = newlis
    newlis = utilities.outside(coords, dims_mod)
    if newlis is not None:
        # Expand mod by making space for an extra item
        mod = spatial_array.copy_array(mod, {}, newlis[0], newlis[1])
        # New origin, so old coords have to be updated
        new_coords = utilities.list_add(coords, newlis[1])
        return add_it(new_coords, rel, item, mod)
    # Reaches here when coords is within dims, rel != 0,0,0 and
    # mod[coords]has space (is None) for a new item, which can be inserted.
    if mod.get(coords) is None:
        mod[coords] = item
        return mod
    # If there is another item at mod[coords], move further along the
    # direction specified by rel, and try to add item there.
    updated_coords = utilities.update_coords(coords, rel)
    return add_it(updated_coords, rel, item, mod)

def add_item_prism(coords, rel, item, mod, annotations):
    """ This function updates coordinates (coords) according to rel
    and then calls add_it. It checks if coordinates are outside the
    model, and if so, expands mod. This is necessary in case rel is
    (0, 0, 0), but assigned coords are outside. """

    item_in_mod = mod[coords]
    coords = utilities.update_coords(coords, rel)
    # Dimensions: 0-based.
    dims_mod = spatial_array.dimensions(mod)
    # Check if new token will fit into the existing array, or if it needs
    # expansion. If it needs expansion, get the new dimensions & new origin.
    # newlis = (newdims, neworigin)
    newlis = utilities.outside(coords, dims_mod)
    if rel == (0, 0, 0) and newlis is not None:
        mod = spatial_array.copy_array(mod, {}, newlis[0], newlis[1])
        coords = utilities.list_add(coords, newlis[1])
    # Package items: item and item_in_mod in a tuple for Pylint
    items = (item, item_in_mod)
    return add_it_prism(coords, rel, items, mod, annotations)

def add_it_prism(coords, rel, items, mod, annotations):
    """ This func. adds item and returns the new model (mod). The first
    test detects 'in same place' and item is added to whatever is at
    coords.  Second test detects when coords are outside, then list of
    new_dims + new_orig is used to copy mod to new mod. item is added to
    this, having changed coords by adding new_orig to them. Third test:
    used to check if mod[coords], i.e. cell at current coords, is empty.
    In this case, item is added.  Finally, where cells already has item,
    update coords according to rel and try again. As it is recursive and
    may increase co-ords, it needs to expand array sometimes. """
    item, item_in_mod = items
    if rel == (0, 0, 0):
        # Get what is currently present in mod at coordinates coords.
        current = mod.get(coords)
        # Concatenate item to what is already present at the given
        # coordinates in mod (item and current are strings).
        item = item + current if current is not None else item
        mod[coords] = item
        return mod, annotations
    dims_mod = spatial_array.dimensions(mod)
    # If coords is not within dims_mod, adjust origin and coords suitably.
    # outside returns ((new_coords),(new_origin)) = newlis
    newlis = utilities.outside(coords, dims_mod)
    if newlis is not None:
        # Expand mod by making space for an extra item
        mod = spatial_array.copy_array(mod, {}, newlis[0], newlis[1])
        # New origin, so old coords have to be updated
        new_coords = utilities.list_add(coords, newlis[1])
        items = (item, item_in_mod)
        return add_it_prism(new_coords, rel, items, mod, annotations)
    # Reaches here when coords is within dims, rel != 0,0,0 and
    # mod[coords] has space (is None) for a new item, which can be inserted.
    if mod.get(coords) is None:
        # An annotation is necessary only in the special case where the already
        # -present item is the 2nd element (LO) of some annotation AND we
        # are moving in the same direction as rel. In this case, we add
        # the annotation: [rel, item, item_in_mod]
        new_annotation = subj_or_obj_in_annotations(annotations, item,
                                                    rel, item_in_mod)
        if new_annotation is not None:
            annotations.append(new_annotation)
        mod[coords] = item
        return mod, annotations
    # If there is another item at mod[coords], move further along the
    # direction specified by rel, and try to add item there.

    # Find the coords of the token already present in the model (this might be
    # the subject or the object. Remember, coords has already been updated in
    # add_item. We don't need to reverse this update.

    # Whether it's add subj or add obj, rel will be the same (as add_obj gets
    # the negation of rel. The new item is somewhere to the rel of item_in_mod
    # The ref obj always goes at the end of the annotation
    new_annotation = [rel, item, item_in_mod]
    if new_annotation not in annotations:
        annotations.append(new_annotation)
    # Continue as per fff strategy and add the token.
    updated_coords = utilities.update_coords(coords, rel)
    items = (item, item_in_mod)
    return add_it_prism(updated_coords, rel, items, mod, annotations)

def subj_or_obj_in_annotations(annotations, item, rel, item_in_mod):
    """ Looks for item in the second pos of annotations AND rel in the first
    pos of the annotation. If both are found, it creates a new annotation
    [rel, item, item_in_mod] and returns it. Otherwise, it returns None"""
    annotations_copy = copy.deepcopy(annotations)
    for annotation in annotations_copy:
        if annotation[1] == item_in_mod and annotation[0] == rel:
            # For add_object, we have already switched the direction of
            # rel. So the annotation is going to be the same whether this
            # function was called from add_subject or add_object.
            new_annotation = [rel, item, item_in_mod]
            return new_annotation
    return None
