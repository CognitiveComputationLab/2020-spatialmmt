#-------------------------------------------------------------------------------
# Name:        Spatial Reasoning Utility Functions
# Purpose:     Module of utility functions, used by the Spatial Reasoning
#              class, as well as the spatial_array module.
#
# Author:      Ashwath Sampath
# Based on: http://mentalmodels.princeton.edu/programs/space-6.lisp
# Created:     29-04-2018
# Copyright:   (c) Ashwath Sampath 2018
#-------------------------------------------------------------------------------
""" Module which contains functions which are called by the
SpatialReasoning class in spatial_reasoning.py. Based on LISP
code developed by PN Johnson-Laird and R.Byrne as part of their
1991 book 'Deduction' and their 1989 paper 'Spatial Reasoning'.  """


def convert(rel):
    """ Switches pos nums to neg, and vice versa, leaving 0s unchanged"""
    return tuple((-i if i > 0 else abs(i) for i in rel))

def extract(mod, models):
    """ Returns models with mod removed. """
    if mod == {}:
        # No mod to delete, return models as it is.
        return models
    return [model for model in models if model != mod]

def outside(coords, dims):
    """ Returns None if coords is within dimensions dims. Otherwise, it
    returns a list of newdims and neworigin, eg. for coords = (1, 3, -2)
    and dims = (2, 2, 2), it returns ((2, 3, 4), (0, 0, 2))"""
    new_ds = out(coords, dims)
    if new_ds == dims:
        return None
    # Add (1,1,1) to new_ds, this is only needed in calls to make_none_array
    # where each dim of dims indicates the no. of models in that direction
    new_dims = list_add(new_ds, (1, 1, 1))
    new_origin = new_orig(coords)
    return (new_dims, new_origin)

def out(coords, dims):
    """ This function  recurses through the two lists, adjusting output
    if a coord is too big or too small (compared to corresponding dim).
    Eg. For coords = (1,3,-2) and dims=(2,2,2), (2,3,4) is returned. """
    new_coords = []
    for coord, dim in zip(coords, dims):
        if coord > dim:
            new_coords.append(coord)
        elif coord < 0:
            new_coords.append(abs(coord) + dim)
        else:
            new_coords.append(dim)
    return tuple(new_coords)

def new_orig(coords):
    """ Sets coordiantes of origin to (0, 0, 0) or to absolute value of
    negative coordinates. Returns a tuple."""
    # Create a generator
    new_coords = (0 if i >= 0 else abs(i) for i in coords)
    return tuple(new_coords)

def find_index_neg_num(lis):
    """ This func. returns position (0 to n) of negative number in
    lis, and None if there is no negative no. """
    return next((index for index, val in enumerate(lis) if val < 0), None)

def find_item(item, mods):
    """ Searches for item in each model in the mods list, returns a list
    containing coords of item (if found) and the model in which it is found.
    If item is not found in any model, it returns None. """
    # Look for item in each individual model
    if mods == []:
        return None
    for mod in mods:
        # Look for item in each individual mod
        coords = finds(item, mod)
        if coords is not None:
            # Model found in mod, return coords and mod
            return (coords, mod)
    # item not found in any of the mods
    return None

def finds(item, mod):
    """ Returns tuple of coordinates of item in mod, if found.
    Otherwise, it returns None """
    coords = [key for key, val in mod.items() if contains(item, val)]
    # If coords!=[], it'll be for eg. [(0,0,0)]. Extract the tuple, return
    return coords[0] if coords != [] else None

def contains(item, cell_value):
    """ This func. returns True if item = cell-value or item is contained
    in cell value (when there is more than 1 token in a cell). """
    return False if cell_value is None \
            else False if cell_value.find(item) == -1 \
            else True

def list_add(lis1, lis2):
    """ Adds numbers in lis1 with the corresponding numbers in lis2,
    and returns the the tuple form of the resulting list"""
    # Create a generator which yields the item-wise sum of the 2 lists
    result = (lis1[i] + lis2[i] for i in range(len(lis1)))
    return tuple(result)

def update_coords(coords, reln):
    """ Returns coordinates of item after updating it to a new position
    that satisfies reln. """
    return list_add(coords, reln)

def subjfn(prop):
    """ Function to retrieve the subject of prop (obtained from the
    intensional representation produced by SpatialParser).
    E.g. prop[1] = ['[]'], prop[1][0] = '[] """
    return prop[1][0]

def relfn(prop):
    """ Function to retrieve the relation of prop (obtained from the
    intensional representation produced by SpatialParser). Spatial
    parser returns a list, relfn converts the list into a tuple
    (tuple is hashable: needed for the spatial array dict)"""
    return tuple(prop[0])

def objfn(prop):
    """ Function to retrieve the object of prop (obtained from the
    intensional representation produced by SpatialParser).
    E.g prop[2] = ['[]'], prop[2][0] = '[] """
    return prop[2][0]

def list_ints(number):
    """ Makes a list from number down to 0. If number = 2, returned: [2,1,0]"""
    return list(range(number, -1, -1))

def rem_num(num, lis):
    """ Removes all instances of a number 'num', from list lis. """
    return [ele for ele in lis if ele != num]

def print_premises(premises):
    """ Prints the premises given in the list 'premises'. """
    print("Premises:")
    for premise in premises:
        print(premise[0])

def ortho(subj_coord, obj_coord, subj_dim, obj_dim):
    """ It returns a tuple of 3 values: new dim for combined array,
    component of subj_origin in it, component of obj_origin in it. """
    if subj_coord > obj_coord:
        return (subj_coord + (obj_dim - obj_coord), 0,
                subj_coord - obj_coord)
    if subj_coord < obj_coord:
        return (obj_coord + (subj_dim - subj_coord),
                obj_coord - subj_coord, 0)
    if subj_dim > obj_dim:
        # There is place for obj_mod's tokens in subj_mod,
        # no increase of dims needed: use subj_mod's dims.
        return (subj_dim, 0, 0)
    # There is place for subj_mod's tokens in obj_mod,
    # no increase of dims needed: use obj_mod's dims.
    return (obj_dim, 0, 0)

def get_coordinates_from_token(token, mod):
    """ A function which takes a mod and a token, finds the token in the mod,
    and then returns the coordinate (tuple) at which the token is found. If it
    is not found, it returns None. """
    for coordinates, token_in_mod in mod.items():
        # No possibility of duplicate tokens. If found, return the token.
        if token_in_mod == token:
            return coordinates
    return None

def same_dir_movement(rel1, rel2):
    """ Takes 2 relations, and checks if they move in the same axis (i.e.
    right and left, right and right, left and left, front and front, back and
    back, back and front and so on.
    If yes, it returns True. If no, it returns False. It assumes that movement
    can be in only one direction, in line with the rest of the code."""
    for dimension in range(3):
        if rel1[dimension] != 0 and rel2[dimension] != 0:
            return True
    return False

def not_none_dim(rel):
    """ Returns the index of dimension which is not None (which is 1/-1) in rel
    Index is 0 based: 0 is right/left, 1 is front/back, 2 is above/below. """
    for index, element in enumerate(rel):
        if element == 1:
            return index
        continue
    # Should never reach here: None only on error
    return None
