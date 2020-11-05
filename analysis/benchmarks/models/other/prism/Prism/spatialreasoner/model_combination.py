#-------------------------------------------------------------------------------
# Name:        Spatial Reasoning Model Combination
# Purpose:     Single function module which combines 2 separate models
#              into a single model.
# Author:      Ashwath Sampath
# Based on: http://mentalmodels.princeton.edu/programs/space-6.lisp
# Created:     26-05-2018
# Copyright:   (c) Ashwath Sampath 2018
#-------------------------------------------------------------------------------
"""Single function module which combines 2 separate models into a single
model """
from . import utilities
from . import spatial_array
def combine(rel, subj_coords, obj_coords, subj_mod, obj_mod):
    """ This func. combines subj_mod and obj_mod in a way that
    satisfies rel."""
    # Get the dimensions of subj_mod and obj_mod, we need to add
    # (1, 1, 1) as the dimensions function returns 0-indexed dims.
    subj_dims = utilities.list_add(spatial_array.dimensions(subj_mod),
                                   (1, 1, 1))
    obj_dims = utilities.list_add(spatial_array.dimensions(obj_mod),
                                  (1, 1, 1))
    # dims origins will return a 3-tuple: new dims, new origin for
    # subj_mod and new origin for obj_mod
    tmp = spatial_array.dims_origins(rel, subj_dims, obj_dims,
                                     subj_coords, obj_coords)
    new_dims, new_subj_orig, new_obj_orig = tmp
    # The call to copy_array with subj_mod as inarr creates an outarr
    # of desired dimensions, and then inserts the values in inarr at the
    # correct positions in the new outarr through the copy_arr function.
    outarr = spatial_array.copy_array(subj_mod, {}, new_dims, new_subj_orig)
    # The call to copy_array with obj_mod doesn't need to create a new
    # array, it uses the array produced by the previous call with subj_mod
    # (which already has the correct dims). So this only inserts values
    # in outarr from inarr through the copy_arr function.
    outarr = spatial_array.copy_array(obj_mod, outarr, new_dims,
                                      new_obj_orig)
    return outarr
