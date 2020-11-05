#-------------------------------------------------------------------------------
# Name:        Spatial Array
# Purpose:     Functions used by the SpatialReasoning class. These functions
#              either operate on a spatial array (a Python dictionary with
#              tuple coordinates as keys), or are used by the functions which
#              operate on the spatial array.
#
# Author:      Ashwath Sampath
# Based on: http://mentalmodels.princeton.edu/programs/space-6.lisp
# Created:     29-04-2018
# Copyright:   (c) Ashwath Sampath 2018
#-------------------------------------------------------------------------------
""" Module with functions used by the SpatialReasoning class in
spatial_reasoning.py. These functions either operate on spatial
arrays (a Python dictionary with tuple coordinates as keys), or
are used by the functions which operate on spatial arrays.
Based on LISP code developed by PN Johnson-Laird and R.Byrne as
part of their 1991 book 'Deduction' and their 1989 paper
'Spatial Reasoning'. """

import math
import copy
from collections import OrderedDict
import matplotlib.pyplot as plt
from matplotlib import gridspec
from mpl_toolkits.mplot3d import Axes3D
from . import utilities

def make_array(dims):
    """ Creates an array (dictionary with tuple coordinates as keys) of
    the given dimensions (the 3 dims together indicate how many values are
    to be inserted into outarr) initialized with None (value=None).
    For example, if dims given = (1 2 1),
    returned arr = {(0,0,0): None, (0,1,0): None}. If dims = (1,2,2), 4
    values are inserted. """
    arr = {}
    dim1, dim2, dim3 = dims
    for i in range(dim1):
        for j in range(dim2):
            for k in range(dim3):
                arr[i, j, k] = None
    return arr

def dimensions(arr):
    """ Returns list of no. of items on each dimension counting from 0.
    Note: arr is a dict of the form {(0,0,0):1, {(1,0,0):2} where the
    elements of the tuple are the 3d coordinates"""
    horiz = []
    vertical = []
    sideways = []
    for (i, j, k) in arr.keys():
        vertical.append(k)
        horiz.append(j)
        sideways.append(i)
    return tuple([max(sideways), max(horiz), max(vertical)])

def copy_array(inarr, outarr, new_dims, new_origin):
    """ This func copies inarr to outarr.  If outarr is {}, it creates a new
    outarr with newdims dimensions, and if newdims is null, it makes an
    exact copy of inarr. new_origin specifies origin of inarr within outarr.
    newdims must have larger dims than those of original inarr!
    If new-origin is (0,0,0), then inarr is located at origin of outarr.
    Otherwise, it can be located elsewhere by setting value of new-origin,
    to, say, (1,0,0). This shifts array to right by 1. Now, any change in
    values of cells of outarr are independent of inarr, and vice versa"""

    if outarr == {}:
        if new_dims is None:
            new_dims = [x + 1 for x in dimensions(inarr)]
        # new_dims isn't sent to copy_arr, it is only used to indicate
        # how many elements to insert in outarr in make_array
        # Creates a new 'None' element in outarr
        outarr = make_array(new_dims)
    # If outarr is not null (e.g. when called from combine with obj_mod
    # after call with subj_mod already produced an outarr of correct dims.

    # Note: dictionary outarr will be changed by the following call
    # to recursive_copy as dict is a mutable datatype.
    recursive_copy(inarr, outarr, dimensions(inarr), new_origin)
    return outarr

def recursive_copy(inarr, outarr, dims, new_origin):
    """ Recursive function. Called by copy_array, it changes outarr
    destructively, it doesn't return anything. It can copy inarr
    into outarr at either new_origin, or at (0, 0, 0). Dims are
    dimensions of inarr, and may be smaller than those of outarr."""
    pos = utilities.find_index_neg_num(dims)
    # If there are no negative no.s in dims
    if pos is None:
        # Insert value in inarr into outarr at coords dims + new_origin
        item = inarr.get(dims)
        out_dims = utilities.list_add(dims, new_origin)
        outarr[out_dims] = item
        tmp_dims = tuple((v-1 if i == 0 else v for i, v in enumerate(dims)))
        recursive_copy(inarr, outarr, tmp_dims, new_origin)
    # There are negative no.s in dims:
    elif pos < (len(dims) - 1):
        tmp_dims = update(pos, dims, dimensions(inarr))
        recursive_copy(inarr, outarr, tmp_dims, new_origin)

def update(pos, dims, orig_dims):
    """ Finds neg number in dims and restores it to the value in
    original-dims and subtracts one from the number after it
    e.g (0 -1 0) (0 1 0)-> (0 1 -1) """
    ret_dim = []
    for i, dim in enumerate(dims):
        if pos == i:
            ret_dim.insert(i, orig_dims[i])
        else:
            ret_dim.insert(i, dim)
    ret_dim[pos+1] = dims[pos+1] - 1
    return tuple(ret_dim)

def dims_origins(rel, subj_dims, obj_dims, subj_coords, obj_coords):
    """ This function works out the dims of the new array obtained by
    combining the 2 arrays, and their new origins within the respecive
    array. """
    out_dims = []
    subj_mod_origin = []
    obj_mod_origin = []
    # subj_mod origin and obj_mod origin are moved such that when they
    # are later combined (in combine), they don't clash with each other
    # in the new array (created using copy-array)
    for i, item in enumerate(rel):
        if item > 0:
            out_dims.append(subj_dims[i] + obj_dims[i])
            # Change subj origin while keeping subj origin constant
            subj_mod_origin.append(obj_dims[i])
            obj_mod_origin.append(0)
        elif item < 0:
            out_dims.append(subj_dims[i] + obj_dims[i])
            subj_mod_origin.append(0)
            # Move obj origin while keeping subj origin constant
            obj_mod_origin.append(subj_dims[i])
        else:
            # component of rel is 0, dim is orthogonal to it.
            dim, s_orig, o_orig = utilities.ortho(subj_coords[i],
                                                  obj_coords[i],
                                                  subj_dims[i], obj_dims[i])
            out_dims.append(dim)
            subj_mod_origin.append(s_orig)
            obj_mod_origin.append(o_orig)
    return tuple(out_dims), tuple(subj_mod_origin), tuple(obj_mod_origin)

def copy_shrink_array(oldarr):
    """ Func that controls shrinking of arr to newarr by eliminating empty row,
    col, or plane slices. It works out lists of dims (row/col/plane) to
    eliminate, makes new arr using newdims computed by newds. Finally, it
    calls copy-shr-arr to copy oldarr to newarr, working out new coords.
    If no empty slices are found, it returns the unaltered oldarr. """
    # Shrink the array by getting rid of empty dimensions. If it is not
    # shrinkable, shrink_array returns [[],[],[]]
    dlists = shrink_array(oldarr)
    # Create a new array after working out its dimensions from oldarr, dlists.
    newarr = newds(oldarr, dlists)
    old_dims = dimensions(oldarr)
    # If dlists = [[],[],[]], there are no dimensions to eliminate
    if all((True if ele == [] else False for ele in dlists)):
        # Nothing to reduce
        return oldarr
    dim1, dim2, dim3 = old_dims
    return copy_shrink(oldarr, (dim1, dim2, dim3), dlists, newarr)

def copy_shrink(oldarr, dims, dlists, newarr):
    """ Goes through old arr, copying it to newarr, working out newcoords
    to eliminate empty slices. dlist= list of rows/cols/planes to remove"""
    od1, od2, _ = dimensions(oldarr)
    dim1, dim2, dim3 = dims
    if dim3 >= 0:
        if dim2 >= 0:
            if dim1 >= 0:
                cell = oldarr[dim1, dim2, dim3]
                coords = new_coords(dim1, dim2, dim3, dlists)
                if coords is not None:
                    newarr[coords] = cell
                return copy_shrink(oldarr, (dim1-1, dim2, dim3), dlists,
                                   newarr)
            return copy_shrink(oldarr, (od1, dim2-1, dim3), dlists, newarr)
        return copy_shrink(oldarr, (od1, od2, dim3-1), dlists, newarr)
    return newarr

def shrink_array(arr):
    """ Returns a list of three lists each containing the row slices,
    columns slices, and plane slices to remove from the array. First,
    it generates for each a list equal to all possible sets of d's (i.e.
    row/col/plane coords), e.g. [3,2,1,0] [1,0] [0] and then calls shrink.
    Whenever shrink detects a not None cell in arr, it removes the 3
    corresponding co-ords from these three lists. The array is
    then copied with empty rows/columns/planes deleted from it"""
    # Note: d1: col index, d2: row index, d3 plane index (origin at top left)
    dim1, dim2, dim3 = dimensions(arr) # 0-based
    # Get all values of d1, d2, d3 in array (from d down to 0)
    cols = utilities.list_ints(dim1)
    rows = utilities.list_ints(dim2)
    planes = utilities.list_ints(dim3)
    return shrink((dim1, dim2, dim3), arr, cols, rows, planes)

def shrink(dims, arr, cols, rows, planes):
    """ Returns a list of lists of empty row slices, column slices and plane
    slices to remove from arr. Note: indexes -> d1: col, d2: row, d3: plane.
    If there are no empty row/col/plane slices, it returns [[],[],[]]. """
    orig_d1, orig_d2, _ = dimensions(arr)
    dim1, dim2, dim3 = dims
#    if cols == [] and rows == [] and planes == []:
#        return [[],[],[]]
    # Go through set of planes
    if dim3 >= 0:
        # Go through columns in a plane
        if dim2 >= 0:
            # Go through rows in a column
            if dim1 >= 0:
                if arr[dim1, dim2, dim3] is None:
                    return shrink((dim1-1, dim2, dim3), arr,
                                  cols, rows, planes)
                # If there is an element in arr[dim1,dim2,dim3], it
                # shouldn't be removed: should be part of the final result.
                return shrink((dim1-1, dim2, dim3), arr,
                              utilities.rem_num(dim1, cols),
                              utilities.rem_num(dim2, rows),
                              utilities.rem_num(dim3, planes))
            return shrink((orig_d1, dim2-1, dim3), arr, cols, rows, planes)
        return shrink((orig_d1, orig_d2, dim3-1), arr, cols, rows, planes)
    return [cols, rows, planes]

def new_coords(old1, old2, old3, dlists):
    """ Returns new set of coords for an item in order to shrink array,
    but if one of the co-ords is None, then returns None"""
    dim1 = new_coordinate(old1, dlists[0])
    dim2 = new_coordinate(old2, dlists[1])
    dim3 = new_coordinate(old3, dlists[2])
    coord_list = [dim1, dim2, dim3]

    # Check if dim1, dim2 or dim3 returned None
    if any((True if ele is None else False for ele in coord_list)):
        return None
    return tuple(coord_list)

def new_coordinate(old_coord, coord_list):
    """ Takes one co-ordinate of item in oldarr and checks its relation to
    items in coords_list, which is a list of d's for that dimension that
    are empty;  if old_coord is in this list, then nothing is going to be
    copied to newarr;  if old_coord is smaller than all d's in coord_list,
    i.e. n = 0, then same co-ord is used in copying to newarr;  otherwise,
    new co-ord equals old_coord minus n, i.e. number of smaller items
    in coord_list"""
    num_larger = num_larger_items(old_coord, coord_list)
    if old_coord in coord_list:
        return None
    if num_larger == 0:
        return old_coord
    if num_larger > 0:
        return old_coord - num_larger
    # Control never reaches here, only for Pylint warning
    return old_coord

def num_larger_items(coord_ref, coord_list):
    """ Returns the no. of items in coord_list which coord_ref is larger
    than. E.g. co=3, coord_list=[5,2,1], returned: 2"""
    return len([coord for coord in coord_list if coord < coord_ref])

def newds(oldarr, dlists):
    """ Uses dims of oldarr and rows/cols/planes to slice empty rows/cols/
    planes based on dlists, and get the dimensions of a new array. Finally
    it creates the new array. If dlists is [[],[],[]], i.e. no values for
    rows, cols, planes to eliminate, it returns oldarr. """
    old_dims = utilities.list_add(dimensions(oldarr), (1, 1, 1))
    # Get the newdims: oldarr dim - corresponding dlist dim
    new_dims = []
    for i, old_dim in enumerate(old_dims):
        new_dims.append(old_dim - len(dlists[i]))
    new_arr = make_array(new_dims)
    return new_arr

def insert_moved_objects(tokens_coordinates, mod):
    """ Takes a list of objects with their new coordinates, and mod, and
    inserts them into mod. """
    mod_copy = copy.deepcopy(mod)
    for token, coordinates in tokens_coordinates.items():
        # We need to remove all the tokens we are adding (some may already
        # be present). So we set it to None here.
        temp_coords = utilities.get_coordinates_from_token(token, mod_copy)
        if temp_coords is not None:
            mod_copy[temp_coords] = None
        mod_copy[coordinates] = token
    return mod_copy

def print_array(mod, prob_type):
    """" This func. prints the supplied array (model) in a matplotlib
    3D graph. """
    fig = plt.figure()
    #axis = fig.add_subplot(111, projection='3d')
    axis = fig.add_subplot(111, projection=Axes3D.name)
    title = "Combined Model" if prob_type == 'combination' \
          else "Initial (and final) model" if prob_type == 'deductive' \
          else "Model (conclusion false)" if prob_type == 'inconsistent' \
          else "Generated model" if prob_type == 'generatedet' \
          else "Initial model"
    plt.title(title)
    x_val, y_val, z_val = dimensions(mod)
    xdim = list(range(x_val))
    ydim = list(range(y_val))
    zdim = list(range(z_val))
    axis.set_xticks(xdim)
    axis.set_yticks(ydim)
    axis.set_zticks(zdim)
    marker_dict = {'[]':'s', 'v': '^', 'O': 'o', 'I': '|', '+': 'X',
                   'L': '$L$', '^': '$V$', '*': '*', 'S': '$S$'}
    # axis.set_zlim(ax.get_zlim()[::-1])
#    axis.invert_yaxis()
#    axis.invert_xaxis()
    axis.invert_zaxis()
    #axis.xaxis.tick_top()
    for key, val in mod.items():
        if val is not None:
            if len(val) > 1 and val != '[]':
                axis.scatter(key[0], key[1], key[2])
                axis.annotate(val, (key[0], key[1], key[2]))
            else:
                axis.scatter(key[0], key[1], key[2], marker=marker_dict[val],
                             s=100)
    plt.show()

def print_array_prism(mod, prob_type):
    """" This func. prints the supplied array (model) in a matplotlib
    3D graph. """
    fig = plt.figure()
    #axis = fig.add_subplot(111, projection='3d')
    axis = fig.add_subplot(111, projection=Axes3D.name)
    title = "Combined Model" if prob_type == 'combination' \
          else "Preferred model (only model)" if prob_type == 'deductive' \
          else "Preferred Model (conclusion false)" if prob_type == 'inconsistent' \
          else "Generated model" if prob_type == 'generatedet' \
          else "Generated model (only model)"
    plt.title(title)
    x_val, y_val, z_val = dimensions(mod)
    xdim = list(range(x_val))
    ydim = list(range(y_val))
    zdim = list(range(z_val))
    axis.set_xticks(xdim)
    axis.set_yticks(ydim)
    axis.set_zticks(zdim)
    marker_dict = {'[]':'s', 'v': '^', 'O': 'o', 'I': '|', '+': 'X',
                   'L': '$L$', '^': '$V$', '*': '*', 'S': '$S$'}
    # axis.set_zlim(ax.get_zlim()[::-1])
#    axis.invert_yaxis()
#    axis.invert_xaxis()
    axis.invert_zaxis()
    #axis.xaxis.tick_top()
    for key, val in mod.items():
        if val is not None:
            if len(val) > 1 and val != '[]':
                axis.scatter(key[0], key[1], key[2])
                axis.annotate(val, (key[0], key[1], key[2]))
            else:
                axis.scatter(key[0], key[1], key[2], marker=marker_dict[val],
                             s=100)
    plt.show()

def print_multiple_mods(mods, prob_type, num_models):
    """" This func. prints the supplied arrays (models) in a matplotlib
    3D graph. """
    fig = plt.figure()
    marker_dict = {'[]':'s', 'v': '^', 'O': 'o', 'I': '|', '+': 'X',
                   'L': '$L$', '^': '$V$', '*': '*', 'S': '$S$'}
    # Get the suitable number of columns and rows. This doesn't have to deal
    # with the case where there is only one model, that is handled by
    # print_array.
    grid_specification = get_gridspec(num_models)
    plt.suptitle('Multiple models')
    plt.axis('off')
    for modelnumber, mod in enumerate(mods):

        axis = fig.add_subplot(grid_specification[modelnumber],
                               projection=Axes3D.name)
                               #aspect='equal')
        if modelnumber == 0:
            title = 'Initial Model' if prob_type not in ['generatedet',
                                                         'generateindet',
                                                         'generateall'] \
            else 'Initial generated model'
        else:
            title = 'Altered model'if prob_type not in ['generatedet',
                                                        'generateindet',
                                                        'generateall'] \
            else 'Altered generated model'

        plt.title(title)
        x_val, y_val, z_val = dimensions(mod)
        # Set xticks, yticks and zticks based on no dims of x, y and z.
        axis.set_xticks(list(range(x_val)))
        axis.set_yticks(list(range(y_val)))
        axis.set_zticks(list(range(z_val)))

        axis.invert_zaxis()
        for key, val in mod.items():
            if val is not None:
                if len(val) > 1 and val != '[]':
                    axis.scatter(key[0], key[1], key[2])
                    axis.annotate(val, (key[0], key[1], key[2]))
                else:
                    axis.scatter(key[0], key[1], key[2],
                                 marker=marker_dict[val], s=100)
    plt.show()

def print_multiple_mods_prism(mods_dict, threshold, prob_type, num_models):
    """" This func. prints each model in the defaultdict mods_dict
    in a Matplotlib 3D graph based on the threshold value. For generate all
    problems, it prints all the models"""
    fig = plt.figure()
    # Get the suitable number of columns and rows. This doesn't have to deal
    # with the case where there is only one model, that is handled by
    # print_array.
    grid_specification = get_gridspec(num_models)
    plt.suptitle('Multiple models')
    plt.axis('off')
    modelnum = 0
    # Send the preferred models (key 0) to mods_dict and then remove the key 0
    plot_each_model_prism(mods_dict.get(0)[0], "Preferred Model", 0,
                          grid_specification, fig)
    print("Preferred model: {}".format(mods_dict.get(0)[0]))
    mods_dict.pop(0, None)
    # For all prob_types except generate_all, remove keys greater than the
    # threshold (only values in the neighbourhood graph above the threshold
    # should be printed). If there are no keys greater than threshold, nothing
    # is removed.
    if prob_type != 'generateall':
        mods_dict = {k: v for k, v in mods_dict.items() if k <= threshold}
    # Order the defaultdict by key: convert it into an OrderedDict
    mods_dict = OrderedDict(sorted(mods_dict.items()))
    for key, mod_list in mods_dict.items():
        for mod in mod_list:
            modelnum += 1
            title = 'Alternative model {} (dist {})'.format(
                modelnum, key)
            # Also, print the model on the command line
            print("{}: {}".format(title, mod))
            plot_each_model_prism(mod, title, modelnum, grid_specification, fig)

    plt.show()

def plot_each_model_prism(mod, title, modelnumber, grid_specification, fig):
    """ Plots each individual model when called from plot_multiple_models_prism
    """
    marker_dict = {'[]':'s', 'v': '^', 'O': 'o', 'I': '|', '+': 'X',
                   'L': '$L$', '^': '$V$', '*': '*', 'S': '$S$'}
    axis = fig.add_subplot(grid_specification[modelnumber],
                           projection=Axes3D.name)
    plt.title(title)
    x_val, y_val, z_val = dimensions(mod)
    # Set xticks, yticks and zticks based on no dims of x, y and z.
    axis.set_xticks(list(range(x_val)))
    axis.set_yticks(list(range(y_val)))
    axis.set_zticks(list(range(z_val)))

    axis.invert_zaxis()
    for key, val in mod.items():
        if val is not None:
            if len(val) > 1 and val != '[]':
                axis.scatter(key[0], key[1], key[2])
                axis.annotate(val, (key[0], key[1], key[2]))
            else:
                axis.scatter(key[0], key[1], key[2],
                             marker=marker_dict[val], s=100)

def get_gridspec(num_models):
    """ Function which returns a matplotlib gridspec with the correct no. of
    subplots (rows and columns) based on the no. of models."""
    cols = 2
    rows = int(math.ceil(num_models / cols))
    grid_specification = gridspec.GridSpec(rows, cols)
    return grid_specification
