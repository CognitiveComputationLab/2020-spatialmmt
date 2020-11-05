# Name:        Spatial Reasoning using Prism
# Purpose:     This module produces a model for spatial reasoning described in
#              'A Theory and a Computational Model of Spatial Reasoning
#              with Preferred Mental Models' by Marco Ragni and Markus Knauff

#-------------------------------------------------------------------------------
# Author:      Ashwath Sampath
# Based on: http://mentalmodels.princeton.edu/programs/space-6.lisp
# Created:     09-04-2018
# Copyright:   (c) Ashwath Sampath 2018
#-------------------------------------------------------------------------------

"""
This module produces a model for spatial reasoning described in 'A Theory
and a Computational Model of Spatial Reasoning with Preferred Mental Models'
by Marco Ragni and Markus Knauff.
"""
import argparse
import copy
from collections import defaultdict, OrderedDict
from spatialreasoner import spatial_parser, spatial_array, model_builder, \
    model_combination, model_validation, utilities, spatial_reasoning

PARSER = spatial_parser.SpatialParser()

class Prism(spatial_reasoning.SpatialReasoning):
    """ This is a class used to implement the Prism spatial reasoning model.
    It inherits from the SpatialReasoning class """

    def __init__(self, verbose=False):
        """ Gets the class variables from the parent class"""
        super().__init__()
        self.modelslist = []
        self.neighbourhoodmodels = defaultdict(list)
        self.annotations = []
        self.intensionalrepr = []
        # Set a threshold for distance of model from primary model
        self.threshold = 2
        # When the relation doesn't hold in the preferred model, find which
        # model it holds in.
        self.true_index = None
        self.v = verbose

    def reset(self):
        return Prism(self.v)

    def build_premise(self, relation_interpret, o1, o2):
        return [relation_interpret, [o1], [o2]]

    def build_model(self, prems):
        prems = [self.build_premise(*premise) for premise in prems]
        models = self.interpret(prems)
        clean_models = []
        for model in models:
            clean_models.append({})
            for key, value in model.items():
                if value:
                    clean_models[-1][key] = value

        models = clean_models
        return models

    def interpret(self, prems):
        """ High-level function takes list of premises and then applies decide
        to result, It then calls decide, which uses
        call_appropriate_func to decide whether to start a new model,
        add a subject or an object to an existing model, combine models
        or verify if a conclusion premise (in which all tokens are already
        present in the mental model) has a relation in the model according
        to its intensional representation. Each of the functions including
        verify_model, returns a model. The updated list of mods is returned to
        interpret by decide. OVERRIDES the spatial reasoning function"""

        if self.v:
            print("Set of premises = {}".format(prems))
        # mods is a list of models. Each individual model is a dict with
        # coordinates as tuple keys. The whole mods list is essentially
        # a history of models at the end of each premise
        mods = []
        for premise in prems:
            # mods stores all intermeidate models
            mods = self.decide(premise, mods)
        return mods

    def decide(self, prop, mods):
        """ This function decides what func to call depending on what is
        already in the mods list of dictionaries. Each mod in mods is a
        separate model. prop (proposition) is a list containing reln, subject
        and object (e.g. [[0, 1, 0], ['[]'], ['O']]).
        find_item is used to check if subj and obj are in mods in 2 separate
        calls, it returns a list containing coords and mod if subj (or obj) is
        present, otherwise it returns None. The extract function extracts the
        model containing subject and object (if a model contains them) from
        the list of mods. It then calls call_appropriate function, which
        decides which functions to call based on if subj and/or obj are in
        mods. OVERRIDES the spatial reasoning function."""

        # For 'generate all 'models'
        if prop == [[]]:
            model = self.generate_all_models(mods[-1])
            return mods
        #rel = utilities.relfn(prop)
        obj = utilities.objfn(prop)
        subj = utilities.subjfn(prop)
        # Inttialize None and {} for coords and mods. These values are used if
        # find_item does not find subj and/or obj in mods (if it returns None).
        subj_coords = None
        obj_coords = None
        subj_mod = {}
        obj_mod = {}
        # Find subj and obj in mods
        subj_coords_mod = utilities.find_item(subj, mods)
        obj_coords_mod = utilities.find_item(obj, mods)
        if subj_coords_mod is not None:
            subj_coords = subj_coords_mod[0]
            subj_mod = subj_coords_mod[1]
        if obj_coords_mod is not None:
            obj_coords = obj_coords_mod[0]
            obj_mod = obj_coords_mod[1]
        # Extract the mod in which subj/obj were found from the list of mods,
        # this removes the relevant mod from mods. The updated model is
        # appended to model returned by call_appropriate_function.
        mods = utilities.extract(subj_mod, mods)
        mods = utilities.extract(obj_mod, mods)
        # Call call_appropriate_func to, well, call the appropriate function,
        # a model is returned whichever function is called from there. Before
        # that, package up the arguments, as there would be too many to pass
        # without upsetting Pylint.
        coords = (subj_coords, obj_coords)
        models_to_pass = (subj_mod, obj_mod)
        model = self.call_appropriate_func(prop, coords, models_to_pass)
        mods.append(model)
        # mods contains intermediate models
        return mods

    def call_appropriate_func(self, prop, coords, models):
        """ This function calls appropriate functions depending on
        whether subj_coords or obj_coords are already in mods (not None).
        If subj and obj are in models , it calls verify_model. If one of
        them is in mods, it calls add-item (add subj or add object).
        If neither is in models, it calls startmod. If subj_mod
        and obj_mod are mutually independent, it calls combine.
        OVERRIDES the spatial_reasoning function"""
        # Unpack coords and mods: this is necessary only to avoid upsetting
        # Pylint.
        subj_coords = coords[0]
        obj_coords = coords[1]
        subj_mod = models[0]
        obj_mod = models[1]
        rel = utilities.relfn(prop)
        subj = utilities.subjfn(prop)
        obj = utilities.objfn(prop)
        if subj_coords is not None and obj_coords is not None:
            if subj_mod == obj_mod:
                # We have reached a conclusion premise, i.e. subj and obj
                # were found in the same mod. OR we need to generate a
                # conclusion if rel = (), empty tuple returned by relfn
                if rel == ():
                    # We have to generate the relation between subj and
                    # obj as we have a generate conclusion current premise
                    rel = model_validation.find_rel_prop(subj_coords,
                                                         obj_coords)
                    prop[0] = list(rel)
                    # Add initial mod to modelslist and to neighbourhoodmodels
                    self.modelslist.append(subj_mod)
                    self.neighbourhoodmodels[0].append(subj_mod)
                    # Call a function to generate the conclusion(s) and print
                    # them. If the conclusions are different in the preferred
                    # model and in alternative models, both conclusions are
                    # printed, along with the models.
                    self.gen_and_print_conclusions(prop, subj_mod)
                    return subj_mod
                if self.v:
                    print("Verifying if the conclusion is correct!")
                mod = model_validation.verify_model(prop, subj_mod)
                if mod is not None:
                    # If premises are true in preferred model, no need for
                    # variation. Return the model.
                    if self.v:
                        print("Premises are true in model, no model variation "
                              "required (possibly invalid result if multiple "
                              "models are possible).")
                    if self.v:
                        print("Final model: \n{},".format(mod))
                    return mod
                # verify_model returned None: model variation may be required
                # to try to find a true result in the alternate models.
                if self.annotations == []:
                    # Determinate, return the false model: no model variation
                    if self.v:
                        print("Premises are false in model. No model variation "
                              "necessary!")
                    if self.v:
                        print("Final model: \n{},".format(subj_mod))
                    return mod
                # Annotation(s) present: alternative models present.
                # Model variation required.
                if self.v:
                    print("Model variation necessary as annotation(s) are present")
                # First, put the preferred model in the models list and in
                # the neighbourhood models defaultdict
                self.modelslist.append(subj_mod)
                self.neighbourhoodmodels[0].append(subj_mod)
                self.get_alternate_models(copy.deepcopy(subj_mod))
                # Print alternative models upto threhold
                self.print_alt_mods_threshold()
                # Dummy return: we are going to use self.modelsist for print
                # in this case
                return subj_mod

            # There are separate subj and obj mods which need to be combined
            # (subj_mod != obj_mod)
            if self.v:
                print("Combining 2 separate models together.")
            mod = model_combination.combine(rel, subj_coords, obj_coords,
                                            subj_mod, obj_mod)
            if self.v:
                print("Intermediate model: \n{}".format(mod))
            return mod
        elif subj_coords is not None and obj_coords is None:
            if self.v:
                print("Adding object!")
            # Subj-obj order interchanged, rel interchanged.
            rel = utilities.convert(rel)
            mod, annotations_p = model_builder.add_item_prism(subj_coords, rel,
                                                              obj, subj_mod,
                                                              self.annotations)
            if self.v:
                print("Intermediate model: \n{}".format(mod))
            #return mod
        elif subj_coords is None and obj_coords is not None:
            if self.v:
                print("Adding subject!")
            # Regular 2nd premise, rel unchanged
            mod, annotations_p = model_builder.add_item_prism(obj_coords, rel,
                                                              subj, obj_mod,
                                                              self.annotations)
            self.annotations = annotations_p
            if self.v:
                print("Intermediate model: \n{}".format(mod))
            #return mod
        else:
            if self.v:
                print("Starting model")
            mod = model_builder.start_mod(rel, subj, obj)
            if self.v:
                print("Intermediate model: \n{}".format(mod))
        # return model: applies for add subj, add obj and start model
        return mod

    def print_alt_mods_threshold(self):
        """ Function which takes the list of models, checks if the conclusion
        is true in any of the models less than threshold distance from the
        preferred model.
        NOTE: preferred model has already been found to be false"""
        # If models upto threshold contain 1 true alternative model,
        # Prism returns the incorrect response that the premise
        # follows from the previous premises.

        # Remove all models greater than the threshold. PRISM's theory says
        # that these models will not be taken into acoount by the user.
        mods_dict = {k: v for k, v in self.neighbourhoodmodels.items()
                     if k <= self.threshold}
        # Pop out the preferred model (key 0) as it has already been found to
        # be false in the calling function
        mods_dict.pop(0, None)
        # Order mods_dict by key: convert it into an OrderedDict
        mods_dict = OrderedDict(sorted(mods_dict.items()))
        # Print preferred model
        if self.v:
            print("Preferred model (premises false): {}"
                  .format(self.neighbourhoodmodels.get(0)[0]))
        # Check if all the models within threshold are true. In this case,
        # PRISM returns the possibly incorrect results that the conclusion
        # is true.
        # modelnum just keeps track of the number of the alternative model.
        modelnum = 0
        for key, mod_list in mods_dict.items():
            for mod in mod_list:
                modelnum += 1
                if self.v:
                    print("Alternative model {}: {}".format(modelnum, mod))
                if model_validation.verify_model(self.intensionalrepr[-1],
                                                 mod) is not None:
                    self.true_index = (modelnum, key)
        if self.true_index is None:
            # It still has its initial value: no change in the for loops
            if self.v:
                print("Conclusion is false in preferred and alternative "
                      "models (threshold = distance {} from the preferred model)."
                      .format(self.threshold))
        else:
            # NOTE: True index is a tuple which has alternative model number
            # and key.
            if self.v:
                print("Premises are true in alternative model {} (dist from PMM: "
                      "{}). Prism returns that the conclusion follows from the "
                      "previous premises (invalid result!). Threshold = distance {}"
                      " from the preferred model".format(self.true_index[0],
                                                         self.true_index[1],
                                                         self.threshold))

    def generate_all_models(self, mod):
        """ Function which generates all alternative models of mod and
        returns a list of all these models.
        It generates alternative models by calling the get_alternate_models
        recursive function which recursively creates alternate models for each
        of the alternate  models created in its own for loop until no more
        alternative models can be produced."""

        if self.annotations == []:
            return mod
        # Append to modelslist and the neighbourhood 0 key of the defaultdict
        # neighbourhoodmodels

        self.modelslist.append(mod)

        self.neighbourhoodmodels[0].append(mod)
        # Get alternate models by calling the foll. recursive function. It
        # recursively creates alternate models for each of the alternate
        # models created in its own for loop until no more alternative models
        # can be produced.
        self.get_alternate_models(copy.deepcopy(mod))
        # Dummy return
        return self.modelslist

    def get_alternate_models(self, mod, i=0, neighbourhood=0):
        """ Recursive: goes through annotations, find the reference object (3rd
        term in annotation) and located object (2nd term) in the model. Find
        intermed object by moving from loc to ref object in the correct dir.
        The intermediate and located objects are effectively exchanged, along
        with objects which are grouped with them.
        KWARGS: i: iteration number (used to get alternate models from each
                   of the models in the changing list self.modelslist).
                neighbourhood: current neigbhourhood
        The recursive call at the end gets alternative models for every member
        in self.modelslist: i.e. this includes models created inside the for
        loop in the previous iteration. The stopping condition is when the
        iteration variable becomes len(self.modelslist) - 1.
        """
        orig_mod = copy.deepcopy(mod)
        new_neighbourhood = neighbourhood + 1
        for annotation in self.annotations:
            # annotation: [[rel], l.o., r.o]
            # coords_to_swap is of the form [[coords_loc1, coords_int1],
            # [coords_loc2, coords_int2],...]. mod[coords_int_m] and
            # mod[coords_loc_n] need to be swapped.
            located_object = annotation[1]
            # Get the coords of reference_object and located_object in mod.
            # reference_object = annotation[2]
            ref_coords = utilities.get_coordinates_from_token(annotation[2],
                                                              orig_mod)
            loc_coords = utilities.get_coordinates_from_token(located_object,
                                                              orig_mod)
            # Move in direction negate(rel) from the located object - i.e. find
            # tokens in between ref object and loc object.
            rel_to_move = utilities.convert(annotation[0])
            # Find instances in which the reference object is found in the
            # subject of the premise (intensionalrepr), and instances in which
            # it is found in the object of the premise.
            intermediate_coords = utilities.update_coords(loc_coords,
                                                          rel_to_move)
            # If intermeidate_coords = ref_coords (and therefore, ref_obj
            # = intermediate_object, this annotation should NOT be processed.
            # The ref and loc object SHOULD NOT be swapped in any case)
            if intermediate_coords == ref_coords:
                continue
            intermediate_object = orig_mod.get(intermediate_coords)
            tokens_coordinates = self.create_groups(
                rel_to_move, (intermediate_object, intermediate_coords),
                (located_object, loc_coords), orig_mod)

            mod = spatial_array.insert_moved_objects(tokens_coordinates,
                                                     orig_mod)

            if mod not in self.modelslist:
                self.modelslist.append(mod)
                self.neighbourhoodmodels[new_neighbourhood].append(mod)

        if i < len(self.modelslist) - 1:
            # Set the value of new_neighbourhood to send in the recursive call
            # based on if a model has already been inserted in the current
            # neighbourhood or not. If not, the recursive call will have the
            # old value of neighbourhood again.
            new_neighbourhood = new_neighbourhood \
                if self.neighbourhoodmodels[new_neighbourhood] != [] \
                else neighbourhood
            # The above check produces an empty list because it's a defaultdict
            # Remove this new empty list element at a new key
            self.delete_empty_keys()
            # Recursive call with next model in modelslist
            #  newmod = copy.deepcopy(self.modelslist[i+1])
            self.get_alternate_models(copy.deepcopy(self.modelslist[i+1]),
                                      i + 1, new_neighbourhood)

    def delete_empty_keys(self):
        """ Deletes empty keys created in defaultdict self.neighbourhoodmodels
        during setting of new_neighbourhood based on an if condition."""
        empty_keys = []
        for key in self.neighbourhoodmodels.keys():
            if self.neighbourhoodmodels[key] == []:
                empty_keys.append(key)
        for empty_key in empty_keys:
            del self.neighbourhoodmodels[empty_key]

    def gen_and_print_conclusions(self, prop, mod):
        """ Function which generates the conclusion based on the intensional
        representaiton of the question premise -- prop (obtained from the rel
        between the subject and object in the question, and the model.
        OVERRIDES the spatial_reasoning function"""
        # Generate the conclusion premise in words from prop.
        conclusion = PARSER.generate_conclusion(prop)
        pmm_rel = list(utilities.relfn(prop))
        obj = utilities.objfn(prop)
        subj = utilities.subjfn(prop)
        # Replace the last premise with the generated conclusion
        self.premises[-1] = [conclusion]
        # No model variation necessary
        if self.annotations == []:
            if self.v:
                print("No model variation necessary!")
            if self.v:
                print("Conclusion: {}".format(conclusion))
            if self.v:
                print("Preferred model: {}".format(self.neighbourhoodmodels[0][0]))
            return

        if self.v:
            print("Conclusion in preferred model (usually accepted even though it "
                  "can be incorrect): {}".format(conclusion))
        if self.v:
            print("Preferred model: {}".format(self.neighbourhoodmodels[0][0]))
        if self.v:
            print("However, Prism can also look for alternative models as "
                  "annotations are present: model variation phase!")
        self.get_alternate_models(copy.deepcopy(mod))

        mods_dict = {k: v for k, v in self.neighbourhoodmodels.items()
                     if k <= self.threshold}
        # Pop out the preferred model (key 0) as we have printed out its
        # conclusion
        mods_dict.pop(0, None)
        # Order mods_dict by key: convert it into an OrderedDict
        mods_dict = OrderedDict(sorted(mods_dict.items()))

        # Check if all the models within threshold have the same conclusion as
        # the preferred model. In this case, PRISM returns the possibly
        # incorrect result (based on threshold) that the conclusion is the
        # same in all the models.
        self.alternate_model_conclusion(mods_dict, subj, obj, pmm_rel)
        if self.true_index is None:
            # It still has its initial value: no change in the for loops
            if self.v:
                print("Same conclusion found in preferred and alternative "
                      "models.")
        else:
            if self.v:
                print("NO VALID CONCLUSION possible! But Prism says that the first"
                      " conclusion generated from the preferred model is usually"
                      " accepted.")

    def alternate_model_conclusion(self, mods_dict, subj, obj, pmm_rel):
        """ Get the conclusion in the alternative models, and set
        self.true_index to (altmodel_num, dist_to_pmm) if the conclusion in
        the alternative model(s) is diff. to the conclusion in the preferred
        model (the test is based on the relation in both models). The alt
        models and the conclusions in them are also printed."""
        # modelnum just keeps track of the number of the alternative model.
        modelnum = 0
        for key, mod_list in mods_dict.items():
            for model in mod_list:
                modelnum += 1
                if self.v:
                    print("Alternative model {}: {}".format(modelnum, model))
                subj_coords = utilities.get_coordinates_from_token(subj, model)
                obj_coords = utilities.get_coordinates_from_token(obj, model)
                rel = list(model_validation.find_rel_prop(subj_coords, obj_coords))
                alt_conclusion = PARSER.generate_conclusion([rel, [subj], [obj]])
                if self.v:
                    print("Conclusion in alternative model {}: {}"
                          .format(modelnum, alt_conclusion))
                if rel != pmm_rel:
                    self.true_index = (modelnum, key)

    def create_groups(self, rel_to_move, intermediate, located, mod):
        """ Takes the cooedinates of the located and intermediate object,
        and the rel to move from the int to the loc onject, and create groups
        of tokens in a different direction to rel_to_move."""
        # Set int new coords and loc new coords to the current loc coords and
        # int coords respectively.
        int_object = intermediate[0]
        # Create a list of intermediate objects for the loop through the
        # intensional representations.
        int_obj_list = []
        int_obj_list.append(int_object)
        int_new_coords = located[1]
        loc_object = located[0]
        loc_obj_list = []
        loc_obj_list.append(loc_object)
        loc_new_coords = intermediate[1]
        # Create a dict of tokens to move. List is of form: token: newcoords
        move_tokens_coordinates = {}
        move_tokens_coordinates[int_object] = int_new_coords
        move_tokens_coordinates[loc_object] = loc_new_coords

        # Search for intermediate object in the subject and object of the
        # internsional representations of all the premises, all tokens which
        # are reached in a different direction than the direction between int,
        # ref and loc objects are to be moved (they are indirectly grouped)

        # rel_to_move is from loc object to intermediate object, as we are
        # moving from int to loc object, this has to be reversed.
        opp_rel = utilities.convert(rel_to_move)
        move_tokens_coordinates = self.loop_throught_intnesionalrepr(
            int_obj_list, opp_rel, move_tokens_coordinates, mod)

        # Search for located object in subj and obj of all the intensional
        # representations of all the premises, all tokens which are reached
        # in a different direction than the direction between int, ref and
        # loc objects are to be moved (they are indirectly grouped)

        # This time, rel_to_move is the correct direction to move: no need
        # to reverse it.
        move_tokens_coordinates = self.loop_throught_intnesionalrepr(
            loc_obj_list, rel_to_move, move_tokens_coordinates, mod)
        # Return the dictionary of the new locations where each token has to
        # be moved.
        return move_tokens_coordinates

    def loop_throught_intnesionalrepr(self, grouped_list, rel_to_move,
                                      move_tokens_coordinates, mod):
        """ Loops through intensional representations of premises and compares
        tokens to the intermediate object or located object based on the
        parameters passed, and adds to the move_token_coordinates dictionary,
        which it returns.
        ARGUMENTS: grouped_list: list of objects which have already been
                   grouped, this is used to group the next suitable object
                   which is reached from one of the grouped objects in the
                   correct direction (not same dir as rel_to_move)
                   rel_to_move: direction from ref to int to loc object"""
        # Last intensional representation is the verification/generation
        # premise (conclusion)
        for representation in self.intensionalrepr[:-1]:
            # Get groups of tokens by checking movement in dirs
            # other than that between the loc.obj, int.obj and ref.obj
            # (done using not same_dir_movement).

            # Find if current object is in subj or obj position in
            # the prems. Eg. representation[1] = ['[]'], [1][0] = '[]'
            if representation[1][0] in grouped_list and \
                not utilities.same_dir_movement(rel_to_move, representation[0]):
                # intermediate_obj is the subject in prop, we need
                # to move in neg(rel) from subj to obj
                cur_object = representation[2][0]
                grouped_list.append(cur_object)
                cur_coords = utilities.get_coordinates_from_token(
                    cur_object, mod)
                curobj_new_coords = utilities.list_add(cur_coords, rel_to_move)
                move_tokens_coordinates[cur_object] = curobj_new_coords

            elif representation[2][0] in grouped_list and \
                  not utilities.same_dir_movement(rel_to_move, representation[0]):
                   # intermediate obj is the object in prop, we need
                   # to move in rel from obj to subj
                cur_object = representation[1][0]
                grouped_list.append(cur_object)
                cur_coords = utilities.get_coordinates_from_token(
                    cur_object, mod)
                curobj_new_coords = utilities.list_add(cur_coords, rel_to_move)
                move_tokens_coordinates[cur_object] = curobj_new_coords
        return move_tokens_coordinates
