""" Interface for .

"""

import sys, os, importlib
from collections import OrderedDict
from pprint import pprint
import numpy as np
import ccobra
import pandas as pd
from spatialreasoner.prism import Prism


UP = np.array([0,1,0])
DOWN = UP * (-1)
RIGHT = np.array([1,0,0])
LEFT = RIGHT * (-1)


interpretations = OrderedDict([("north", UP), ("south", DOWN), ("west", LEFT), ("east", RIGHT),
                               ("north-west", UP + LEFT), ("north-east", UP + RIGHT), 
                               ("south-west", DOWN + LEFT), ("south-east", DOWN + RIGHT)])

correspondence = {"left":"west", "right":"east", "above":"north", "below":"south",
                  "west":"left", "east":"right", "north":"above", "south":"below"}

VP = 0


def interpretation(relation, cd):
    if not cd:
        relation = correspondence[relation.lower()]
    return interpretations[relation.lower()]


def build_model(model, cd):
    print(model)
    model = [[interpretation(prem[0], cd)] + prem[1:] for prem in model]
    rep = {}
    objects = []
    for prem in model:
        items = prem[1], prem[2]
        for i in items:
            if i not in objects:
                objects.append(i)

    base = np.zeros(3, dtype=int)

    for obj in objects:
        rep[tuple(base)] = obj
        base += RIGHT

    return rep


class PrismModel(ccobra.CCobraModel):
    """ Model producing randomly generated responses.

    """

    def __init__(self, name='PRISM\n(Ragni & Knauff, 2013)'):
        """ Initializes the random model.

        Parameters
        ----------
        name : str
            Unique name of the model. Will be used throughout the CCOBRA
            framework as a means for identifying the model.

        """

        super(PrismModel, self).__init__(
            name, ["spatial-relational"], ["verify", "single-choice", "model_constr"])

        self.prism = Prism(True)
        self.last_prediction = None
        self.counter = 0
        self.model = None
    
    def start_participant(self, **kwargs):
        global VP
        VP += 1


    def target_positions_in_model(self, targets, model):
        target_positions = {}
        for position, obj in model.items():
            if obj in targets:
                target_positions[obj] = np.array(position)
        return target_positions

    def predict_model_constr(self, premises, choices, cd):
        model = self.prism.build_model(premises)[-1]
        self.model = model
        return model

    def predict_verification(self, premises, choices, cd):
        
        model_verification = len(choices[0]) != 1 # is the choice a model?
        model = self.prism.build_model(premises)[-1]
        self.model = model

        # if the task is a model verification, build the deduced model and compare
        if model_verification:
            choice = build_model(choices[0], cd)
            # [['West', 'lime tree', 'apricot tree'], ['West', 'apricot tree', 'plum tree'], ['West', 'plum tree', 'kiwi tree'], ['West', 'kiwi tree', 'fig tree']]
            # ->  {(0, 0, 0): 'lime tree', (1, 0, 0): 'apricot tree', (2, 0, 0): 'plum tree', (3, 0, 0): 'kiwi tree', (4, 0, 0): 'fig tree'}

            # is the model determinate? if not, annotations exist
            determinate = not self.prism.annotations
            if not determinate:
                alternate = self.prism.generate_all_models(model)
                return choice in alternate
            return choice == model
        
        # else the task is a single relation verification
        choice = choices[0][0]
        choice[0] = interpretation(choice[0], cd)
    
        validity = self.prism.decide(self.prism.build_premise(*choice), [model])
        return validity != [None]

    def predict_single_choice(self, premises, choices, cd):

        targets = [choices[0][0][1], choices[0][0][2]]
        model = self.prism.build_model(premises)[-1]
        self.model = model

        # position of the targets in the model
        target_positions = self.target_positions_in_model(targets, model)

        # the direction from target 0 to target 1
        diff = target_positions[targets[0]] - target_positions[targets[1]]

        # the solution is the direction that maximizes the dot product
        directions = list(interpretations.items())
        solution = directions[np.argmax([np.dot(diff, value) for key, value in directions])][0]

        # is the model determinate? if not, annotations exist
        determinate = not self.prism.annotations
        if not determinate:
            # right now, nothing is done with it
            alternate = self.prism.generate_all_models(model)

        for choice in choices:
            choice = choice[0]
            if choice[0] == solution:
                return choice


    def predict(self, item, **kwargs):
        """ Predicts weighted responses to a given syllogism.

        Parameters
        ----------
        task : str
            task to produce a response for.

        """
        self.counter += 1


        self.prism = self.prism.reset()

        cd = item.task[0][0].lower() not in ["left", "right","above", "below"]
        premises = [[interpretation(prem[0], cd)] + prem[1:] for prem in item.task]

        if item.response_type == 'verify':
            self.last_prediction = self.predict_verification(premises, item.choices, cd)
        
        elif item.response_type == 'single-choice':
            self.last_prediction = self.predict_single_choice(premises, item.choices, cd)

        elif item.response_type == 'model_constr':
            self.last_prediction = self.predict_model_constr(premises, item.choices, cd)
            print(self.last_prediction)
        
        return self.last_prediction

        return False # default
