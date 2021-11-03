import ccobra
import numpy as np
import time

import spatialreasoner as sr

class SpatialReasoner(ccobra.CCobraModel):
    def __init__(self, name='SpatialReasoner', decide_method='adapted'):
        super(SpatialReasoner, self).__init__(
            name, ['spatial-relational'], ['verify', "accept", 'single-choice'])

        # Determine decide method
        if decide_method not in ['skeptical', 'credulous', 'initial', 'adapted']:
            raise ValueError(
                "decide_method not in ['skeptical', 'credulous', 'initial', 'adapted']: {}".format(
                    decide_method))
        self.decide_method = decide_method

        # Initialize spatialreasoner
        ccl = sr.ccl.ClozureCL()
        self.model = sr.spatialreasoner.SpatialReasoner(ccl)

        # Prepare spatialreasoner templates
        self.PREMISE_TEMPLATE = 'the {} is {} the {}'
        self.REL_MAP = {
            'left': 'on the left of',
            'right': 'on the right of',
            'behind': 'behind',
            'front': 'in front of',
            'above': 'above',
            'below': 'below',
            'north': 'north',
            'south': 'south',
            'east': 'east',
            'west': 'west',
            'north-west': 'north-west',
            'north-east': 'north-east',
            'south-west': 'south-west',
            'south-east': 'south-east'
        }

        # Initialize adaption parameters
        self.skeptical_score = 0
        self.credulous_score = 0
        self.initial_score = 0

    def __deepcopy__(self, memo):
        return SpatialReasoner(self.name, self.decide_method)

    def start_participant(self, **kwargs):
        self.start_time = time.time()

    def end_participant(self, identifier, model_log, **kwargs):
        print("Finished participant {} ({:.2f}s)".format(identifier, time.time() - self.start_time))

        if self.decide_method == "adapted":
            print("    Skeptical:", self.skeptical_score)
            print("    Credulous:", self.credulous_score)
            print("    Initial:  ", self.initial_score)

            model_log["skeptical"] = self.skeptical_score
            model_log["credulous"] = self.credulous_score
            model_log["initial"] = self.initial_score

        self.model.terminate()

    def normalize_task(self, task, choice):
        full_task = task + choice

        # Extract unique terms
        terms = []
        for premise in full_task:
            for term in premise[1:]:
                if term not in terms:
                    terms.append(term)

        # Prepare text replacement dictionary
        replacement_dict = {}
        for idx, term in enumerate(terms):
            replacement_dict[term] = sr.spatialreasoner.TERMS[idx]

        # Prepare normalized problem
        norm_problem = []
        for premise in full_task:
            norm_premise = self.PREMISE_TEMPLATE.format(
                replacement_dict[premise[1]],
                self.REL_MAP[premise[0].lower()],
                replacement_dict[premise[2]]
            )
            norm_problem.append(norm_premise)

        return norm_problem

    def predict(self, item, **kwargs):
        # the handling of accept-tasks is currently the same as for verification tasks,
        # as the general approach relies on adding the conclusion to the premises
        # and check for consistency problems
        if item.response_type == 'verify' or item.response_type == 'accept':
            norm_problem = self.normalize_task(item.task, item.choices[0])
            sub_predictions = self.model.query(norm_problem)
            self.last_responses = (item.choices[0], sub_predictions)
            prediction = np.all([self.decide(x) for x in sub_predictions])
            return prediction
        elif item.response_type == 'single-choice':
            possible_predictions = []
            for choice in item.choices:
                norm_problem = self.normalize_task(item.task, choice)
                prediction = self.model.query(norm_problem)[0]
                possible_predictions.append((choice, prediction))

            self.last_responses = possible_predictions

            decisions = [(x, self.decide(y)) for x, y in possible_predictions]
            pred_filter = [x for x, y in decisions if y]

            if not pred_filter:
                return item.choices[int(np.random.randint(0, len(item.choices)))]
            else:
                return pred_filter[int(np.random.randint(0, len(pred_filter)))]

    def decide(self, prediction):
        if self.decide_method == 'skeptical':
            return self.decide_skeptical(prediction)
        if self.decide_method == 'credulous':
            return self.decide_credulous(prediction)
        if self.decide_method == 'initial':
            return self.decide_initial(prediction)
        if self.decide_method == 'adapted':
            return self.decide_adapted(prediction)
        else:
            raise ValueError('Invalid decide method: {}'.format(prediction))

    def decide_skeptical(self, prediction):
        if prediction == 'true':
            return True
        elif prediction == 'false':
            return False
        elif prediction == 'indeterminate-true':
            return False
        elif prediction == 'indeterminate-false':
            return False
        else:
            raise ValueError('Invalid prediction: {}'.format(prediction))

    def decide_credulous(self, prediction):
        if prediction == 'true':
            return True
        elif prediction == 'false':
            return False
        elif prediction == 'indeterminate-true':
            return True
        elif prediction == 'indeterminate-false':
            return True
        else:
            raise ValueError('Invalid prediction: {}'.format(prediction))

    def decide_initial(self, prediction):
        if prediction == 'true':
            return True
        elif prediction == 'false':
            return False
        elif prediction == 'indeterminate-true':
            return True
        elif prediction == 'indeterminate-false':
            return False
        else:
            raise ValueError('Invalid prediction: {}'.format(prediction))

    def decide_adapted(self, prediction):
        participant_scheme = np.argmax([
            self.skeptical_score,
            self.credulous_score,
            self.initial_score
        ])

        if participant_scheme == 0:
            return self.decide_skeptical(prediction)
        elif participant_scheme == 1:
            return self.decide_credulous(prediction)
        else:
            return self.decide_initial(prediction)

    def pre_train_person(self, dataset):
        if self.decide_method != 'adapted':
            return

        for task_data in dataset:
            self.predict(task_data['item'])
            self.adapt(task_data['item'], task_data['response'])

    def adapt(self, item, truth, **kargs):
        if self.decide_method != 'adapted':
            return

        if item.response_type == 'verify':
            sub_predictions = self.last_responses[1]
            pred_skeptical = np.all([self.decide_skeptical(x) for x in sub_predictions])
            if pred_skeptical == truth:
                self.skeptical_score += 1

            pred_credulous = np.all([self.decide_credulous(x) for x in sub_predictions])
            if pred_credulous == truth:
                self.credulous_score += 1

            pred_initial = np.all([self.decide_initial(x) for x in sub_predictions])
            if pred_initial == truth:
                self.initial_score += 1
        elif item.response_type == 'single-choice':
            possible_predictions = self.last_responses

            decisions_skeptical = [(x, self.decide_skeptical(y)) for x, y in possible_predictions]
            skeptical_preds = [x for x, y in decisions_skeptical if y]
            if truth in skeptical_preds:
                self.skeptical_score += 1/len(skeptical_preds)

            decisions_credulous = [(x, self.decide_credulous(y)) for x, y in possible_predictions]
            credulous_preds = [x for x, y in decisions_credulous if y]
            if truth in credulous_preds:
                self.credulous_score += 1/len(credulous_preds)

            decisions_initial = [(x, self.decide_initial(y)) for x, y in possible_predictions]
            initial_preds = [x for x, y in decisions_initial if y]
            if truth in initial_preds:
                self.initial_score += 1/len(initial_preds)
