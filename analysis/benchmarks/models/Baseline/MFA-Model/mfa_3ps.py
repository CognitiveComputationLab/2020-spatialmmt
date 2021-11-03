import ccobra
import numpy as np

REL_MAP = {
    'left': 'L',
    'right': 'R'
}

class Relational():
    def __init__(self, item):
        # Extract premises
        self.premise1 = item.task[0]
        self.premise2 = item.task[1]

        # Extract terms
        end1 = set(self.premise1[1:]).difference(set(self.premise2[1:]))
        end2 = set(self.premise2[1:]).difference(set(self.premise1[1:]))
        mid = set(self.premise1[1:]).intersection(set(self.premise2[1:]))
        assert len(end1) == 1 and len(end2) == 1 and len(mid) == 1
        self.terms = {'A': list(end1)[0], 'B': list(mid)[0], 'C': list(end2)[0]}

        # Extract figure
        pos1 = self.premise1[1:].index(self.terms['A'])
        pos2 = self.premise2[1:].index(self.terms['C'])

        if (pos1, pos2) == (0, 1):
            self.figure = 1
        elif (pos1, pos2) == (1, 0):
            self.figure = 2
        elif (pos1, pos2) == (0, 0):
            self.figure = 3
        elif (pos1, pos2) == (1, 1):
            self.figure = 4
        else:
            raise ValueError('Invalid Figure encountered')

        # Encode the task
        rel1 = REL_MAP[self.premise1[0].lower()]
        rel2 = REL_MAP[self.premise2[0].lower()]
        self.enc_task = rel1 + rel2 + str(self.figure)

        # Copy auxiliary information
        self.choices = item.choices

    def encode_response(self, response):
        enc_rel = REL_MAP[response[0].lower()]

        direction = None
        if (response[1] == self.terms['A']) and (response[2] == self.terms['C']):
            direction = 'ac'
        elif (response[1] == self.terms['C']) and (response[2] == self.terms['A']):
            direction = 'ca'
        else:
            raise ValueError('Invalid direction')

        return enc_rel + direction

    def __repr__(self):
        s = ['Relational {}:'.format(self.enc_task)]
        s.append('   premise1: {}'.format(self.premise1))
        s.append('   premise2: {}'.format(self.premise2))
        s.append('   figure: {}'.format(self.figure))
        s.append('   choices: {}'.format(self.choices))
        return '\n'.join(s)


class MFA(ccobra.CCobraModel):
    def __init__(self, name='MFA'):
        super(MFA, self).__init__(name, ['spatial-relational'], ['verify', 'accept'])

        # Define supported tasks and relations
        self.supp_relas = ['left', 'right']
        self.supp_tasks = ['LL1', 'LR3', 'RL4', 'RR2']
        self.supp_resps = ['Lac', 'Lca', 'Rac', 'Rca']

        # Define the MFA memory
        self.database = np.zeros((len(self.supp_tasks) * (len(self.supp_relas) * 2))) - 1

    def pre_train(self, dataset):
        for subj_data in dataset:
            for task_data in subj_data:
                item = task_data['item']
                response = task_data['response']

                # Extract the task idx
                rel = Relational(item)
                enc_task = rel.enc_task
                task_idx = self.supp_tasks.index(enc_task)

                # Extract the response idx
                concl = item.choices[0][0]
                enc_concl = rel.encode_response(concl)
                concl_idx = self.supp_resps.index(enc_concl)

                # Store the response in the database
                self.database[task_idx * len(self.supp_resps) + concl_idx] += (2 * int(response)) - 1

    def predict(self, item, **kwargs):
        # Extract task information
        rel = Relational(item)
        enc_task = rel.enc_task
        task_idx = self.supp_tasks.index(enc_task)

        # Extract the response idx
        concl = item.choices[0][0]
        enc_concl = rel.encode_response(concl)
        concl_idx = self.supp_resps.index(enc_concl)

        # Determine the prediction
        value = self.database[task_idx * len(self.supp_resps) + concl_idx]

        # Determine the response to predict
        if value > 0:
            return True
        else:
            return False

    def adapt(self, item, truth, **kwargs):
        # Extract task information
        rel = Relational(item)
        enc_task = rel.enc_task
        task_idx = self.supp_tasks.index(enc_task)

        # Extract the response idx
        concl = item.choices[0][0]
        enc_concl = rel.encode_response(concl)
        concl_idx = self.supp_resps.index(enc_concl)

        # Update the database
        profile_idx = task_idx * len(self.supp_resps) + concl_idx
        self.database[profile_idx] += (2 * int(truth)) - 1
