import ccobra
import numpy as np


REL_MAP = {
    'east': 'E',
    'north': 'N',
    'north-east': 'NE',
    'north-west': 'NW',
    'south': 'S',
    'south-east': 'SE',
    'south-west': 'SW',
    'west': 'W',
}

class Relational():
    def __init__(self, item):
        # Extract premise information
        self.prem1 = item.task[0]
        self.prem2 = item.task[1]

        # Encode relations
        q1 = REL_MAP[self.prem1[0]]
        q2 = REL_MAP[self.prem2[0]]

        # Determine figure
        terms_q1 = set(self.prem1[1:])
        terms_q2 = set(self.prem2[1:])

        end1 = terms_q1.difference(terms_q2)
        end2 = terms_q2.difference(terms_q1)
        mid = terms_q1.intersection(terms_q2)
        assert len(end1) == 1 and len(end2) == 1 and len(mid) == 1

        self.terms = {'A': list(end1)[0], 'B': list(mid)[0], 'C': list(end2)[0]}

        pos_end1 = self.prem1[1:].index(self.terms['A'])
        pos_end2 = self.prem2[1:].index(self.terms['C'])

        fig = None
        if pos_end1 == 0 and pos_end2 == 1:
            fig = 1
        elif pos_end1 == 1 and pos_end2 == 0:
            fig = 2
        elif pos_end1 == 0 and pos_end2 == 0:
            fig = 3
        elif pos_end1 == 1 and pos_end2 == 1:
            fig = 4
        else:
            raise ValueError('Invalid figure')

        self.encoded_task = '{}{}{}'.format(q1, q2, fig)

    def encode_response(self, resp):
        assert len(resp) == 1
        resp = resp[0]

        enc_term1 = [x for x, y in self.terms.items() if y == resp[1]]
        enc_term2 = [x for x, y in self.terms.items() if y == resp[2]]
        assert len(enc_term1) == 1 and len(enc_term2) == 1

        return '{}{}{}'.format(REL_MAP[resp[0]], enc_term1[0].lower(), enc_term2[0].lower())

    def decode_response(self, resp):
        enc_rel = resp[:-2]
        direction = resp[-2:]

        relation = [x for x, y in REL_MAP.items() if y == enc_rel]
        assert len(relation) == 1
        relation = relation[0]

        response = '{};{};{}'.format(relation, self.terms[direction[0].upper()], self.terms[direction[1].upper()])
        return response

    def __repr__(self):
        s = []
        s.append('Relational {}'.format(self.encoded_task))
        s.append('   prem1: {}'.format(self.prem1))
        s.append('   prem2: {}'.format(self.prem2))
        s.append('   terms: {}'.format(self.terms))

        return '\n'.join(s)

class MFA(ccobra.CCobraModel):
    def __init__(self, name='MFA'):
        super(MFA, self).__init__(name, ['spatial-relational'], ['single-choice'])

        # Initialize task and response lists
        task_list = []
        for rel1 in REL_MAP.values():
            for rel2 in REL_MAP.values():
                task_list.append('{}{}1'.format(rel1, rel2))
        self.TASK_LIST = task_list

        resp_list = []
        for rel in REL_MAP.values():
            resp_list.append('{}ca'.format(rel))
        self.RESP_LIST = resp_list

        # Initialize MFA list
        self.mfa = np.zeros((len(self.TASK_LIST), len(self.RESP_LIST)))

    def pre_train(self, dataset):
        for subj_data in dataset:
            for task_data in subj_data:
                # Obtain task and response encodings
                rel = Relational(task_data['item'])
                resp_enc = rel.encode_response(task_data['response'])

                # Update MFA database
                task_idx = self.TASK_LIST.index(rel.encoded_task)
                resp_idx = self.RESP_LIST.index(resp_enc)
                self.mfa[task_idx, resp_idx] += 1

    def predict(self, item, **kwargs):
        # Obtain MFA predictions
        rel = Relational(item)
        task_idx = self.TASK_LIST.index(rel.encoded_task)

        mfa_vec = self.mfa[task_idx]
        max_idxs = [x for x, y in enumerate(mfa_vec) if y == mfa_vec.max()]

        pred_idx = np.random.choice(max_idxs)
        pred = self.RESP_LIST[pred_idx]
        return rel.decode_response(pred)
