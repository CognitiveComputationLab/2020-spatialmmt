import ccobra
import numpy as np

TASK_LIST_4PS = [
    'Left;A;B/Left;B;C/Left;C;D',
    'Left;A;B/Left;C;D/Left;B;C',
    'Left;B;C/Left;A;B/Left;C;D',
    'Left;B;C/Left;C;D/Left;A;B',
    'Left;C;D/Left;A;B/Left;B;C',
    'Left;C;D/Left;B;C/Left;A;B',
    'Right;B;A/Right;C;B/Right;D;C',
    'Right;B;A/Right;D;C/Right;B;C',
    'Right;C;B/Right;B;A/Right;D;C',
    'Right;C;B/Right;D;C/Right;B;A',
    'Right;D;C/Right;B;A/Right;C;B',
    'Right;D;C/Right;C;B/Right;B;A',
]

RESPONSE_LIST_4PS = [
    'Left;A;D',
    'Left;D;A',
    'Right;A;B',
    'Right;D;A',
]

class MFA(ccobra.CCobraModel):
    def __init__(self, name='MFA'):
        super(MFA, self).__init__(name, ['spatial-relational'], ['verify', 'accept'])

        self.mfa = np.zeros((len(TASK_LIST_4PS) * len(RESPONSE_LIST_4PS),))

    def pre_train(self, dataset):
        for subj_data in dataset:
            for task_data in subj_data:
                # Get idxs
                task_idx = TASK_LIST_4PS.index(task_data['item'].task_str)
                resp_idx = RESPONSE_LIST_4PS.index(task_data['item'].choices_str)
                self.mfa[task_idx * len(RESPONSE_LIST_4PS) + resp_idx] += (int(task_data['response']) * 2) - 1

    def predict(self, item, **kwargs):
        # Get idxs
        task_idx = TASK_LIST_4PS.index(item.task_str)
        resp_idx = RESPONSE_LIST_4PS.index(item.choices_str)
        if self.mfa[task_idx * len(RESPONSE_LIST_4PS) + resp_idx] > 0:
            return True
        else:
            return False
