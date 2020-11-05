import numpy as np
import ccobra

class MFA(ccobra.CCobraModel):
    def __init__(self, name='MFA'):
        super(MFA, self).__init__(name, ['spatial-relational'], ['verify'])

        self.mfa = None

    def pre_train(self, dataset, **kwargs):
        self.mfa = {}
        for subj_data in dataset:
            for task_data in subj_data:
                task_str = '{}_{}'.format(task_data['item'].task_str, task_data['item'].choices_str)
                if not task_str in self.mfa:
                    self.mfa[task_str] = 0
                self.mfa[task_str] += (int(task_data['response']) * 2) - 1

    def pre_train_person(self, data, **kwargs):
        for task_data in data:
            task_str = '{}_{}'.format(task_data['item'].task_str, task_data['item'].choices_str)
            if not task_str in self.mfa:
                assert False
                self.mfa[task_str] = 0
            self.mfa[task_str] += (int(task_data['response']) * 2) - 1

    def predict(self, item, **kwargs):
        task_str = '{}_{}'.format(item.task_str, item.choices_str)

        if self.mfa[task_str] > 0:
            return True
        elif self.mfa[task_str] < 0:
            return False
        else:
            return np.random.choice([True, False])

    def adapt(self, item, truth, **kwargs):
        pass