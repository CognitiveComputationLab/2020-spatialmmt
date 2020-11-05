import ccobra
import numpy as np

def task_to_placeholder(task):
    split = [x.split(';') for x in task.split('/')]

    # Extract the terms
    prem_terms = [set(x[1:]) for x in split]
    all_terms = []
    for x in prem_terms:
        all_terms.extend(x)

    term_cnts = np.unique(all_terms, return_counts=True)

    # Extract identifiable placeholder terms
    term_two = [x for x, y in zip(*term_cnts) if y == 2]
    assert len(term_two) == 1
    term_two = term_two[0]

    term_three = [x for x, y in zip(*term_cnts) if y == 3]
    assert len(term_three) == 1
    term_three = term_three[0]

    # Generate placeholder dict
    plh = {term_two: 'D', term_three: 'E'}
    placeholders = ['A', 'B', 'C']
    for term in all_terms:
        if term in plh:
            continue
        plh[term] = placeholders.pop(0)

    return plh

def get_plh_task(task, plh):
    plh_task = task
    for term, enc in plh.items():
        plh_task = plh_task.replace(term, enc)

    if plh_task.endswith('/'):
        plh_task = plh_task[:-1]

    return plh_task

class MFA(ccobra.CCobraModel):
    def __init__(self, name='MFA'):
        super(MFA, self).__init__(name, ['spatial-relational'], ['verify'])

        # Task structure
        self.task_list = [
            'West;E;A/West;B;E/West;E;D/West;D;C',
            'West;A;E/West;E;B/West;E;D/West;D;C',
            'Left;E;A/Left;B;E/Left;E;D/Left;D;C',
            'Left;A;E/Left;E;B/Left;E;D/Left;D;C',
        ]

        self.choices_list = [
            'Left;A;B/Left;B;E/Left;E;D/Left;D;C',
            'Left;A;D/Left;D;B/Left;B;E/Left;E;C',
            'Left;A;E/Left;E;B/Left;B;D/Left;D;C',
            'Left;A;E/Left;E;D/Left;D;B/Left;B;C',
            'Left;A;E/Left;E;D/Left;D;C/Left;C;B',
            'Left;B;A/Left;A;E/Left;E;D/Left;D;C',
            'Left;B;D/Left;D;A/Left;A;E/Left;E;C',
            'Left;B;E/Left;E;A/Left;A;D/Left;D;C',
            'Left;B;E/Left;E;D/Left;D;A/Left;A;C',
            'Left;B;E/Left;E;D/Left;D;C/Left;C;A',
            'Left;C;E/Left;E;A/Left;A;D/Left;D;B',
            'Left;C;E/Left;E;B/Left;B;D/Left;D;A',
            'West;A;B/West;B;E/West;E;D/West;D;C',
            'West;A;D/West;D;B/West;B;E/West;E;C',
            'West;A;E/West;E;B/West;B;D/West;D;C',
            'West;A;E/West;E;D/West;D;B/West;B;C',
            'West;A;E/West;E;D/West;D;C/West;C;B',
            'West;B;A/West;A;E/West;E;D/West;D;C',
            'West;B;D/West;D;A/West;A;E/West;E;C',
            'West;B;E/West;E;A/West;A;D/West;D;C',
            'West;B;E/West;E;D/West;D;A/West;A;C',
            'West;B;E/West;E;D/West;D;C/West;C;A',
            'West;C;E/West;E;A/West;A;D/West;D;B',
            'West;C;E/West;E;B/West;B;D/West;D;A',
        ]

        # MFA storage
        self.mfa = np.zeros((len(self.task_list) * len(self.choices_list),))

    def pre_train(self, dataset):
        for subj_data in dataset:
            for task_data in subj_data:
                # Encode task and choices
                plh = task_to_placeholder(task_data['item'].task_str)
                enc_task = get_plh_task(task_data['item'].task_str, plh)
                enc_choices = get_plh_task(task_data['item'].choices_str, plh)

                # Update MFA storage
                task_idx = self.task_list.index(enc_task)
                chioces_idx = self.choices_list.index(enc_choices)
                idx = task_idx * len(self.choices_list) + chioces_idx

                self.mfa[idx] += (int(task_data['response']) * 2) - 1

    def predict(self, item, **kwargs):
        # Encode task and choices
        plh = task_to_placeholder(item.task_str)
        enc_task = get_plh_task(item.task_str, plh)
        enc_choices = get_plh_task(item.choices_str, plh)

        # Update MFA storage
        task_idx = self.task_list.index(enc_task)
        chioces_idx = self.choices_list.index(enc_choices)
        idx = task_idx * len(self.choices_list) + chioces_idx

        return self.mfa[idx] > 0
