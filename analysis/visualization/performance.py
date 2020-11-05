import sys
import os

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
import seaborn as sns


if len(sys.argv) < 2:
    print('usage: python performance.py <ccobra-csv> [out-file]')
    exit(99)

csv_file = sys.argv[1]
out_file = 'plot.pdf'
if len(sys.argv) >= 3:
    out_file = sys.argv[2]

# Load the data
result_df = pd.read_csv(csv_file)
result_df['model'] = result_df['model'].apply(lambda x: x.replace('\r', ''))

# Model names
result_df['model'] = result_df['model'].apply(
    lambda x: x.replace('Verbal Reasoning', 'VerbalReasoner').replace('SpatialReasoner-', 'SpatialReasoner\n'))

BASELINE_MODELS = [
    'RandomModel',
    'TransitiveClosure',
    'MFA',
    'Optimal',
    'BestModel'
]

# Task-based optimal model
optim_data = []
for item, task_df in result_df[~result_df['model'].isin(BASELINE_MODELS)].groupby(['id', 'sequence']):
    response_type = task_df['response_type'].unique()
    assert len(response_type) == 1
    response_type = response_type[0]

    optim_data.append({
        'model': 'Optimal',
        'id': item[0],
        'domain': 'spatial-relational',
        'response_type': response_type,
        'sequence': item[1],
        'task': np.nan,
        'choices': np.nan,
        'truth': np.nan,
        'prediction': np.nan,
        'score_response': task_df['score_response'].max(),
        'task_enc': np.nan,
        'truth_enc_response': np.nan,
        'prediction_enc_response': np.nan
    })

result_df = pd.concat([result_df, pd.DataFrame(optim_data)], sort=True)

# Aggregate the data
subj_df = result_df.groupby(['id', 'model'], as_index=False)['score_response'].agg('mean')

# Attach optimal model to df
optim_data = []
for dude_id, dude_df in subj_df[~subj_df['model'].isin(BASELINE_MODELS)].groupby('id'):
    optim_score = dude_df['score_response'].max()
    optim_data.append({
        'id': dude_id,
        'model': 'BestModel',
        'score_response': optim_score
    })

subj_df = pd.concat([subj_df, pd.DataFrame(optim_data)], sort=True)

# Determine plot order
order = subj_df.groupby(
    'model', as_index=False)['score_response'].agg('median').sort_values('score_response')['model']
order = [x for x in order if x not in ['BestModel', 'Optimal']] + ['BestModel', 'Optimal']

# Color type assignment
ctype_dict = {
    'RandomModel': 'C0',
    'MFA': 'C0',
    'PRISM\n(Ragni & Knauff, 2013)': 'C1',
    'SpatialReasoner\nadapted': 'C1',
    'SpatialReasoner\ncredulous': 'C1',
    'SpatialReasoner\ninitial': 'C1',
    'SpatialReasoner\nskeptical': 'C1',
    'TransitiveClosure': 'C0',
    'VerbalReasoner\n(Krumnack et al., 2010)': 'C1',
    'BestModel': 'C0',
    'Optimal': 'C0'
}

cols = [ctype_dict[x] for x in order]

# Plot the data
sns.set(style='whitegrid', palette='colorblind')
plt.figure(figsize=(10, 5))

sns.boxplot(x='model', y='score_response', data=subj_df, order=order, palette=cols)
sns.swarmplot(x='model', y='score_response', data=subj_df, order=order, color='#000000', size=4, alpha=0.7)

plt.xlabel('')
plt.xticks(rotation=45)

plt.ylabel('Predictive Accuracy')
plt.ylim(0, 1.05)

sns.despine()

# Legend specification
lels = [
    Line2D([0], [0], marker='o', color='w', markerfacecolor='C0', markersize=10, label='Baseline Models'),
    Line2D([0], [0], marker='o', color='w', markerfacecolor='C1', markersize=10, label='Cognitive Models'),
]
plt.legend(
    handles=lels, bbox_to_anchor=(0., 1.02, 1., .102), loc='center', ncol=2, borderaxespad=0.,
    frameon=False
)

plt.tight_layout()
plt.savefig(out_file)
plt.show()
