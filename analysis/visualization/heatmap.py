import pandas as pd
import numpy as np
import matplotlib as mpl
import matplotlib.pyplot as plt
import seaborn as sns


BASELINE_MODELS = [
    'RandomModel',
    'TransitiveClosure',
    'MFA',
    'Optimal',
    'BestModel'
]

# Load datasets
df_4ps = pd.read_csv('../benchmarks/4ps.csv')
df_smalllarge = pd.read_csv('../benchmarks/smalllarge.csv')

df_4ps['model'] = df_4ps['model'].apply(lambda x: x.replace('\r', ''))
df_smalllarge['model'] = df_smalllarge['model'].apply(lambda x: x.replace('\r', ''))

# Model names
df_4ps['model'] = df_4ps['model'].apply(
    lambda x: x.replace('Verbal Reasoning', 'VerbalReasoner').replace('SpatialReasoner-', 'SpatialReasoner\n'))
df_smalllarge['model'] = df_smalllarge['model'].apply(
    lambda x: x.replace('Verbal Reasoning', 'VerbalReasoner').replace('SpatialReasoner-', 'SpatialReasoner\n'))

# Attach max scores
df_4ps_max = df_4ps[~df_4ps['model'].isin(BASELINE_MODELS)].groupby(['id', 'sequence'], as_index=False)['score_response'].agg('max').rename(columns={'score_response': 'max_score'})
df_4ps = df_4ps.merge(df_4ps_max, on=['id', 'sequence'])

df_smalllarge_max = df_smalllarge[~df_smalllarge['model'].isin(BASELINE_MODELS)].groupby(['id', 'sequence'], as_index=False)['score_response'].agg('max').rename(columns={'score_response': 'max_score'})
df_smalllarge = df_smalllarge.merge(df_smalllarge_max, on=['id', 'sequence'])

# Extract meta information
mlist_4ps = df_4ps[~df_4ps['model'].isin(BASELINE_MODELS)]['model'].unique().tolist()
mlist_smalllarge = df_smalllarge[~df_smalllarge['model'].isin(BASELINE_MODELS)]['model'].unique().tolist()
assert sorted(mlist_4ps) == sorted(mlist_smalllarge)
model_list = mlist_4ps

def get_neighborhood_mat(df):
    neighborhood = np.zeros((len(model_list), len(model_list)))
    counts = np.zeros((len(model_list,)))

    for item, task_df in df[~df['model'].isin(BASELINE_MODELS)].groupby(['id', 'sequence']):
        # Ignore 0 max scores
        max_score = task_df['max_score'].unique()
        assert len(max_score) == 1
        if max_score == 0:
            continue

        bestest = task_df[task_df['score_response'] == task_df['max_score']]['model'].values
        for source in bestest:
            source_idx = model_list.index(source)
            counts[source_idx] += 1
            for target in bestest:
                target_idx = model_list.index(target)
                neighborhood[source_idx, target_idx] += 1

    neighborhood /= counts.reshape(-1, 1)

    return neighborhood

# Obtain neighborhoods
nh_4ps = get_neighborhood_mat(df_4ps)
nh_smalllarge = get_neighborhood_mat(df_smalllarge)

# Plot neighborhoods
sns.set(style='whitegrid', palette='colorblind')
fig, axs = plt.subplots(1, 2, figsize=(10, 6))
ax_cbar = fig.add_axes([.9, 0.4, .02, .475])

sns.heatmap(nh_4ps, lw=1, cmap='Blues', annot=True, vmin=0, vmax=1, cbar=False, ax=axs[0])
sns.heatmap(nh_smalllarge, lw=1, cmap='Blues', annot=True, vmin=0, vmax=1, cbar=True, ax=axs[1], cbar_ax=ax_cbar)
# mpl.colorbar.ColorbarBase(axs[2], cmap='Blues', norm=mpl.colors.Normalize(vmin=0, vmax=1), orientation='horizontal')

axs[0].set_xticks(np.arange(len(model_list)) + 0.5)
axs[0].set_xticklabels(model_list, rotation=90)
axs[1].set_xticks(np.arange(len(model_list)) + 0.5)
axs[1].set_xticklabels(model_list, rotation=90)

axs[0].set_yticks(np.arange(len(model_list)) + 0.5)
axs[0].set_yticklabels(model_list, rotation=0)
axs[1].set_yticklabels([])

plt.tight_layout(rect=[0, 0, 0.9, 1])
plt.savefig('heatmap.pdf')
plt.show()
