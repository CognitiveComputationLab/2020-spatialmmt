2020-spatialmmt
===============

Companion repository for the 2020 article *"The Predictive Power of Spatial Relational Reasoning Models: A New Evaluation Approach"* by Marco Ragni, Daniel Brand, and Nicolas Riesterer.

### Content

- `analysis/`: Contains the analysis scripts and corresponding datasets
    - `benchmarks/`: Contains the CCOBRA benchmark configuration files.
        - `data/`: Contains the datasets.
        - `models/`: Contains the CCOBRA model scripts.
        - `*.json`: CCOBRA benchmark configuration files.
        - `*.csv`: CCOBRA benchmark result files.
    - `visualization/`: Contains the visualization scripts.
        - `results/`: Contains the resulting image files.
        - `heatmap.py`: Plots the optimality comparison heatmap (Figure 6).
        - `performance.py`: Plots the performance boxplots (Figures 2-5).

### Dependencies

- Python 3
    - [numpy](https://numpy.org/)
    - [pandas](https://pandas.pydata.org/)
    - [matplotlib](https://matplotlib.org/)
    - [seaborn](https://seaborn.pydata.org/)
    - [ccobra](https://github.com/CognitiveComputationLab/ccobra)

### Quickstart

#### Benchmark Evaluations

Navigate into the `analysis/benchmarks/` directory and run the respective benchmark `.json` file via CCOBRA:

```
$> cd /path/to/repository/analysis/benchmarks
$> ccobra 3ps.json -s 3ps.csv
```

The command line option `-s <filename>` stores the results directly in a CSV file.

#### Performance Boxplot Generation

Navigate into the `analysis/visualization/` directory and run the `performance.py` script:

```
$> cd /path/to/repository/analysis/visualization
$> python performance.py ../benchmarks/3ps.csv
```

#### Optimality Heatmap Generation

Navigate into the `analysis/visualization/` directory and run the `heatmap.py` script:

```
$> cd /path/to/repository/analysis/visualization
$> python heatmap.py
```

### References

- [The CCOBRA repository](https://github.com/CognitiveComputationLab/ccobra)
- [The pyspatialreasoner repository](https://github.com/nriesterer/pyspatialreasoner)
