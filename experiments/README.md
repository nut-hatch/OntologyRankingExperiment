# Ranking Experiments

This folder includes the R scripts and relevant resources to perform the experiments.

## Overview.
The scripts for the experiments include:
- Main experiment: ./experiments-train-test.R
- Computing the feature frequencies of the trained models: ./feature-statistics/experiments-feature_frequencies.R
- Creating the plots presented in the paper: 
	- ./figures/experiments-figures-validation.R
	- ./figures/experiments-figures-test-lov.R
	- ./figures/experiments-figures-test-bioportal.R

### Helpful pointers
- More detailed results than given in the paper about the performance of the ranking model are given in ./train/output/results/
- The learned models (which can be used to apply re-ranking), are located at ./train/output/models/

## Replicating results
Instructions to execute the scripts.

### Main experiment
- The main script to run learn and evaluate the ranking models is ./experiments-train-test.R.
- The results are written to 
	- train/output
	- test-lov/output
	- test-bioportal/output
- If desired to replicate the results, *the output folders should be deleted first*.
- The relevant input folders contain the relevant resources as a result of the data collection needed to run the experiments.
- Further instructions are given in the file header.

### Feature statistics
- Computes the feature frequencies for each model over all folds and computes the mean.
- These counts are discussed in the paper to understand the influence of the qualitative features on the model.

### Figures
- The code to generate the boxplots used in the paper.