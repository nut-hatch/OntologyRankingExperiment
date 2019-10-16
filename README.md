# Ontology Ranking Experiments

This repository contains the supplemental material for a paper submitted to ISWC 2019.

## Repository overview.

The resources for this submission include:
1) resources: summarizes and includes further computed resources which are used as input for the experiments.
	- 1-lov4iot-metadata: The cleaned and extended metadata set for the collected ontologies through LOV4IoT.
	- 2-scholar-data: The respectively crawled scholar data and cleaned citation history.
	- 3-popularity-scores: The computed popularity scores.
	- 4-ground-truth: The mined ground truth, with labels mapped to a pointwise scale (0-4).
	- 5-training-data: The training data with extracted features for all query-ontology pairs in the training set.
	- 6-benchmarks: The mined ground truths from LOV and BioPortal, with labels mapped to a pointwise scale (0-4).
	- 7-test-data: The test data with extracted features for all query-ontology pairs in the test sets.
2) experiments: includes the R scripts and relevant resources to perform the experiments.
3) others:
	- lov-motivation: includes the data and R script to generate the figure for the motivation of this study.
	- data-collection: includes the R scripts and relevant resources to generate the ground truth for training (experiment 1) and the benchmarks of the LOV and BioPortal platforms (experiment 2).


## Helpful Pointers
- The derived resources can be reviewed without any script execution. The main contributions in terms of resources are:
	- Training set (./resources/5-training-data/TrainingSet.txt)
	- LOV test set (./resources/7-test-data/LOVTestSet.txt)
	- BioPortal test set (./resources/7-test-data/BioPortalTestSet.txt)
- The main contribution in terms of experiments is located at: experiments/experiments-train-test.R, more detailed instructions for replicated the paper results are given in the file header.


### Prerequisites
- R: The scripts are implemented in R, a convenient way to set up a R environment is RStudio: https://www.rstudio.com/
- Java: The experiments further require Java, as the scripts rely on the RankLib library.
- The experiments have been performed and tested on MacOS.

### General hints for executing the R scripts
- Before execution of any R script, the working directory needs to be set to the source file location.
- Some scripts may have further requirements which are detailed in the file header (e.g., might require a valid API key).
- Some scripts may be resource intensive and require a long time to complete. The data collection (if desired to execute them) will require ~6GB available disk storage.