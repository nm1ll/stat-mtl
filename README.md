# Code repository for PhD dissertation "Statistical Extensions of Multi-Task Learning with Semiparametric Methods and Task Diagnostics"

This repository includes data and code used in PhD dissertation "Statistical Extensions of Multi-Task Learning with Semiparametric Methods and Task Diagnostics" by Nikolay Miller, submitted as part of requirements for PhD in Statistics at University of New Mexico at July 2022. When it is published, it will become available for download at https://digitalrepository.unm.edu/math_etds/

## Model codes

The folder "Model - additive MTL model example" contains the components of two-step model example in Chapter 5 of the dissertation. Note that the codes need to be run sequentially: first linear component, then SVR adjustment. The paths for models need to be adjusted accordingly to your download location. The models are implemented as Slurm batch scripts.

The folder "Model - best performance subsets" contains best performance subsets algorithm which is a two-step multi-task model as described in Chapter 4. It is implemented using "doParallel" R package, so there is no need for Slurm cluster. There are several slightly different versions; the differences can be found in code descriptions.

## ILEA schools data splits

The folder "Dataset creation - ILEA data" contains ILEA schools data that was used in my dissertation. For reference, the original dataset can be found at:
http://www.bristol.ac.uk/cmm/learning/support/datasets/ _
"School effectiveness (zip, 0.1 mb) Examination data for school leavers in Inner London with intake achievement measures"
I have formatted it to a CSV file, ILEA567.csv, which is included here

The Slurm script "slurm_DATA_CREATION.sh" calls "DATASET_CREATION.R" in order to run it in parallel. The parallelizable arguments are split number (contained in the Slurm script) and lambda (contained in lambdas.txt) for the mean-regularized multi-task kernel ("Learning Multiple Tasks with Kernel Methods" by Evgeniou, Michelli and Pontil (2005)). You may also edit "DATASET_CREATION.R" to run without parallelization; it can be done by manually specifying split and lambda values, and then running a loop over those.

The seed is set to split number (set.seed(split)), making the results of the dissertation repeatable.

In case you just want to access the data quickly, the folder "Full data - no splits" includes cleaned data without splits, for 3 values of lambda, and the folder "Splits collection" includes 100 splits for lambdas 0, 0.001 and 0.01. In every RData file in the folder "Splits collection", the raw untransformed data is also included.
