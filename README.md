# Can we measure effort in cognitive tasks? Examining the application of Additive Conjoint Measurement and the Rasch model

This is the repository for the above titled paper

## What's included

### Datasets
In this folder are all of the simulated datasets used in the paper, in addition to the empirical datasets.

### Analyses
In this folder are all of the scripts for simulating the datasets used, conducting checks for the axioms of additive conjoint measurement, fitting Rasch models and deriving effort operationalisations, as well as analysis of the empirical datasets. 

Note, as I was trying to get this finished up for the (already extended) submission deadline for the special issue this was invited for, I have ended up cleaning up the code as best as I can for now. It should all run, but it is very verbose and will likely be pretty inefficient and time consuming to reproduce for most computers. I encountered issues I couldn't work out trying to parallelise things either as vectorised functions or foreach loops in order to run things more efficiently over the varying simulated datasets. I hope to find some time to work this out and update though.

### Models
This folder contains all of the models produced as well as some of the compiled statistics in dataframes from analyses. 

Note, three models are missing due to their file size and I intend get these uploaded via LFS shortly.

### Plots
This folder contains all the plots as R files and PNGs.

### Paper
This contains the RMD file for the manuscript.
