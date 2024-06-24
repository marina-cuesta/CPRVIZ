# CPRViz: Consistent Probability Regions Visualization for model understanding

This repository is the official implementation of the CPRViz method. 


## Requirements

The CPRViz method has been implemented using the R software version 4.4.0.


## Folders

* src: This folder contains the code to reproduce the results and to apply the CPRViz method to any new dataset if desired.

* data: This folder contains the 59 datasets used to illustrate and evaluate the effectiveness of the method. The datasets are already processed and fitted with an adequate classifier, so that each of them already contains the predicted probabilities.

* results: This folder contains the CPRViz results for the 59 datasets in the train and the test sample.



## Reproducing the results

In the src folder, open main.R. Choose the dataset you want to get the CPRViz results from, uncomment the corresponding line ```dataset="data_name"``` and run everything. 


The parameters of each function are specified in the each function script at the beginning of the implementation. These parameters can be modified in the main.R script by changing the following lines:

```
#############################################
######   COMPUTING CPRViz SUBSPACES    ######
#############################################
## parameters
k_neighbours=0.05
epsilon_parameter=0.05
scale_data=TRUE
```

```
#################################################
######   CPRViz VISUALIZATION PARAMETERS   ######
#################################################

min_data=0.05
max_complexity=1
prob_breaks=c(0.5)
show_disaggregated_info=F
```



## Applying the CPRViz method to new data

To apply the CPRViz method to new data, first fit and predict the data with an adequate classifier. Generate a new dataset with the X variables used for the model (as processed as for the model fitting), the predicted probabilities (dataProb) and the sample for each dataset (dataSample={"train","test"\). Place the new data file in the data folder. Add the line ```dataset="new_data_name"``` in the main.R file and run the code. 