###########################################################################################
###########################################################################################
######   Extracting a consistent probabilistic subset of points of a 2D  subspace    ######
###########################################################################################
###########################################################################################

## loading necessary scripts
# regression complexity measures function
source(file=paste0(path,"/src/functions/regression_complexity_measure_function.R"))

## Function to extract a consistent probabilistic subset of points out of 2D subspace which can be a 
# data set restricted to two features variables. The subset is obtained deleting data points.
## PARAMETERS:
# - dataX_2D: the 2D data frame with the features variables
# - dataProb: the vector with the predicted probabilities
# - k_neighbors: proportion of data to be considered neighbors
# - epsilon_parameter: epsilon parameter of the CPRViz method, i.e., the maximum mS3 value to consider a subset as probabilistic consistent
# - scale_data: a logical {T/F} indicating if dataX_2D should be scaled 
extract_consistent_subset_2D <- function(dataX_2D, dataProb,k_neighbours,epsilon_parameter,scale_data){ 
  
  #######################################
  #### Checking initial requirements ####
  #######################################
  
  ## Error if dataX_2D and dataProb don't have same number of data
  if (dim(dataX_2D)[1]!=length(dataProb)) {
    stop("dataX_2D and dataProb must have same number of data")
  }
  
  ## Error if there is no data
  if (dim(dataX_2D)[1]==0) {
    stop("dataX_2D and dataProb are empty")
  }
  
  
  #################################
  ##### Data charactheristics ##### 
  #################################
  
  ## number of points
  n=dim(dataX_2D)[1]
  
  
  ###########################################
  ##### Deleting data points iteration  #####
  ###########################################
  
  ## vector to track and store complexity data set level values 
  vector_complexity_dataset=numeric(0)
  
  ## matrix to track and store the complexity of the deleted value and the threshold
  matrix_max_complexity_threshold=NULL
  
  ## vector to track and store the index of the deleted data points 
  ix_deleted=numeric(0)
  
  ## Computing complexity with the current dataset
  results_complexity=mS3(dataX=dataX_2D,dataY=dataProb,k=k_neighbours ,scale_data=scale_data)
  complexity_dataset=results_complexity$dataset_level
  
  ## Filling vector_complexity_dataset with the current value
  vector_complexity_dataset[1]=complexity_dataset
  
  ## indexes of the original data before manipulating them
  ix_data_points=1:n
  
  ## compute stoping condition: no data or complexity=0
  stop_condition=length(dataProb)==0|complexity_dataset<epsilon_parameter
  
  iter=1
  while(!stop_condition){
    ##################################################################
    ##### Deleting the data point with maximum complexity value  #####
    ##################################################################
    
    ## complexity instance level value of the current dataset
    complexity_instance_level_values=results_complexity$instance_level
    
    ## Finding the index of the data point with maximum complexity instance level
    max_complexity_instance_level=sort(complexity_instance_level_values,decreasing = T)[1]
    ix_max_complexity_instance_level=order(complexity_instance_level_values,decreasing = T)[1]
    
    ## computing the deleting threshold based on the truncated complexity values
    mean_complexity=mean(complexity_instance_level_values)
    sd_complexity=sd(complexity_instance_level_values)
    complexity_deleting_threshold=mean_complexity+1.96*sd_complexity

    ## storing max_complexity_instance_level and threshold
    matrix_max_complexity_threshold = rbind(matrix_max_complexity_threshold,c(max_complexity_instance_level,complexity_deleting_threshold))
    
    #### If max(complexity) is smaller than complexity_deleting_threshold, break ####
    if(max_complexity_instance_level < complexity_deleting_threshold)  {
      break
    }
    
    #### If max(complexity) is bigger than complexity_deleting_threshold, the data point is deleted ####
    else{
      ## Deleting from dataX_2D the data point with that index
      dataX_2D=dataX_2D[-ix_max_complexity_instance_level,]
      
      ## Deleting from dataProb the data point with that index
      dataProb=dataProb[-ix_max_complexity_instance_level]
      
      ## Filling the ix_deleted vector with the deleted index on this iteration 
      ix_deleted=c(ix_deleted,ix_data_points[ix_max_complexity_instance_level])
      
      ## Deleting from ix_data_points the deleted ix data points to
      #  update the remaining data points in the subspace
      ix_data_points=ix_data_points[-ix_max_complexity_instance_level]
      
      ## complexity of the current dataset after the deleting of the data point
      results_complexity=mS3(dataX=dataX_2D,dataY=dataProb,k_neighbours,scale_data)
      complexity_dataset=results_complexity$dataset_level
      
      ## Filling vector_complexity_dataset with the current value
      vector_complexity_dataset[iter+1]=results_complexity$dataset_level
    }
    
    ## re computing the stopping condition. no data or complexity=0
    stop_condition=(length(dataProb)==0)|(complexity_dataset<epsilon_parameter)
    iter=iter+1
  }
  complexity_subspace=vector_complexity_dataset[iter]
  
  
  ########################################
  ##### Preparing results to return  ##### 
  ########################################
  
  results_separable_subspace=list()
  results_separable_subspace$tracking_deleting_condition = matrix_max_complexity_threshold
  results_separable_subspace$tracking_dataset_complexities = vector_complexity_dataset
  # ix data points that belong to the subspace
  results_separable_subspace$ix_subspace=ix_data_points
  # ix data points that don't belong to the subspace
  results_separable_subspace$ix_not_subspace=ix_deleted
  # complexity of the resulting subspace
  results_separable_subspace$complexity=complexity_subspace
  
  return(results_separable_subspace)
}

