##########################################
##########################################
######    mS3 complexity measure    ######
##########################################
##########################################

## mS3 computation at instance, class and data set level. 
## PARAMETERS: 
# - dataX is the dataframe with the feature variables
# - dataY is the vector with the target variable
# - scale_data = {T,F} indicates if the data must be scaled before computing mS3

mS3 <- function(dataX, dataY , k_neighbours,scale_data){
  
  #######################################
  #### Checking initial requirements ####
  #######################################
  
  ## Error if the number of data in dataX is not the same as in dataY
  nx <- dim(dataX)[1]
  ny <- length(dataY)
  # If nx != ny, ERROR
  if (nx != ny){
    stop('dataX and dataY must have same number of data')
    return(NULL)
  }

  ###################################################
  #### If there is only one data point, mS3 is 0 ####
  ###################################################
  if(nx==1){
    
    mS3_instance_level <- 0
    mS3_class_level <- numeric(length(levels(dataY)))
    mS3_dataset_level <- 0
    
    ####################################################################    
    #### If there is more than one data point, mS3 must be computed ####
    #################################################################### 
  } else{
    
    ## scaling data if indicated
    if (scale_data){
      dataX <- as.data.frame(scale(dataX))
    }

    
    #####################
    ## mS3 computation ##
    #####################
    
    ## number of data to be considered as neighbours
    n_neighbours <- ceiling(nx*k_neighbours)
    
    ## computing the knn model to find k_max neighbours for each data item
    knn_model_aux <- dbscan::kNN(dataX,k=n_neighbours)
    knn_model <- knn_model_aux$id
    knn_model_dists <- knn_model_aux$dist
    knn_model_dists[knn_model_dists==0]=0.0001
    
    ## computing the probability of each of the neighbors of each data item
    knn_model_probs <- as.vector(knn_model)
    knn_model_probs <- matrix(dataY[knn_model],ncol=n_neighbours)

    ## computing the weights of each of the neigbours
    knn_model_weights <- 1/knn_model_dists
    sum_weights <- rowSums(knn_model_weights)
    knn_model_weights <-knn_model_weights * (1/sum_weights)
    
    ## computing the predicted probability 
    knn_predict <- rowSums(knn_model_probs* knn_model_weights)
    
    ## mS3 instance level is the absolute error between the predicted prob and the real one
    mS3_instance_level <- abs(knn_predict-dataY)

    ## computing mS3 dataset level
    mS3_dataset_level=mean(mS3_instance_level)
  }
  
  
  #######################
  ## Preparing results ##
  #######################
  
  results_mS3=list()
  results_mS3$instance_level=mS3_instance_level
  results_mS3$dataset_level=mS3_dataset_level
  
  return(results_mS3)
  
  
}
