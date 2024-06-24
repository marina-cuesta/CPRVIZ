#################################################################
#################################################################
######   probabilistic consistent subspaces computation    ######
#################################################################
#################################################################

## loading necessary scripts
# consistent 2D subset extraction function
source(file=paste0(path,"/src/functions/consistent_2D_subset_extraction_function.R"))

## Function to extract all probabilistic consistent subspaces in a data set.
## PARAMETERS:
# - dataX: data frame with the features variables
# - dataProb: vector with the probs
# - dataSample: vector indicating wich data points belong to train or test sample
# - k_neighbours: proportion of data to be considered neighbours
# - epsilon_parameter: epsilon parameter of the CPRViz method, i.e., the maximum mS3 value to consider a subset as probabilistic consistent

CPRViz_subspaces_computation <- function(dataX, dataProb, dataSample,
                                             k_neighbours,epsilon_parameter, 
                                             scale_data=TRUE){
  
  #######################################
  #### Checking initial requirements ####
  #######################################
  
  ## Error if parameter k_neighbours is not in (0,1)
  if (k_neighbours<=0 | k_neighbours> 1) {
    stop("k_neighbours must be between 0 and 1")
  }
  
  ## Error if parameter epsilon_parameter is not in (0,1)
  if (epsilon_parameter<0 | epsilon_parameter> 1) {
    stop("epsilon_parameter must be between 0 and 1")
  }
  

  ## Error if dataX and dataProb don't have same number of data
  if ((dim(dataX)[1]!=length(dataProb))|(dim(dataX)[1]!=length(dataSample))) {
    stop("dataX, dataProb and dataSample must have the same number of data")
  }
  
  ## Error if there is no data
  if (dim(dataX)[1]==0) {
    stop("dataX is empty")
  }
  
  ## Error if all the variables are categorical (less or iqual than 2 different values, since they
  # have already been processed in the best model selection and categorical values have
  # already been transformed to dummy variables)
  count_unique <- rapply(dataX,  function(x) length(unique(x)))
  if(sum(count_unique>2)==0){
    stop("all variables in dataX are categorical according to parameter unique_values_factor")
  }
  
  ## Error if dataProb is not a numeric vector
  if(!(is.numeric(dataProb))){
    stop('dataProb must be a numeric vector')
    return(NULL)
  }
  
  ## Error if dataSample is not composed by "train" or "test"
  if(any(!(dataSample %in% c("train", "test")))){
    stop("dataSample must be a vector containing only values 'train' or 'test'")
    return(NULL)
  }
  
  ## Error if there is not train in dataSample
  if(!("train" %in% dataSample)){
    stop("There is not train set in the dataset. Check dataSample.")
    return(NULL)
  }
  
  ## warning if there is not train in dataSample
  if(!("test" %in% dataSample)){
    warning("There is not test set in the dataset. Check dataSample.")
  }
  
  
  ## Paralleling is needed. Compute the number of cores to use
  n.cores <- parallel::detectCores() - 1
  
  
  #############################
  #### Data preprocessing  ####
  #############################
  
  #######  !!!!!!!!!!!!!!!!!! not necessary since this have been already done in
  #### in the best model selection function. If the data to be analyzed is not from
  #### the output of a classification model (data and probs), but the input data 
  #### with the target Y continuous, this preprocess should be done
  
  
  #########################################################
  ##### selecting data from the train and test sample ##### 
  #########################################################
  
  ## train sample
  dataX_train <- dataX[dataSample=="train",]
  dataProb_train <-  dataProb[dataSample=="train"]
  ## test sample
  dataX_test <- dataX[dataSample=="test",]
  dataProb_test <-  dataProb[dataSample=="test"]
  
  ## for big datasets, we randomly select a sample of 5000 data points
  n_data <- length(dataProb)
  n_data_test <- length(dataProb_test)
  if(n_data>5000){
    set.seed(1234)
    ix_reduced_test_sample=sample(1:n_data_test, min(5000,n_data_test))
    ## modifying test sample
    dataSample[dataSample=="test"][-ix_reduced_test_sample]="test_noplot"
    ## test sample
    dataX_test <- dataX_test[ix_reduced_test_sample,]
    dataProb_test <-  dataProb_test[ix_reduced_test_sample]
  }
  
  ##############################################################
  ##### Scaling data if indicated by "scale_data" argument #####
  ##############################################################
  
  if (scale_data){
    dataX_train=as.data.frame(scale(dataX_train))
    dataX_test=as.data.frame(scale(dataX_test))
  }
  
  ################################################################
  ##### common data characteristics in train and test sample #####
  ################################################################
  
  # Data characteristics: number of variables 
  p=dim(dataX_train)[2]
  
  ## Data characteristics: identifying the factor: those that are dummys (based on dataX which is the entire dataset)
  count_unique <- rapply(dataX,  function(x) length(unique(x)))
  var_factors <- count_unique <=2
  
  
  ########################################################################
  ######   obtaining the consistent subspaces in the train sample   ######
  ########################################################################
  
  ## list to store the obtained subspaces in the train sample
  obtained_subspaces_train=list()
  
  ## Data characteristics: number of points in the train sample
  n_train=dim(dataX_train)[1]
  
  ## auxiliar variables to compute subspaces in train sample
  dataProb_train_aux= dataProb_train
  dataX_train_aux=dataX_train
  
  ## computing all the possible pair of variables in a matrix
  pair_variables=t(combinat::combn(1:p,2))
  
  ## selecting the valid pair of variables: those where at least one variable is continous
  valid_pair_variables=matrix(!var_factors[as.vector(pair_variables)],ncol=2)
  pair_variables=matrix(pair_variables[apply(valid_pair_variables, 1, any),],ncol=2)
  n_pair_variables_total=dim(pair_variables)[1]
  
  ## matrix to track the pair of variables used in each iteration
  used_pair_variables=matrix(nrow = 0, ncol = 2)
  
  ## vector to track the remaining data obs (at first they are the same as initial)
  remaining_data=1:n_train
  
  ## computing the stopping condition: all pairs of variables have been used or no data
  stop_condition=(dim(used_pair_variables)[1]==n_pair_variables_total)|(length(dataProb_train_aux)==0)
  
  ## iterating
  iter=1
  while(!stop_condition){
    print(paste("computation of subspace in train sample, number", iter))
    
    ## selecting the pair of variables that have not been selected yet
    pair_variables=suppressMessages(as.matrix(dplyr::anti_join(data.frame(pair_variables),data.frame(used_pair_variables))))
    n_pair_variables=dim(pair_variables)[1]
    
    ## parallel: we define the cluster and register it  so it can be used by %dopar%
    my.cluster <- parallel::makeCluster(
      n.cores,
      type = "PSOCK"
    )
    doParallel::registerDoParallel(cl = my.cluster)
    
    ## preparing cross validation samples to compute complexity on
    k_folds <- 5
    set.seed(1234)
    if (dim(dataX_train_aux)[1]>1){
      folds <- createFolds(1:dim(dataX_train_aux)[1], k = k_folds, list = TRUE, returnTrain = T)
    }else{
      folds <-list(1)
    }
    ## parallel loop to compute complexity on every pair of variables
    complexity_values_cv <-
      foreach::foreach(pair=1:n_pair_variables, .combine='rbind' ,.export=c("mS3"),.packages = "dbscan")  %dopar% {
        complexity_values_cv_pair=numeric(length(folds))
        for (iter_cv in 1:length(folds)){
          id_fold <- folds[[iter_cv]]
          results_complexity=mS3(dataX_train_aux[id_fold,pair_variables[pair,]],dataProb_train_aux[id_fold],
                                k_neighbours,scale_data=F)
          complexity_values_cv_pair[iter_cv]=results_complexity$dataset_level
        }
        complexity_values_cv_pair
      }
    parallel::stopCluster(cl = my.cluster)
    registerDoSEQ()
    
    ## mean complexity value for each pair of variables along the cross validation computations
    complexity_values <- rowMeans(matrix(complexity_values_cv,ncol=length(folds)))
    
    ## Choosing the pair of variables for which its subspace has minimum complexity 
    min_complexity_value=which.min(complexity_values) 
    pair_variables_selected=pair_variables[min_complexity_value,]
    
    ## Computing separable subset of points out of the 2D subspaces
    dataX_train_aux_2D=dataX_train_aux[,pair_variables_selected]
    subspace_iter=extract_consistent_subset_2D(dataX_2D=dataX_train_aux_2D, dataProb=dataProb_train_aux, 
                                               k_neighbours=k_neighbours, epsilon_parameter=epsilon_parameter,
                                               scale_data=F)
    
    ## Identifying the data points that belong and don't to the obtained subpace at this iteration
    ix_not_subspace=subspace_iter$ix_not_subspace
    ix_subspace=subspace_iter$ix_subspace
    
    ## Filling the variable to track the obtained subspaces
    obtained_subspaces_train[[iter]]=list()
    obtained_subspaces_train[[iter]]$variables=pair_variables_selected
    obtained_subspaces_train[[iter]]$data_subspace=remaining_data[ix_subspace]
    obtained_subspaces_train[[iter]]$data_not_subspace=remaining_data[ix_not_subspace]
    obtained_subspaces_train[[iter]]$complexity=subspace_iter$complexity
    
    ## updating data
    remaining_data=remaining_data[ix_not_subspace]
    dataProb_train_aux=dataProb_train_aux[ix_not_subspace]
    dataX_train_aux=dataX_train_aux[ix_not_subspace,]
    
    ## We update the used pair of variables in this iteration
    used_pair_variables=rbind(used_pair_variables,pair_variables_selected)
    
    ## recomputing stoping condition
    stop_condition=(dim(used_pair_variables)[1]==n_pair_variables_total)|(length(dataProb_train_aux)==0)
    
    ## increasing iteration
    iter=iter+1
  }
  
  
  #######################################################################
  ######   obtaining the consistent subspaces in the test sample   ######
  #######################################################################
  
  ## list to store the obtained subspaces in the test sample
  obtained_subspaces_test=list()
  
  ## number of subspaces obtained in the train sample
  n_subspaces_train <- length(obtained_subspaces_train)
  
  ## Data characteristics: number of points in the test sample
  n_test=dim(dataX_test)[1]
  
  if(n_test!=0){
    
    ## auxiliar variables to compute subspaces in test sample
    dataProb_test_aux= dataProb_test
    dataX_test_aux=dataX_test
    
    ## vector to track the remaining data obs (at first they are the same as initial)
    remaining_data=1:n_test
    
    ## computing the stopping condition: no data
    stop_condition=length(dataProb_train_aux)==0
    
    ## iterating over the pair of variables obtained for the train sample
    for(iter in 1:n_subspaces_train){
      print(paste("computation of subspace in test sample, number", iter))
      
      ## selecting the pair of variables 
      pair_variables_subspace = used_pair_variables[iter,]
      
      ## Computing separable subset of points out of the 2D subspaces
      dataX_test_aux_2D=dataX_test_aux[,pair_variables_subspace]
      subspace_iter=extract_consistent_subset_2D(dataX_2D=dataX_test_aux_2D, dataProb=dataProb_test_aux, 
                                                 k_neighbours=k_neighbours,epsilon_parameter,
                                                 scale_data=F)
      
      ## Identifying the data points that belong and don't to the obtained subpace at this iteration
      ix_not_subspace=subspace_iter$ix_not_subspace
      ix_subspace=subspace_iter$ix_subspace
      
      ## Filling the variable to track the obtained subspaces
      obtained_subspaces_test[[iter]]=list()
      obtained_subspaces_test[[iter]]$variables=pair_variables_subspace
      obtained_subspaces_test[[iter]]$data_subspace=remaining_data[ix_subspace]
      obtained_subspaces_test[[iter]]$data_not_subspace=remaining_data[ix_not_subspace]
      obtained_subspaces_test[[iter]]$complexity=subspace_iter$complexity
      
      ## updating data
      remaining_data=remaining_data[ix_not_subspace]
      dataProb_test_aux=dataProb_test_aux[ix_not_subspace]
      dataX_test_aux=dataX_test_aux[ix_not_subspace,]
      
      ## recomputing stoping condition
      stop_condition=(length(dataProb_test_aux)==0)
      if (stop_condition){
        break
      }
  }
  
  }
  
  
  ###############################################
  ######   preparing variables to return   ######
  ###############################################
  
  results = list()
  
  ## data from which the subspaces have been computed
  results$data <- list()
  results$data$dataX <- dataX
  results$data$dataProb <- dataProb
  results$data$dataSample <-dataSample
  
  ## parameters used to obtain subspaces
  results$parameters <- list()
  results$parameters$k_neighbours <- k_neighbours
  results$parameters$epsilon_parameter <- epsilon_parameter
  results$parameters$scale_data <- scale_data
  
  ## obtained subspaces 
  results$obtained_subspaces <- list()
  # in train
  results$obtained_subspaces$train <- obtained_subspaces_train
  # in test
  results$obtained_subspaces$test <- obtained_subspaces_test
  
  return(results)
}

