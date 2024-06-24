######################################################
######################################################
######  PLAIN INFORMATION TABLE OF SUBSPACES    ######
######################################################
######################################################

## Function to obtain a plain table with the detailed information about the the obtained subspaces 
## PARAMETERS:
# - consistent_computed_subspaces is the return of the consistent_subspaces_computation function
# - min_data: minimum allowed percentage of data in a subspace for it to be visualized; must be in [0,1]
# - max_complexity: maximum complexity allowed for a subspace to be displayed; must be in [0,1]
# - plot_train_sample: logical T/F indicating whether to plot the subspaces of the train sample or not
# - prob_breaks: vector with the breaks to summarize probs in intervals
# - show_disaggregated_info: logical T/F indicating whether to show or not the information of each subspace
#   in a disaggragated format apart from the accumulated one 

info_table <- function(consistent_computed_subspaces, min_data,max_complexity,
                       plot_train_sample=F, prob_breaks=c(0.5), show_disaggregated_info=F){
  
  #######################################
  #### Checking initial requirements ####
  #######################################
  
  ## Error if parameters min_data or max_complexity are not in [0,1]
  if ((min_data<0 | min_data> 1)|(max_complexity<0 | max_complexity> 1)) {
    stop("min_data and max_complexity must be between 0 and 1")
  }
  
  ## Error if parameters min_data or max_complexity are not in [0,1]
  if (!(plot_train_sample %in% c(T,F))) {
    stop("plot_train_sample must be a logical T or F")
  }
  
  if(!is.numeric(prob_breaks)|any(prob_breaks>1)|any(prob_breaks<0)){
    stop("prob_breaks must be a numeric with elements between 0 and 1")
  }
  
  ## Error if parameters min_data or max_complexity are not in [0,1]
  if (!(show_disaggregated_info %in% c(T,F))) {
    stop("show_accum_info must be a logical T or F")
  }
  
  
  
  ###########################
  #### gathering results ####
  ###########################
  
  ## Gathering dataX, dataProb and obtained subspaces
  if (!plot_train_sample){
    ## test samples
    obtained_subspaces=consistent_computed_subspaces$obtained_subspaces$test
    sample_test <- consistent_computed_subspaces$data$dataSample=="test"
    dataX <- consistent_computed_subspaces$data$dataX[sample_test,]
    dataProb <- consistent_computed_subspaces$data$dataProb[sample_test]
    
  }else{
    ## train samples
    obtained_subspaces=consistent_computed_subspaces$obtained_subspaces$train
    sample_train <- consistent_computed_subspaces$data$dataSample=="train"
    dataX <- consistent_computed_subspaces$data$dataX[sample_train,]
    dataProb <- consistent_computed_subspaces$data$dataProb[sample_train]
  }
  
  ## Number of obtained subspaces
  n_subspaces=length(obtained_subspaces)
  
  
  ################################
  ##### Data characteristics ##### 
  ################################
  
  ## number of points
  n=dim(dataX)[1]
  
  ## discretizing dataProb variable by  prob_breaks
  prob_breaks_aux = c(0, prob_breaks, 1)
  prob_breaks_aux_lag = lag(prob_breaks_aux)
  intervals_aux_1 <- paste("[",prob_breaks_aux_lag[c(-1,-length(prob_breaks_aux))],
                           "," ,prob_breaks_aux[c(-1,-length(prob_breaks_aux))], ")",sep="")
  intervals_aux_2 <- paste("[",prob_breaks_aux_lag[length(prob_breaks_aux)],
                           "," ,prob_breaks_aux[length(prob_breaks_aux)], "]",sep="")
  intervals <- c(intervals_aux_1,intervals_aux_2)
  
  dataProb_discrete <- cut( dataProb, 
                         breaks = c(-0.5,prob_breaks,1.5), 
                         labels = intervals)
  
  ## number of intervals in dataProb_discrete
  n_intervals=length(intervals)
  
  ## number of points in each prob interval
  n_data_intervals=table(factor(dataProb_discrete, intervals))
  
  
  #########################################################
  ##### Creating data frame to store the information  ##### 
  #########################################################
  
  n_col_df=5+n_intervals*2+1
  table_information=data.frame(matrix(nrow=0,ncol = n_col_df))
  
  ## names of the dataframe to store the information
  names(table_information)=c("subspace","var1", "var2", "% acccum data total", paste("% acccum data prob",intervals),
                             "% data total" , paste("% data prob",intervals), "complexity")
  
  ##########################################################################
  ##### Obtaining information for each subspace and filling the table  ##### 
  ##########################################################################
  
  ## auxiliar variables to compute the information
  sum_data=0
  sum_data_intervals=numeric(n_intervals)
  
  ## Looping over each subspace
  for (subspace in 1:n_subspaces){
    
    ## subspace of this iteration
    subspace_iter=obtained_subspaces[[subspace]]
    
    ## % of data showed in this iteration
    n_data_subspace_iter=length(subspace_iter$data_subspace)
    perc_data_subspace_iter= n_data_subspace_iter/n
    
    ## complexity value of the subspace in this iteration
    complexity_dataset_level=round(subspace_iter$complexity,2)
    
    ## if the subspace has a lower percentage of data than min_data or higher complexity than max_complexity, the method does not plot it
    if ((perc_data_subspace_iter<min_data)|(complexity_dataset_level>max_complexity)){
      next
    }
    
    ## format of  % of data showed in this iteration
    perc_data_subspace_iter= round(100*n_data_subspace_iter/n,2)
    
    ## Variables showed in this iteration
    variables_iter=subspace_iter$variables
    
    ## cumulative % of data showed in this iteration
    sum_data=sum_data+n_data_subspace_iter
    cum_perc_data_subspace_iter=round(100*sum_data/n,2)
    
    ## % of data of each prob interval showed in this iteration
    dataProb_subspace=dataProb[subspace_iter$data_subspace]
    # discretizing dataProb_subspace variable by prob_breaks
    dataProb_subspace_discrete <- cut( dataProb_subspace, 
                                    breaks = c(-0.5, prob_breaks,1.5), 
                                    labels = intervals)
    
    ## number of points in each prob interval
    n_data_intervals_subspace_iter=table(factor(dataProb_subspace_discrete, intervals))
    perc_data_intervals_subspace_iter= round(100*n_data_intervals_subspace_iter/n_data_intervals,2)
    
    ## cumulative % of data of each prob interval showed in this iteration
    sum_data_intervals=sum_data_intervals+n_data_intervals_subspace_iter
    cum_perc_data_intervals_subspace_iter= round(100*sum_data_intervals/n_data_intervals,2)
    
    ## Filling the data frame
    table_information[subspace,]=c(subspace,names(dataX)[variables_iter[1]],
                                   names(dataX)[variables_iter[2]],
                                   cum_perc_data_subspace_iter,cum_perc_data_intervals_subspace_iter,
                                   perc_data_subspace_iter,perc_data_intervals_subspace_iter,
                                   complexity_dataset_level)
  }
  
  ## if there is not any original prob in one interval, the result will be NaN
  # so we change it to "-"
  table_information[table_information==NaN]="-"
  
  
  ###############################################################################################
  ##### selecting only the accumulated information if indicated in show_disaggregated_info  ##### 
  ###############################################################################################
  if(!show_disaggregated_info){
    table_information = table_information[,c(1:(4+n_intervals),length(table_information))]
  }
  
  ####################################################################
  ##### message if there is not any subspace meeting conditions  ##### 
  ####################################################################
  
  if(dim(table_information)[1]==0){
    cat("There is not any subspace meeting conditions: total data proportion >= min_data and complexity data set <= max_complexity.")
    return(NULL)
  }
  
  return(table_information)
}




############################################################
############################################################
######   FORMATTED INFORMATION TABLE OF SUBSPACES     ######
############################################################
############################################################

## Function to display a formatted table with the detailed information about the the obtained subspaces 
## PARAMETERS:
# - consistent_computed_subspaces is the return of the consistent_subspaces_computation function
# - min_data: minimum allowed percentage of data in a subspace for it to be visualized; must be in [0,1]
# - max_complexity: maximum complexity allowed for a subspace to be displayed; must be in [0,1]
# - plot_train_sample: logical T/F indicating whether to plot the subspaces of the train sample or not
# - prob_breaks: vector with the breaks to summarize probs in intervals
# - show_disaggregated_info: logical T/F indicating whether to show or not the information of each subspace
#   in a disaggragated format apart from the accumulated one 
display_info_table <- function(consistent_computed_subspaces, min_data,max_complexity,
                               plot_train_sample=F, prob_breaks, show_disaggregated_info=F){
  
  
  ###############################
  ##### Obtaining intervals ##### 
  ###############################
  
  ## discretizing dataProb variable by  prob_breaks
  prob_breaks_aux = c(0, prob_breaks, 1)
  prob_breaks_aux_lag = lag(prob_breaks_aux)
  intervals_aux_1 <- paste("[",prob_breaks_aux_lag[c(-1,-length(prob_breaks_aux))],
                           "," ,prob_breaks_aux[c(-1,-length(prob_breaks_aux))], ")",sep="")
  intervals_aux_2 <- paste("[",prob_breaks_aux_lag[length(prob_breaks_aux)],
                           "," ,prob_breaks_aux[length(prob_breaks_aux)], "]",sep="")
  intervals <- c(intervals_aux_1,intervals_aux_2)
  n_intervals <- length(intervals)
  
  
  
  #############################################################
  ##### Obtaining plain information table to be formatted ##### 
  #############################################################
  
  ## Obtaining plain table to be formatted
  table_information = info_table(consistent_computed_subspaces, 
                                 min_data,max_complexity,
                                 plot_train_sample, prob_breaks, show_disaggregated_info)
  
  
  ############################################################
  ##### Formatting table when show_disaggregated_info=T  ##### 
  ############################################################
  
  if (show_disaggregated_info){
    ## new colnames of the dataframe to store the information
    names(table_information)=c("subspace","var1", "var2", "total", intervals,
                               "total \n",paste0(intervals,c("\n")), "complexity")
    
    ## Format of the table to export
    table_information_format <- flextable::flextable(table_information) %>% 
      bg(bg="white",part="all") %>% 
      flextable::theme_box() %>% 
      flextable::add_header_row(colwidths = c(1,1,1,1,n_intervals,1,n_intervals, 1), 
                                values = c("","","", "total","probability intervals","total","probability intervals","")) %>% 
      flextable::add_header_row(colwidths = c(1,2,n_intervals+1,n_intervals+1,1), 
                                values = c("subspace","variables","%n accumulated","%n disaggregated", "complexity")) %>% 
      flextable::merge_at(i = 1:3 ,j = 1, part = "header")%>%
      flextable::merge_at(i = 1:3 ,j = 2:3, part = "header")%>%
      flextable::merge_at(i = 2:3 ,j = 4, part = "header") %>% 
      flextable::merge_at(i = 2:3 ,j = (4+n_intervals+1), part = "header") %>% 
      flextable::merge_at(i = 1:3 ,j = (4+n_intervals+1+n_intervals+1), part = "header")%>%
      flextable::fontsize(size = 10,part='all') %>% 
      flextable::autofit(add_w=-2,add_h=-2) %>% 
      flextable::vline(i=1:3,j = 1, border = officer::fp_border(color = "black", style = "solid"), part = "header")
    table_information_format <- flextable::align(table_information_format, align = "center", part = "all")
    
    ############################################################
    ##### Formatting table when show_disaggregated_info=T  ##### 
    ############################################################    
  }else if(!show_disaggregated_info){
    ## new colnames of the dataframe to store the information
    names(table_information)=c("subspace","var1", "var2", "total", intervals,
                               "complexity")
    
    ## Format of the table to export
    table_information_format <- flextable::flextable(table_information) %>% 
      bg(bg="white",part="all") %>% 
      flextable::theme_box() %>% 
      flextable::add_header_row(colwidths = c(1,1,1,1,n_intervals, 1), 
                                values = c("","","", "total","probability intervals","")) %>% 
      flextable::add_header_row(colwidths = c(1,2,n_intervals+1,1), 
                                values = c("subspace","variables","%n accumulated", "complexity")) %>% 
      flextable::merge_at(i = 1:3 ,j = 1, part = "header")%>%
      flextable::merge_at(i = 1:3 ,j = 2:3, part = "header")%>%
      flextable::merge_at(i = 2:3 ,j = 4, part = "header") %>% 
      flextable::merge_at(i = 1:3 ,j = (4+n_intervals+1), part = "header") %>% 
      flextable::fontsize(size = 10,part='all') %>% 
      flextable::autofit(add_w=-2,add_h=-2) %>% 
      flextable::vline(i=1:3,j = 1, border = officer::fp_border(color = "black", style = "solid"), part = "header")
    table_information_format <- flextable::align(table_information_format, align = "center", part = "all")
  }
  
  return(table_information_format)
}



#####################################################################
#####################################################################
######  TABLE OF ORIGINAL PROBABILITY INTERVALS PERCENTAGES    ######
#####################################################################
#####################################################################

## Function to obtain formatted table with the probability intervals 
#  percentage of the original data set
## PARAMETERS:
# - dataProb: vector with the probs  with the already selected train or test sample
# - prob_breaks: vector with the breaks to summarize probs in intervals


prob_intervals_table <- function(dataProb, prob_breaks=c(0.5)){
  
  #######################################
  #### Checking initial requirements ####
  #######################################
    if(!is.numeric(prob_breaks)|any(prob_breaks>1)|any(prob_breaks<0)){
    stop("prob_breaks must be a numeric with elements between 0 and 1")
  }
  
  ###############################################
  ##### PLAIN TABLE OF INTERVALS PERCENTAGE ##### 
  ###############################################
  
  ## number of points
  n=length(dataProb)
  
  ## discretizing dataProb variable by  prob_breaks
  prob_breaks_aux = c(0, prob_breaks, 1)
  prob_breaks_aux_lag = lag(prob_breaks_aux)
  intervals_aux_1 <- paste("[",prob_breaks_aux_lag[c(-1,-length(prob_breaks_aux))],
                           "," ,prob_breaks_aux[c(-1,-length(prob_breaks_aux))], ")",sep="")
  intervals_aux_2 <- paste("[",prob_breaks_aux_lag[length(prob_breaks_aux)],
                           "," ,prob_breaks_aux[length(prob_breaks_aux)], "]",sep="")
  intervals <- c(intervals_aux_1,intervals_aux_2)
  
  dataProb_discrete <- cut( dataProb, 
                         breaks = c(-0.5,prob_breaks,1.5), 
                         labels = intervals)
  
  ## percentage table in each prob interval
  perct_table=round(table(dataProb_discrete)/n*100,2)
  perct_table = as.data.frame(t(unclass(perct_table)))


  #############################
  ##### FORMATTING TABLE  ##### 
  #############################

  ## Format of the table to export
  perct_table_format <- flextable::flextable(perct_table) %>% 
    bg(bg="white",part="all") %>% 
    flextable::theme_box() %>% 
    flextable::add_header_row(colwidths = c(length(intervals)), values = c("% n in prob. intervals")) %>% 
    flextable::fontsize(size = 10,part='all') %>% 
    flextable::autofit(add_w=-2,add_h=-2) 
  perct_table_format <- flextable::align(perct_table_format, align = "center", part = "all")
  
  return(perct_table_format)
}

