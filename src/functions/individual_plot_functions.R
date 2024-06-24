
#########################################################
#########################################################
######   PLOT OF A DATA SET WITH A FIXED FORMAT    ######
#########################################################
#########################################################

## Function to plot data with the appropiate format
## PARAMETERS:
# - data_plot is the data frame to be plotted, containing X1, X2 and prob variables
# - type_x1, type_x2 :strings indicating the data type of variables X1 and X2. 
#   "cont" for continuous and "cat" for categorical.
# - fixed_xlim: vector of length 2 with the selected min and max limit for the x axis
# - fixed_ylim: vector of length 2 with the selected min and max limit for the y axis
plot_fixed_format <- function(data_plot,type_x1,type_x2, fixed_xlim=NULL, fixed_ylim=NULL){
  
  ##################
  ##### Errors ##### 
  ##################
  
  ## error if type_x1 or type_x2 as not what expected
  if (!(type_x1 %in% c("cont","cat")) | !(type_x2 %in% c("cont","cat")) ){
    stop("type_x1 and type_x2 must be 'cont' or 'cat'")
  }
  
  ###########################################################
  ##### plot aesthetic: Colors palette and point shapes ##### 
  ###########################################################
  
  ## colors palette of the classes for the plots
  palette_colors <-grafify::graf_palettes$fishy[1:2]
  
  mid_color <- colorRampPalette(colors=c(palette_colors[1],palette_colors[2]))(3)[2]
  
  ## point shapes  of the classes
  point_shapes= c(16, 17, 15, 18,3,4,8,25,6)[1:2]
  
  
  ###############################
  ##### preprocessing data ##### 
  ###############################
  
  ## merging the types of x1 and x2 in a vector
  type_vars <- c(type_x1, type_x2)
  
  ## if one variable is categorical, data_plot is reordered
  # so that the categorical variable is on first position (and 
  # then, in the x axis in the plot)
  if(any(type_vars == "cat")){
    # identifying the variable to plot in x axis and in y axis
    var_x=which(type_vars=="cat")
    var_y=c(1,2)[!(c(1,2) %in% var_x)]
    data_plot=data_plot[,c(var_x,var_y,3)]
  }
  
  
  #######################
  ##### axes limits ##### 
  #######################
  
  ## computing axes limits if they are null
  if(is.null(fixed_xlim)&(is.null(fixed_ylim))){
    ## Obtaining x and y limits for the plots
    extra_limit=0.01
    x_lim_sup=max(data_plot[,1])+extra_limit*abs(max(data_plot[,1]))
    y_lim_sup=max(data_plot[,2])+extra_limit*abs(max(data_plot[,2]))
    
    x_lim_inf=min(data_plot[,1])-extra_limit*abs(min(data_plot[,1]))
    y_lim_inf=min(data_plot[,2])-extra_limit*abs(min(data_plot[,2]))
    
    fixed_xlim=c(x_lim_inf,x_lim_sup)
    fixed_ylim=c(y_lim_inf,y_lim_sup)
  }
  
  
  ####################
  ##### plotting ##### 
  ####################
  
  ## initialize the plot 
  plot_data <-ggplot2::ggplot(data=data_plot,
                              aes(colour=prob
                              ))  
  
  ## scatter plot if both variables are numeric (more than two unique variables)
  if(all(type_vars == "cont")){
    plot_data <-plot_data + ggplot2::geom_point(aes(x=data_plot[,1],y=data_plot[,2],),size = 2.2)
    
    ## setting xlim and ylim if they are specified in the parameters of the function
    plot_data=  plot_data +
      # setting x and y axes limits
      ggplot2::coord_cartesian(xlim=fixed_xlim, ylim=fixed_ylim)
    
    
    ## geom_jitter if one variable is not numeric (less than two unique variables)
  } else if(any(type_vars == "cat")){
    # plotting geom jitter
    set.seed(1234)
    plot_data <- plot_data+ ggplot2::geom_jitter(aes(x=factor(data_plot[,1]),y=data_plot[,2]),
                                                 height=0, size = 2.2)
    
    ## setting ylim if it is are specified in the parameters of the function
    plot_data=  plot_data +
      ggplot2::coord_cartesian( ylim=fixed_ylim)
    
  }
  
  ## adding color
  plot_data <- plot_data + ggplot2::scale_color_gradient2(low=palette_colors[1], 
                                                          mid=mid_color,
                                                          high=palette_colors[2], 
                                                          midpoint=0.5,
                                                          limits=c(0,1),
                                                          space ="Lab" ,
                                                          breaks=c(0,0.5,1))+

    ## plot theme
    ggplot2::theme_bw() +
    
    ## axes names
    ggplot2::xlab(colnames(data_plot)[1]) +
    ggplot2::ylab(colnames(data_plot)[2]) +
    
    ## theme of title and axes
    ggplot2::theme(plot.title = element_text(hjust = 0.5),
                   legend.position = "bottom")  
  
  return(plot_data)
}



##################################################################
##################################################################
######   PLOT OF THE AVAILABLE DATA FOR A FIXED SUBSPACE    ######
##################################################################
##################################################################

## Function to plot the available data for a subspace
## PARAMETERS:
# - consistent_computed_subspaces is the return of the consistent_subspaces_computation function
# - subspace is an integer selecting one of the obtained subspaces
# - plot_train_sample: logical T/F indicating whether to plot the subspaces of the train sample or not

plot_available_data <- function(consistent_computed_subspaces, subspace,plot_train_sample=F){
  
  ###########################
  #### gathering results ####
  ###########################
  
  ## Gathering dataX, dataProb and obtained subspaces
  if (!plot_train_sample){
    obtained_subspace=consistent_computed_subspaces$obtained_subspaces$test[[subspace]]
    sample_test <- consistent_computed_subspaces$data$dataSample=="test"
    dataX <- consistent_computed_subspaces$data$dataX[sample_test,]
    dataProb <- consistent_computed_subspaces$data$dataProb[sample_test]
    
  }else{
    obtained_subspace=consistent_computed_subspaces$obtained_subspaces$train[[subspace]]
    sample_train <- consistent_computed_subspaces$data$dataSample=="train"
    dataX <- consistent_computed_subspaces$data$dataX[sample_train,]
    dataProb <- consistent_computed_subspaces$data$dataProb[sample_train]
  }
  
  
  ###################################################
  ##### obtaining data to plot of this subspace #####
  ###################################################
  
  ## obtained results for the subspace in the corresponding iteration
  ix_subspace=obtained_subspace$data_subspace
  ix_not_subspace=obtained_subspace$data_not_subspace
  variables=obtained_subspace$variables
  
  ## Data for plotting
  dataX_plot=dataX[c(ix_subspace,ix_not_subspace),variables]
  dataProb_plot=dataProb[c(ix_subspace,ix_not_subspace)]
  
  ## data plot
  data_plot = cbind(dataX_plot,"prob"=dataProb_plot)
  
  ## getting the data type of the variables to be plotted
  count_unique <-   rapply(dataX[,variables],  function(x) length(unique(x)))
  type_x1 <- ifelse(count_unique[1]<=2, "cat", "cont")
  type_x2 <- ifelse(count_unique[2]<=2, "cat", "cont")
  
  
  ####################
  ##### plotting ##### 
  ####################
  
  available_data_plot <- plot_fixed_format(data_plot, type_x1,type_x2)
  return(available_data_plot)
}



###########################################
###########################################
######   PLOT OF A FIXED SUBSPACE    ######
###########################################
###########################################

## Function to plot the available data for a subspace
## PARAMETERS:
# - consistent_computed_subspaces is the return of the consistent_subspaces_computation function
# - subspace is an integer selecting one of the obtained subspaces
# - fixed_xlim is a vector of length 2 containing the minimum and maximum limits for the x axis
# - fixed_ylim is a vector of length 2 containing the minimum and maximum limits for the y axis
# - plot_train_sample: logical T/F indicating whether to plot the subspaces of the train sample or not

plot_subspace_data <- function(consistent_computed_subspaces, subspace, fixed_xlim=NULL, fixed_ylim=NULL,plot_train_sample){
  
  ###########################
  #### gathering results ####
  ###########################
  
  ## Gathering dataX, dataProb and obtained subspaces
  if (!plot_train_sample){
    obtained_subspace=consistent_computed_subspaces$obtained_subspaces$test[[subspace]]
    sample_test <- consistent_computed_subspaces$data$dataSample=="test"
    dataX <- consistent_computed_subspaces$data$dataX[sample_test,]
    dataProb <- consistent_computed_subspaces$data$dataProb[sample_test]
    
  }else{
    obtained_subspace=consistent_computed_subspaces$obtained_subspaces$train[[subspace]]
    sample_train <- consistent_computed_subspaces$data$dataSample=="train"
    dataX <- consistent_computed_subspaces$data$dataX[sample_train,]
    dataProb <- consistent_computed_subspaces$data$dataProb[sample_train]
  }
  
  
  ###################################################
  ##### obtaining data to plot of this subspace #####
  ###################################################
  
  ## obtained results for the subspace in the corresponding iteration
  ix_subspace=obtained_subspace$data_subspace
  variables=obtained_subspace$variables
  
  ## Data for plotting
  dataX_plot=dataX[c(ix_subspace),variables]
  dataProb_plot=dataProb[c(ix_subspace)]
  
  ## data plot
  data_plot = cbind(dataX_plot,"prob"=dataProb_plot)
  
  ## getting the data type of the variables to be plotted
  count_unique <-   rapply(dataX[,variables],  function(x) length(unique(x)))
  type_x1 <- ifelse(count_unique[1]<=2, "cat", "cont")
  type_x2 <- ifelse(count_unique[2]<=2, "cat", "cont")
  
  
  ####################
  ##### plotting ##### 
  ####################
  
  subspace_data_plot <- plot_fixed_format(data_plot,type_x1, type_x2, fixed_xlim, fixed_ylim)
  
  return(subspace_data_plot)
}

