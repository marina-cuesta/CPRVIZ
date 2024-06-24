# lo load plotting functions
source(file=paste0(path,"/src/functions/individual_plot_functions.R"))
source(file=paste0(path,"/src/functions/info_table_functions.R"))


######################################################
######################################################
######   LIST OF PLOTS AND INFORMATION TABLE    ######
######################################################
######################################################

## Function to obtain a list containing the plot of each of the obtained subspace and the information table
## PARAMETERS:
# - consistent_computed_subspaces is the return of the consistent_subspaces_computation function
# - min_data: minimum allowed percentage of data in a subspace for it to be visualized; must be in [0,1]
# - max_complexity: maximum complexity allowed for a subspace to be displayed; must be in [0,1]
# - plot_train_sample: logical T/F indicating whether to plot the subspaces of the train sample or not
# - prob_breaks: vector with the breaks to summarize probs in intervals
# - show_disaggregated_info: logical T/F indicating whether to show or not the information of each subspace
#   in a disaggragated format apart from the accumulated one 

list_plots_table <- function(consistent_computed_subspaces, min_data, max_complexity,
                             plot_train_sample=F, show_disaggregated_info=F,prob_breaks){
  
  #######################################
  #### Checking initial requirements ####
  #######################################
  
  ## if there is not test subspaces but plot_train_sample=F
  
  if(length(consistent_computed_subspaces$obtained_subspaces$test)==0 & plot_train_sample==F){
    stop("There is not subspaces in the test data to plot")
  }
  
  ## other initial check requirements are performed in CSViz_table function
  
  
  ####################################################
  ##### obtaining information table of subspaces ##### 
  ####################################################
  
  information_table=display_info_table(consistent_computed_subspaces, min_data, max_complexity,
                                       plot_train_sample, prob_breaks, show_disaggregated_info)
  
  ## Number of obtained subspaces meeting min_data and max_complexity condiions
  n_subspaces=dim(information_table$body$dataset)[1]
  
  ############################
  ##### storing results  ##### 
  ############################
  
  ## list to store the table and plots 
  table_plots_list=list()
  
  ## storing the table at the end of the list
  table_plots_list[[n_subspaces+1]]=information_table
  
  
  #############################################
  ##### Obtaining plots of the subspaces  ##### 
  #############################################
  
  table_plots_list[1:n_subspaces] = lapply(1:n_subspaces, plot_subspace_data, 
                                           consistent_computed_subspaces =
                                             consistent_computed_subspaces,plot_train_sample=plot_train_sample )
  return(table_plots_list)
}


################################################################
################################################################
######  DISPLAY PLOT OF SUBSPACES AND TABLE INFORMATION   ######
################################################################
################################################################

## Function to display the plot of the subspaces and their information table in a single plot
## PARAMETERS:
# - consistent_computed_subspaces is the return of the CSViz_subspaces_computation function
# - min_data: minimum allowed percentage of data in a subspace for it to be visualized; must be in [0,1]
# - max_complexity: maximum complexity allowed for a subspace to be displayed; must be in [0,1]
# - plot_train_sample: logical T/F indicating whether to plot the subspaces of the train sample or not
# - prob_breaks: vector with the breaks to summarize probs in intervals
display_subspaces <- function(consistent_computed_subspaces, min_data, max_complexity,
                                  plot_train_sample=F, prob_breaks){
  
  ## the initial check requirements is performed in info_table function

  ################################################################################
  #### percentage table of each probability intervals in the original dataset ####
  ################################################################################
  
  ## Gathering dataProb
  if (!plot_train_sample){
    sample_test <- consistent_computed_subspaces$data$dataSample=="test"
    dataProb <- consistent_computed_subspaces$data$dataProb[sample_test]
    
  }else{
    sample_train <- consistent_computed_subspaces$data$dataSample=="train"
    dataProb <- consistent_computed_subspaces$data$dataProb[sample_train]
  }
  
 
  ###############################
  ##### Obtaining intervals ##### 
  ###############################
  prob_breaks_aux = c(0, prob_breaks, 1)
  prob_breaks_aux_lag = lag(prob_breaks_aux)
  intervals_aux_1 <- paste("[",prob_breaks_aux_lag[c(-1,-length(prob_breaks_aux))],
                           "," ,prob_breaks_aux[c(-1,-length(prob_breaks_aux))], ")",sep="")
  intervals_aux_2 <- paste("[",prob_breaks_aux_lag[length(prob_breaks_aux)],
                           "," ,prob_breaks_aux[length(prob_breaks_aux)], "]",sep="")
  intervals <- c(intervals_aux_1,intervals_aux_2)
  n_intervals <- length(intervals)
  table_intervals=prob_intervals_table(dataProb, prob_breaks=prob_breaks)
    
  
  ########################################
  #### information table of subspaces ####
  ########################################
  
  ## obtaining plain table
  table_information=info_table(consistent_computed_subspaces, min_data, max_complexity,
                   plot_train_sample, prob_breaks,show_disaggregated_info=F)
  
  ## getting the information needed for the display
  table_information=table_information[,c(1,4:dim(table_information)[2])]
  names(table_information)=c("subspace","total", intervals,"complexity")
  
  ## Number of obtained subspaces
  n_subspaces=dim(table_information)[1]
  
  ## formatting table
  ## Format of the table to export
  table_information_format <- flextable::flextable(table_information) %>% 
    bg(bg="white",part="all") %>% 
    flextable::theme_box() %>% 
    flextable::add_header_row(colwidths = c(1,1,n_intervals, 1), 
                              values = c("", "total","probability intervals","")) %>% 
    flextable::add_header_row(colwidths = c(1,n_intervals+1,1), 
                              values = c("subspace","%n accumulated", "complexity")) %>% 
    flextable::merge_at(i = 1:3 ,j = 1, part = "header")%>%
    flextable::merge_at(i = 2:3 ,j = 2, part = "header") %>% 
    flextable::merge_at(i = 1:3 ,j = (2+n_intervals+1), part = "header") %>% 
    flextable::fontsize(size = 10,part='all') %>% 
    flextable::autofit(add_w=-2,add_h=-2) 
  table_information_format <- flextable::align(table_information_format, align = "center", part = "all")
  
  
  ########################
  #### grid of tables ####
  ########################
  
  ## converting both tables into a grob
  table_intervals_grob <- gen_grob(table_intervals,
                                   fit="width",just="centre", scaling="min")
  table_information_format_grob <- gen_grob(table_information_format,
                         fit="fixed",just="centre", scaling="min")

  ## grid of tables
  grid_tables=cowplot::plot_grid(table_intervals_grob,table_information_format_grob, ncol=2,
                                 scale=c(0.75,1), rel_widths = c(2,4.5))

  
  ########################################
  ##### Obtaining the grid of plots  ##### 
  ########################################
  
  ## list to store the plots and tables
  list_grid=vector("list", n_subspaces)
  
  ## Looping over the subspaces to obtain each plot and information table
  for (subspace in 1:(n_subspaces)){
    
    ## subspace plot of the subspace
    plot_subspace=plot_subspace_data(consistent_computed_subspaces, subspace, plot_train_sample = plot_train_sample)+
      ## adding title
      ggplot2::ggtitle(paste(english::ordinal(subspace),"subspace"))
    
    if(subspace ==1){
      ## obtaining legend for the plot
      legend=cowplot::get_plot_component(plot_subspace +
                           theme(legend.key.size = unit(0.35, 'cm')), 'guide-box-bottom', return_all = TRUE)
      }
    
    # deleting the legend and formating title
    plot_subspace <- plot_subspace +
      ggplot2::theme( legend.position = "none",
                      plot.title = element_text(size=12, face = "bold"))  
    
    ## converting to  grob
    set.seed(1234)
    plot_subspace=ggplotGrob(plot_subspace)
    
    ## grid of the plot and table of this subspace
    list_grid[[subspace]]= plot_subspace
  }
  
  
  ##########################
  ##### grid of plots  ##### 
  ##########################
  
  ## grid of plots and tables
  grid_plots=cowplot::plot_grid(plotlist=list_grid, ncol=2)
  ## adding legend
  grid_plots=cowplot::plot_grid(grid_plots,legend, nrow = 2, rel_heights = c(1, 0.1))+
    theme(panel.background = element_rect(fill = 'white', color = NA))
  
  #####################################
  ##### grid of plots and tables  ##### 
  #####################################
  
  display_subspaces=cowplot::plot_grid(grid_plots,grid_tables, nrow=2,rel_heights = c(1, 0.3))+
    theme(panel.background = element_rect(fill = 'white', color = NA))
  return(display_subspaces)
}


######################################################
######################################################
######  DISPLAY STORYTELLING OF THE SUBSPACES   ######
######################################################
######################################################

## Function to display the storytelling of the subspaces: plot of the available data for the 
# subspace and plot of the subspace in a single plot
## PARAMETERS:
# - consistent_computed_subspaces is the return of the consistent_subspaces_computation function
# - min_data: minimum allowed percentage of data in a subspace for it to be visualized; must be in [0,1]
# - max_complexity: maximum complexity allowed for a subspace to be displayed; must be in [0,1]
# - plot_train_sample: logical T/F indicating whether to plot the subspaces of the train sample or not

display_storytelling <- function(consistent_computed_subspaces, min_data, max_complexity,plot_train_sample){
  
  ## the initial check requirements is performed in info_table function
  
  ##################################################
  #### obtaining information table of subspaces ####
  ##################################################
  
  table=info_table(consistent_computed_subspaces, min_data, max_complexity,plot_train_sample)
  
  ## Number of obtained subspaces
  n_subspaces=dim(table)[1]
  
  ###################################################
  ##### Obtaining the grid of plots and tables  ##### 
  ###################################################
  
  ## list to store the plots and tables
  list_grid=vector("list", n_subspaces)
  
  ## Looping over the subspaces to obtain each plot and information table
  for (subspace in 1:(n_subspaces)){
    
    ## plot of the available data for the subspace
    plot_data=plot_available_data(consistent_computed_subspaces,subspace,plot_train_sample=plot_train_sample)+

      ## adding title
      ggplot2::ggtitle("available data") +
      # deleting the legend and formating title
      ggplot2::theme( legend.position = "none",
                      plot.title = element_text(face = "bold")) 
    

    ## getting xlim and ylim of the plot
    ylim=layer_scales(plot_data)$y$get_limits()
    xlim=layer_scales(plot_data)$x$get_limits()
    
    ## converting to  grob
    set.seed(1234)
    plot_data=ggplotGrob(plot_data)
    
    ## subspace plot of the subspace
    plot_subspace=plot_subspace_data(consistent_computed_subspaces, subspace,
                                      fixed_xlim = xlim, fixed_ylim = ylim ,plot_train_sample=plot_train_sample)+
      ## adding title
      ggplot2::ggtitle(paste(english::ordinal(subspace),"subspace"))
    
    if(subspace==1){
      ## obtaining legend for the plot
      legend=cowplot::get_plot_component(plot_subspace +
                                            theme(legend.key.size = unit(0.35, 'cm')), 'guide-box-bottom', return_all = TRUE)   
      }
      # deleting the legend and formating title
    plot_subspace <- plot_subspace +
      ggplot2::theme( legend.position = "none",
                      plot.title = element_text(face = "bold"))  
    
    ## converting to  grob
    set.seed(1234)
    plot_subspace=ggplotGrob(plot_subspace)
    
    ## grid of the plot and table of this subspace
    list_grid[[subspace]]= cowplot::plot_grid(plot_data,plot_subspace, 
                                              nrow = 1, align = "v")
  }
  
  ##########################
  ##### grid of plots  ##### 
  ##########################

  ## grid of plots and tables
  display_storytelling=cowplot::plot_grid(plotlist=list_grid, nrow=length(list_grid))
  
  ## adding legend
  display_storytelling=cowplot::plot_grid(display_storytelling,legend, nrow = 2,
                                          rel_heights = c(1, 0.1),greedy=T)+
    theme(panel.background = element_rect(fill = 'white', color = NA))
  return(display_storytelling)
}

