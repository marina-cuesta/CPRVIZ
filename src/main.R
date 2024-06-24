## loading necessary scripts
## path of the project
path=getwd()
# to load packages
source(file=paste0(path,"/src/packages_loading.R"))
# lo load extracting subspace function
source(file=paste0(path,"/src/functions/consistent_subspaces_computation_function.R"))
# lo load plotting functions
source(file=paste0(path,"/src/functions/display_plot_functions.R"))
source(file=paste0(path,"/src/functions/info_table_functions.R"))


## path to save the results
path_save=paste0(path,"/results/CPRViz_plots/")


###################################################
######   SELECT DATASET TO COMPUTE CPRVIZ    ######
###################################################

###### SELECT ONE of the following datasets and uncomment the corresponding line
# dataset="albert" 
# dataset="arrhythmia"
# dataset="asteroid"
dataset= "bands"
# dataset="bodyfat"
# dataset= "boston" 
# dataset="Breastcancer"
# dataset=  "chatfield_4"
# dataset= "clean1"
# dataset="clean2" 
# dataset="creditcard"
# dataset= "elephant" 
# dataset="emotions_amazed.surprised"
# dataset="emotions_amazed.surprised"
# dataset="emotions_happy.pleased"
# dataset= "emotions_quiet.still"
# dataset="emotions_relaxing.calm" 
# dataset="emotions_sad.lonely"  
# dataset="fox"
# dataset="fri_c0_250_50" 
# dataset="hepatitis"
# dataset="image_testing"  
# dataset="ionosphere"
# dataset="jannis" 
# dataset="kc1" 
# dataset="letter"
# dataset="madelon" 
# dataset="magic"
# dataset="MI_class6"
# dataset="MI_class9" 
# dataset="MiniBooNE"
# dataset="mv"
# dataset="nomao" 
# dataset="ozone-level-8hr"
# dataset="parkinsons"
# dataset="pc4"
# dataset="pima"
# dataset="Satellite"
# dataset="scene_Beach"
# dataset="scene_FallFoliage"
# dataset="scene_Field"
# dataset="scene_Sunset"
# dataset= "simulated_chaotic_model"
# dataset="simulated_DT"
# dataset="simulated_indecisive_model"
# dataset="simulated_LR"
# dataset="sonar"
# dataset="spambase"
# dataset="spectfheart" 
# dataset="steel-plates-fault" 
# dataset="sylva_prior" 
# dataset= "sylvine"
# dataset="tiger"
# dataset="vehicleNorm" 
# dataset="waveform-5000" 
# dataset= "yeast_class1"  
# dataset="yeast_class2"
# dataset= "yeast_class4"
# dataset="yeast_class4"


###################################
######   READING DATA SET    ######
###################################
  
path_data <- paste0("data/",dataset,"_plot.csv")
data <- read.csv(path_data)


##############################################
######   PREPARING DATA FOR FUNCTION    ######
##############################################

dataX <- data %>% select(-c(class,prob,sample))
dataProb <- data %>% pull(prob)
dataSample <- data %>% pull(sample)


#############################################
######   COMPUTING CPRViz SUBSPACES    ######
#############################################
## parameters
k_neighbours=0.05
epsilon_parameter=0.05
scale_data=TRUE

## applying CPRViz function to extract consistent subspaces
CPRViz_subspaces <- CPRViz_subspaces_computation(dataX,dataProb,dataSample,
                                                                  k_neighbours,epsilon_parameter, 
                                                                  scale_data=TRUE)


#################################################
######   CPRViz VISUALIZATION PARAMETERS   ######
#################################################

min_data=0.05
max_complexity=1
prob_breaks=c(0.5)
show_disaggregated_info=F
## saving name params
params_name <- paste0("_k",k_neighbours,"_trunc", epsilon_parameter)


###############################################
######   CPRViz VISUALIZATION IN TRAIN   ######
###############################################

plot_train_sample=T
path_save_sample="train/"

######## information table ########
information_table=display_info_table(CPRViz_subspaces,min_data,max_complexity,
                                     plot_train_sample,prob_breaks,show_disaggregated_info)
# number of subspaces meeting min_data and max_complexity conditions
n_subspaces_conditions=dim(information_table$body$dataset)[1]
# saving the table as png in the results folder
table_name=paste0(path_save,path_save_sample,dataset,"_train_table_info",params_name,".png")
flextable::save_as_image(information_table, path = table_name)


######## display of consistent subspaces ########
subspaces_info=display_subspaces(CPRViz_subspaces,min_data,max_complexity,
                                 plot_train_sample,prob_breaks)
# saving the plot in the results folder
filename=paste0(dataset,"_train_plots_info",params_name,".png")
ggplot2::ggsave( filename,
                 plot = subspaces_info,
                 device = png(),
                 path = paste0(path_save,path_save_sample),
                 width = 20,
                 height =ceiling(n_subspaces_conditions/2)*10+7,
                 units = "cm",
                 dpi = 300,  limitsize = TRUE,  bg = NULL)
dev.off()

######## storytelling of consistent subspaces  ########
subspaces_storytelling=display_storytelling(CPRViz_subspaces,min_data,
                                            max_complexity,plot_train_sample)
# saving the plot
filename=paste0(dataset,"_train_plots_storytelling",params_name,".png")
ggplot2::ggsave( filename,plot = subspaces_storytelling,
                 device = png(),
                 path = paste0(path_save,path_save_sample),
                 width = 20,
                 height = n_subspaces_conditions*10+2,
                 units = "cm",
                 dpi = 300,  limitsize = TRUE,  bg = NULL)
dev.off()




##############################################
######   CPRViz VISUALIZATION IN TEST   ######
##############################################

plot_train_sample=F
path_save_sample="test/"

######## information table ########
information_table=display_info_table(CPRViz_subspaces,min_data,max_complexity,
                                     plot_train_sample,prob_breaks,show_disaggregated_info)
# number of subspaces meeting min_data and max_complexity conditions
n_subspaces_conditions=dim(information_table$body$dataset)[1]
# saving the table as png in the results folder
table_name=paste0(path_save,path_save_sample,dataset,"_test_table_info",params_name,".png")
flextable::save_as_image(information_table, path = table_name)


######## display of consistent subspaces ########
subspaces_info=display_subspaces(CPRViz_subspaces,min_data,max_complexity,
                                 plot_train_sample,prob_breaks)
# saving the plot in the results folder
filename=paste0(dataset,"_test_plots_info",params_name,".png")
ggplot2::ggsave( filename,
                 plot = subspaces_info,
                 device = png(),
                 path = paste0(path_save,path_save_sample),
                 width = 20,
                 height =ceiling(n_subspaces_conditions/2)*10+7,
                 units = "cm",
                 dpi = 300,  limitsize = TRUE,  bg = NULL)
dev.off()

######## storytelling of consistent subspaces  ########
subspaces_storytelling=display_storytelling(CPRViz_subspaces,min_data,
                                            max_complexity,plot_train_sample)
# saving the plot
filename=paste0(dataset,"_test_plots_storytelling",params_name,".png")
ggplot2::ggsave( filename,plot = subspaces_storytelling,
                 device = png(),
                 path = paste0(path_save,path_save_sample),
                 width = 20,
                 height = n_subspaces_conditions*10+2,
                 units = "cm",
                 dpi = 300,  limitsize = TRUE,  bg = NULL)
dev.off()
