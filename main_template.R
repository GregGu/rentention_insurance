####### MAIN SCRIPT ##########


workdir <- "C:/Classification/classification"  #Gregs wd
setwd(workdir)
Rfiles <- list.files(file.path(paste0(getwd(),"/R/")), ".R") #gets names of r files
#Rfiles <- Rfiles[grepl(".R", Rfiles)] # select R scripts in that folder
sapply(paste0(paste0(getwd(),"/R/"), Rfiles), source) #gets functions from file

# First source the data
source("preprocess.R")
# This script produces the following two data frames
# continuous data: temp
# categorical data: dummy_data

# make a output folder to store results from run
# Specify these parameteres before run
# run 1 dummy data
# run 2 dummy data + selection
# run 3 continuous data
# run 4 continuous data + selection

# number of runs, this will be used in the ensemble
num_runs = 4

# Run 1
runname <- "run1"
selection = F # no variable selection for our models
MakeDirs(runname) # makes a folder in the respective directory
output.dir <- MakeDirs(runname) # gets the directory of the folder made above, this will be used to store files acordingly when we run our model script
chosen_data <- dummy_data # choose the data for our model script
source("model.R") # run the model script

# Run 2
runname <- "run2"
selection = T 
MakeDirs(runname) 
output.dir <- MakeDirs(runname) 
chosen_data <- dummy_data 
source("model.R") 

# Run 3
runname <- "run3"
selection = F # no variable selection for our models
MakeDirs(runname) # makes a folder in the respective directory
output.dir <- MakeDirs(runname) # gets the directory of the folder made above, this will be used to store files acordingly when we run our model script
chosen_data <- temp # choose the data for our model script
source("model.R") # run the model script

# Run 4
runname <- "run4"
selection = T # 
MakeDirs(runname) # makes a folder in the respective directory
output.dir <- MakeDirs(runname) # gets the directory of the folder made above, this will be used to store files acordingly when we run our model script
chosen_data <- temp # choose the data for our model script
source("model.R") # run the model script
