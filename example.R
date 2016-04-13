#example use
#Use 'setwd()' to set current directory of research files.
#######################Need my directory
setwd("C:/Users/Zachary/Desktop/Perf Lab R 1example")
source("data_variables.R")
source("functions.R") #Loads built research functions used

load("example_data.rda")
#packages that are required to run some of the models
library(neuralnet)
library(nnet)
library(lme4)

dat<-exData

response<-"METs" #Identify variable of interest for following functions
fact<- "Participant"

head(mvar)  #'mvar' is a pre-created matrix that contains all the variable names of interest

#The following are conditions on each model that is run
moniter     <- rep(1,3)  #rep(1:5,rep(40,5))
level<-participant <- rep(27,3) # rep(27:40,5)
runtype     <- rep(3,3)  #rep(3,200)
neuronnode  <- NA # rep(2:6,rep(40,5))

#neuron needs to be NA
results<- imodel(run=1:3,dat=exData,response="METs",mvar=mvar,moniter=moniter,level=participant,runtype=runtype,runparallel=F)  #  'imodel' iterated models

