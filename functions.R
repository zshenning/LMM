

require(parallel)    #'parallel' is base and would at least detect desired cores
                     #The following code will run in parallel on Windows, Linux, and Mac

#Functions built to avoid needing package hydroGOF
#'rmse' is finding the root mean square error for predicted values based on observations
rmse <- function(predicted, obs)   sqrt(mean((obs-predicted)^2, na.rm=T))
#'pbias' is percent bias for predicted values based on observations
pbias<- function(predicted, obs) (100 * sum( predicted - obs,na.rm=T )) / sum( obs,na.rm=T)

############################################

#The following is the funcion for running machine learning models
#the below function runs the model and outputs the calculation as well as the the actual predicted data
#outputing only the predicted data might make sense
#issues might develop from getting the residuals from lmm will have to check
#model Beta values being export to the global environment might be useful (at least to the "imodel" function)

model<- function(i,dat, mvar,  response,fact, level, runtype, moniter=1, neuronnode=NA){
  #output
  outa<-list()   #outa  holds relevent conditions and output
  
  #current moniter features being used
  vname<-mvar[,moniter[i]]
  
  #person to be removed
  leaveout<-level[i]
  testIndex = sapply(subset(dat, select=fact), function(x) (x==leaveout))  #testIndex is the true and false values to partition the data
  
  #formula to be used
  fmla <- as.formula(paste(response,"~", paste(vname, collapse="+")))
  
  #should operate like this in the future
  testdata<-dat[testIndex,vname]
  trainingdata<-dat[!testIndex,]
  
  observed <-subset(dat,testIndex,select=response)
    
  
  if(!exists("neuronnode")) neuronnode<-NA
  #number of neurons being used if applicable 
  outa$neuron<-neuron_num<-neuronnode[i]

  
  
  #formula will be moved here    ZH
  #fmla<- {some source}    ZH
  
  #Training
  
  #run_type indicates which model will be run
  #this will probably be changed to include typed words
  #runtype   #1 = neuralnet, 2 = nnet, 3 = linear model, 4 = linear mix model
  
  #Select type of model with switch
  switch(runtype[i],{
    
    #neuralnet linear output
    ptm<-proc.time()[3]
    trained<-neuralnet(fmla, trainingdata,hidden=neuron_num,threshold = 0.01,stepmax = 10^7,
                       rep=1, err.fct="sse", linear.output=TRUE)
    fn_time<- proc.time()[3] - ptm
    
    predicted <- try(compute(trained,testdata)$net.result)
    
    #The below code checks for an error from compute. If error it will retrain up to six trys to converge model and avoid error 
    autoStop<-0
    while(inherits(predicted,"try-error")){
      if(autoStop>5)stop("Neuralnet doesn't converge.  Fatal Error in repeated neuralnet")  #need to keep it from looping forever
      ptm<-proc.time()[3]
      trained<-neuralnet(fmla, trainingdata,hidden=neuron_num,threshold = 0.01,stepmax = 10^7,
                         rep=1, err.fct="sse", linear.output=TRUE)
      fn_time<- proc.time()[3] - ptm
      
      predicted <- try(compute(trained,testdata)$net.result)
      autoStop=autoStop+1
    }
    
  },{
    
    
    #nnet linear output
    ptm<-proc.time()[3]
    
    trained<-nnet(fmla ,trainingdata, size = neuron_num, rang = 0.1,
                  decay = 5e-4, na.rm=T,maxit = 100000,linout=T) 
    #previous has value at decay=0.2666667
    
    fn_time<- proc.time()[3] - ptm
    
    predicted <- predict(trained, testdata)
  },{
    
    #lm linear model
    ptm<-proc.time()[3]
    
    trained<-lm(fmla ,trainingdata)
    
    fn_time<- proc.time()[3] - ptm
    
    predicted <- predict(trained, testdata)*1
    #mode(observed)="zoo"     #For some reason they started throwing an error for being included might be exclusion of some packages
    #mode(predicted)="zoo"
    
  },{
    
    #lmm : linear mixed model
    
    ptm<-proc.time()[3]
    
    trained<-lmer(fmla, trainingdata, REML=F)    
    
    fn_time<- proc.time()[3] - ptm
    
    predicted <- predict(trained, dat[testIndex,], allow.new.levels=TRUE)             #this one has to be "dat[testIndex,]" This particular model needs "Participant"
    
    #Extra output
    if(special){
      residuals<-data.frame(VarCorr(trained))
      outa$Prtcpt_std.Dev<-residuals$sdcor[1]
      outa$Resid_std.Dev<-residuals$sdcor[2]
    }
  })
  
  #Prep
  #observed <-dat[testIndex,c("METs")]
  
  #observed <-ob # This method of finding observed elsewhere and then, running the batch makes sense.     (Old data) data.frame(dat[testIndex,2])    #Seems to work fine if the value is 2. Can't seem to use the name "METs"
  
  prep <- data.frame(predicted, observed)
  
  #to get lapply to record actual model is possible
  
  #General output
  outa$moniter<-moniter[i]
  outa$participant<-participant[i]
  outa$runtype<-runtype[i]
  outa$time<-fn_time
  
  #Testing and Output
  outa$rmse <- rmse(predicted, observed)#self written code might work better
  outa$cor <- cor(prep, use="pairwise.complete.obs", method="pearson")[1,2] 
  outa$bias <- pbias(predicted,observed)
  
  #to check the Mets with predicted values
  #and to create hidden outputs for later
  #The next three lines duplicate moniter, runtype, participant, and neuron to mmatch predicted values
  #It  is easier to match the predicted values with model 
  n<-length(predicted)
  kouta<-list(moniter=outa$moniter,participant=outa$participant,runtype=outa$runtype,neuron=outa$neuron)
  hid<-data.frame(sapply(kouta, function(x) rep(x,n)),prep)
  
  coutput<-list(show=outa,hide=hid)     #This function outputs a list [rmse, cor, bias] and predicted values
  
  #outa
}

#this next bit is a function to assign names to the outputs created
#very predictable
#output_name <- function(x){  #This is a function built  to change the output into words. A different location is better
#  #assuming that only the output is being used 
#  #no check for if it is
#  x$moniter <- sapply(x$moniter, switch, "AGhip", "AGthigh", "GEleft", "GEright", "APthigh")
#  x$runtype <- sapply(x$runtype, switch, "neuralnet", "neuralbinary", "nnet", "nnet_logistic", "linear", "lme")
#  #x$neuron <-if (sum(!is.na(x$neuron))==0)x$neuron <- NULL
#  x
#}

#This function handle the output and runs the models in parallel
#the inputs are 
#run - vector of which instances of moniter, neuron, runtype, and model should be run
#This allows the jobs to be seperated into componet parts faster
#runparallel - binary input that doesn't need to bbe present. Gives user choice to run models in parallel or not.
#future idea: make runparallel an integer to determine the max number of cores the user wishes to use   ZH

#This below function is a wrapper that feeds expressions from the user to the imodelH. This is for me only
imodel  <- function(run=1, ...,special=F, runparallel=F,hidden=F){
   
  #if runparallel is logical TRUE then use max availlable cores if integer use max specific cores
  if(length(run)==1) runparallel = FALSE   #Someone may try running one instance without realizing to turn off parallel
  
  #lazy calls to the arguments
  if(!exists("neuronnode")) neuronnode<-NA
    
    #If  parallel isn't installed install it
    require(parallel) #parallel is part of the base package. It is available for every R program
    num_cores <- detectCores()-1 #detect the number of available logical cores. This number can be greater than number of actual cores 
    #leave one core for operating system
    runpar<-min(runparallel,num_cores)#an integer can be put for 'runparallel' to limit the number of cores used
  
    if (runpar && num_cores > 1) {    #if one core is available then no point to running in parallel. Need one core exclusively for OS 
      switch(.Platform[[1]],    #this switch is for the possibility of something other than Mac, Windows, Unix. Maybe SolarSun
             windows={   #parallel commands for different systems  (windows != (linux OR mac))
               #if the following packages are not included the program can run with the ones that are installed
               list.of.packages <- c("lme4", "neuralnet", "nnet")
               exp.packages <- list.of.packages[(list.of.packages %in% installed.packages()[,"Package"])]
               parExport<-c("dat","runif","response", "moniter", "mvar", "level", "runtype","model", "rmse","pbias","neuronnode","fact")
               if(length(exp.packages)){
                 if("lme4" %in% exp.packages){parExport<-c(parExport,"lmer","VarCorr");require("lme4")}
                 if("nnet" %in% exp.packages){parExport<-c(parExport,"nnet");require("nnet")}
                 if("neuralnet" %in% exp.packages){parExport<-c(parExport,"neuralnet","compute");require("neuralnet")}
               }
               #construction of cluster in windows enviroment
               cl <- makeCluster(num_cores)   #make cluster and export all necessary functions and variables to make it run
               clusterExport(cl, varlist=parExport, envir=environment())#this is important because at some point these commands will be put in a function
               #cluster export will be changed to take in values from the imodel function and others #############CHANGE
             },{
               
               #the cluster will come up as unix
               cl <- makeCluster(num_cores, type="FORK")   #make cluster with the same memory address for all cores. Not possible on windows
             })
      
      #Have to export values so that all cores on one node can function for Windows OS
      #multiple versions of make cluster for differnt OS
      
      #before the cluster is run a balancing call is created
      #the indicators might for example have all the intensive models at the end. This is a problem for economical parallelization
      #it is best to reduce communication between cores to keep them fast
      # a random order decreases the probability that all intentsive models are run on one core of the cluster
      bal<-order(runif(length(run)))   #create random ordering for parLappy
      unbal<-order(bal)                    #create inverse to undo random ordering
      bal_run<-run[bal]            #this is the actual point where run becomes reordered 
      
      clusterSetRNGStream(cl)
      
      msets<<-parLapply(cl,bal_run, model,...)
      stopCluster(cl)
      
      hidden<-lapply(msets, function(x) x$hide)
      hidden<<-do.call(rbind.data.frame,hidden[unbal]) #hidden becomes a global variable
      msets<-lapply(msets, function(x) x$show)
      output<-do.call(rbind.data.frame,msets[unbal])
      print("Parallel")
      
    } else {msets<-lapply(run, model,...) #else run single core code if only one core is available
            
            hidden<-lapply(msets, function(x) x$hide)
            hidden<<-do.call(rbind.data.frame,hidden)
            msets<-lapply(msets, function(x) x$show)
            output<-do.call(rbind.data.frame,msets)
            print("Series")
    }
    
  if(dim(output)[1]>1){         #to do this fix there must be at least two rows
    x<-rownames(output)         #This code has the output rownames fixed. haven't figured out why the first row is 2 and the second is 210
    x[1]<-1
    x[2]<-2
    rownames(output)<-x
  }
  output
}

print("Research Functions")
