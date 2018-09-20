require(parallel)    #'parallel' is base and would at least detect desired cores
                     #The following code will run in parallel on Windows, Linux, and Mac

#Functions built to avoid needing package hydroGOF
#'rmse' is finding the root mean square error for predicted values based on observations
rmse <- function(predicted, obs)   sqrt(mean((obs-predicted)^2, na.rm=T))
#'pbias' is percent bias for predicted values based on observations
pbias<- function(predicted, obs) (100 * sum( predicted - obs,na.rm=T )) / sum( obs,na.rm=T)

############################################

#The following is the funcion for running machine learning models

model<- function(i){
  #output
  outa<-list()
  
  #person to be removed
  leaveout<-level[i]
  testIndex = sapply(dat$Participant, function(x) (x==leaveout))
  
  #current moniter features being used
  vname<-mlist[,moniter[i]]
  
  #formula to be used
  fmla <- as.formula(paste(response,"~", paste(vname, collapse="+")))
  
  #split data
  testdata<-dat[testIndex,vname]
  trainingdata<-dat[!testIndex,]
  
  observed <-subset(dat,testIndex,select=response)
  
  #number of neurons being used if applicable 
  neuron_num<-neuronnode[i]
  
  #Training
  
  #run_type indicates which model will be run
  #this will probably be changed to include typed words
  #runtype   #1 = neuralnet, 2 = nnet, 3 = linear model, 4 = linear mixed model
  #hidden has been changed to 2
  
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
    outa$neuron <-neuron_num          #output number of neurons out
    
    
    
  },{
    
    #nnet linear output
    ptm<-proc.time()[3]
    
    trained<-nnet(fmla ,trainingdata, size = neuron_num, rang = 0.1,
                  decay = 5e-4, na.rm=T,maxit = 100000,linout=T) 
    #previous has value at decay=0.2666667
    
    fn_time<- proc.time()[3] - ptm
    
    predicted <- predict(trained, dat[testIndex,])
    outa$neuron <-neuron_num          #output number of neurons out
    
  },{
    
    #lm linear model
    ptm<-proc.time()[3]
    
    
    trained<-lm(fmla ,trainingdata)
    
    
    fn_time<- proc.time()[3] - ptm
    
    predicted <- predict(trained, dat[testIndex,])*1
    #mode(observed)="zoo"     #For some reason they started throwing an error for being included might be exclusion of some packages
    #mode(predicted)="zoo"
    outa$neuron <-NA #The new construction file will output everything and not just be dynamic
    
  },{
    
    #lmm : linear mixed model
    
    fmla <- as.formula(paste(response, "~ 1+ (1|Participant)+ ", paste(vname, collapse="+")))
    ptm<-proc.time()[3]
    
    trained<-lmer(fmla, trainingdata, REML=F)
    
    
    fn_time<- proc.time()[3] - ptm
    
    predicted <- predict(trained, dat[testIndex,], allow.new.levels=TRUE)
    
    #Extra output
    #if(lmex){
    if(F) {
      residuals<-data.frame(VarCorr(trained))
      outa$Prtcpt_std.Dev<-residuals$sdcor[1]
      outa$Resid_std.Dev<-residuals$sdcor[2]
    }
    outa$neuron <-NA #The new construction file will output everything and not just be dynamic
    
    
  })
  
  #Prep
  #observed <-dat[testIndex,c("METs")]
  
  #observed <-ob # This method of finding observed elsewhere and then, running the batch makes sense.     (Old data) data.frame(dat[testIndex,2])    #Seems to work fine if the value is 2. Can't seem to use the name "METs"
  
  prep <- data.frame(predicted, observed)
  
  #to get lapply to record actual model is possible
  
  #General output
  outa$moniter<-moniter[i]
  outa$participant<-level[i]
  outa$runtype<-runtype[i]
  outa$time<-fn_time
  
  #Testing and Output
  outa$rmse <- rmse(predicted, observed)#self written code might work better
  outa$cor <- cor(prep, use="pairwise.complete.obs", method="pearson")[1,2] 
  outa$bias <- pbias(predicted,observed)
  
  #to check the Mets with predicted values
  #and to create hidden outputs for later
  n<-length(predicted)
  kouta<-list(moniter=outa$moniter,participant=outa$participant,runtype=outa$runtype,neuron=outa$neuron)
  hid<-data.frame(sapply(kouta, function(x) rep(x,n)),prep)
  
  coutput<-list(show=outa,hide=hid)
  
  #outa
}

#this next bit is a function to assign names to the outputs created
#very simple
output_name <- function(x){
  #assuming that only the output is being used 
  #no check for if it is
  x$moniter <- sapply(x$moniter, switch, "AGhip", "AGthigh", "GEleft", "GEright", "APthigh")
  x$runtype <- sapply(x$runtype, switch, "neuralnet", "neuralbinary", "nnet", "nnet_logistic", "linear", "lme")
  x$neuron <-if (sum(!is.na(x$neuron))==0)x$neuron <- NULL
  x
}


imodel  <- function(int_run,runparallel=F, lmex=F,hidePredicted=F){
  
  #if runparallel is logical TRUE then use max availlable cores if integer use max specific cores
  if(length(int_run)==1) runparallel = FALSE   #may try running one instance without realizing to turn off parallel
  
  if(!exists("neuronnode")) neuronnode<-NA
  
  require(parallel)
  #int_run is a vector that contains the indicators to run
  
  if (runparallel)    num_cores <- detectCores()-1 else num_cores <- 1#detect the number of available logical cores. This number can be greater than number of actual cores 
  #leave one core for operating system
  
  if (num_cores > 1) {    #if one core is available then no point to running in parallel. Need one core exclusively for OS 
    switch(.Platform[[1]],    #this switch is for the possibility of something other than Mac, Windows, Unix. Maybe SolarSun
           windows={   #parallel commands for different systems  (windows != (linux OR mac))
             cl <- makeCluster(num_cores)   #make cluster and export all necessary functions and variables to make it run
             clusterExport(cl, varlist=c("dat","runif","response", "moniter", "mlist", "level", "runtype","model", "lmer","VarCorr","nnet", "rmse","pbias","neuronnode"), envir=environment())#this is important because at some point these commands will be put in a function
             #cluster export will be changed to take in values from the imodel function and others #############CHANGE
           },{
             
             #the cluster will come up as unix
             cl <- makeCluster(num_cores, type="FORK")   #make cluster with the same memory address for all cores. Not possible on windows
           })
    
    #Have to export values so that all cores on one node can function for Windows OS
    #multiple versions of make cluster for differnt OS
    
    bal<-order(runif(length(int_run)))   #create random ordering for parLappy
    unbal<-order(bal)                    #create inverse to undo random ordering
    bal_int_run<-int_run[bal]            #this is the actual point where int_run becomes reordered 
    
    clusterSetRNGStream(cl)
    
    msets<<-parLapply(cl,bal_int_run, model)
    
    stopCluster(cl)
    
    if(hidePredicted){
      hidden<-lapply(msets, function(x) x$hide)
      hidden<<-do.call(rbind.data.frame,hidden[unbal]) #hidden becomes a global variable
    }
    msets<-lapply(msets, function(x) x$show)
    output<-do.call(rbind.data.frame,msets[unbal])
    
    
  } else {msets<-lapply(int_run, model) #else run single core code if only one core is available for parallization
          
          if(hidePredicted){
            hidden<-lapply(msets, function(x) x$hide)
            hidden<<-do.call(rbind.data.frame,hidden)
          }
          msets<-lapply(msets, function(x) x$show)
          output<-do.call(rbind.data.frame,msets)
          
  }
  
  
  x<-rownames(output)         #Output rownames fixed.
  x[1]<-1
  x[2]<-2
  rownames(output)<-x
  if(FALSE) {    
    hit<-function(x) moniterName[x]
    output$moniter<-sapply(output$moniter, hit)
  }
  
  output
}

UseNames <- function(x) {
  #x$moniter <- 
  sapply(x$moniter, switch,moniterName)
}

print("Research Functions Loaded")
