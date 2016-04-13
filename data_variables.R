#prebuilt variables for the example data


#creation of indicators to specify what models to create 
#this section (next four lines) is to be an example and to help with error checking
moniter     <- rep(1:5,rep(40,5))
participant <- rep(1:40,5)
runtype     <- rep(3,200)
neuronnode  <- rep(2:6,rep(40,5))


#formula building 
#This section builds the actual formula terms
#Without binary needed to be run, can predictplify to actual formula building. Revise to insert actual formulas into the code
#makes sense that person running this code is responsible for building the formulas to be used
#however there would need to be some formula decomposition involed for neuralnet as the compute formula needs EXACTLY the feature data and nothing else

response<-"METs"

moniterName<-c("AGhip", "AGthigh", "GEleft", "GEright", "APthigh")   #moniter names
varNUM = length(moniterName)    #another number of loops

mvar<-sapply(1:varNUM,function(i) {
  underscore="_"
  varset <- c("mean", "stdev", "min", "p10", "p25", "p50", "p75", "p90", "max", "cov")
  curmoniterName <- rep(moniterName[i],30)
  curvarorn <- rep(c("X","Y","Z"),c(10,10,10))
  vname <- paste0(curmoniterName,curvarorn,underscore,varset)  #varset repeats automatically (may change)
})

print("Example Research Data Variables/ This file should only be included for the example")
