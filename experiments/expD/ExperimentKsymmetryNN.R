
library(xegaBNF)
library(xega)
library(xegaCE)

trials<-30

###
experimentname<-"EB"
popsize<-400
generations<-500
outpath<-"data"
executionModel<-"MultiCore"
verbose<-1
everyk<-10

top1<-function(k)
{
return(c(k, (2*k), k, 1))
}


# Standard Grammar

for (k in (2:6))
{
algorithm<-"sga"
s1<-kSymNN(treatmentname=paste0("NNtop1", algorithm, k, "k"), 
    experimentname=experimentname, k=k, topology=top1(k), trials=trials, 
               algorithm=algorithm,
               popsize=popsize, generations=generations,
               executionModel="MultiCore", verbose=verbose, 
               outpath=outpath, everyk=everyk)

algorithm<-"sgde"
s2<-kSymNN(treatmentname=paste0("NNtop1", algorithm, k, "k"), 
    experimentname=experimentname, k=k, topology=top1(k), trials=trials, 
               algorithm=algorithm,
               popsize=popsize, generations=generations,
               mutation="MutateGeneDE", scalefactor="Uniform",
               crossover="UCrossGene", replication="DE",
               selection="UniformP", mateselection="UniformP", accept="Best",
               executionModel="MultiCore", verbose=verbose, 
               outpath=outpath, everyk=everyk)
}

