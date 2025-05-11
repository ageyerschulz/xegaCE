
library(xegaBNF)
library(xega)
library(xegaCE)

trials<-20

###
experimentname<-"EE"
popsize<-200
generations<-500
outpath<-"data"
executionModel<-"MultiCore"
verbose<-1
everyk<-10
algorithm<-"sgp"

# Standard Grammar

for (k in (2:6))
{
sym3DFMCAndOrNot<-kSym(treatmentname=paste0("BoolT5", algorithm, k, "k"), 
               experimentname=experimentname, k=k, trials=trials, 
               grammarfn="AndOrNotTuned5.txt", algorithm=algorithm,
               popsize=popsize, generations=generations,
               executionModel="MultiCore", verbose=verbose, 
               Gpath=kSymmetryGrammarPath, outpath=outpath, everyk=everyk)

}

