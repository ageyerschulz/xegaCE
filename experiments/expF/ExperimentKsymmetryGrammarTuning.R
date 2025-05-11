
library(xegaBNF)
library(xega)
library(xegaCE)

trials<-5

###
experimentname<-"EE"
popsize<-400
generations<-1000
outpath<-"data"
executionModel<-"MultiCore"
verbose<-1
everyk<-5
algorithm<-"sgp"

# Standard Grammar

for (k in (2:12))
{
newk<-formatC(k, width = 2, format = "d", flag = "0")
sym3DFMCAndOrNot<-kSym(treatmentname=paste0("BoolT5", algorithm, newk, "k"), 
               experimentname=experimentname, k=k, trials=trials, 
               grammarfn="AndOrNotTuned5.txt", algorithm=algorithm,
               popsize=popsize, generations=generations,
               executionModel="MultiCore", verbose=verbose, 
               Gpath=kSymmetryGrammarPath, outpath=outpath, everyk=everyk)

}

