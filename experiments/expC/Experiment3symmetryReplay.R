
library(xegaBNF)
library(xega)
library(xegaCE)

trials<-10
rtrials<-10

###
experimentname<-"ExpC"
popsize<-50
generations<-5000
k<-3
outpath<-"data"
algorithm<-"sgp"
executionModel<-"MultiCore"
verbose<-1
everyk<-10
batch<-FALSE

# Standard Grammar

sym4replay13<-kSym(treatmentname="BoolT1SGPreplayXIII", 
               experimentname=experimentname, k=k, trials=trials, 
               grammarfn="AndOrNotTuned1.txt", algorithm=algorithm,
               popsize=popsize, generations=generations,
               executionModel="MultiCore", verbose=verbose, 
               semantics="byValue", replay=13, batch=batch,
               Gpath=kSymmetryGrammarPath, outpath=outpath, everyk=everyk)

sym4replay19<-kSym(treatmentname="BoolT1SGPreplayXIX", 
               experimentname=experimentname, k=k, trials=trials, 
               grammarfn="AndOrNotTuned1.txt", algorithm=algorithm,
               popsize=popsize, generations=generations,
               executionModel="MultiCore", verbose=verbose, 
               semantics="byValue", replay=19, batch=batch,
               Gpath=kSymmetryGrammarPath, outpath=outpath, everyk=everyk)

sym4replay0a<-kSym(treatmentname="BoolT1SGPreplayZa", 
               experimentname=experimentname, k=k, trials=rtrials, 
               grammarfn="AndOrNotTuned1.txt", algorithm=algorithm,
               popsize=popsize, generations=generations,
               executionModel="MultiCore", verbose=verbose, 
               semantics="byValue", replay=0, batch=batch,
               Gpath=kSymmetryGrammarPath, outpath=outpath, everyk=everyk)

sym4replay0a<-kSym(treatmentname="BoolT1SGPreplayZb", 
               experimentname=experimentname, k=k, trials=rtrials, 
               grammarfn="AndOrNotTuned1.txt", algorithm=algorithm,
               popsize=popsize, generations=generations,
               executionModel="MultiCore", verbose=verbose, 
               semantics="byValue", replay=0, batch=batch,
               Gpath=kSymmetryGrammarPath, outpath=outpath, everyk=everyk)
