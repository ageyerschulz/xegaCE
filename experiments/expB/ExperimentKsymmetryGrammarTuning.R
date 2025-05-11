
library(xegaBNF)
library(xega)
library(xegaCE)

trials<-20

###
experimentname<-"EB"
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
sym3DFMCAndOrNot<-kSym(treatmentname=paste0("BoolT0", algorithm, k, "k"), 
               experimentname=experimentname, k=k, trials=trials, 
               grammarfn="AndOrNot.txt", algorithm=algorithm,
               popsize=popsize, generations=generations,
               executionModel="MultiCore", verbose=verbose, 
               Gpath=kSymmetryGrammarPath, outpath=outpath, everyk=everyk)

sym3DFMCAndOrNotT1<-kSym(treatmentname=paste0("BoolT1", algorithm, k, "k"), 
               experimentname=experimentname, k=k, trials=trials, 
               grammarfn="AndOrNotTuned1.txt", algorithm=algorithm, 
               popsize=popsize, generations=generations,
               executionModel="MultiCore", verbose=verbose,
               Gpath=kSymmetryGrammarPath, outpath=outpath, everyk=everyk)

sym3DFMCAndOrNotT2<-kSym(treatmentname=paste0("BoolT2", algorithm, k, "k"),
               experimentname=experimentname, k=k, trials=trials, 
               grammarfn="AndOrNotTuned2.txt", algorithm=algorithm,
               popsize=popsize, generations=generations,
               executionModel="MultiCore", verbose=verbose, 
               Gpath=kSymmetryGrammarPath, outpath=outpath, everyk=everyk)

sym3DFMCAndOrNotT3<-kSym(treatmentname=paste0("BoolT3", algorithm, k, "k"),
               experimentname=experimentname, k=k, trials=trials, 
               grammarfn="AndOrNotTuned3.txt", algorithm=algorithm, 
               popsize=popsize, generations=generations,
               executionModel="MultiCore", verbose=verbose,
               Gpath=kSymmetryGrammarPath, outpath=outpath, everyk=everyk)
 
sym3DFMCAndOrNotT4<-kSym(treatmentname=paste0("BoolT4", algorithm, k, "k"),
               experimentname=experimentname, k=k, trials=trials, 
               grammarfn="AndOrNotTuned4.txt", algorithm=algorithm, 
               popsize=popsize, generations=generations,
               executionModel="MultiCore", verbose=verbose, 
               Gpath=kSymmetryGrammarPath, outpath=outpath, everyk=everyk)
}

