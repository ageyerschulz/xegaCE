
library(xegaBNF)
library(xega)
library(xegaCE)

trials<-200
outpath<-"data"

###
experimentname<-"ExpA"
popsize<-400
generations<-1000
k<-3
grammarfn<-"Nand.txt"
replay<-0
verbose<-1
everyk<-10
batch<-FALSE
reportEvalErrors<-TRUE

# Standard Grammar

sym3SQSGP<-kSym(treatmentname="SQSGP", 
               experimentname=experimentname, k=k, trials=trials, 
               grammarfn=grammarfn, algorithm="sgp",
               popsize=popsize, generations=generations,
               executionModel="Sequential", verbose=verbose, 
               semantics="byValue", 
               reportEvalErrors=reportEvalErrors, replay=replay, batch=batch,
               Gpath=kSymmetryGrammarPath, outpath=outpath, everyk=everyk)

sym3MCSGP<-kSym(treatmentname="MCSGP", 
               experimentname=experimentname, k=k, trials=trials, 
               grammarfn=grammarfn, algorithm="sgp",
               popsize=popsize, generations=generations,
               executionModel="MultiCore", verbose=verbose, 
               semantics="byValue", 
               reportEvalErrors=reportEvalErrors, replay=replay, batch=batch,
               Gpath=kSymmetryGrammarPath, outpath=outpath, everyk=everyk)

sym3SQSGE<-kSym(treatmentname="SQSGE", 
               experimentname=experimentname, k=k, trials=trials, 
               grammarfn=grammarfn, algorithm="sge",
               popsize=popsize, generations=generations,
               executionModel="Sequential", verbose=verbose, 
               semantics="byValue", 
               reportEvalErrors=reportEvalErrors, replay=replay, batch=batch,
               Gpath=kSymmetryGrammarPath, outpath=outpath, everyk=everyk)

sym3MCSGE<-kSym(treatmentname="MCSGE", 
               experimentname=experimentname, k=k, trials=trials, 
               grammarfn=grammarfn, algorithm="sge",
               popsize=popsize, generations=generations,
               executionModel="MultiCore", verbose=verbose, 
               semantics="byValue", 
               reportEvalErrors=reportEvalErrors, replay=replay, batch=batch,
               Gpath=kSymmetryGrammarPath, outpath=outpath, everyk=everyk)

sym3SQSGV<-kSym(treatmentname="SQSGV", 
               experimentname=experimentname, k=k, trials=trials, 
               grammarfn=grammarfn, algorithm="sgede",
               popsize=popsize, generations=generations,
               scalefactor="Uniform", selection="UniformP", 
               mateselection="UniformP", replication="DE", 
               crossover="UCrossGene", mutation="MutateGeneDE", 
               accept="Best", 
               executionModel="Sequential", verbose=verbose, 
               semantics="byValue", 
               reportEvalErrors=reportEvalErrors, replay=replay, batch=batch,
               Gpath=kSymmetryGrammarPath, outpath=outpath, everyk=everyk)

sym3MCSGV<-kSym(treatmentname="MCSGV", 
               experimentname=experimentname, k=k, trials=trials, 
               grammarfn=grammarfn, algorithm="sgede",
               popsize=popsize, generations=generations,
               scalefactor="Uniform", selection="UniformP", 
               mateselection="UniformP", replication="DE", 
               crossover="UCrossGene", mutation="MutateGeneDE", 
               accept="Best", 
               executionModel="MultiCore", verbose=verbose, 
               semantics="byValue", 
               reportEvalErrors=reportEvalErrors, replay=replay, batch=batch,
               Gpath=kSymmetryGrammarPath, outpath=outpath, everyk=everyk)
