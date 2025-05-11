# k-Symmetry Wrapper
# (c) 2025 Andreas Geyer-Schulz
#

#' The path to the grammars of the experiment.
#'
#' @description For convenience. Should not be on cran.
#'
#'@export
kSymmetryGrammarPath="/home/dj2333/dev/cran/xegaCE/BNF"

#' booleanGrammarK 
#'
#' @family xegaBNF
#'
#' @importFrom xegaBNF booleanGrammarK
#' @export
booleanGrammarK<-xegaBNF::booleanGrammarK

#' Generates a list of alternative variable names.
#'
#' @param varSym Variable symbol. E.g. "D"-
#' @param k      Number of bits. 
#' 
#' @return Text.  Contains the LHS of a production rule with 
#'           a list of k variable names in EBNF.     
#'
#' @family xegaBNF
#'
#' @importFrom xegaBNF booleanGrammarK
#' @export
variableNamesLHS<-xegaBNF::variableNamesLHS

#' Write a grammar file.
#'
#' @param g     A grammar object.
#' @param fn    A filename.
#' @param eol   End of line symbol. 
#'
#' @family xegaBNF
#'
#' @importFrom xegaBNF writeBNF
#' @export
writeBNF<-xegaBNF::writeBNF

#' Wrapper for xegaRun for k-symmetry experiments.
#'
#' @description Tentative. Restricts the parameter space of xegaRun. 
#'              Not yet flexible at all.
#'
#' @details Repeats xegaRun several times. 
#'         \itemize{
#'          \item
#'          As a side effect writes files with the experimental data
#'          into the directory \code{outpath} which must exist.
#'          \item
#'          The treatmentname may not contain numbers, because
#'          the files generated \code{everyk} trials have serial 
#'          numbers with 3 decimal places at the end. 
#'          }
#'
#' @param expReplay      Integer. If \code{expReplay>0}, 
#'                       then use \code{expReplay} 
#'                       as the seed of the random number generator and  
#'                       store it for the exact repetition of this experiment.
#'                       Default: 0.
#' @param treatmentname  Default: "t0".
#' @param experimentname Default: "e0". 
#' @param replay         Default: 0. 
#' @param k              Defautl: 2.
#' @param trials         Default: 1. 
#' @param grammarfn      Grammar file name. 
#' @param algorithm      Default: "sgp".
#' @param popsize        Default: 200. 
#' @param generations    Default: 500000.
#' @param initgene       Default: "InitGene".
#' @param selection      Default: "SUS".
#' @param mateselection  Default: "SUS".
#' @param replication    Default: "Kid2".
#' @param crossover      Default: "Cross2Gene".
#' @param mutation       Default: "MutateGene".
#' @param accept         Default: "All".
#' @param reportEvalErrors, Default: "FALSE".
#' @param scalefactor    Default: "Uniform".
#' @param crossrate      Default: 0.2.
#' @param mutrate        Default: 0.4. 
#' @param ivmutrate      Default: "Const". 
#' @param mutrate2       Default: 0.4.
#' @param ivcrossrate    Default: "Const". 
#' @param crossrate2     Default: 0.2.
#' @param executionModel Default: "Sequential",
#' @param verbose        Default: 0. 
#' @param tVerbose       Default: 1. 
#' @param Gpath          Default: kSymmetryGrammarPath.
#' @param outpath          Default: ".".
#' @param semantics      Default: "byValue".
#' @param batch          Default: FALSE
#' @param everyk         Integer. Save results after every k trials. 
#'                       Default:10.
#' 
#' @return A named list with the following elements
#'   \itemize{
#'   \item \code{$lastGAResult}: The return object of \code{xegaRun()}.
#'   \item \code{$resultDF}: A dataframe with the following columns:
#'   \itemize{
#'     \item \code{$Fit}: The best fitness value of a trial.
#'     \item \code{$Solution}: The best solution (the phenotype of the gene).
#'     \item \code{$Seconds}:  Execution time of the trial in seconds. 
#'     \item \code{$Generations}: The number of generations until termination.
#'     \item \code{$Evaluations}: The number of fitness evaluations.   
#'     }
#'     \item \code{$tArgs}:    The arguments for the treatment.
#'     \item \code{$xegaArgs}: The arguments passed to xega.
#'     \item \code{$filename}: The name of the last file written.
#'     }
#'
#' @family Experiment
#' 
#' @examples
#' tmpPath<-tempdir()
#' gfn<-newFileName(fn="BooleanK", ftype="txt", path=tmpPath)
#' g<-booleanGrammarK()
#' writeBNF(g, gfn)
#' r<-kSym(treatmentname="t0", experimentname="e0", k=2, trials=1, 
#'       grammarfn=gfn, algorithm="sgp", popsize=20, 
#'       generations= 20, crossrate=0.2, mutrate=0.4, 
#'       ivmutrate="Const", mutrate2=0.4,
#'       ivcrossrate="Const", crossrate2=0.2,
#'       executionModel="Sequential", verbose=1, 
#'       Gpath="", outpath=tmpPath, semantics="byValue")
#' print(r$resultDF)
#' 
#' @importFrom xegaBNF newBNF
#' @importFrom xegaBNF preBNF
#' @importFrom xegaBNF compileBNF
#' @importFrom xega    xegaRun
#' @export
kSym<-function(
               expReplay=0, 
               treatmentname="t0", 
               experimentname="e0",
               replay=0, k=2, trials=1, tVerbose=1,
               grammarfn="AndOrNot.txt", algorithm="sgp", popsize=200, 
               generations= 500000, initgene="InitGene", 
               selection="SUS", mateselection="SUS",
               replication="Kid2", 
               crossover="Cross2Gene", mutation="MutateGene",
               accept="All",
               reportEvalErrors="FALSE",
               crossrate=0.2, mutrate=0.4, 
               ivmutrate="Const", mutrate2=0.8, 
               ivcrossrate="Const", crossrate2=0.4,
               scalefactor="Uniform",
               executionModel="Sequential", verbose=0, 
               Gpath=kSymmetryGrammarPath, outpath=".",
               semantics="byValue", batch=FALSE, everyk=10)
{
RGused<-RNGkind("L'Ecuyer-CMRG")
if (expReplay>0) {set.seed(expReplay)} else {set.seed(NULL)}
RGseed<-expReplay  
if (!(verbose==0)) {cat("Treatment:", treatmentname, "\n")}
if (Gpath=="") {newGpath<-Gpath} else
    {newGpath<-paste0(Gpath, .Platform$file.sep)}
fgrammarfn<-paste0(newGpath, grammarfn)
gl<-xegaBNF::compileBNF(xegaBNF::preBNF(xegaBNF::newBNF(fgrammarfn), list(k=k)))
penvl<-newEnvKsymmetry(k=k)
result<-data.frame()
genemap="Bin2Dec"
reportEvalErrors=TRUE
if (algorithm=="sge") 
   {genemap<-"Mod"; initgene<-"InitGene"}
if (algorithm=="sgede") 
   {genemap<-"Identity"; initgene<-"InitGene"}

sav<-treatmentRun(
           experimentName=experimentname,
           treatmentName=treatmentname,
           tReplay=expReplay,
           trials=trials,
           everyK=everyk,
           outpath=outpath,
           tVerbose=tVerbose,
### To xegaRun:
           penv=penvl, 
           grammar=gl, 
           replay=replay,
           algorithm=algorithm, 
           maxdepth=7,
           max=FALSE, 
           worstFitness=-2^k,
           popsize=popsize, 
           generations=generations, 
           crossrate=crossrate, 
           mutrate=mutrate, 
           ivmutrate=ivmutrate, 
           mutrate2=mutrate2,
           ivcrossrate=ivcrossrate, 
           crossrate2=crossrate2,
           scalefactor=scalefactor,
           genemap=genemap, 
           initgene=initgene, 
           selection=selection, 
           mateselection=mateselection,
           replication=replication, 
           crossover=crossover, 
           mutation=mutation,
           accept=accept,
           reportEvalErrors=reportEvalErrors, 
           codons=40*k, 
           codonPrecision="LCM",
           terminationEps=-0.1, 
           terminationCondition="AbsoluteError",
           evalmethod="Deterministic", 
           executionModel=executionModel, 
           verbose=verbose, 
           batch=batch,
           semantics=semantics)

return(sav)
}


