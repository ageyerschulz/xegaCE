# k-Symmetry Wrapper
# (c) 2025 Andreas Geyer-Schulz
#

#' Reporter of and observer treatment performance and solution data.
#'
#' @description Returns a dataframe with the performance data and 
#'              the phenotype of the solution of a single treatment.
#'              If \code{!(verbose==0)} print data on console. 
#'
#' @param experimentName  Name of experiment.
#' @param treatmentName  Name of treatment.
#' @param s     A solution object of \code{xegaRun} (with elements
#'             \code{$popStat}, \code{$fit}, \code{$solution}, 
#'             \code{$evalFail}, \code{$GAconfig}, \code{$GAenv}, and \code{$timer}.
#' @param i     Number of iteration.
#' @param verbose If not \code{0}, output treatment data to console.
#'                Progress report.
#'                Default: 0 - Not output to console.
#'
#' @return A dataframe with a single row and the following columns:
#'         \itemize{
#'         \item \code{$Fit}: The phenotype value of the solution.
#'         \item \code{$Solution}: The solution (its phenotype).
#'         \item \code{$Seconds}: Execution time of \code{xegaRun()} in seconds.
#'         \item \code{$Generations}: Number of generations until termination.
#'         \item \code{$Evaluations}: Number of evaluations until termination.
#'         }
#'
#' @family Wrapper (Internal)
#'
#' @export 
treatmentReporter<-function(experimentName, treatmentName, s, i, verbose=0)
{ if (verbose>1) 
   {cat(experimentName, " ", treatmentName, " ", s$GAenv$penv$name(), ":", 
          s$solution$phenotypeValue, "Iteration:", i, "Date:", date(), "\n") 
    cat("   Solution:", s$solution$phenotype, "\n")
    cat("    Fitness:", s$solution$phenotypeValue, "\n")
    cat("       Time:", s$timer$tMainLoop, "\n")
    cat(" Generation:", (-1+nrow(s$popStat)), "\n")
    cat("Evaluations:",(s$GAenv$popsize*(-1+nrow(s$popStat))), "\n")}

### data frame can not represent complex solutions. 
### TODO!

tmp<-s

if (s$GAenv$algorithm=="sga") 
   {tmp$solution$phenotype<-NA}
if (s$GAenv$algorithm=="sgde") 
   {tmp$solution$phenotype<-NA}
if (s$GAenv$algorithm=="sgperm") 
   {tmp$solution$phenotype<-NA}

return(data.frame(Fit=s$solution$phenotypeValue, 
                  Solution=tmp$solution$phenotype, 
                  Seconds=s$timer$tMainLoop, 
                  Generations=(-1+nrow(s$popStat)), 
                  Evaluations=(s$GAenv$popsize*(-1+nrow(s$popStat)))))
}

#' Reporter of mean treatment performance data (fitness, generations) after k trials.
#'
#' @description If \code{!(verbose==0)} print data on console. 
#'
#' @param experimentName  Name of experiment.
#' @param treatmentName   Name of treatement.
#' @param trials          Number of trails.
#' @param i               Number of iteration.
#' @param resultDF        Data frame of results.     
#' @param verbose         Progress report. 
#'                        If not \code{0}, output performance data to console.
#'                        Default: 0 - Not output to console.
#'
#' @return Invisible zero.
#'
#' @family Wrapper (Internal)
#'
#' @export 
iterationReporter<-function(experimentName, treatmentName, trials, i, resultDF, verbose=0)
{ if (verbose>0) 
{cat("Report of Iteration:", i, " of ", trials,
     "Experiment", experimentName, "Treatment:", treatmentName, "\n",
     "Mean Generations:", mean(resultDF$Generations),
     "Mean Fitness:", mean(resultDF$Fit), "\n")}
return(invisible(0))
}

#' Runs a treatment with xegaRun repeatedly \code{trials} times.
#'
#' @description Tentative. 
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
#' @param experimentName Name of experiment. Default: "e0". 
#' @param treatmentName  Name of treatment.  Default: "t0".
#' @param tReplay        Integer. Seed of Random Number.  Default: 0 (No seeding)
#' @param trials         Default: 1. 
#' @param everyK         Save results of trials after \code{everyK} trials (Default: 10). 
#' @param outpath        Path for treatment results. Deafult: ".". 
#' @param tVerbose        Screen output. Default: 0 (no output).
#'                       \itemize{
#'                       \item \code{tVerbose=0}: No output. 
#'                       \item \code{tVerbose=1}: Mean performance after i trials. 
#'                       \item \code{tVerbose>1}: Mean performance after i trials 
#'                                               and performance of i-th trial.
#'                       }
#' @param ...            Arguments for xegaRun of this treatment. 
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
#'   \item \code{$tArgs}: Arguments of a treatmentRun:
#'       \code{$tRNG},
#'       \code{$tReplay},
#'       \code{$treatmentName},
#'       \code{$trials},
#'       \code{$everyK},
#'       \code{$outPath},
#'       \code{$batchPath},
#'       \code{$tVerbose}.
#'   \item \code{$xegaArgs}: Arguments of \code{xega::xegaRun}.
#'   \item \code{$filename}: The name of the last file written.
#'     }
#'
#' @family Experiment
#' 
#' @examples
#' require(xegaBNF)
#' tmpPath<-tempdir()
#' gBNF<-compileBNF(preBNF(booleanGrammarK, list(k=3)))
#' kSymEnv<-newEnvKsymmetry(k=3)
#' r<-treatmentRun(experimentName="ExpA", treatmentName="t0", tReplay=0, 
#'       trials=2, everyK=1, outpath=tmpPath, tVerbose=1,  
#'       penv=kSymEnv, grammar=gBNF, algorithm="sgp", max=FALSE, popsize=20, 
#'       generations= 5, crossrate=0.2, mutrate=0.4, 
#'       ivmutrate="Const", mutrate2=0.4,
#'       ivcrossrate="Const", crossrate2=0.2,
#'       executionModel="Sequential", verbose=1, 
#'       semantics="byValue")
#' print(r$resultDF)
#' 
#' @importFrom xegaBNF newBNF
#' @importFrom xegaBNF preBNF
#' @importFrom xegaBNF compileBNF
#' @importFrom xega    xegaRun
#' @export
treatmentRun<-function(
               experimentName="e0",
               treatmentName="t0", 
               tReplay=0, 
               trials=1, 
               everyK=10, 
               outpath=".", 
               tVerbose=0, 
               ...
               ) 
{
xegaArgs<-list(...)
if (!is.null(xegaArgs$batch)) 
   { if (xegaArgs$batch==TRUE) 
     { batchPath<-file.path(outpath, treatmentName)
       if (!dir.exists(batchPath)) {dir.create(batchPath)}}
     else
     { batchPath<-"." }  
   } else { batchPath<-"." }  
xegaArgs$path<-batchPath
tRNG<-RNGkind("L'Ecuyer-CMRG")
tRNG<-RNGkind("L'Ecuyer-CMRG")
if (tReplay>0) {set.seed(tReplay)} else {set.seed(NULL)}
tArgs<-list()
tArgs$tRNG<-Reduce(paste, tRNG)
tArgs$tReplay<-tReplay
tArgs$experimentName<-experimentName
tArgs$treatmentName<-treatmentName
tArgs$trials<-trials
tArgs$everyK<-everyK
tArgs$outpath<-outpath
tArgs$batchPath<-batchPath
tArgs$tVerbose<-tVerbose
##### loop
resultColNames<-c("Fit", "Solution", "Seconds", "Generations", "Evaluations")
allresults<-data.frame()
result<-data.frame()
for (i in (1:trials))
{ ### calling xegaRun
e<-do.call(xega::xegaRun, xegaArgs)
lastresult<-treatmentReporter(experimentName, treatmentName, e, i, tVerbose)
result<-rbind(result, lastresult)
allresults<-rbind(allresults, lastresult)

if (tVerbose>0) 
{iterationReporter(experimentName, treatmentName, i, allresults, tVerbose)}

if (0==(i%%everyK)) 
{ names(result)<-resultColNames
newFN<-newFileName(fn=treatmentName, ftype="rds", path=outpath)
sav<-list(lastGAResult=e, resultDF=result, tArgs=tArgs, xegaArgs=xegaArgs, fileName=newFN)
saveRDS(sav, file=newFN)
result<-data.frame() }
}

if (!(ncol(result)==0))
{ names(result)<-resultColNames
newFN<-newFileName(fn=treatmentName, ftype="rds", path=outpath)
sav<-list(lastGAResult=e, resultDF=result, tArgs=tArgs, xegaArgs=xegaArgs, fileName=newFN)
saveRDS(sav, file=newFN) }

names(allresults)<-resultColNames
sav<-list(lastGAResult=e, resultDF=allresults, tArgs=tArgs, xegaArgs=xegaArgs, fileName=newFN)
return(sav)
}

