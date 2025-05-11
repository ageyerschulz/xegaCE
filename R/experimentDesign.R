
# Experiment Design
# (c) 2025 Andreas Geyer-Schulz

#' Convert a named parameter list into a dataframe.
#'
#' @description Three parameters need special handling:
#'  \enumerate{
#'     \item \code{$tRNG}: Convert to one string.
#'     \item \code{$penv}: Replace penv by name of penv.
#'     \item \code{$grammar}: Replace grammar by its name.
#'     }
#'
#' @param pl    Parameter list.
#'
#' @return A dataframe with one column: The parameter values.
#'         The rownames are the (raw) parameter names.
#'
#' @family Design of Experiment
#'
#' @examples
#' cat("TBD!\n")
#'
#' @export
pl2df<-function(pl)
{npl<-pl
n<-names(pl)
if ("tRNG" %in% names(pl)) {npl$tRNG<-Reduce(paste, pl$tRNG)}
if ("penv" %in% names(pl)) {npl$penv<-pl$penv$name()}
if ("grammar" %in% names(pl)) {npl$grammar<-pl$grammar$name}
nulls<-lapply(npl, FUN=is.null)
npl[unlist(nulls)]<-rep("NULL", sum(unlist(nulls)))
df<-as.data.frame(npl)
df<-t(df)
colnames(df)<-c("Parameter Values")
return(df)}

#' Return a dataframe with all parameters of a treatment.
#'
#' @param treatmentdf  A dataframe with the results of a treatment.
#'
#' @return A dataframe with all parameters of a treatment.
#' 
#' @family Design of Experiment
#'
#' @examples
#' cat("TBD!\n")
#'
#' @export
designOfTreatment<-function(treatmentdf)
{
GAenv<-treatmentdf$lastGAResult$GAenv
r<-list()
r$expname<-treatmentdf$tArgs$experimentName
r$treatmentname<-treatmentdf$tArgs$treatmentName
r$penvName<-GAenv$penv$name()
r$optimize<-c("Minimize!", "Maximize")[1+as.integer(GAenv$max)]
r$n<-nrow(treatmentdf$resultDF)
r$algorithm<-GAenv$algorithm
r$maxdepth<-GAenv$maxdepth
if (is.null(GAenv$grammar))
       {r$grammarName<-"NULL"} 
     else
       {g<-unlist(strsplit(GAenv$grammar$name, "/"))
        r$grammarName<-g[[length(g)]]}
r$replay<-GAenv$replay
r$evalmethod<-GAenv$evalmethod
r$executionModel<-GAenv$executionModel
r$verbose<-GAenv$verbose
r$semantics<-GAenv$semantics
r$reportEvalErrors<-GAenv$reportEvalErrors
r$terminationCondition<-GAenv$terminationCondition
r$terminationEps<-GAenv$terminationEps
r$worstFitness<-GAenv$worstFitness
r$genemap<-GAenv$genemap
r$initgene<-GAenv$initgene
r$codons<-GAenv$codons
r$codonPrecision<-GAenv$codonPrecision
r$popsize<-GAenv$popsize
r$generations<-format(GAenv$generations, scientific=FALSE)
r$crossrate<-GAenv$crossrate
r$mutrate<-GAenv$mutrate
r$ivcrossrate<-GAenv$ivcrossrate
r$crossrate2<-GAenv$crossrate2
r$ivmutrate<-GAenv$ivmutrate
r$mutrate2<-GAenv$mutrate2
DF<-data.frame(r)
x<-c("Experiment", "Treatment", "Problem Environment",
"Optimize", "Trials", "Algorithm", "Max Depth of DTs",
"Grammar", "Replay", "Evaluation Method", "Execution Model", 
"Verbose", "Semantics",
"Report Eval Errors", "Termination Condition", "Termination Eps",
"Worst Fitness",
"Gene Map", "Init Gene", "Codons", "Codon Precision",
"Population Size", "Max Generations",
"Crossover Rate",
"Mutation Rate",
"IV Crossover Rate",
"Crossover Rate 2",
"IV Mutation Rate",
"Mutation Rate 2")
colnames(DF)<-x
return(DF) }

#' Generate a dataframe with all parameters of all treatments of an experiment.
#' 
#' @param treatments List of treatment names of experiments.
#' @param inpath   Path for treatment files. Default: ".".
#' 
#' @return A dataframe with all parameters of all treatments.
#'
#' @family Design of Experiment
#'
#' @examples
#' cat("TBD!\n")
#'
#' @export
designOfAllTreatments<-function(treatments, inpath=".")
{ full<-fullFileNames(treatments=treatments, inpath=inpath)
  OL<-lapply(full, FUN=readRDS)
  DFL<-lapply(OL, FUN=designOfTreatment)
  DF<-data.frame()
  for (i in (1:length(DFL))) {  DF<-rbind(DF, DFL[[i]]) }
  return(DF) }

#' Split dataframe in common and different parameters.
#'
#' @param DF Dataframe with all parameters of all treatments of an experiment. 
#'
#' @return A list with two dataframes
#'         \itemize{
#'         \item \code{$common} Dataframe of parameters common to all treatments of an experiment.
#'         \item \code{$different} Dataframe of parameters different in the treatments of an experiment.
#'         } 
#'
#' @family Design of Experiment
#'
#' @examples
#' cat("TBD!\n")
#'
#' @export
designSplitParameter<-function(DF)
{ x<-t(DF)
x1<-apply(x, 1, unique)
i<-(1==lapply(x1, FUN=length))
common<-data.frame(x1[i])
different<-data.frame(x[(!i),])
CandDPOE<-list(common=common, different=different)
return(CandDPOE) }

#' Design of Experiment.
#'
#' @param treatments List of treatment names of experiments.
#' @param inpath   Path for treatment files. Default: ".".
#'
#' @return A list with two dataframes
#'         \itemize{
#'         \item \code{$common} Dataframe of parameters common to all treatments of an experiment.
#'         \item \code{$different} Dataframe of parameters different in the treatments of an experiment.
#'         } 
#'
#' @family Design of Experiment
#'
#' @examples
#' cat("TBD!\n")
#'
#' @export
designOfExperiment<-function(treatments, inpath=".")
{ designSplitParameter(designOfAllTreatments(treatments=treatments, inpath=inpath))}

#' Get treatment.
#'
#' @param treatmentname  Name of the rds-file of a treatment.
#' @param inpath     Path for treatment files. Default: ".".
#'
#' @return A treatment.
#'
#' @family Design of Experiment
#'
#' @examples
#' cat("TBD!\n")
#'
#' @export
getTreatment<-function(treatmentname="", inpath=".")
{fullfn<-paste0(inpath, .Platform$file.sep, treatmentname)
 tmp<-readRDS(fullfn)
 return(tmp)}

