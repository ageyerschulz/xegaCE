
# experimentStatistics.
# (c) 2025 Andreas Geyer-Schulz

#' Summarize a performance variable of one treatment of an experiment.
#'
#' @param name       Name of treatment.
#' @param variable   Name of variable.
#' @param column     Vector of values of variable.  
#' 
#' @return A dataframe with the following columns:
#'         \enumerate{
#'         \item \code{$Treatment}: Name of treatment.
#'         \item \code{$Trials}:    Number of trials.
#'         \item \code{$Variable}:  Name of Variable.
#'         \item \code{$min}:       Minimum.
#'         \item \code{$mean}:      Mean.
#'         \item \code{$sd}:        Standard deviation. 
#'         \item \code{$max}:       Minimum.
#'                   }
#'
#' @family Descriptive Statistics (Internal)
#'
#' @examples
#' summaryVariable("MC-GP", "Seconds", sample(100, 10))
#'
#' @importFrom stats sd
#' @export
summaryVariable<-function(name, variable, column)
   { r<-list(name=name, trials=length(column), variable=variable,
     min=min(column), mean=mean(column), sd=stats::sd(column), max=max(column))
     df<-data.frame(r)
     colnames(df)<-c("Treatment", "Trials", "Variable",
                     "min", "mean", "sd", "max")
     return(df)}

#' Dataframe of descriptive statistics of all variables of a treatment.
#'
#' @param name       Name of treatment.
#' @param df         Experimental results of treatment.
#'                   Each record is the result of one trial. 
#' 
#' @return A dataframe with the following columns:
#'         \enumerate{
#'         \item \code{$Treatment}: Name of treatment.
#'         \item \code{$Trials}:    Number of trials.
#'         \item \code{$Variable}:  Name of Variable.
#'         \item \code{$min}:       Minimum.
#'         \item \code{$mean}:      Mean.
#'         \item \code{$sd}:        Standard deviation. 
#'         \item \code{$max}:       Minimum.
#'                   }
#'
#' @family Descriptive Statistics (Internal)
#'

#' @examples
#' df<-data.frame(Fitness=sample(100, 10), Seconds=sample(50, 10), 
#'                Generations=sample(30, 10), Evaluations=sample(3000, 10)) 
#' summaryTreatment("MC-GP", df)
#'
#' @importFrom stats sd
#' @export
summaryTreatment<-function(name, df)
{sdf<-data.frame()
sdf<-rbind(sdf, summaryVariable(name, "Fitness", df$Fit))
sdf<-rbind(sdf, summaryVariable(name, "Seconds", df$Seconds))
sdf<-rbind(sdf, summaryVariable(name, "Generations", df$Generations))
sdf<-rbind(sdf, summaryVariable(name, "Evaluations", df$Evaluations))
return(sdf) }


#' Dataframe of descriptive statistics for experiment.
#'
#' @param treatments      List of treatment names of experiment.
#' @param byVars          Boolean. If TRUE, Sort by variables. 
#'                        Default: TRUE.
#' @param inpath          Path to treatement rds-files of experiment.
#'                        Default: ".".
#' 
#' @return A dataframe with the following columns:
#'         \enumerate{
#'         \item \code{$Treatment}: Name of treatment.
#'         \item \code{$Trials}:    Number of trials.
#'         \item \code{$Variable}:  Name of Variable.
#'         \item \code{$min}:       Minimum.
#'         \item \code{$mean}:      Mean.
#'         \item \code{$sd}:        Standard deviation. 
#'         \item \code{$max}:       Minimum.
#'                   }
#'
#' @family Statistics of Experiment
#'
#' 
#' @examples
#' cat("TBD! \n")
#'
#' @export
summaryExperiment<-function(treatments, byVars=TRUE, inpath=".")
{
full<-fullFileNames(treatments=treatments, inpath=inpath)
OL<-lapply(full, FUN=readRDS)
df<-data.frame()
for (i in (1:length(OL))) {
r<-summaryTreatment(name=OL[[i]]$tArgs$treatmentName, df=OL[[i]]$resultDF)
df<-rbind(df,r)}
if (!byVars) {return(df)}
vars<-df$Variable
s<-sort(vars, index.return=TRUE)
df<-df[s$ix,]
return(df)}


#' Select a subset of the descriptive statistics of an experiment.
#'
#' @description Specify either treatment name or variable name or 
#'              none. Returns the appropriate subset of records of 
#'              the dataframe of all descriptive statistics of the 
#'              experiment.
#'
#' @param treatments      List of treatment names of experiment.
#' @param treatment       Name of treatment. Default: NULL.
#' @param variable        Name of variable.  Defautl: NULL.
#' @param inpath          Path to treatement rds-files of experiment.
#'                        Default: ".".
#' 
#' @return A dataframe with the following columns:
#'         \enumerate{
#'         \item \code{$Treatment}: Name of treatment.
#'         \item \code{$Trials}:    Number of trials.
#'         \item \code{$Variable}:  Name of Variable.
#'         \item \code{$min}:       Minimum.
#'         \item \code{$mean}:      Mean.
#'         \item \code{$sd}:        Standard deviation. 
#'         \item \code{$max}:       Minimum.
#'                   }
#'
#' @family Statistics of Experiment
#'
#' 
#' @examples
#' cat("TBD! \n")
#'
#' @export
selectFromExperiment<-function(treatments, 
      treatment=NULL, variable=NULL, inpath=".")
{ df<-summaryExperiment(treatments, inpath=inpath)
  if ((is.null(variable)) & (!is.null(treatment)))
  {df<-df[df$Treatment %in% treatment,]}
  if ((is.null(treatment)) & (!is.null(variable)))
  {df<-df[df$Variable %in% variable,]}
return(df)}

