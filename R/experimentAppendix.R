

#' Appendix of an experimental report.
#'
#' @description   Three parts:
#'   \enumerate{
#'   \item Summary of descriptive statistics of the distribution 
#'         of the performance
#'         variables of all treatments for n trials.
#'   \item Analysis of each treatment. 
#'   \item All xega parameters (and their setting for one treatment).
#'   }
#'
#' @param treatments      List of treatment file names.
#' @param name            Name of experiment. Default: "".
#' @param miniframe       Boolean. Default: TRUE.
#'                        If false, turn off miniframes in treatment subsection.
#' @param inpath          Path to treatment rds-files of experiment.
#'                          Default: ".".
#' @param outpath          Path to report of experiment. Default: "." 
#' @param type  Output to console (type="console") 
#'              or to latex files (type="latex" | "beamer")?  
#'              Default: "console".
#' 
#' @return 0 (invisible).
#'
#' @family Report of Experiment
#'
#' @examples
#' cat("TBD!\n")
#'
#' @export
experimentAppendix<-function(treatments,
                   name="",
                   miniframe=TRUE,
                   inpath=".",
                   outpath=",",
                   type="console"
                   )
{
experimentSection(name=name,
                  secname="A Summary",
                  level="section",
                  clearpage=TRUE, type=type, outpath=outpath)

experimentStatistic(treatments=treatments,
       caption=paste0("Summary of statistics of experiment ", name,"."),
                 name=name,
                 inpath=inpath, outpath=outpath, type=type)

experimentSection(name=name,
                  secname="B Treatments",
                  level="section",
                  clearpage=TRUE, miniframe=miniframe, 
                  type=type, outpath=outpath)

for (i in (1:length(treatments)))
{ experimentAnalysisOfTreatment(name=name, miniframe=miniframe,
     treatmentname=treatments[i],
     inpath=inpath, outpath=outpath, type=type) }

experimentSection(name=name,
                  secname="C xega",
                  level="section",
                  clearpage=TRUE, type=type, outpath=outpath)

experimentAllXegaParameters(treatmentName=treatments[1],
     name=name,
     inpath=inpath, outpath=outpath, type=type)

invisible(0)
}
