
#' Analysis of a treatment.
#'
#' @param name   Name of Experiment.
#' @param treatmentname Name of rds-file of treatment
#' @param inpath          Path to treatment rds-files of experiment.
#'                          Default: ".".
#' @param outpath          Path to report of experiment. Default: "." 
#' @param type  Output to console (type="console") 
#'              or to latex files (type="latex")?  
#'              Default: "console".
#' @param miniframe  Boolean. Default: TRUE.
#'              If false, turn off miniframes.
#'
#' @return 0 (invisible). 
#'
#' @family Report of Experiment
#' 
#' @examples 
#' cat("TBD! \n")
#'
#' @export
experimentAnalysisOfTreatment<-function(
      name="", treatmentname="", miniframe=TRUE,   
      inpath=".", outpath=".", type="console")
{
tmp<-unlist(strsplit(treatmentname, "\\."))
treatmentName<-gsub("merge", "", tmp[1])
experimentSection(name=name, 
                  secname=paste0("Treatment ",treatmentName),
                  level="subsection", miniframe=miniframe, 
                  clearpage=TRUE, type=type, outpath=outpath)
t<-experimentTreatments(inpath=inpath)

experimentTreatmentParameters(treatmentName=treatmentname, inpath=inpath, 
                              name=name, outpath=outpath, type=type)
experimentXegaParameters(treatmentName=treatmentname, inpath=inpath, 
                              name=name, outpath=outpath, type=type)

experimentGrammarTable(treatmentname=treatmentname, 
                       name=name, common=FALSE,
                       inpath=inpath, outpath=outpath, type=type)

experimentStatistic(treatments=t, treatment=treatmentName,
                 caption=paste0("Treatment: ", treatmentName),
                 name=name,
                 inpath=inpath, outpath=outpath, type=type)

experimentSolutionTable(treatmentname=treatmentname, 
                        name=name, 
                        inpath=inpath, outpath=outpath, type=type, n=6)

experimentNNTables(treatmentname=treatmentname, 
                        name=name, 
                        inpath=inpath, outpath=outpath, type=type, n=6)

experimentSolutionGenotype(treatmentname=treatmentname, 
                       name=name, 
                       inpath=inpath, outpath=outpath, type=type)

experimentPlotPopStats(treatmentname=treatmentname, 
                       name=name, 
                       inpath=inpath, outpath=outpath, type=type)

invisible(0)
}

