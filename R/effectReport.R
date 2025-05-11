
#' Report the effects of two different treatments-
#'
#' @param name            Name of Experiment.
#' @param treatmentname1  Name of rds-file of treatment 1 (data vector x).
#' @param treatmentname2  Name of rds-file of treatment 2 (data vector y).
#' @param variable        Name of variable tested.
#' @param test            Name of test. Default: "wilcox.test".
#'                        Available: "wilcox.test" or "t.test". 
#'                        If both data vectors are normal, 
#'                        "t.test" is used.
#' @param mu              Mean of difference of variables of treatments.
#'                        Default: 0.
#' @param alternative     Alternative hypothesis. Default: "two.sided"
#'                        \itemize{
#'                        \item "two.sided": \code{0==(mean(x) - mean(y))}.
#'                        \item "less": \code{(mean(x) - mean(y))} smaller 0.
#'                        \item "greate": \code{(mean(x) - mean(y))} greater 0.
#'                        }
#' @param alpha           Level of significance. Default: 0.05.
#' @param coef            Scaling of window for removing outliers.
#'                        Default: 1.5 (as in boxplot.stats). 
#'                        0 means all data are retained.
#'                        The windox size is given by the length of the box
#'                        plus/minus coef times the length of the box.
#' @param silent          If FALSE, supress some output.
#' @param inpath          Path to treatment rds-files of experiment.
#'                          Default: ".".
#' @param outpath          Path to report of experiment. Default: "." 
#' @param type  Output to console (type="console") 
#'              or to latex files (type="latex")?  
#'              Default: "console".
#' 
#' @return 0 (invisible).
#'
#' @family Report of Experiment
#'
#' @examples
#' cat("TBD\n")
#'
#' @export
experimentEffectReport<-function(
      name="", treatmentname1="", treatmentname2="", variable="",
      test="wilcox.test",
      mu=0, alternative="two.sided", alpha=0.05, coef=1.5,
      silent=TRUE, inpath=".", outpath=".", type="console") 
{
isNormal1<-experimentIsNormal(treatmentname=treatmentname1,
                             name=name, variable=variable,
                             alpha=alpha, coef=coef,
        silent=silent, inpath=inpath, outpath=outpath, type=type)

isNormal2<-experimentIsNormal(treatmentname=treatmentname2,
                             name=name, variable=variable,
                             alpha=alpha, coef=coef,
        silent=silent, inpath=inpath, outpath=outpath, type=type)

isNormal<-(isNormal1 && isNormal2)

if (isNormal) 
   {newtest<-"t.test"} else
   {newtest<-"wilcox.test"}

if (!silent)
{
if (isNormal)  
{iText<-paste0("Because both distributions of the variable ", 
                variable, " \n",
              "are normal, a t-test is used for comparing the means \n", 
              "of the variable ", variable, " of the treatments.\n")}
else
{iText<-paste0("Because  of the non-normality of variable ",
                variable, " \n",
              "a two sample Wilcoxon test is used for comparing the means \n", 
              "of the variable ", variable, " of the treatments.\n")}
if (type=="console")
{    cat(iText) }
if (type=="latex") {
  fn<-newFileName(fn="main", ftype="tex", expname=name, path=outpath)
  writeText(iText, fn) }
if (type=="beamer") {
  fn<-newFileName(fn="main", ftype="tex", expname=name, path=outpath)
  iText<-paste("\\begin{frame}\n", iText, "\\end{frame}\n", sep=" ")
  writeText(iText, fn) }
}

experimentHypothesisTest(name=name,
                treatmentname1=treatmentname1,
                treatmentname2=treatmentname2,
                variable=variable,
                test=newtest, 
                mu=mu, alternative=alternative, alpha=alpha, coef=coef,
                inpath=inpath, outpath=outpath, type=type)

invisible(0)
}

