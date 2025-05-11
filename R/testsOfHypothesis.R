#' Removes outliers of data vector.
#'
#' @description An outlier is a datapoint outside \code{coef} times
#'              of the box size. See \code{boxplot.stats}.
#' 
#' @param  x      A numeric vector. 
#' @param  coef   Scaling of threshold for outliers. Default: 1.5.
#'
#' @return A numeric vector.
#' 
#' @family Transformation
#'
#' @examples
#' x<-rnorm(30, mean=0, sd=2)
#' rmOutliers(x, coef=0)
#' rmOutliers(x, coef=0.5)
#' rmOutliers(x, coef=1.0)
#' rmOutliers(x)
#'
#' @importFrom grDevices boxplot.stats
#' @export
rmOutliers<-function(x, coef=1.5) 
{y<-grDevices::boxplot.stats(x, coef=coef)
 return(x[!(x %in% y$out)])}

#' Analysis of a treatment.
#'
#' @param name            Name of Experiment.
#' @param treatmentname1  Name of rds-file of treatment 1 (data vector x).
#' @param treatmentname2  Name of rds-file of treatment 2 (data vector y).
#' @param variable        Name of variable tested.
#' @param test            Name of test. Default: "wilcox.test".
#'                        Available: "wilcox.test" or "t.test". 
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
#' @param inpath          Path to treatment rds-files of experiment.
#'                          Default: ".".
#' @param outpath          Path to report of experiment. Default: "." 
#' @param type  Output to console (type="console") 
#'              or to latex files (type="latex")?  
#'              Default: "console".
#' 
#' @return Boolean (invisible). TRUE: H0: Accepted. 
#'                              FALSE: H0: Rejected.
#'
#' @family Report of Experiment
#'
#' @examples
#' cat("TBD\n")
#'
#' @importFrom stats wilcox.test
#' @importFrom stats t.test
#' @export
experimentHypothesisTest<-function(
      name="", treatmentname1="", treatmentname2="", variable="", 
      test="wilcox.test",
      mu=0, alternative="two.sided", alpha=0.05, coef=1.5, 
      inpath=".", outpath=".", type="console")
{tformat<-function(x) {format(round(x,5), digits=3, scientific=FALSE)}
if (test=="wilcox.test") {testfun<-wilcox.test}
if (test=="t.test") {testfun<-t.test}
# Treatment 1
  obj<-getTreatment(treatmentname=treatmentname1, inpath=inpath)
  tmp<-unlist(strsplit(treatmentname1, "\\."))
  treatmentName1<-gsub("merge", "", tmp[1])
  z<-obj$resultDF[, variable]
  x1<-rmOutliers(z, coef=coef)
if (coef==0) 
{out1Text<-paste0("\n Outliers of treatment ", treatmentName1, 
                  "  are not removed (coef=0).\n")}
       else 
{out1Text<-paste0("\n ", (length(z)-length(x1)),
                       " outliers of treatment ", treatmentName1, 
                       " are removed (coef=",coef,").\n")}
# Treatment 2
  obj<-getTreatment(treatmentname=treatmentname2, inpath=inpath)
  tmp<-unlist(strsplit(treatmentname2, "\\."))
  treatmentName2<-gsub("merge", "", tmp[1])
  z<-obj$resultDF[, variable]
  x2<-rmOutliers(z, coef=coef)

if (coef==0) 
{out2Text<-paste0("\n Outliers of treatment ", treatmentName2, 
                  "  are not removed (coef=0).\n")}
       else 
{out2Text<-paste0("\n ", (length(z)-length(x2)),
                       " outliers of treatment ", treatmentName2, 
                       " are removed (coef=",coef,").\n")}

# Setup done.
  testresult<-testfun(x1, x2, alternative=alternative)
  acceptH0<-testresult$p.value>alpha
# H0: (mean1 - mean2) == 0
if (alternative=="two.sided") 
{H0<-paste0("{\\bf Hypothesis 0}: mean(", variable," of ", treatmentName1, 
    ")=", tformat(mean(x1)), " - mean(", variable, " of ",  treatmentName2, 
    ")=", tformat(mean(x2)), " is equal to ", mu, ".\n")
 H0name<-paste0("$H_{0}$: Means of treatments ", treatmentName1, 
                " and ", treatmentName2, " of variable ", variable, 
                 " are equal.")
H1<-paste0("{\\bf Hypothesis 1}: mean(", variable," of ", treatmentName1, 
    ")=", tformat(mean(x1)), " - mean(", variable, " of ",  treatmentName2, 
    ")=", tformat(mean(x2)), " is not equal to ", mu, ".\n") 
}
# H0: (mean1 -mean2) > 0
if (alternative=="less") 
{H0<-paste0("{\\bf Hypothesis 0}: mean(", variable," of ", treatmentName1, 
    ")=", tformat(mean(x1)), " - mean(", variable, " of ",  treatmentName2, 
    ")=", tformat(mean(x2)), " is equal or greater than ", mu, ".\n")
 H0name<-paste0("$H_{0}$: Mean of treatment ", treatmentName2, 
                " is less than mean of ", treatmentName1, 
                " of variable ", variable) 
H1<-paste0("{\\bf Hypothesis 1}: mean(", variable," of ", treatmentName1, 
    ")=", tformat(mean(x1)), " - mean(", variable, " of ",  treatmentName2, 
    ")=", tformat(mean(x2)), " is less than ", mu, ".\n") }
# H0: (mean1 -mean2) < 0
if (alternative=="greater") 
{H0<-paste0("{\\bf Hypothesis 0}: mean(", variable," of ", treatmentName1, 
    ")=", tformat(mean(x1)), " - mean(", variable, " of ",  treatmentName2, 
    ")=", tformat(mean(x2)), " is equal or less than ", mu, ".\n")
 H0name<-paste0("$H_{0}$: Mean of treatment ", treatmentName2, 
                " is greater than mean of ", treatmentName1, 
                " of variable ", variable) 
H1<-paste0("{\\bf Hypothesis 1}: mean(", variable," of ", treatmentName1, 
    ")=", tformat(mean(x1)), " - mean(", variable, " of ",  treatmentName2, 
    ")=", tformat(mean(x2)), " is greater than ", mu, ".\n") }
iText<-paste0("For variable ", variable,
               " of treatments ", treatmentName1, " and ", treatmentName2,
               " of experiment ", name, ":\n\n\\vspace{1mm}\n",
               H0, "\n\n \\begin{center} is tested at a significance level ", alpha, 
               " against: \\end{center}\n\n", H1, "\\vspace{1mm}\n")
# Outlier text.
iText<-paste0(iText, "\\vspace{1mm}\n",  out1Text, out2Text, "\\vspace{1mm}\n")
iText<-paste(iText, "\n The test-statistic W of the",
        testresult$method, "is",
        tformat(testresult$statistic), "with a p-value of",
        tformat(testresult$p.value), ".\n", sep=" ")
if (is.na(acceptH0))  
{ iText<-paste(iText, "\n {\\bf Warning:} The test could not be done! \n", 
               sep=" ")}
else
{
iText<-paste(iText, "Since the p-value", tformat(testresult$p.value), sep=" ")
if (acceptH0==TRUE) {compText<-"above"} else {compText<-"below"}
iText<-paste(iText, "is", compText, "the significance level $\\alpha=",
                 alpha, "$,\n",
                "for variable", variable, 
                "of treatments", treatmentName1, "and", treatmentName2,
                "of experiment", name, 
                "\n", H0, sep=" ")   
if (acceptH0==TRUE) 
         {iText<-paste0(iText, "is {\\bf accepted}.\n\n")} else   
         {iText<-paste0(iText, "is {\\bf rejected}.\n\n")}   
}
if (type=="console")
{    cat(iText) }
if (type=="latex") {
  fn<-newFileName(fn="main", ftype="tex", expname=name, path=outpath)
  writeText(iText, fn) }
if (type=="beamer") {
  fn<-newFileName(fn="main", ftype="tex", expname=name, path=outpath)
 iText<-paste("\\begin{frame}[t]\n",
               "\\frametitle{Test of", H0name, "}\n",
               "\\scriptsize\n",
               iText,
               "\\end{frame}\n", sep=" ")
  writeText(iText, fn) }
invisible(acceptH0)}

