% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/consoleElements.R
\name{consoleBoxPlot}
\alias{consoleBoxPlot}
\title{Produces box-and-whisker plots of treatments of an experiment.}
\usage{
consoleBoxPlot(
  treatments,
  variable,
  title = "",
  outline = TRUE,
  notch = FALSE,
  horizontal = TRUE,
  varwidth = FALSE,
  name = "",
  inpath = ".",
  epsfilename = NULL
)
}
\arguments{
\item{treatments}{List of filenames of treatments.}

\item{variable}{Name of variable.}

\item{title}{Title of Boxplot. Default: "".}

\item{outline}{Boolean. If FALSE, remove outliers. Default: TRUE.}

\item{notch}{Boolean. If TRUE, notches are drawn. Default: FALSE.
Nonoverlapping notches indicate different 
medians of treatments.}

\item{horizontal}{Boolean. Default: TRUE. Plots are horizontal.}

\item{varwidth}{Boolean. Default: FALSE.
If TRUE,  width of the plot indicates sample size.}

\item{name}{Name of experiment. Default: "".}

\item{inpath}{Path to treatment files of experiment.
Default: ".".}

\item{epsfilename}{File name of epsfile. Default: NULL.}
}
\value{
0 (invisible).
}
\description{
Produces box-and-whisker plots of treatments of an experiment.
}
\examples{
cat("Example: TODO.\n")

}
\seealso{
Other Console Elements: 
\code{\link{beamerText}()},
\code{\link{consoleEndDocument}()},
\code{\link{consoleFigure}()},
\code{\link{consoleProlog}()},
\code{\link{consoleSection}()},
\code{\link{consoleTable}()},
\code{\link{consoleText}()},
\code{\link{pause}()}
}
\concept{Console Elements}
