% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/experimentElements.R
\name{experimentTreatments}
\alias{experimentTreatments}
\title{Get list of files with treatment results.}
\usage{
experimentTreatments(pattern = "*\\\\.rds", inpath = ".")
}
\arguments{
\item{pattern}{A regular expression for selecting filenames of treatments.
Assumption: One rds file per treatment.
Default: "*\\.rds".}

\item{inpath}{Path for treatment files. Default: ".".}
}
\value{
List of filenames with treatments.
}
\description{
Get list of files with treatment results.
}
\examples{
experimentTreatments()

}
\seealso{
Other Report of Experiment: 
\code{\link{experimentAllXegaParameters}()},
\code{\link{experimentAnalysisOfTreatment}()},
\code{\link{experimentAppendix}()},
\code{\link{experimentBoxPlot}()},
\code{\link{experimentDesign}()},
\code{\link{experimentEffectReport}()},
\code{\link{experimentEnd}()},
\code{\link{experimentGrammarTable}()},
\code{\link{experimentHypothesisTest}()},
\code{\link{experimentIsNormal}()},
\code{\link{experimentMeanMatrix}()},
\code{\link{experimentNNTables}()},
\code{\link{experimentPlotPopStats}()},
\code{\link{experimentSection}()},
\code{\link{experimentSolutionGenotype}()},
\code{\link{experimentSolutionTable}()},
\code{\link{experimentStart}()},
\code{\link{experimentStatistic}()},
\code{\link{experimentText}()},
\code{\link{experimentTreatmentParameters}()},
\code{\link{experimentXegaParameters}()}
}
\concept{Report of Experiment}
