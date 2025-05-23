% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/beamerElements.R
\name{beamerProlog}
\alias{beamerProlog}
\title{Prolog of minimal latex document for beamer class.}
\usage{
beamerProlog(
  name = "Missing",
  title = "Undefined",
  author = "Unknown",
  purpose = "Unknown",
  beamertheme = "default",
  beamercolor = "electricultramarine"
)
}
\arguments{
\item{name}{Name of experiment. Default: "Missing".}

\item{title}{Title of the experiment. Default: "Undefined".}

\item{author}{Author of document. Default: "Unknown".}

\item{purpose}{Purpose of experiment. A short description. 
Default: "Unknown"}

\item{beamertheme}{Beamer theme. Default: "default".}

\item{beamercolor}{Beamer color. Default: "electricultramarine".}
}
\value{
A text string with LaTex markup.
}
\description{
Contains title, author and purpose of experiment.
}
\examples{
a<-beamerProlog(name="Experiment A.", 
               title="First test", author="U.N. Owen",
               purpose="To show the impossible.\n", beamertheme="AnnArbor")
cat(a)
}
\seealso{
Other beamer Elements: 
\code{\link{beamerBoxPlot}()},
\code{\link{beamerEndDocument}()},
\code{\link{beamerFigure}()},
\code{\link{beamerMetaTable}()},
\code{\link{beamerTable}()},
\code{\link{beamerTables}()}
}
\concept{beamer Elements}
