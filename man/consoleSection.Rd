% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/consoleElements.R
\name{consoleSection}
\alias{consoleSection}
\title{Shows the section of an experiment.}
\usage{
consoleSection(name = "Missing", level = "section", clearpage = TRUE)
}
\arguments{
\item{name}{Name of the document element. Default: "Missing".}

\item{level}{Latex markup of the document element.
Default: "section". Latex supports:
"section", "subsection", "subsubsection".
Used for indentation of output.}

\item{clearpage}{Boolean. Default: TRUE. 
Ignored.}
}
\value{
A text string (invisible).
}
\description{
Shows the section of an experiment.
}
\examples{
consoleSection(name="Design", level="section", clearpage=FALSE)
consoleSection(name="Design", level="subsection", clearpage=TRUE)
}
\seealso{
Other Console Elements: 
\code{\link{beamerText}()},
\code{\link{consoleBoxPlot}()},
\code{\link{consoleEndDocument}()},
\code{\link{consoleFigure}()},
\code{\link{consoleProlog}()},
\code{\link{consoleTable}()},
\code{\link{consoleText}()},
\code{\link{pause}()}
}
\concept{Console Elements}
