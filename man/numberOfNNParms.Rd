% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NNnFF.R
\name{numberOfNNParms}
\alias{numberOfNNParms}
\title{Computes the number of parameters of a NN for a given topology vector.}
\usage{
numberOfNNParms(topology)
}
\arguments{
\item{topology}{An integer vector. The i-th element defined the 
number of nodes of the i-th layer. The first layer
is the input layer. The last layer is the output 
layer.}
}
\value{
The number of parameters.
}
\description{
Computes the number of parameters of a NN for a given topology vector.
}
\examples{
  numberOfNNParms(c(2, 3, 1))
}
\seealso{
Other Feedforward Neural Network: 
\code{\link{NN}()},
\code{\link{P2NN}()},
\code{\link{ReLU}()},
\code{\link{RsquareNN}()},
\code{\link{printNNweights}()},
\code{\link{rndParms}()}
}
\concept{Feedforward Neural Network}
