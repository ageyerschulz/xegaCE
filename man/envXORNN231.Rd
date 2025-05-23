% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NNnFF.R
\name{envXORNN231}
\alias{envXORNN231}
\title{Generate a problem environment for a NN for the XOR problem with topology \code{c(2, 3, 1)}.}
\usage{
envXORNN231()
}
\value{
A problem environment.
}
\description{
Generate a problem environment for a NN for the XOR problem with topology \code{c(2, 3, 1)}.
}
\examples{
require(xega)
cat("\n XOR Topology: c(2, 3, 1). Activation: ReLU \n")
p<-envXORNN231()
t1<-xegaRun(penv=p, algorithm="sga", generations=100, popsize=20, 
  evalmethod="Deterministic", max=FALSE, verbose=1)
printNNweights(t1$solution$phenotype, p$topology())
as<-NN(t1$solution$phenotype, p$topology(), data=xorData)
cat("The activation values for the xor data set:\n")
print(as)
cat("Fitness:", t1$solution$fitness, "Above 0.5? \n")
r3<-data.frame(as, (as>0.5), xorData[,3])
colnames(r3)<-c("Activation", "Predicted", "xor")
print(r3)

}
