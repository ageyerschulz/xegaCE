#
# (c) 2024 Andreas Geyer-Schulz
#

#' Factory for wrapping smoof functions.
#' 
#' @description A minimal wrapper for smoof functions. 
#'          This is an R-example for the facade architectural pattern.
#'
#' @details The R-package smoof provides a large number 
#'          of benchmark and test functions for single and 
#'          multiobjective optimization. See
#'         https://cran.r-project.org/web/packages/smoof/index.html
#'
#' @param smoofFN   smoof function (usually a call to the generator of a 
#'           smoof function.
#'
#' @references Bossek, Jakob (2017)
#'          smoof: Single- and Multi-Objective Optimization Test Functions.
#'           The R Journal, 9(1), 103-113. 
#'
#' @return A list of functions (
#'         \code{$name()},
#'         \code{$bitlength()},
#'         \code{$genelength()},
#'         \code{$lb()},
#'         \code{$ub()},
#'         \code{$f(parm, gene=0, lF=0)}, ...)
#'         For details, see the interface description
#'         in the package description.
#' 
#' @family Constructor for Problem Environments
#'
#' @examples
#' require(smoof)
#' t1<-newSmoofWrapper(makeSchwefelFunction(10))
##  a<-RunSGA(t1, max=FALSE, popsize=100, generations=100)
#' @importFrom smoof getName
#' @importFrom smoof getNumberOfParameters
#' @importFrom smoof getLowerBoxConstraints
#' @importFrom smoof getUpperBoxConstraints
#' @export
newSmoofWrapper<-function(smoofFN)
{
   parm<-function(x){function() {return(x)}}
   self<-list()
   self$name<-parm(smoof::getName(smoofFN))
   self$bitlength<-parm(rep(64, smoof::getNumberOfParameters(smoofFN)))
   self$genelength=function() {sum(self$bitlength())}
   self$lb<-parm(as.vector(smoof::getLowerBoxConstraints(smoofFN)))
   self$ub<-parm(as.vector(smoof::getUpperBoxConstraints(smoofFN)))
   self$f<-function(param, gene=0, lF=0) {smoofFN(param) }
   return(self)
}

