
# Problem Environment k-Symmetry 
# (c) 2025 A. Geyer-Schulz 

# Test Cases

#
# Problem environment for Boolean Functions in n Variables.
#

#' Constructor for a k-symmetry feedforward NN problem environment.
#' 
#' @param k  Integer. Number of bits.
#' @param topology  Vector of integers. 
#'                  \code{topology[1]} defines the number of neurons 
#'                  on the input layer. 
#'                  \code{topology[length(topology)]} defines the 
#'                  number of neurons on the output layer.
#'
#' @return A problem environment for the k-symmetry problem.
#'         A closure with at least (TODO) the following elements:
#'         \enumerate{
#'         \item $name() The name of the problem environment.
#'         \item $k()    The number of bit of the problem.     
#'         \item $BuildTest(expr)  Returns the fitness function 
#'                        for the boolean expression \code{expr}.
#'         \item $dec2bin(x) Converts an integer into a boolean vector.
#'         \item $kSymmetry(v) Tests if the boolean vector \code{v} is 
#'                        symmetric. Returns \code{0} or \code{1}.   
#'         \item $f(expr, gene, lF) The fitness function of the 
#'                        k-symmetry problem. Tests \code{2^k} boolean 
#'                        vectors for the expression \code{expr}. 
#'                        Returns the number of test errors.
#'         \item $globalOptimum() The minimal number of errors is \code{0}.
#'         }
#'
#' @family Problem Environment Factory
#'
#' @examples
#' e3NN<-newEnvKsymmetryNN(k=3, topology=c(3, 9, 1))
#' e3NN$f(rndParms(e3NN$nNNparms()))
#'
#' @importFrom utils   tail
#' @importFrom xegaBNF bindKvariables
#' @export
newEnvKsymmetryNN<-function(k=5, topology=c(5, 20, 1))
{
if (k>32) {stop("Works only up to k = < 32 bits.\n")}
self<-list()
self$name<-function() {paste(k,"-Symmetry Problem NN", sep="")}
self$bitlength<-function() {rep(10, self$nNNparms())}
self$genelength<-function() {sum(self$bitlength())}
self$lb<-function() {rep(-1, self$nNNparms())}
self$ub<-function() {rep(1, self$nNNparms())}
self$k<-function(){k}; a<-self$k()
self$topology<-function() {topology}; a<-self$topology()
self$nNNparms<-function() {numberOfNNParms(self$topology())}
self$terminate<-function(solution, lF)
        {
        # cat("Testing early termination ...\n")
        data<-self$kSymmetryTable(self$k())
        s1<-NN(solution$phenotype, self$topology(), data=data)>0.5
        correct<-sum(s1==data[,ncol(data)]) 
        # cat("correct:", correct, "\n")
        if ((2^self$k())==correct) 
           {return(TRUE)} else {return(FALSE)}
        }
### R base conversion function intToBits:
self$dec2bin<-function(d)
{utils::tail(rev(as.integer(intToBits(d))), self$k())}

### Test function
self$kSymmetry<-function(v)
{ l<-length(v)
        # cat("v:", v, "\n")
	for (i in 1:(floor(l/2)))
          { 
           # cat("v[i]", v[i], "v[1+l-i]", v[1+l-i], "\n")
           if (!identical(v[i],v[1+l-i])) {return(0)} }
        return(1) }

### We recompute this.
self$kSymmetryTable<-function(pos)
{
	r<-2^pos
	t<-matrix(0,nrow=r, ncol=pos+1)
        for (i in (0:(r-1)))
	 {v<-self$dec2bin(i)
	t[i+1,]<-c(v, self$kSymmetry(v))}
        return(t)	
}

# penv[["TestCases"]]<-kSymmetryTable(5)

self$f<-function(expr, gene=NULL, lF=NULL)
{
d<-self$kSymmetryTable(self$k())
a<-d[,ncol(d)]
as<-RsquareNN(predicted=NN(expr, self$topology(), data=d), actual=a)
return(as)
}

self$error<-function(expr, gene=NULL, lF=NULL)
{
d<-self$kSymmetryTable(self$k())
a<-d[,ncol(d)]
as<-NN(parm=expr, topology=self$topology(), data=d)
return(sum(!((as>0.5)==a)))
}

self$errorTable<-function(expr, gene=NULL, lF=NULL)
{ # TODO works only for one output neuron.
d<-self$kSymmetryTable(self$k())
a<-d[,ncol(d)]
as<-NN(parm=expr, topology=self$topology(), data=d)
df<-data.frame(as, (as>0.5), a, (!((as>0.5)==a)))
colnames(df)<-c("Activation", "Predicted", "Actual", "Error")
return(df)
}

self$globalOptimum<-function()
{
s<-list()
s$value<-0
a<-s
return(s)
}

return(self)
}

#' A problem environment for the 5-symmetry function.
#'
#' @family Problem Environment
#'
#' @examples
#' env5symmetryNN$f(rndParms(env5symmetryNN$nNNparms()))
#'
#' @export
env5symmetryNN<-newEnvKsymmetryNN(k=5, topology=c(5, 5, 5, 1))

cat("Loaded Problem Environment env5symmetry\n")
cat("Usage: env5symmetry(expr) \n")      

