
# Problem Environment k-Symmetry 
# (c) 2025 A. Geyer-Schulz 

# Test Cases

#' A test case for the 5-symmetry problem.
#'
#' @export
t1<-"AND(AND(NOT(D1), NOT(D2)), AND(NOT(D4), NOT(D5)))"

#
# Problem environment for Boolean Functions in n Variables.
#

#' Constructor for a k-symmetry problem environment.
#' 
#' @param k  Integer. Number of bits.
#'
#' @return A problem environment for the k-symmetry problem.
#'         A closure with the following elements:
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
#' env3symmetry<-newEnvKsymmetry(k=3)
#' env3symmetry$f("OR(AND(D1, D3), NOT(OR(D1, D3)))")
#' env3symmetry$f("OR(AND(D1, D3), AND(NOT(D1), NOT(D3)))")
#' env3symmetry$f("AND(AND(NOT(D1), NOT(D3)), AND(NOT(D2), NOT(D1)))")
#'
#' @importFrom utils   tail
#' @importFrom xegaBNF bindKvariables
#' @export
newEnvKsymmetry<-function(k=5)
{
if (k>32) {stop("Works only up to k = < 32 bits.\n")}
self<-list()
self$name<-function() {paste(k,"-Symmetry Problem", sep="")}
self$k<-function(){k}; a<-self$k()
self$BuildTEST<-function(expr) {
	f<-paste("function(v) {
	AND<-function(x,y){return(x & y)}
        NAND<-function(x,y){return(!(x & y))}
	OR<-function(x,y){return(x|y)}
	NOT<-function(x){return(!x)}
        sPair<-function(x,y){OR(AND(x,y),AND(NOT(x),NOT(y)))}\n",
        xegaBNF::bindKvariables("D", "v", self$k()),
	"return(", expr, ")}", sep="")
	return(eval(parse(text=f)))
}

#self$dec2binvec <- function(x, pos) 
#{
#       	a<-paste(as.integer(rev(intToBits(x))), collapse = ",")
#	b<-   c("c(",a,")")
#        c<-tail(eval(parse(text=b)), pos)
#        return(c)
#}

### R base conversion function intToBits:
self$dec2bin<-function(d)
{utils::tail(rev(as.integer(intToBits(d))), self$k())}

### Test function
self$kSymmetry<-function(v)
{ l<-length(v)
	for (i in 1:(floor(l/2)))
          { if (!identical(v[i],v[1+l-i])) {return(0)} }
        return(1) }

### Avoid this. Increases communication cost!
#self$kSymmetryTable<-function(pos)
#{
#	r<-2^pos
#	t<-matrix(0,nrow=r, ncol=pos+1)
#	 {v<-self$dec2binvec(i,pos)
#	t[i+1,]<-c(v, self$kSymmetry(v))}
#        return(t)	
#}

# penv[["TestCases"]]<-kSymmetryTable(5)

self$f<-function(expr, gene=NULL, lF=NULL)
{ TEST<-self$BuildTEST(expr)
s<-0
n<-(2^self$k())-1
for (i in (0:n))
{ 
v<-self$dec2bin(i)
s<-s+
(self$kSymmetry(v)==TEST(v))
}

return(2^self$k()-s)
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
#' t3<-"AND(OR(AND(D2,D4),AND(NOT(D2),NOT(D4)))," 
#' t3<-paste0(t3,"OR(AND(D1, D5), AND(NOT(D1), NOT(D5))))")
#' env5symmetry$f(t1)
#' env5symmetry$f(t3)
#'
#' @export
env5symmetry<-newEnvKsymmetry(k=5)

cat("Loaded Problem Environment env5symmetry\n")
cat("Usage: env5symmetry(expr) \n")      

