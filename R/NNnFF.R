# (c) 2023 Andreas Geyer-Schulz

### The xor problem. 

#' Data for the xor problem.
#' @export
xorData<-matrix(c(0, 0, 0,
		  0, 1, 1, 
		  1, 0, 1,
		  1, 1, 0), 
		nrow=4, ncol=3, byrow=TRUE)

### An (optimal) parameter set for solving the xor problem. 
# with a (2, 3, 1) NN.
parm<-c(0, -1, 0, 1, 1, 1, 1, 1, 1, 0, 1, -2, 0) 

# topology vector
top<-c(2, 3, 1)

#' Computes the number of parameters of a NN for a given topology vector.
#'
#' @param topology   An integer vector. The i-th element defined the 
#'                   number of nodes of the i-th layer. The first layer
#'                   is the input layer. The last layer is the output 
#'                   layer. 
#'
#' @return The number of parameters.
#'
#' @family Feedforward Neural Network
#'
#' @examples
#'   numberOfNNParms(c(2, 3, 1))
#' @export
numberOfNNParms<-function(topology)
{
	n<-0
	l<-length(topology)-1
	for (j in (1:l)) {n<-n+(1+topology[j])*topology[j+1]}
	return(n)
}

#' Generates a random vector of length n in an n-dimensional hypercube.
#'
#' @param  n     Number of elements.
#' @param  lb    Lower bound.
#' @param  ub    Upper bound.
#'
#' @return A random vector of length n with elements in [lb, ub].
#'
#' @family Feedforward Neural Network
#'
#' @examples
#' rndParms(numberOfNNParms(c(2, 3, 1)))
#'
#' @importFrom stats runif
#' @export
rndParms<-function(n, lb=-1, ub=1)
{   return(lb+runif(n)*(ub-lb)) }

#' Activation function of a rectified linear unit (RELU).
#'
#' @param z   A (real) matrix or vector.
#'
#' @return   In r is identical to z except for the negative elements of z.
#'           These are set to 0.
#'
#' @family Feedforward Neural Network
#'
#' @examples
#' a<-rndParms(numberOfNNParms(c(2, 3, 1)))
#' a
#' ReLU(a)
#' @export
ReLU<-function(z)
{
	r<-z
	r[r<0]<-0
	return(r)
}

#' Identity function
#'
#' @param x   A number.
#'
#' @return x.
#'
#' @export
Identity<-function(x) {x}

#' Convert the parameter vector of a NN into a list of weight matrices.
#'
#' @description The dimensions of the weight matrices are defined by 
#'              the vector \code{topology}. 
#' 
#' @param  parm       A parameter vector of 
#'                    length \code{numberOfNNParms(topology)}.
#' @param  topology   The topology of the neural network. 
#' 
#' @return A list of weight matrices.
#'
#' @family Feedforward Neural Network
#'
#' @examples
#' topology<-c(1, 3, 2, 1)
#' parms<-rndParms(numberOfNNParms(topology))
#' P2NN(parms, topology)
#'
#' @export
P2NN<-function(parm, topology)
{
# cat("parm\n")
# print(parm)
 pos<-0
 l<-length(topology)-1
 W<-vector(mode="list", l)
 for (j in (1:l)) 
	 { W[[j]]<-matrix(parm[pos+(1:((1+topology[j])*topology[j+1]))], 
		     nrow=(1+topology[j]), ncol=topology[j+1], byrow=TRUE)
		 pos<-pos+(1+topology[j])*topology[j+1] }
 return(W)
}

#' A feedforward neural network with topology \code{top}.
#' 
#' @param  parm  A parameter vector of length \code{numberOfNNParms(top)}.
#' @param  topology   The topology of the neural network. 
#' @param  data  An input data matrix. The column number must match \code{top[1]}.  E.g. for xor.
#' @param  AH    Activation function hidden layers. Default: ReLU.
#' @param  AO    Activation function output layer. Default: ReLU.
#'
#' @return A vector of activation values of the output layer.
#'
#' @family Feedforward Neural Network
#'
#' @examples 
#' NN(rndParms(numberOfNNParms(c(2, 3, 1))), c(2, 3, 1), data=xorData)
#'
#' @export
NN<-function(parm, topology, AH=ReLU, AO=ReLU, data)
{
  l<-length(topology)-2
  # input data with constants row. top[1] contains number of input nodes.
  Ij<-cbind(matrix(1, nrow=nrow(data), ncol=1), data[, (1:topology[1])])
  W<-P2NN(parm, topology)
  ### Hidden layers
  for (j in (1:l))
  { Oj<-AH(Ij%*%W[[j]])
    Ij<-cbind(matrix(1, nrow=nrow(Oj), ncol=1), Oj) }
  ### Output layer
  return(AO(Ij%*%W[[l+1]]))
}

#' R-square. 
#'
#' @param predicted  Predicted output vector.
#' @param actual     Actual output vector.
#'
#' @return The sum of squared errors.
#'
#' @family Feedforward Neural Network
#'
#' @examples
#' RsquareNN(c(1, 2, 3), c(0, 3, 3))
#'
#' @export
RsquareNN<-function(predicted, actual)
{
	sum((predicted-actual)^2)
}

#' Print the parameter vector of a NN with topology c(2, 3, 1) 
#'
#' @description TODO: Add row and column names.
#'
#' @param parm      A parameter vector.
#' @param topology  A topology vector.
#'
#' @return Invisible zero.
#'
#' @family Feedforward Neural Network
#' 
#' @examples 
#' printNNweights(parm=rndParms(numberOfNNParms(c(2, 3, 1))), topology=c(2, 3, 1))
#' 
#' @export
printNNweights<-function(parm, topology)
{
 l<-length(topology)-1
 W<-P2NN(parm, topology)
 for (j in (1:l))
 {
 cat("Layer", j, "(b|W)^T: \n")
 print(W[[j]])  	 
 }
invisible(0)
}

#### The GA as a solver.

#' Generate a problem environment for a NN for the XOR problem with topology \code{c(2, 3, 1)}.
#'
#' @return A problem environment. 
#'
#' @examples
#' require(xega)
#' cat("\n XOR Topology: c(2, 3, 1). Activation: ReLU \n")
#' p<-envXORNN231()
#' t1<-xegaRun(penv=p, algorithm="sga", generations=100, popsize=20, 
#'   evalmethod="Deterministic", max=FALSE, verbose=1)
#' printNNweights(t1$solution$phenotype, p$topology())
#' as<-NN(t1$solution$phenotype, p$topology(), data=xorData)
#' cat("The activation values for the xor data set:\n")
#' print(as)
#' cat("Fitness:", t1$solution$fitness, "Above 0.5? \n")
#' r3<-data.frame(as, (as>0.5), xorData[,3])
#' colnames(r3)<-c("Activation", "Predicted", "xor")
#' print(r3)
#'
#' @export
envXORNN231<-function()
{
	self<-list()
	self$name<-function() {"XORNN231"}
	self$bitlength<-function() {rep(10, self$nNNparms())}
	self$genelength<-function() {sum(self$bitlength())}
	self$lb<-function() {rep(-1, self$nNNparms())}
	self$ub<-function() {rep(1, self$nNNparms())}
	self$topology<-function() {return(c(2, 3, 1))}; a<-self$topology()
	self$nNNparms<-function() {numberOfNNParms(self$topology())}; a<-self$nNNparms()
	self$f<-function(parm, gene=0, lF=0)
	{
	RsquareNN(NN(parm, self$topology(), data=xorData), xorData[,3])
	}
	self$terminate<-function(solution)
	{
	#cat("Testing early termination ...\n")
        s1<-NN(solution$phenotype, self$topology(), data=xorData)>0.5
	if (4==sum(s1==xorData[,3])) {return(TRUE)} else {return(FALSE)}
	}
	return(self)
}

#' Generate a problem environment for the XOR problem.
#' Topology: c(2, 4,  2, 1)
#'
#' @examples
#' require(xega)
#' cat("\n Topology: c(2, 4, 2, 1). Activation: ReLU \n")
#' p3<-envXORNN2421()
#' t3<-xegaRun(penv=p3, max=FALSE, algorithm="sgde", popsize=30, generations=100, 
#'    mutation="MutateGeneDE", scalefactor="Uniform", crossover="UCrossGene", 
#'    genemap="Identity", replication="DE", 
#'    selection="UniformP", mateselection="UniformP", accept="Best")
#' printNNweights(t3$solution$phenotype, p3$topology())
#' as<-NN(t3$solution$phenotype, p3$topology(), data=xorData)
#' cat("Fitness:", t3$solution$fitness, "Above 0.5? \n")
#' r3<-data.frame(as, (as>0.5), xorData[,3])
#' colnames(r3)<-c("Activation", "Predicted", "xor")
#' print(r3)
#' 
#' @export
envXORNN2421<-function()
{
	self<-list()
	self$name<-function() {"XORNN2421"}
	self$bitlength<-function() {rep(10, self$nNNparms())}
	self$genelength<-function() {sum(self$bitlength())}
	self$lb<-function() {rep(-1, self$nNNparms())}
	self$ub<-function() {rep(1, self$nNNparms())}
	self$f<-function(parm, gene=0, lF=0)
	{ 
        RsquareNN(NN(parm, self$topology(), data=xorData), xorData[,3])
	}
	self$nNNparms<-function() {numberOfNNParms(self$topology())}
	self$topology<-function() {c(2, 4, 2, 1)}
	self$terminate<-function(solution)
	{
	#cat("Testing early termination ...\n")
        s1<-NN(solution$phenotype, self$topology(), data=xorData)>0.5
	if (4==sum(s1==xorData[,3])) {return(TRUE)} else {return(FALSE)}
	}
	return(self)
}

