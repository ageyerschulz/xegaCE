
# Grammar tuning for the k-symmetry problem.
# (c) 2025 Andreas Geyer-Schulz

#' Returns a the set of production rules with symmetric variable pairs.
#' 
#' @description Symmetric variable pairs are the pairs of variables 
#'              with the same truth value in the k-symmetry problem.
#'              E.g. In a 4-symmetry problem with variables V1-V4, 
#'              the 2 symmetric variable pairs are (V1, V4) and (V2, V3).
#'
#' @details     For the BNF notation, see the R-package \code{xegaBNF}
#'              The function is called by the preprocessor 
#'              \code{xegaBNF::preBNF} and is expected to return
#'              a part of a BNF grammar specification. 
#'              Used in the grammar AndOrNotTuned2.txt.
#'
#' @param varNT  The non-terminal symbol on the left-hand side of a BNF rule.
#' @param varSym The symbol of the variable name. E.g. "V". 
#' @param k      The number of bits.
#'
#' @return A text string with the production rules in BNF.
#'
#' @family Grammar Tuning
#'
#' @examples
#' sympairs(varNT="<sympairs>", varSym="V", k=4) 
#'
#' @export
sympairs<-function(varNT, varSym, k)
{ if (k == 1) { return(paste(" ", sep = "")) }
# a local function: pastes the parts of  the production rule.
  stringPair<-function(v, varSym, varNT)
   { t<-paste("", varNT, " := ")
     t<-paste(t, "(\"",varSym, v[1], "\",\"", varSym, v[2], "\") | ", sep="")
     t<-paste(t, "(NOT(\"",varSym, v[1], "\"), ", sep="") 
     t<-paste(t, "NOT(\"", varSym, v[2], "\")); ", sep="")
   return(t)}
# end of local function
    p<-matrix(ncol=2, c((1:k), (k:1)))
    n<-floor(nrow(p)/2)
    t<-""
    for (i in (1:n))
    { t<-paste(t,stringPair(p[i,], varSym, varNT)) }
    return(t) }

##

#' Returns a the set of production rules with symmetric variable pairs and their negation.
#' 
#' @description Symmetric variable pairs are the pairs of variables 
#'              with the same truth value in the k-symmetry problem.
#'              E.g. In a 4-symmetry problem with variables V1-V4, 
#'              the 2 symmetric variable pairs are (V1, V4) and (V2, V3).
#'
#' @details     For the BNF notation, see the R-package \code{xegaBNF}
#'              The function is called by the preprocessor 
#'              \code{xegaBNF::preBNF} and is expected to return
#'              a part of a BNF grammar specification. 
#'              Used in the grammar AndOrNotTuned2.txt.
#'              
#'              Combines a symmetric pairs with its negation. 
#'              This further reduces the search space size for 
#'              grammar-based genetic programming.
#' 
#' @param varNT  The non-terminal symbol on the left-hand side of a BNF rule.
#' @param varSym The symbol of the variable name. E.g. "V". 
#' @param k      The number of bits.
#'
#' @return A text string with the production rules in BNF.
#'
#' @family Grammar Tuning
#'
#' @examples
#' sym2pairs(varNT="<sympairs>", varSym="V", k=4) 
#'
#' @export
sym2pairs<-function(varNT, varSym, k)
{ if (k == 1) { return(paste(" ", sep = "")) }
# Local function
string2Pair<-function(v, varSym, varNT)
{ t<-paste("", varNT, " := OR(AND")
t<-paste(t, "(\"",varSym, v[1], "\",\"", varSym, v[2], "\"), ", sep="")
t<-paste(t, "AND(NOT(\"",varSym, v[1], "\"), ", sep="")
t<-paste(t, "NOT(\"", varSym, v[2], "\"))); ", sep="")
return(t)}
# end of local function
    p<-matrix(ncol=2, c((1:k), (k:1)))
    n<-floor(nrow(p)/2)
    t<-""
    for (i in (1:n))
    { t<-paste(t,string2Pair(p[i,], varSym, varNT)) }
    return(t) }

### Nand

#' Returns a the set of production rules with symmetric variable pairs (Nand)
#' 
#' @description Symmetric variable pairs are the pairs of variables 
#'              with the same truth value in the k-symmetry problem.
#'              E.g. In a 4-symmetry problem with variables V1-V4, 
#'              the 2 symmetric variable pairs are (V1, V4) and (V2, V3).
#'
#' @details     For the BNF notation, see the R-package \code{xegaBNF}
#'              The function is called by the preprocessor 
#'              \code{xegaBNF::preBNF} and is expected to return
#'              a part of a BNF grammar specification. 
#'              Used in the grammar NandTuned2.txt.
#'
#'              The pattern used needs to be improved.
#'
#' @param varNT  The non-terminal symbol on the left-hand side of a BNF rule.
#' @param varSym The symbol of the variable name. E.g. "V". 
#' @param k      The number of bits.
#'
#' @return A text string with the production rules in BNF.
#'
#' @family Grammar Tuning
#'
#' @examples
#' nandpairs(varNT="<sympairs>", varSym="V", k=4) 
#'
#' @export
nandpairs<-function(varNT, varSym, k)
{ if (k == 1) { return(paste(" ", sep = "")) }
# local function
  nandPair<-function(v, varSym, varNT)
    { t<-paste("", varNT, " := \"NAND(NAND(\"")
      t<-paste(t, " \"",varSym, v[1], "\",\"", varSym, v[1], "\" ", sep="")
      t<-paste(t, "\"),NAND(NAND(\"", sep="")
      t<-paste(t, " \"",varSym, v[2], "\",\"", varSym, v[2], "\" ", sep="")
      t<-paste(t, "\"),NAND(\"", sep="")
      t<-paste(t, " \"",varSym, v[1], "\",\"", varSym, v[2], "\" ", sep="")
      t<-paste(t, "\")))\";", sep="")
      return(t)}
# end of local function
    p<-matrix(ncol=2, c((1:k), (k:1)))
    n<-floor(nrow(p)/2)
    t<-""
    for (i in (1:n))
    { t<-paste(t,nandPair(p[i,], varSym, varNT)) }
    return(t) }


