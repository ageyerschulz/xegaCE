
#' xegaCE Support for compuational experiments with xega.
#'
#' @section Organization of a Computational Experiment:
#'
#' The following is a guideline of how to organize the files
#' of a computational experiment:
#'
#' \enumerate{
#' \item Use a separate directory for each computational experiment.
#'       E.g. \code{~/experiment}.
#' \item The directory \code{~/experiment} contains:
#'       \itemize{
#'       \item A \code{README} file informing about the experiment.
#'       \item The R-file \code{Experiment.R} with the specification of 
#'             of the treatments of the experiment.
#'       \item The R-file \code{Merge.R} which merges multiple treatment files.
#'       \item The R-file \code{Report.R} which generates the report 
#'             about the result(s) of the experiment.
#'       \item The directory \code{./data} for the rds-files with the 
#'             experimental results.
#'       \item The directory \code{./merge} for the merged rds-files of the
#'             treatments of an experiment.
#'       \item The directory \code{./report} for the latex elements 
#'             of the report.
#'       }
#' }
#'
#' @section The Process of a Computational Experiment:
#'
#' A computational experiment has the following steps:
#'
#' \itemize{
#' \item \strong{I. The Experiment.}
#'        A computational experiments consists of the repeated execution 
#'        of each of its treatments. 
#'        \strong{The function \code{kSym()}.} implements the set 
#'        of potential treatments of the experiment.
#'        A treatment is a call of \code{kSym()} with
#'        a set of parameters specified (definition of the treatment).
#'       A definition of an experiment is an R-script 
#'       which contains all treatments of the experiment.
#'       The directory for the experimental data must exist
#'       and is specified by the \code{outpath} parameter of \code{kSym()}.
#'       The parameter \code{trials} specifies the number of repetitions 
#'       of a treatment. The parameter \code{everyk} specifies the number of 
#'       repetitions between saves of experimental results.
#'       A call of \code{kSym()} produces at least one rds-file with 
#'       the experimental results of a treatment 
#'       (if \code{everyk>trials}). Repeated runs of the 
#'       R-script defining a computational experiment 
#'       produce new rds-files and never overwrite rds-files.
#' \item \strong{II. Merging Results of the Experiment.} 
#'       The function \code{mergeTreatments} reads all treatment files 
#'       from the directory specified by \code{inpath}, merges multiple 
#'       experimental result files from this directory and writes 
#'       a single file for each treatment to the directory specified 
#'       by \code{outpath}. Files in this directory are overwritten.  
#' \item \strong{III. Report of the Experiment.} 
#'       The report of the experiment is specified in an R-script.
#'       The report elements read the rds-files of the treatments
#'       from the merge directory and write the latex code to the 
#'       report directory.
#' }
#'
#' @section Elements of the Report of a Computational Experiment:
#'
#' \itemize{
#' \item Document structure: Preamble, sections, and end of document.
#'       \itemize{
#'       \item experimentStart(name, title, author, purpose)
#'       \item experimentSection()
#'       \item experimentEnd()
#'         }
#' \item Parts of experiment: Design of experiment, 
#'                            treatments and their analysis.
#'       \itemize{
#'       \item experimentDesign()
#'       \item experimentTreatments()
#'       \item experimentAnalysisOfTreatment()
#'       \item experimentEffectReport()
#'         }
#' \item Tests of Properties of Random Variables
#'       \itemize{
#'       \item experimentIsNormal()
#'             shapiro.test()
#'        }
#' \item Tests of Hypothesis:
#'       \itemize{
#'       \item experimentHypothesisTests()
#'             t.test(), wilcox.test()
#'        }
#' \item Tables:
#'       \itemize{
#'       \item experimentGrammarTable()
#'       \item experimentSolutionTable()
#'       \item experimentStatistic()
#'         }
#' \item Figures:
#'       \itemize{
#'       \item experimentSolutionGenotype()
#'       \item experimentPlotPopStats()
#'       \item experimentBoxPlot()
#'         }
#' }
#'
#' @section k-Symmetry Boolean Functions:
#' 
#' \enumerate{
#' \item \code{newEnvKsymmetry}, a constructor
#'       (a function facotry) for problem environments for k-symmetry functions.
#'       Implemented in file \code{envKsymmetry.R}.
#' \item Some functions for supporting grammar tuning.
#'       Implemented in file \code{grammarTuning.R}.
#' \item File I/O. Implemented in file \code{fileIO.R}.
#' \item A wrapper function for xega for running a single treatment
#'       of a computational experiment repeatedly.
#'       Implemented in file \code{kSymmetryWrapper.R}.
#' \item A merge function for combining the result of 
#'       multiple runs of treatments of 
#'       a computational experiment. 
#' \item A set of functions for documenting a computational experiment.
#'
#' }
#'
#' @section Additional Problem Environments:
#'
#' Problem environments for the package \code{xega} must be implemented as 
#' lists of functions or function factories which return with at least 
#' the following functions:
#' \itemize{
#' \item \code{<factory name>$name()} 
#'       is a constant function which returns 
#'       the name of the environment.
#' \item \code{<factory name>$bitlength()} is a constant function which returns
#'       a vector which returns for each parameter the number of bits 
#'       used for coding the parameter value.
#' \item \code{<factory name>$genelength()} is a constant function which 
#'       returns the number of bits of a gene. 
#' \item \code{<factory name>$lb()} is a constant function which returns
#'       the vector of lower bounds of the parameters.
#' \item \code{<factory name>$ub()} is a constant function which returns
#'       the vector of upper bounds of the parameters.
#' \item \code{<factory name>$f(parm, gene, lF)} specifies the function  
#'       with the parameter vector \code{parm} which should be 
#'       optimized. The variables \code{gene} and \code{lF} 
#'       extend the interface between genetic algorithm and problem 
#'       environment. Both variables are not used in this package.
#' }
#'
#' The constant functions 
#' \code{<factory name>$bitlength()},
#' \code{<factory name>$genelength()},
#' \code{<factory name>$lb()},
#' \code{<factory name>$ub()}
#' specify the precision of the parameters as well as
#' the hypercube in which the optimal solution(s) are searched.
#' These four functions provide the information needed for decoding
#' binary genes. For real coded genes, 
#' \code{length(<factory name>$bitlength())} indicates the number of 
#' parameters.
#' 
#' Three additional functions can be provided:
#' \itemize{
#' \item \code{<factory name>$terminate(solution)} TRUE, if the solution 
#'            meets an early termination condition.
#' \item \code{<factory name>$describe()} prints a description of the 
#'       function (known mathematical properties and a reference to the
#'       origin of the function).
#' \item \code{<factory name>$solution()} specifies 
#'       the global minimum/maximum solution value and
#'       the list of minimal/maximal points.
#'       The function \code{penvValidate}(penv) validates the correct
#'       implementation of the function \code{f} of the environment.
#' }
#'
#' At the moment, the package contains: 
#'   \enumerate{
#'    \item A wrapper for the single-objective optimization test functions
#'          of the smoof package of Bossek (2017).
#'    }
#'
#' @references 
#' Bossek, Jakob (2017)
#'          smoof: Single- and Multi-Objective Optimization Test Functions.
#'           The R Journal, 9(1), 103-113.
#'
#' @family Package Description
#'
#' @name xegaCE
#' @aliases xegaCE
#' @docType package
#' @title Package xegaCE
#' @author Andreas Geyer-Schulz
#' @section Copyright: (c) 2025 Andreas Geyer-Schulz
#' @section License: MIT
#' @section URL: https://github.com/ageyerschulz/xegaCE
#' @section Installation: From CRAN by \code{install.packages('xegaCE')} 
"_PACKAGE"
