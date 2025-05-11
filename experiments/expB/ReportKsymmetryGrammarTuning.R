
library(xegaCE)

inpath="merge"
outpath="report"

type="beamer"
#type="latex"
#type="console"

t<-"k-Symmetry: Grammar Tuning"
experiment<-"ExpB"

experimentStart(name=experiment, 
   title=t, author="Andreas Geyer-Schulz", 
   purpose=paste(
           "Context-free grammars control the stochastic process for generating programs",
           "in grammar-based genetic programming algorithms.",
           "The stochastic process can be tuned by adding additional production rules.",
           "In this experiment we compare 4 manually tuned grammars for boolean functions",
           "with a standard grammar for boolean functions for learning k-symmetry problems.",
           "Grammar tuning (the grammar dimension) is a distinctive feature of",
           "grammar-based genetic programming.",
           sep=" "),
   beamertheme="Antibes", beamercolor="deepskyblue",
   type=type, outpath=outpath)

treatments<-experimentTreatments(inpath=inpath)

experimentText(
"{\\bf Grammar tuning} means adding additional production rules",
"to a context-free grammar with the goal of improving the learning",
"performance of grammar-based genetic programming algorithms.",
" ",
"{\\bf Example:} Repeating production rules allows to change the distribution",
"of programs (and their sizes) generated during the initialization of a grammar-based",
"genetic programming algorithm.", 
header="Definition",
name=experiment, type=type, outpath=outpath)

experimentText(
"{\\bf Families of functions} are parametrized functions whose difficulty",
"can be controlled by one or more parameters.",
" ",
"{\\bf Example:} The family of k-symmetry functions.",
"The k-symmetry problem requires finding a function which classifies", 
"a k-bit string as symmetric.",
"The parameter $k$ defines the length of the bit string.", 
"The problem is NP hard, because the number of test cases increases by $2^k$.",
header="Definition",
name=experiment, type=type, outpath=outpath)

experimentText(
"The purpose of this computational experiment is to show the improvement",
"of performance by grammar tuning.",
" ",
"The {\\bf problem environment} is the k-symmetry problem: ",
"Finding a boolean expression (with and, or, and not)",
"which is TRUE for symmetric k-bit strings.",
" ",
"The {\\bf solution method} is grammar-based genetic programming",
"(option {\\tt algorithm=\"sgp\"}  of {\\tt xegaRun}).",
"The {\\bf solver} used is {\\tt xegaRun} from the R-package {\\tt xega}.",
" ",
"The experiment consists of 25 treatments, 5 grammars for 5 problem sizes $k\\in 2,\\dots, 6$.",
header="Description of Experiment",
name=experiment, type=type, outpath=outpath)

experimentText(
"The two control variables in this experiment are",
"\\begin{itemize}",
"\\item The bit-length of the k-symmetry problem: 2, 3, 4, 5, and 6.",
"\\item The grammar for boolean expressions:",
"\\begin{itemize} ",
"\\item {\\bf T0}: A standard grammar for boolean expressions.",
"\\item {\\bf T1}: A standard grammar for boolean expressions.",
"            With two rules for OR and a rule for a template ",
"            which tests for the symmetry of two bits.",
"\\item {\\bf T2}: With two rules for AND and one rule for variables replaced", 
"            by symmetric pairs.",
"\\item {\\bf T3}: With two rules for OR and one rule variables replaced", 
"            by symmetric pairs.",
"\\item {\\bf T4}: With two rules for OR and two rules for variables replaced", 
"            by symmetric pairs.",
"\\end{itemize}",
"\\end{itemize}",
header="Description of Experiment", block=FALSE,
name=experiment, type=type, outpath=outpath)

experimentSection(name=experiment, 
                  secname="Design of Experiment", 
                  level="section",
                  clearpage=TRUE, type=type, outpath=outpath)

experimentDesign(treatments=treatments,
                 inpath=inpath, outpath=outpath, 
                 name=experiment, type=type)

experimentSection(name=experiment, 
                  secname="Exploratory Analysis", 
                  level="section",
                  clearpage=TRUE, type=type, outpath=outpath)

experimentSection(name=experiment, 
                  secname="Do we always find an optimal solution?", 
                  level="subsection",
                  clearpage=TRUE, type=type, outpath=outpath)

experimentMeanMatrix(treatments, tpvec=c("T0", "T1", "T2", "T3", "T4"), 
                  variable="Fitness", statistic="mean", 
                  type=type, inpath=inpath, outpath=outpath, name=experiment,
        caption="Matrix of Mean of Errors (Fitness).  Rows: k=2, 3, 4, 5, 6)")

experimentMeanMatrix(treatments, tpvec=c("T0", "T1", "T2", "T3", "T4"), 
                  variable="Fitness", statistic="min", 
                  type=type, inpath=inpath, outpath=outpath, name=experiment,
        caption="Matrix of Min of Errors (Fitness). Rows: k=2, 3, 4, 5, 6)")

experimentText(
"\\begin{itemize}",
"\\item {\\bf No.} The non-zero elements in the first table", 
"         indicate the mean number of remaining errors given", 
"         a limit of 500 generations.",
"\\item The standard boolean grammar (T0) has the highest error rate",
"  for the 4, 5, and 6-symmetry problems given a limit of 500 generations.",
"\\item The grammars T3 and T4 have only problems with the 6-symmetry problem",
"       given a limit of 500 generations.",
"\\item For the 6-symmetry problem, the grammar T0 has at least 4 errors.",
"\\end{itemize}",
header="Do we always find the optimal program?", block=FALSE,
name=experiment, type=type, outpath=outpath)

experimentSection(name=experiment, 
                  secname="How long to find an optimal solution?", 
                  level="subsection",
                  clearpage=TRUE, type=type, outpath=outpath)

experimentMeanMatrix(treatments, tpvec=c("T0", "T1", "T2", "T3", "T4"), 
                  variable="Seconds", statistic="mean", 
                  type=type, inpath=inpath, outpath=outpath, name=experiment,
        caption="Matrix of Mean of Seconds.  Rows: k=2, 3, 4, 5, 6)")

experimentMeanMatrix(treatments, tpvec=c("T0", "T1", "T2", "T3", "T4"), 
                  variable="Generations", statistic="mean", 
                  type=type, inpath=inpath, outpath=outpath, name=experiment,
        caption="Matrix of Mean of Generations.  Rows: k=2, 3, 4, 5, 6)")

tsr<-"Distribution of Number of Generations"
tpvec<-c("2k", "3k", "4k", "5k", "6k")
for (i in (1:length(tpvec)))
{
experimentBoxPlot(treatments=treatments[grepl(treatments, pattern=tpvec[i])],
                 variable="Generations",
                 title=paste(tsr,"for Grammars.", tpvec[i], " symmetry."),
                 name=experiment, notch=TRUE,
                 inpath=inpath, outpath=outpath, type=type)
}

experimentText(
"\\begin{itemize}",
"\\item Grammar T4 performs best (mean number of generations).",
"  But not always for the medians (e.g. Box-plot for 4-symmetry).",
" ",
"Statistically significant? No - for medians. Not tested for means.",
"\\item Grammar T0 performs worst.",
"\\item Grammar tuning works:",
" All modified grammars perform better than the grammar T0.",
"\\end{itemize}",
header="Which grammar performs best?", block=FALSE,
name=experiment, type=type, outpath=outpath)

experimentSection(name=experiment, 
                  secname="Computational Complexity?",
                  level="subsection",
                  clearpage=TRUE, type=type, outpath=outpath)

tsr<-"Distribution of Number of Generations"
tpvec<-c("T0", "T1", "T2", "T3", "T4")
for (i in (1:length(tpvec)))
{
experimentBoxPlot(treatments=treatments[grepl(treatments, pattern=tpvec[i])],
                 variable="Generations",
                 title=paste(tsr,"for Grammar", tpvec[i]),
                 name=experiment, notch=TRUE,
                 inpath=inpath, outpath=outpath, type=type)
}

experimentText(
"\\begin{itemize}",
"\\item Complexity grows in steps of 2.",
"       E.g. the 2- and 3-symmetry problem need the same boolean expression,",
"       but with different variables:",  
"       For the 2-symmetry problem, D1 and D2.",  
"       For the 3-symmetry problem, D1 and D3.",  
"In the 3-symmetry problem, the middle variable is ignored.",
"\\item The search problem grows harder, because of the need to include all ",
"       relevant variable pairs twice into the solution.",
"\\end{itemize}",
header="Growth of Complexity?", block=FALSE,
name=experiment, type=type, outpath=outpath)


###
experimentAppendix(treatments=treatments,
                   name=experiment,
                   miniframe=FALSE,
                   inpath=inpath,
                   outpath=outpath,
                   type=type)

experimentEnd(name=experiment, type=type, outpath=outpath)

