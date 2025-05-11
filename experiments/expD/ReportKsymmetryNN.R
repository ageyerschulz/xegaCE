
library(xegaCE)

inpath="merge"
outpath="report"

type="beamer"
#type="latex"
#type="console"

t<-"k-Symmetry: Training Neural Networks. GAs or DE?"
experiment<-"ExpD"

experimentStart(name=experiment, 
   title=t, author="Andreas Geyer-Schulz", 
   purpose=(paste(
   "In this experiment we compare the training of feedforward neural networks",
   "with topology $(k, 2k, k, 1)$ for k-symmetry problems ($k\\in 2, \\dots, 6$)",
   "for a genetic algorithm and a differential evolution algorithm." 
   )),
   beamertheme="Berlin", beamercolor="blue", 
   type=type, outpath=outpath)

experimentSection(name=experiment, 
                  secname="Design of Experiment", 
                  level="section",
                  clearpage=TRUE, type=type, outpath=outpath)

experimentText(
"The purpose of this computational experiment is to find out",
"which algorithm performs better, genetic algorithms or differential evolution",
"for the training of a feed-forward neural network for k-symmetry problems.",
" ",
"The {\\bf problem environment} is the k-symmetry problem: ",
"Finding a boolean expression (with and, or, and not)",
"which is TRUE for symmetric k-bit strings.",
" ",
"The {\\bf solution method} is grammar-based genetic programming",
"(options {\\tt algorithm=\"sga\"} and {\\tt algorithm=\"sgde\"}  of {\\tt xegaRun}).",
"The {\\bf solver} used is {\\tt xegaRun} from the R-package {\\tt xega}.",
" ",
"The experiment consists of 10 treatments, 2 algorithms for 5 problem sizes $k\\in 2,\\dots, 6$.",
header="Description of Experiment",
name=experiment, type=type, outpath=outpath)

experimentText(
"The control variable in this experiment is:",
"\\begin{itemize}",
"\\item The algorithm used for training the feed-forward neural network:",
"\\begin{itemize} ",
"\\item {\\tt \"sga\"}: A simple binary genetic algorithm.",
"\\item {\\tt \"sgde\"}: A differential evolution algorithm.",
"\\end{itemize}",
"Both algorithms use a plain-vanilla standard parameter configuration.",
"\\end{itemize}",
header="Description of Experiment", block=FALSE,
name=experiment, type=type, outpath=outpath)

treatments<-experimentTreatments(inpath=inpath)

experimentDesign(treatments=treatments,
                 inpath=inpath, outpath=outpath, 
                 name=experiment, type=type)

experimentSection(name=experiment,
                  secname="Analysis of Experiment",
                  level="section",
                  clearpage=TRUE, type=type, outpath=outpath)

experimentSection(name=experiment,
                  secname="How long to find an optimal solution?",
                  level="subsection",
                  clearpage=TRUE, type=type, outpath=outpath)

tgroups<-c("sga", "sgde")

experimentMeanMatrix(treatments, tpvec=tgroups,
                  variable="Seconds", statistic="mean",
                  type=type, inpath=inpath, outpath=outpath, name=experiment,
        caption="Matrix of Mean of Seconds.  Rows: k=2, 3, 4, 5, 6)")

experimentMeanMatrix(treatments, tpvec=tgroups,
                  variable="Generations", statistic="mean",
                  type=type, inpath=inpath, outpath=outpath, name=experiment,
        caption="Matrix of Mean of Generations.  Rows: k=2, 3, 4, 5, 6)")

tsr<-"Distribution of Number of Generations"
tpvec<-c("2k", "3k", "4k", "5k", "6k")
for (i in (1:length(tpvec)))
{
experimentBoxPlot(treatments=treatments[grepl(treatments, pattern=tpvec[i])],
                 variable="Generations",
                 title=paste(tsr,"for k-symmetry problem", tpvec[i]),
                 name=experiment, notch=TRUE,
                 inpath=inpath, outpath=outpath, type=type)
}

experimentText(
"For the 2, 3, 4, and 5-symmetry problems,",
"{\\bf differential evolution} needs {\\bf statistically significant fewer} generations",
"(non-overlapping notches in all Box-plots) than a {\\bf genetic algorithm}.",
" ",
"{\\bf Differential evolution} should be preferred.",
" ",
"For $k=6$ (and beyond) the parameter {\\tt generation} should be increased considerably", 
"for both algorithms.",
header="Result of Experiment", block=FALSE,
name=experiment, type=type, outpath=outpath)

experimentText(
"\\begin{itemize}",
"\\item The fitness function of the NN and the error rate do not correspond.",
"       The reporting mechanism of the experiment shoud be adapted to report both performance measures.",
"\\item The convexity of the NN loss function has not been exploited.",
"       Experiments with fitness scaling (genetic algorithms)", 
"       and selection functions (differential evolution) should be performed.",
"\\item Repeat the experiment with optimal parameters for both algorithms.",
"       Find optimal hyper-parameters for both algorithms first!", 
"\\item Study the balance between population size and number of generations needed!",
"\\end{itemize}",
header="Suggestions for Further Experiments", block=TRUE,
name=experiment, type=type, outpath=outpath)

###
experimentAppendix(treatments=treatments,
                   name=experiment,
                   miniframe=FALSE,
                   inpath=inpath,
                   outpath=outpath,
                   type=type)

experimentEnd(name=experiment, type=type, outpath=outpath)

