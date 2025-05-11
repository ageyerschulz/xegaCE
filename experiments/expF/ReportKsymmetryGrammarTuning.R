
library(xegaCE)

inpath="merge"
outpath="report"

type="beamer"
#type="latex"
#type="console"

t<-"k-Symmetry: Scalability of Learning by Grammar Tuning and Language Tuning  "
experiment<-"ExpF"

experimentStart(name=experiment, 
   title=t, author="Andreas Geyer-Schulz", 
   purpose=paste(
           "Grammar tuning adapts the language.", 
           "Language tuning adds new language elements.",
           "In this experiment we study the complexity of learning",
           " with increasing k",
           "with a grammar with symmetric pairs and an additional function.",
           sep=" "),
   beamertheme="Berlin", beamercolor="blue",
   type=type, outpath=outpath)

treatments<-experimentTreatments(inpath=inpath)

experimentSection(name=experiment, 
                  secname="Design of Experiment", 
                  level="section",
                  clearpage=TRUE, type=type, outpath=outpath)

experimentText(
"The purpose of this computational experiment is to study",
"the development of {\\tt Seconds} and {\tt Generations} with increasing k",
"for the grammar T5.",
" ",
"The {\\bf problem environment} is the k-symmetry problem: ",
"Finding a boolean expression (with and, or, and not)",
"which is TRUE for symmetric k-bit strings.",
" ",
"The {\\bf solution method} is grammar-based genetic programming",
"(option {\\tt algorithm=\"sgp\"}  of {\\tt xegaRun}).",
"The {\\bf solver} used is {\\tt xegaRun} from the R-package {\\tt xega}.",
" ",
"The experiment consists of 11 treatments, 1 grammar for 11 problem sizes $k\\in 2,\\dots, 12$.",
header="Description of Experiment",
name=experiment, type=type, outpath=outpath)

experimentText(
"The control variable in this experiment is",
"\\begin{itemize}",
"\\item the bit-length of the k-symmetry problem: 2 to 12.",
"       for grammar {\\bf T5}:", 
"\\begin{enumerate}",
"\\item With symmetric pairs.",  
"\\item And a new base function,", 
"       the function sPair(x, y) which is implemented by",
" ",
" sPair $<-$ function(x,y) {OR(AND(x,y),AND(NOT(x),NOT(y)))}",
"\\end{enumerate}",
"\\end{itemize}",
header="Description of Experiment", block=FALSE,
name=experiment, type=type, outpath=outpath)

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

experimentStatistic(treatments, 
                  variable="Fitness",
                  type=type, inpath=inpath, outpath=outpath, name=experiment,
        caption="Fitness. k=2, ... 12.")

experimentStatistic(treatments, 
                  variable="Seconds",
                  type=type, inpath=inpath, outpath=outpath, name=experiment,
        caption="Seconds. k=2, ... 12.")

experimentStatistic(treatments, 
                  variable="Generations",
                  type=type, inpath=inpath, outpath=outpath, name=experiment,
        caption="Generations. k=2, ... 12.")

experimentText(
"\\begin{itemize}",
"\\item For grammar T5: {\\bf No.} Not for the 12-symmetry problem.",  
"\\item For the the 12-symmetry problem, errors range from $0-32$ ",
"       with a mean of $0.80$ and a standard deviation", 
"       of $5.06$ for $40$ trials.",
"\\end{itemize}",
header="Do we always find the optimal program?", block=FALSE,
name=experiment, type=type, outpath=outpath)

experimentSection(name=experiment, 
                  secname="How long to find an optimal solution?", 
                  level="subsection",
                  clearpage=TRUE, type=type, outpath=outpath)

tsr<-"Distribution of Time (s)"
tpvec<-c("sgp02", "sgp03", "sgp04", "sgp05", "sgp06", "sgp07", 
         "sgp08", "sgp09", "sgp10", "sgp11", "sgp12")
for (i in (1:length(tpvec)))
{
experimentBoxPlot(treatments=treatments[grepl(treatments, pattern=tpvec[i])],
                 variable="Seconds",
                 title=paste0(tsr," for Grammar T5. ", (i+1), "-symmetry."),
                 name=experiment, notch=TRUE,
                 inpath=inpath, outpath=outpath, type=type)
}

tsr<-"Distribution of Number of Generations"
for (i in (1:length(tpvec)))
{
experimentBoxPlot(treatments=treatments[grepl(treatments, pattern=tpvec[i])],
                 variable="Generations",
                 title=paste0(tsr," for Grammar T5. ", (i+1), "-symmetry."),
                 name=experiment, notch=TRUE,
                 inpath=inpath, outpath=outpath, type=type)
}

experimentText(
"\\begin{itemize}",
"\\item The 12-symmetry problem did not find the optimum in 1 of out 40 trials",
"\\end{itemize}",
header="How does the algorithm behave with growing k?", block=FALSE,
name=experiment, type=type, outpath=outpath)

experimentSection(name=experiment, 
                  secname="Computational Complexity?",
                  level="subsection",
                  clearpage=TRUE, type=type, outpath=outpath)

tsr<-"Distribution of Number of Generations"

tpvec<-c("T5")
for (i in (1:length(tpvec)))
{
experimentBoxPlot(treatments=treatments[grepl(treatments, pattern=tpvec[i])],
                 variable="Generations",
                 title=paste(tsr,"for Grammar", tpvec[i]),
                 name=experiment, notch=TRUE,
                 inpath=inpath, outpath=outpath, type=type)
}

tsr<-"Distribution of Seconds"
tpvec<-c("T5")
for (i in (1:length(tpvec)))
{
experimentBoxPlot(treatments=treatments[grepl(treatments, pattern=tpvec[i])],
                 variable="Seconds",
                 title=paste(tsr,"for Grammar", tpvec[i]),
                 name=experiment, notch=TRUE,
                 inpath=inpath, outpath=outpath, type=type)
}

experimentText(
"\\begin{itemize}",
"\\item Complexity grows in steps of 2 of k.",
"       The 2 and 3, 4 and 5, 6 and 7, 8 and 9, 10 and 11 took ",
"       a similar number of generations.",
"{\\bf Reason:}", 
"       The 2- and 3-symmetry problem need the same boolean expression,",
"       but with different variables:",  
"       For the 2-symmetry problem, D1 and D2.",  
"       For the 3-symmetry problem, D1 and D3,",  
"       D2 is ignored.",
"\\item The number of generations grows slower than the time needed.",
" ",
"  {\\bf Reason:} The cost of testing a boolean expression grows with $2^k$", 
" ",
"\\end{itemize}",
header="Growth of Complexity?", block=FALSE,
name=experiment, type=type, outpath=outpath)

experimentText(
"Integrate grammar and language tuning",
"into grammar-based genetic programming algorithms!",
"  ",
"{\\bf Mechanisms:}",
"\\begin{itemize}",
"\\item Automatic function definitions.",
"\\item Make optimal solutions of small problem instances reusable.",
"\\item Grammar evolution from frontiers of derivation trees.",   
"\\item Very few, but costly outliers in execution time. (12-symmetry: 5:30h.)",
"\\end{itemize}",
" ",
"Testing by sampling?",
header="Further Research.", 
name=experiment, type=type, outpath=outpath)

###
experimentAppendix(treatments=treatments,
                   name=experiment,
                   miniframe=FALSE,
                   inpath=inpath,
                   outpath=outpath,
                   type=type)

experimentEnd(name=experiment, type=type, outpath=outpath)

