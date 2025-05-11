
library(xegaCE)

inpath="merge"
outpath="report"

type="beamer"
#type="latex"
#type="console"

t<-"k-Symmetry: Grammar Tuning and Language Tuning"
experiment<-"ExpE"

experimentStart(name=experiment, 
   title=t, author="Andreas Geyer-Schulz", 
   purpose=paste(
           "Grammar tuning adapts the language.", 
           "Language tuning adds new language elements.",
           "In this experiment we compare the best grammar of experiment B",
           "with a grammar with symmetric pairs and an additional function.",
           sep=" "),
   beamertheme="Berlin", beamercolor="ferrarired",
   type=type, outpath=outpath)

treatments<-experimentTreatments(inpath=inpath)

experimentSection(name=experiment, 
                  secname="Design of Experiment", 
                  level="section",
                  clearpage=TRUE, type=type, outpath=outpath)

experimentText(
"{\\bf Grammar tuning} means adding additional production rules",
"to a context-free grammar with the goal of improving the learning",
"performance of grammar-based genetic programming algorithms.",
" ",
"{\\bf Example:} Repeating production rules allows to change the distribution",
"of programs (and their sizes) generated during the initialization a grammar-based",
"genetic programming algorithm.", 
header="Definition",
name=experiment, type=type, outpath=outpath)

experimentText(
"{\\bf Language tuning} means adding additional language elements.",
"This means changing the grammar {\\bf and} the syntax of the language elements",
"and changing the implementation of the language so that the semantics",
"of the language is implemented.",
" ",
"{\\bf Example:} Adding a new base function. E.g. NAND.",
header="Definition",
name=experiment, type=type, outpath=outpath)

experimentText(
"The purpose of this computational experiment is to show the improvement",
"of performance by grammar {\\bf and} language tuning.",
" ",
"The {\\bf problem environment} is the k-symmetry problem: ",
"Finding a boolean expression (with and, or, and not)",
"which is TRUE for symmetric k-bit strings.",
" ",
"The {\\bf solution method} is grammar-based genetic programming",
"(option {\\tt algorithm=\"sgp\"}  of {\\tt xegaRun}).",
"The {\\bf solver} used is {\\tt xegaRun} from the R-package {\\tt xega}.",
" ",
"The experiment consists of 10 treatments, 2 grammars for 5 problem sizes $k\\in 2,\\dots, 6$.",
header="Description of Experiment",
name=experiment, type=type, outpath=outpath)

experimentText(
"The two control variables in this experiment are",
"\\begin{itemize}",
"\\item The bit-length of the k-symmetry problem: 2, 3, 4, 5, and 6.",
"\\item The grammar for boolean expressions:",
"\\begin{itemize} ",
"\\item {\\bf T4}: With two rules for OR and two rules for variables replaced", 
"            by symmetric pairs.",
"\\item {\\bf T5}: With symmetric pairs and a new base function,", 
"       the function sPair(x, y) which implements",
" ",
"sPair $<-$ function)x,y) {OR(AND(x,y),AND(NOT(x),NOT(y)))}",
"\\end{itemize}",
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

experimentMeanMatrix(treatments, tpvec=c("T4", "T5"), 
                  variable="Fitness", statistic="mean", 
                  type=type, inpath=inpath, outpath=outpath, name=experiment,
        caption="Matrix of Mean of Errors (Fitness).  Rows: k=2, 3, 4, 5, 6)")

experimentMeanMatrix(treatments, tpvec=c("T4", "T5"), 
                  variable="Fitness", statistic="min", 
                  type=type, inpath=inpath, outpath=outpath, name=experiment,
        caption="Matrix of Min of Errors (Fitness). Rows: k=2, 3, 4, 5, 6)")

experimentText(
"\\begin{itemize}",
"\\item For grammar T5: {\\bf Yes.}",  
"       For grammar T4: {\\bf No.}", 
"       (Not for the 6-symmetry problem with 500 generations.",   
"\\item For the 6-symmetry problem, the grammar T4 has maximum of 6 errors.",
"\\end{itemize}",
header="Do we always find the optimal program?", block=FALSE,
name=experiment, type=type, outpath=outpath)

experimentSection(name=experiment, 
                  secname="How long to find an optimal solution?", 
                  level="subsection",
                  clearpage=TRUE, type=type, outpath=outpath)

experimentMeanMatrix(treatments, tpvec=c("T4", "T5"), 
                  variable="Seconds", statistic="mean", 
                  type=type, inpath=inpath, outpath=outpath, name=experiment,
        caption="Matrix of Mean of Seconds.  Rows: k=2, 3, 4, 5, 6)")

experimentMeanMatrix(treatments, tpvec=c("T4", "T5"), 
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
"\\item {\\bf Grammar T5} performs {\\bf best} (by two orders of magnitude).",
"        For the 6-symmetry problem:\\\\", 
"       Grammar T4: 460.68 mean(Generations) 100.68 std(Generations).\\\\", 
"       Grammar T5: 7.22 mean(Generations)     4.27 std(Generations).\\\\", 
"       $\\max(\\mbox{Generations}_{T5})=23.00$. \\\\",
"       $\\min(\\mbox{Generations}_{T4})=95.00$. \\\\",
" ",
"Statistically significant? Not tested. Expect highly significant.",
"\\end{itemize}",
header="Which grammar performs best?", block=FALSE,
name=experiment, type=type, outpath=outpath)

experimentSection(name=experiment, 
                  secname="Computational Complexity?",
                  level="subsection",
                  clearpage=TRUE, type=type, outpath=outpath)

tsr<-"Distribution of Number of Generations"
tpvec<-c("T4", "T5")
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
"\\item Complexity grows in steps of 2 of k.",
"       The 2- and 3-symmetry problem need the same boolean expression,",
"       but with different variables:",  
"       For the 2-symmetry problem, D1 and D2.",  
"       For the 3-symmetry problem, D1 and D3,",  
"       D2 is ignored.",
"\\item Grammar T5 scales well with problem size growth.",
" ",
"  {\\bf Reason:} The combination of {\\bf grammar} and", 
"        {\\bf language} tuning.",
" ",
"The generation of pairs of symbols",
"                 (grammar tuning) \\\\",
"  and the new function sPair. It takes a ",
"  symbol pair as its arguments eliminates the need to expand a ",
"  a non-terminal twice with the same symbol pair",
"  (language tuning).", 
"\\end{itemize}",
header="Growth of Complexity?", block=FALSE,
name=experiment, type=type, outpath=outpath)

experimentText(
"Integrate grammar and language tuning",
"into grammar-based genetic programming algorithms.",
"  ",
"{\\bf Mechanisms:}",
"Automatic function definitions e.g. from from best solutions for small $k$.",
" ",
"Grammar evolution from frontiers of derivation trees.",   
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

