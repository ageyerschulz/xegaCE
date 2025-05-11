
library(xegaCE)

inpath="merge"
outpath="report"

#type="latex"
#type="console"
type="beamer"

# suppresses detailed results on normality tests of RVs
silent=TRUE

t<-"3-Symmetry: Sequential versus Multicore for GP, GE, and GE by DE. "
experiment<-"ExpA"

experimentStart(name=experiment, 
   title=t, author="Andreas Geyer-Schulz", 
   purpose=paste(
"This experiment compares sequential and multicore learning", 
"with {\\tt xegaRun} for the 3-symmetry problem with nand functions", 
"by three different evolutionary algorithms:",  
"grammar-based genetic programming, grammatical evolution,", 
"and grammatical evolution by differential evolution.", 
"Grammatical evolution by differential evolution is a", 
"novel combination of grammatical evolution and differential evolution.",
   sep="\n"),
   beamertheme="Berlin", beamercolor="cadmiumgreen",
   type=type, outpath=outpath)

treatments<-experimentTreatments(inpath=inpath)

experimentSection(name=experiment, 
                  secname="Design of Experiment", 
                  level="section",
                  clearpage=TRUE, type=type, outpath=outpath)

experimentText(
"In a {\\bf controlled computational experiment} the influence",
"of one or more control parameters on the outcome is studied with",
"regard to a systematic setting of a set of selected control parameters.",
"All other parameters must be known and held constant.",
" ",
"The experiment should be repeatable.",
header="Definitions",
name=experiment, type=type, outpath=outpath)

experimentText(
"The purpose of this computational experiment is",
"to compare sequential ({\\tt executionModel=\"Sequential\"})",
"and multicore ({\\tt executionModel=\"MultiCore\"}) processing",
"for each algorithm,", 
"and to establish a performance ranking of the three algorithms",
"grammar-based genetic programming ({\\tt algorithm=\"sgp\"}),",
"grammatical evolution ({\\tt algorithm=\"sge\"}), and",
"grammatical evolution by differential evolution ({\\tt algorithm=\"sgede\"}).",
" ",
"The {\\bf problem environment} is the 3-symmetry problem:",
"Finding a boolean expression (with the nand function)",
"which is TRUE for symmetric 3-bit strings.",
" ",
"The {\\bf solver} used is {\\tt xegaRun} from the R-package {\\tt xega}.",
" ",
"The experiment consists of 6 treatments, namely the sequential and",
"multicore version of each of the 3 algorithms.",
header="Description of Experiment",
name=experiment, type=type, outpath=outpath)

experimentDesign(treatments=treatments,
                 inpath=inpath, outpath=outpath, 
                 name=experiment, type=type)

experimentText(
"The experiment has two control variables:",
" ",
"{\\tt exectionModel} with 2 levels: {\\tt \"Sequential\"} and {\\tt \"MultiCore\"}.",
" ",
"{\\tt algorithm} with 3 levels: {\\tt \"sgp\"}, {\\tt \"sge\"} and {\\tt \"sgede\"} .",
" ",
"We investigate sequential versus multicore execution and",
" ",
"the performance of the three algorithms.",
              name=experiment, type=type, outpath=outpath)
            
experimentSection(name=experiment, 
                  secname="Exploratory Analysis", 
                  level="section", 
                  clearpage=TRUE, type=type, outpath=outpath)

 experimentSection(name=experiment, 
                   secname="Do we {\\bf always} solve the 3 symmetry problem?", 
                   level="subsection", 
                   clearpage=FALSE, type=type, outpath=outpath)

 experimentText(
 "In this experiment, the 3-symmetry problem is {\\bf not always} correctly solved.",
 " ",
 "{\\bf Evidence:}",
 "In all treatments of the grammatical evolution algorithm (MCSGE and SQSGE), ",
 "in a few trials, non-optimal solutions were observed with a limit of 1000 generations.",
 "See next table.",
 header="Observation",
               name=experiment, type=type, outpath=outpath)
             
 experimentStatistic(treatments=treatments, variable="Fitness", 
                  caption="Fitness (Number of errors).",
                  name=experiment, 
                  inpath=inpath, outpath=outpath, type=type)
 
 experimentText(
 "It is suggested to use the maximal number of generations as additional control variable.",
 "The goal is to find the (conditional) probability that the optimal solution is found",
 "given a resource constraint.", 
 header="Recommendation: New Experiment.",
               name=experiment, type=type, outpath=outpath)
             
 experimentSection(name=experiment, 
                   secname="Time of Treatments in Seconds: ", 
                   level="subsection", 
                   clearpage=FALSE, type=type, outpath=outpath)
 
 experimentBoxPlot(treatments=treatments, 
                   variable="Seconds", 
                   title=t, 
                   name=experiment, 
                   inpath=inpath, outpath=outpath, type=type)
 
 experimentStatistic(treatments=treatments, variable="Seconds", 
                   caption="Time (s).",
                   name=experiment, 
                   inpath=inpath, outpath=outpath, type=type)
                  
 experimentBoxPlot(treatments=treatments, 
                  variable="Seconds", 
                  title=t,
                  outline=FALSE, notch=TRUE,  
                  name=experiment, 
                  inpath=inpath, outpath=outpath, type=type)
 
# experimentText(
# "The execution time (in seconds) does not allow to identify deterministic results,",
# "because it may depend on other system activity e.g. I/O operations.",
# " ",
# "{\\bf Evidence:}",
# "The time variation in execution time for all treatments.",
#  header="Conclusion",
#               name=experiment, type=type, outpath=outpath)
 
 experimentSection(name=experiment, 
                   secname="Number of Generations of Treatments", 
                   level="subsection", 
                   clearpage=TRUE, type=type, outpath=outpath)
 
 experimentBoxPlot(treatments=treatments, 
                  variable="Generations", 
                  title=t,
                  name=experiment, 
                  inpath=inpath, outpath=outpath, type=type)
 
 experimentStatistic(treatments=treatments, variable="Generations", 
                  caption="Generations",  
                  name=experiment, 
                  inpath=inpath, outpath=outpath, type=type)
 
 experimentBoxPlot(treatments=treatments, 
                  variable="Generations", 
                  title=t,
                  outline=FALSE, notch=TRUE,  
                  name=experiment, 
                  inpath=inpath, outpath=outpath, type=type)
 
# experimentText(
# "The number of generations needed allows to recognize randomness",
# "because of non-zero standard deviation.",
# " ",
# "The number of generations indicates determinism,",
# "if the standard deviation is $0$.",
# header="Observation",
#               name=experiment, type=type, outpath=outpath)
 
experimentSection(name=experiment,
                   secname="Multi-core or Sequential?",
                   level="section",
                   clearpage=TRUE, type=type, outpath=outpath)
 
#experimentSection(name=experiment,
#   secname="Should we prefer multi-core over sequential processing?",
#                   level="subsection",
#                   clearpage=FALSE, type=type, outpath=outpath)

experimentText(
 "For each agorithm we have to test:",
 "\\begin{enumerate}",
 "\\item Is the multi-core version faster (in seconds) than the sequential version?",
 " ",
 "\\item Do both of them need the same resources (in generations)?",
 "\\end{enumerate}",
 "Multi-core processing is only preferable if it is faster (in seconds)",
 "when running for the same number of generations.",
 " ",
 "The evaluation of the population of genes is parallelized.",
  header="Should we prefer multi-core or sequential processing?",
            block=FALSE, name=experiment, type=type, outpath=outpath)

# MC/Seq SGP
experimentSection(name=experiment,
                   secname="Multi-core or sequential processing? Algorithm SGP",
                   level="subsection", clearpage=TRUE, type=type, outpath=outpath)
experimentStatistic(treatments=treatments[grepl(treatments, pattern="SGP")], 
                  variable="Seconds", caption="Time in (s) (SGP)", name=experiment, 
                  inpath=inpath, outpath=outpath, type=type)
experimentStatistic(treatments=treatments[grepl(treatments, pattern="SGP")], 
                  variable="Generations", caption="Number of Generations (SGP)", name=experiment, 
                  inpath=inpath, outpath=outpath, type=type)
experimentEffectReport(name=experiment,
                 treatmentname1="mergeSQSGP.rds", treatmentname2="mergeMCSGP.rds",
                 variable="Seconds", test="t.test",
                 mu=0, alternative="less", alpha=0.05, coef=0,
                 silent=silent, inpath=inpath, outpath=outpath, type=type)
experimentEffectReport(name=experiment,
                 treatmentname1="mergeSQSGP.rds", treatmentname2="mergeMCSGP.rds",
                 variable="Seconds", test="t.test",
                 mu=0, alternative="two.sided", alpha=0.05, coef=0,
                 silent=silent, inpath=inpath, outpath=outpath, type=type)
experimentEffectReport(name=experiment,
                 treatmentname1="mergeMCSGP.rds", treatmentname2="mergeSQSGP.rds",
                 silent=silent, variable="Generations", test="t.test",
                 mu=0, alternative="two.sided", alpha=0.05, coef=1.5,
                 inpath=inpath, outpath=outpath, type=type)

# MC/Seq SGE
experimentSection(name=experiment,
                   secname="Multi-core or sequential processing? Algorithm SGE",
                   level="subsection", clearpage=TRUE, type=type, outpath=outpath)
experimentStatistic(treatments=treatments[grepl(treatments, pattern="SGE")], 
                  variable="Seconds", caption="Time in (s) (SGE)", name=experiment, 
                  inpath=inpath, outpath=outpath, type=type)
experimentStatistic(treatments=treatments[grepl(treatments, pattern="SGE")], 
                  variable="Generations", caption="Number of Generations (SGE)", name=experiment, 
                  inpath=inpath, outpath=outpath, type=type)
experimentEffectReport(name=experiment,
                 treatmentname1="mergeSQSGE.rds", treatmentname2="mergeMCSGE.rds",
                 variable="Seconds", test="t.test",
                 mu=0, alternative="less", alpha=0.05, coef=0,
                 silent=silent, inpath=inpath, outpath=outpath, type=type)
experimentEffectReport(name=experiment,
                 treatmentname1="mergeSQSGE.rds", treatmentname2="mergeMCSGE.rds",
                 variable="Seconds", test="t.test",
                 mu=0, alternative="two.sided", alpha=0.05, coef=0,
                 silent=silent, inpath=inpath, outpath=outpath, type=type)
experimentEffectReport(name=experiment,
                 treatmentname1="mergeMCSGE.rds", treatmentname2="mergeSQSGE.rds",
                 variable="Generations", test="t.test",
                 mu=0, alternative="two.sided", alpha=0.05, coef=1.5,
                 silent=silent, inpath=inpath, outpath=outpath, type=type)

# MC/Seq SGV
experimentSection(name=experiment,
                   secname="Multi-core or sequential processing? Algorithm SGV",
                   level="subsection", clearpage=TRUE, type=type, outpath=outpath)
experimentStatistic(treatments=treatments[grepl(treatments, pattern="SGV")], 
                  variable="Seconds", caption="Time in (s) (SGV)", name=experiment, 
                  inpath=inpath, outpath=outpath, type=type)
experimentStatistic(treatments=treatments[grepl(treatments, pattern="SGV")], 
                  variable="Generations", caption="Number of Generations (SGV)", name=experiment, 
                  inpath=inpath, outpath=outpath, type=type)

experimentEffectReport(name=experiment,
                 treatmentname1="mergeSQSGV.rds", treatmentname2="mergeMCSGV.rds",
                 variable="Seconds", test="t.test",
                 mu=0, alternative="less", alpha=0.05, coef=0,
                 silent=silent, inpath=inpath, outpath=outpath, type=type)
experimentEffectReport(name=experiment,
                 treatmentname1="mergeSQSGV.rds", treatmentname2="mergeMCSGV.rds",
                 variable="Seconds", test="t.test",
                 mu=0, alternative="two.sided", alpha=0.05, coef=0,
                 silent=silent, inpath=inpath, outpath=outpath, type=type)
experimentEffectReport(name=experiment,
                 treatmentname1="mergeMCSGV.rds", treatmentname2="mergeSQSGV.rds",
                 variable="Generations", test="t.test",
                 mu=0, alternative="two.sided", alpha=0.05, coef=1.5,
                 silent=silent, inpath=inpath, outpath=outpath, type=type)

experimentSection(name=experiment,
                   secname="Multi-core or sequential processing? Summary",
                   level="subsection", clearpage=TRUE, type=type, outpath=outpath)

experimentText(
 "\\begin{center}",
 "\\begin{tabular}{c|ccc}",
 "\\hline",
 "  Multi-core             & SGP & SGE & SGV \\\\", 
 "\\hline",
 "  ... equal speed or faster ?  & yes & yes & yes \\\\",  
 "  ... equal speed?       &  no &  no & yes \\\\",  
 "  ... same resources?    & yes & yes & yes \\\\",  
 "\\hline",
 "\\end{tabular}",
 "\\end{center}",
 " ",
 "\\vspace{2mm} ",
 "\\begin{itemize}",
 "\\item {\\bf Recommendation:} Use multi-core for 
       population sizes larger than 400.",
 "\\item {\\bf New experiment:} Population size as control variable.",
 "\\end{itemize}",
  header="Multi-core or sequential processing?",
            block=FALSE, name=experiment, type=type, outpath=outpath)

experimentSection(name=experiment,
                   secname="Which algorithm?",
                   level="section", clearpage=TRUE, type=type, outpath=outpath)

experimentText(
 "We compare the performance (time and resources used) of the three algorithms",
 "\\begin{enumerate}",
 "\\item for sequential processing and ",
 " ",
 "\\item for multi-core processing.",
 "\\end{enumerate}",
 " ",
 "The evaluation of the population of genes is parallelized.",
  header="Which algorithm?",
            block=FALSE, name=experiment, type=type, outpath=outpath)

experimentSection(name=experiment,
                   secname="Which algorithm? Sequential processing.",
                   level="subsection", clearpage=TRUE, type=type, outpath=outpath)
experimentStatistic(treatments=treatments[grepl(treatments, pattern="SQ")], 
           variable="Seconds", caption="Time in (s) (Sequential)", name=experiment, 
           inpath=inpath, outpath=outpath, type=type)
# E(SGP) < E(SGE) in seconds.
experimentEffectReport(name=experiment,
                 treatmentname1="mergeSQSGP.rds", treatmentname2="mergeSQSGE.rds",
                 variable="Seconds", test="t.test",
                 mu=0, alternative="greater", alpha=0.05, coef=0,
                 silent=silent, inpath=inpath, outpath=outpath, type=type)
experimentEffectReport(name=experiment,
                 treatmentname1="mergeSQSGP.rds", treatmentname2="mergeSQSGE.rds",
                 variable="Seconds", test="t.test",
                 mu=0, alternative="two.sided", alpha=0.05, coef=0,
                 silent=silent, inpath=inpath, outpath=outpath, type=type)
# E(SGP) < E(SGV) in seconds.
experimentEffectReport(name=experiment,
                 treatmentname1="mergeSQSGP.rds", treatmentname2="mergeSQSGV.rds",
                 variable="Seconds", test="t.test",
                 mu=0, alternative="greater", alpha=0.05, coef=0,
                 silent=silent, inpath=inpath, outpath=outpath, type=type)
experimentEffectReport(name=experiment,
                 treatmentname1="mergeSQSGP.rds", treatmentname2="mergeSQSGV.rds",
                 variable="Seconds", test="t.test",
                 mu=0, alternative="two.sided", alpha=0.05, coef=0,
                 silent=silent, inpath=inpath, outpath=outpath, type=type)
# E(SGE) < E(SGV) in seconds.
experimentEffectReport(name=experiment,
                 treatmentname1="mergeSQSGE.rds", treatmentname2="mergeSQSGV.rds",
                 variable="Seconds", test="t.test",
                 mu=0, alternative="greater", alpha=0.05, coef=0,
                 silent=silent, inpath=inpath, outpath=outpath, type=type)
experimentEffectReport(name=experiment,
                 treatmentname1="mergeSQSGE.rds", treatmentname2="mergeSQSGV.rds",
                 variable="Seconds", test="t.test",
                 mu=0, alternative="two.sided", alpha=0.05, coef=0,
                 silent=silent, inpath=inpath, outpath=outpath, type=type)
experimentSection(name=experiment,
                   secname="Which algorithm? Sequential processing. Generations.",
                   level="subsection", clearpage=TRUE, type=type, outpath=outpath)
experimentStatistic(treatments=treatments[grepl(treatments, pattern="SQ")], 
           variable="Generations", caption="Number of Generations (Sequential)", 
           name=experiment, inpath=inpath, outpath=outpath, type=type)
# E(SGP) < E(SGE) in generations.
experimentEffectReport(name=experiment,
                 treatmentname1="mergeSQSGP.rds", treatmentname2="mergeSQSGE.rds",
                 variable="Generations", test="t.test",
                 mu=0, alternative="greater", alpha=0.05, coef=0,
                 silent=silent, inpath=inpath, outpath=outpath, type=type)
experimentEffectReport(name=experiment,
                 treatmentname1="mergeSQSGP.rds", treatmentname2="mergeSQSGE.rds",
                 variable="Generations", test="t.test",
                 mu=0, alternative="two.sided", alpha=0.05, coef=0,
                 silent=silent, inpath=inpath, outpath=outpath, type=type)
# E(SGP) < E(SGV) in Generations
experimentEffectReport(name=experiment,
                 treatmentname1="mergeSQSGP.rds", treatmentname2="mergeSQSGV.rds",
                 variable="Generations", test="t.test",
                 mu=0, alternative="greater", alpha=0.05, coef=0,
                 silent=silent, inpath=inpath, outpath=outpath, type=type)
experimentEffectReport(name=experiment,
                 treatmentname1="mergeSQSGP.rds", treatmentname2="mergeSQSGV.rds",
                 variable="Generations", test="t.test",
                 mu=0, alternative="two.sided", alpha=0.05, coef=0,
                 silent=silent, inpath=inpath, outpath=outpath, type=type)
# E(SGE) < E(SGV) in Generations.
experimentEffectReport(name=experiment,
                 treatmentname1="mergeSQSGE.rds", treatmentname2="mergeSQSGV.rds",
                 variable="Generations", test="t.test",
                 mu=0, alternative="greater", alpha=0.05, coef=0,
                 silent=silent, inpath=inpath, outpath=outpath, type=type)
experimentEffectReport(name=experiment,
                 treatmentname1="mergeSQSGE.rds", treatmentname2="mergeSQSGV.rds",
                 variable="Generations", test="t.test",
                 mu=0, alternative="two.sided", alpha=0.05, coef=0,
                 silent=silent, inpath=inpath, outpath=outpath, type=type)
###

experimentText(
 "The tests establish the following order of grammar-based genetic programming (SGP), ",
 "grammatical evolution (SGE), and grammatical evolution by differential evolution (SGV):",
 " ",
 "\\begin{displaymath}",
 "\\mbox{SGP} < \\mbox{SGE} < \\mbox{SGV} \\quad\\quad \\mbox{Seconds}",
 "\\end{displaymath}",
 "\\begin{displaymath}",
 "\\mbox{SGP} < \\mbox{SGV} < \\mbox{SGE} \\quad\\quad \\mbox{Generations}",
 "\\end{displaymath}",
 " ",
 "\\begin{itemize}",
 "\\item {\\bf Recommendation:} Use grammar-based genetic programming.",
 "\\item {\\bf Follow-Up:} Investigate the order reversal between SGV and SGE.",
 "   {\\bf Conjecture:} Grammatical evolution by differential evolution (SGV) scales better than SGE.",
 "\\end{itemize}",
  header="Which algorithm? (Sequential)",
            block=FALSE, name=experiment, type=type, outpath=outpath)

### Multi-core
experimentSection(name=experiment,
                   secname="Which algorithm? Multi-core processing",
                   level="subsection", clearpage=TRUE, type=type, outpath=outpath)
experimentStatistic(treatments=treatments[grepl(treatments, pattern="MC")], 
           variable="Seconds", caption="Time in (s) (Multi-Core)", name=experiment, 
           inpath=inpath, outpath=outpath, type=type)
# E(SGP) < E(SGE) in seconds.
experimentEffectReport(name=experiment,
                 treatmentname1="mergeMCSGP.rds", treatmentname2="mergeMCSGE.rds",
                 variable="Seconds", test="t.test",
                 mu=0, alternative="greater", alpha=0.05, coef=0,
                 silent=silent, inpath=inpath, outpath=outpath, type=type)
experimentEffectReport(name=experiment,
                 treatmentname1="mergeMCSGP.rds", treatmentname2="mergeMCSGE.rds",
                 variable="Seconds", test="t.test",
                 mu=0, alternative="two.sided", alpha=0.05, coef=0,
                 silent=silent, inpath=inpath, outpath=outpath, type=type)
# E(SGP) < E(SGV) in seconds.
experimentEffectReport(name=experiment,
                 treatmentname1="mergeMCSGP.rds", treatmentname2="mergeMCSGV.rds",
                 variable="Seconds", test="t.test",
                 mu=0, alternative="greater", alpha=0.05, coef=0,
                 silent=silent, inpath=inpath, outpath=outpath, type=type)
experimentEffectReport(name=experiment,
                 treatmentname1="mergeMCSGP.rds", treatmentname2="mergeMCSGV.rds",
                 variable="Seconds", test="t.test",
                 mu=0, alternative="two.sided", alpha=0.05, coef=0,
                 silent=silent, inpath=inpath, outpath=outpath, type=type)
# E(SGE) < E(SGV) in seconds.
experimentEffectReport(name=experiment,
                 treatmentname1="mergeMCSGE.rds", treatmentname2="mergeMCSGV.rds",
                 variable="Seconds", test="t.test",
                 mu=0, alternative="greater", alpha=0.05, coef=0,
                 silent=silent, inpath=inpath, outpath=outpath, type=type)
experimentEffectReport(name=experiment,
                 treatmentname1="mergeMCSGE.rds", treatmentname2="mergeMCSGV.rds",
                 variable="Seconds", test="t.test",
                 mu=0, alternative="two.sided", alpha=0.05, coef=0,
                 silent=silent, inpath=inpath, outpath=outpath, type=type)
experimentSection(name=experiment,
                   secname="Which algorithm? Multi-Core processing. Generations.",
                   level="subsection", clearpage=TRUE, type=type, outpath=outpath)
experimentStatistic(treatments=treatments[grepl(treatments, pattern="MC")], 
           variable="Generations", caption="Number of Generations (Multi-Core)", 
           name=experiment, inpath=inpath, outpath=outpath, type=type)
# E(SGP) < E(SGE) in generations.
experimentEffectReport(name=experiment,
                 treatmentname1="mergeMCSGP.rds", treatmentname2="mergeMCSGE.rds",
                 variable="Generations", test="t.test",
                 mu=0, alternative="greater", alpha=0.05, coef=0,
                 silent=silent, inpath=inpath, outpath=outpath, type=type)
experimentEffectReport(name=experiment,
                 treatmentname1="mergeMCSGP.rds", treatmentname2="mergeMCSGE.rds",
                 variable="Generations", test="t.test",
                 mu=0, alternative="two.sided", alpha=0.05, coef=0,
                 silent=silent, inpath=inpath, outpath=outpath, type=type)
# E(SGP) < E(SGV) in Generations
experimentEffectReport(name=experiment,
                 treatmentname1="mergeMCSGP.rds", treatmentname2="mergeMCSGV.rds",
                 variable="Generations", test="t.test",
                 mu=0, alternative="greater", alpha=0.05, coef=0,
                 silent=silent, inpath=inpath, outpath=outpath, type=type)
experimentEffectReport(name=experiment,
                 treatmentname1="mergeMCSGP.rds", treatmentname2="mergeMCSGV.rds",
                 variable="Generations", test="t.test",
                 mu=0, alternative="two.sided", alpha=0.05, coef=0,
                 silent=silent, inpath=inpath, outpath=outpath, type=type)
# E(SGE) < E(SGV) in Generations.
experimentEffectReport(name=experiment,
                 treatmentname1="mergeMCSGE.rds", treatmentname2="mergeMCSGV.rds",
                 variable="Generations", test="t.test",
                 mu=0, alternative="greater", alpha=0.05, coef=0,
                 silent=silent, inpath=inpath, outpath=outpath, type=type)
experimentEffectReport(name=experiment,
                 treatmentname1="mergeMCSGE.rds", treatmentname2="mergeMCSGV.rds",
                 variable="Generations", test="t.test",
                 mu=0, alternative="two.sided", alpha=0.05, coef=0,
                 silent=silent, inpath=inpath, outpath=outpath, type=type)

experimentText(
 "\\scriptsize ",
 "The tests establish the following order of grammar-based genetic programming (SGP), ",
 "grammatical evolution (SGE), and grammatical evolution by differential evolution (SGV):",
 " ",
 "\\begin{displaymath}",
 "\\mbox{SGP} < \\mbox{SGE} < \\mbox{SGV} \\quad\\quad \\mbox{Seconds}",
 "\\end{displaymath}",
 "\\begin{displaymath}",
 "\\mbox{SGP} < \\mbox{SGV} < \\mbox{SGE} \\quad\\quad \\mbox{Generations}",
 "\\end{displaymath}",
 "The multi-core and the sequential setting result in the same orders of the algorithms.",
 " ",
 "\\vspace{2mm} ",
 "\\begin{itemize}",
 "\\item {\\bf Recommendation:} Use grammar-based genetic programming.",
 "\\item {\\bf Follow-Up:} Investigate the order reversal between SGV and SGE.",
 " {\\bf Conjecture:} Grammatical evolution by differential evolution (SGV) scales better than SGE.",
 " Investigate the parallelization strategy.",
 "\\end{itemize}",
  header="Which algorithm? (Multi-core)",
            block=FALSE, name=experiment, type=type, outpath=outpath)

# Appendix
experimentAppendix(treatments=treatments,
                   name=experiment,
                   miniframe=FALSE,
                   inpath=inpath,
                   outpath=outpath,
                   type=type)

experimentEnd(name=experiment, type=type, outpath=outpath)

