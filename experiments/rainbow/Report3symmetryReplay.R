
library(kSymmetry)

inpath="merge"
outpath="report"

#type="latex"
#type="console"
type="beamer"

t<-"3-Symmetry: Replay. "
experiment<-"ExpC"

experimentStart(name=experiment, 
   title=t, author="Andreas Geyer-Schulz", 
   purpose=paste(
   "This experiment tests exact replication and stochastic replication",
   "for {\\tt xegaRun} of R-package {\\tt xega}.",  
   "In the experiment, the 3-symmetry problem is solved",
   "by grammar-based genetic programming for fixed seeding and for",
   "Rs system seeding (option {\\tt replay} of {\\tt xegaRun}).",    
   sep="\n"),
   type=type, outpath=outpath, theme="Berlin")

treatments<-experimentTreatments(inpath=inpath)

experimentSection(name=experiment, 
                  secname="Design of Experiment", 
                  level="section",
                  clearpage=TRUE, type=type, outpath=outpath)

experimentText(
"{\\bf Exact replicability} of a computational experiment means",
"that the experiment procudes in each trial {\\bf identical}",
"numerical result.",
" ",
"{\\bf Stochastic replicability} of a computational experiment means",
"that the experiment procudes in each trial the same ",
"expected result.",
header="Definitions",
name=experiment, type=type, outpath=outpath)

experimentText(
"The purpose of this computational experiment is to show the difference",
"between {\\bf exact} and {\\bf stochastic} replicability of the experiment.",
" ",
"The {\\bf problem environment} is the 3-symmetry problem: ",
"Finding a boolean expression (with and, or, and not)",
"which is TRUE for symmetric 3-bit strings.",
" ",
"The {\\bf solution method} is grammar-based genetic programming",
"(option {\\tt algorithm=\"sgp\"}  of {\\tt xegaRun}).",
"The {\\bf solver} used is {\\tt xegaRun} from the R-package {\\tt xega}.",
" ",
"The experiment consists of 4 treatments, two for each choice of seeding.",
header="Description of Experiment",
name=experiment, type=type, outpath=outpath)

experimentDesign(treatments=treatments,
                 inpath=inpath, outpath=outpath, 
                 name=experiment, type=type)

experimentText(
"Treatments 1 and 2 test the seeding of the random number generator in xegaRun:",
" ",
"Repetition of a treatment should produce exactly the same result.",
"But different seeds should lead to different results.",
" ",
"Treatments 3 and 4 use the system seed of the random number generator:",
" ",
"Repetitions produce random results.",
" ",
"We expect that for enough trials both treatments have the same mean performance.",
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
"In this experiment, the 3-symmetry problem is {\\bf always} correctly solved.",
" ",
"{\\bf Evidence:}",
"In all trials of all treatments the optimal fitness of $0$ was reached.",
"See next table.",
header="Observation",
              name=experiment, type=type, outpath=outpath)
            
experimentStatistic(treatments=treatments, variable="Fitness", 
                 caption="Fitness (Number of errors).",
                 name=experiment, 
                 inpath=inpath, outpath=outpath, type=type)

experimentSection(name=experiment, 
                  secname="Time of Treatments in Seconds", 
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

experimentText(
"The execution time (in seconds) does not allow to identify deterministic results,",
"because it may depend on other system activity e.g. I/O operations.",
" ",
"{\\bf Evidence:}",
"The time variation in execution time for all treatments.",
 header="Conclusion",
              name=experiment, type=type, outpath=outpath)

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

experimentText(
"The number of generations needed allows to recognize randomness",
"because of non-zero standard deviation.",
" ",
"The number of generations indicates determinism,",
"if the standard deviation is $0$.",
header="Observation",
              name=experiment, type=type, outpath=outpath)

experimentSection(name=experiment,
                  secname="Testing of Effects",
                  level="section",
                  clearpage=TRUE, type=type, outpath=outpath)

experimentSection(name=experiment,
                  secname="Exact Replicability",
                  level="subsection",
                  clearpage=FALSE, type=type, outpath=outpath)

experimentStatistic(treatments=treatments[grepl(treatments, pattern="BoolT1SGPreplayXI")], 
                 variable="Generations", 
                 caption="Distribution of the variable Generations for repeated trials.",  
                 name=experiment, 
                 inpath=inpath, outpath=outpath, type=type)

experimentText(
"Each treatment is deterministic.",
" ",
"{\\bf Evidence:}",
" ",
"The standard deviation $sd$ of each treatment is $0.00$",
"(for any number of trials).",
" ",
"Only 1 solution is found by repeated trials.",
" ",
"Different seeds (may) result in different results:",
"One treatment needs 1 generation, the other 2 generations to reach the optimum.",
header="Result",
              name=experiment, type=type, outpath=outpath)

experimentSection(name=experiment,
                  secname="Stochastic Replicability",
                  level="subsection",
                  clearpage=FALSE, type=type, outpath=outpath)

tsr<-"Comparing a treatment and its repetition. Hypothesis: Equal expected mean number of generations."

experimentBoxPlot(treatments=treatments[grepl(treatments, pattern="BoolT1SGPreplayZ")],
                 variable="Generations",
                 title=tsr,
                 name=experiment,
                 inpath=inpath, outpath=outpath, type=type)

experimentBoxPlot(treatments=treatments[grepl(treatments, pattern="BoolT1SGPreplayZ")],
                 variable="Generations",
                 title=tsr,
                 outline=FALSE, notch=TRUE,
                 name=experiment,
                 inpath=inpath, outpath=outpath, type=type)

experimentStatistic(treatments=treatments[grepl(treatments, pattern="BoolT1SGPreplayZ")], 
                 variable="Generations", 
                 caption="Distribution of the variable Generations for repeated trials.",  
                 name=experiment, 
                 inpath=inpath, outpath=outpath, type=type)

experimentEffectReport(name=experiment,
                treatmentname1="mergeBoolT1SGPreplayZa.rds",
                treatmentname2="mergeBoolT1SGPreplayZb.rds",
                variable="Generations",
                test="t.test",
                mu=0, alternative="two.sided", alpha=0.05, coef=0,
                inpath=inpath, outpath=outpath, type=type)

experimentEffectReport(name=experiment,
                treatmentname1="mergeBoolT1SGPreplayZa.rds",
                treatmentname2="mergeBoolT1SGPreplayZb.rds",
                variable="Generations",
                test="t.test",
                mu=0, alternative="two.sided", alpha=0.05, coef=1.5,
                inpath=inpath, outpath=outpath, type=type)

experimentText(
"Each treatment produces a sample drawn from the {\\bf same} random process.",
" ",
"{\\bf Evidence:}",
" ",
"The standard deviation $sd$ of each treatment is {\\bf not} $0.00$",
"(for any number of trials).",
" ",
"The treatments find 14 and 17 {\\bf different, but equivalent} boolean expressions.",
"(See solution tables in section B Treatements.)",
" ",
"Different seeds (may) result in {\\bf different samples}:",
" ",
"The hypothesis test {\\bf supports} the conjecture", 
"that the sample means of the variable Generations of the treatments are equal.",
"This implies that the two treatments come from the same random process.",
header="Result",
              name=experiment, type=type, outpath=outpath)

#
# Appendix
#

experimentAppendix(treatments=treatments,
                   name=experiment,
                   inpath=inpath,
                   outpath=outpath,
                   type=type)

experimentEnd(name=experiment, type=type, outpath=outpath)

