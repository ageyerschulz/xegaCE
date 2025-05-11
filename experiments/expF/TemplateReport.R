
library(kSymmetry)

inpath="merge"
outpath="templateReport"

type="beamer"
#type="latex"
#type="console"

t<-"k-Symmetry: Scalability of Learning by Grammar Tuning and Language Tuning  "
experiment<-"ExpF"

experimentStart(name=experiment, 
   title=t, author="Andreas Geyer-Schulz", 
   purpose=paste(
           "In this experiment we study the complexity of learning",
           " with increasing k",
           "with a grammar with symmetric pairs and an additional function.",
           sep=" "),
   beamertheme="Berkeley", beamercolor="iris",
   type=type, outpath=outpath)

treatments<-experimentTreatments(inpath=inpath)

experimentSection(name=experiment, 
                  secname="Design of Experiment", 
                  level="section",
                  clearpage=TRUE, type=type, outpath=outpath)

experimentDesign(treatments=treatments,
                 inpath=inpath, outpath=outpath, 
                 name=experiment, type=type)

###
experimentAppendix(treatments=treatments,
                   name=experiment,
                   inpath=inpath,
                   outpath=outpath,
                   type=type)

experimentEnd(name=experiment, type=type, outpath=outpath)

