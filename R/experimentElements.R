
# experimentElements.
# (c) 2025 Andreas Geyer-Schulz

#' Beamer table length.
#'
#' @return Integer. Number of lines in beamer table.
#'
#' @family Internal Configuration
#'
#' @examples
#' beamerTableLength()
#'
#' @export
beamerTableLength<-function() {return(15)}

#' latex table length.
#'
#' @return Integer. Number of lines in beamer table.
#'
#' @family Internal Configuration
#'
#' @examples
#' latexTableLength()
#'
#' @export
latexTableLength<-function() {return(40)}

#' Start of experiment.
#'
#' @param name    Name of experiment. Default: "Missing".
#' @param title   Title of the experiment. Default: "Undefined".
#' @param author  Author of document. Default: "Unknown".
#' @param purpose Purpose of experiment. Default: "Unknown".
#' @param beamertheme Beamer layout. Default: "Berlin".
#'              Choose from "default", "AnnArbor", "Antibes", "Bergen", 
#'              "Berkeley", "Berlin", "Boadilla", "CambridgeUS", 
#'              "Copenhagen", "Darmstadt", "Dresden", "Frankfurt", 
#'              "Goettingen", "Hannover", "Ilmenau", "JuanLesPins", 
#'              "Luebeck", "Madrid", "Malmoe", "Marburg", "Montpellier",
#'              "PaloAlto", "Pittsburgh", "Rochester", "Singapore", 
#'              "Szeged", "Warsaw". 
#' @param beamercolor Color of beamer structure elements. 
#'                    Setting \code{beamercolor="default"} uses the 
#'                    standard color scheme of the beamer theme.
#'          Default: "electricultramarine".
#'        \itemize{
#'        \item Yellow(s): "goldenyellow", "electricyellow", "icterine",
#'                         "flavescent", "lemon".
#'        \item Orange(s): "amber", "cadmiumorange", "internationalorange".
#'        \item Red(s): "ferrarired", "fireenginered", "cadmiumred".
#'        \item Blue(s): "ao", "babyblueeyes", "bleudefrance", "blue",
#'                       "cobalt", "darkmidnightblue", "brandeisblue",
#'                       "deepskyblue", "iris", "navyblue", "ultramarine",
#'                       "electricultramarine".
#'        \item Green(s): "darkpastelgreen", "cadmiumgreen".
#'         }
#' @param type  Output to console (type="console") 
#'              or to latex files (type="latex" | "beamer")?  
#'              Default: "console".
#' @param outpath  Path for latex output files. Default: "".              
#'
#' @return 0 (invisible).
#'
#' @family Report of Experiment
#'
#' @examples
#' experimentStart(name="Experiment A.", title="First test", author="U.N. Owen")
#' 
#' @export
experimentStart<-function(name="Missing", 
   title="Undefined", author="Unknown", purpose="Unkown", 
   beamertheme="Berlin", beamercolor="electricultramarine",
   type="console", outpath=".")
{ if (type=="console") 
  {consoleProlog(name=name, title=title, author=author, purpose=purpose)}
  if (type=="latex") 
  {y<-latexProlog(name=name, title=title, author=author, purpose=purpose)
   writeText(y, newFileName(fn="main", ftype="tex", expname=name, path=outpath))} 
  if (type=="beamer") 
  {y<-beamerProlog(name=name, title=title, author=author, 
                   purpose=purpose, 
                   beamertheme=beamertheme, beamercolor=beamercolor)
   writeText(y, newFileName(fn="main", ftype="tex", expname=name, path=outpath))} 
  invisible(0)}

#' Build latex main program.
#'
#' @description \code{buildLatexMain()}
#'              reads all latex files starting with "main" from the 
#'              directory specified by path, catenates them and writes
#'              a new latex file "<main>main.tex" to the same directory.
#'
#' @param name  Name of experiment. 
#' @param path  Path of latex report files.
#'
#' @return List of imported files. (invisible).
#'
#' @family Report of Experiment (Internal)
#'
#' @importFrom xegaBNF readBNF
#' @export
buildLatexMain<-function(name, path=".")
{ fn<-paste0(path, .Platform$file.sep, "main", name,".tex")
LOM<-sort(list.files(path=path, pattern="*\\.tex"))
LOM<-LOM[grepl(LOM, pattern=paste0(name, "main"))]
fullLOM<-fullFileNames(treatments=LOM, inpath=path)
all<-lapply(fullLOM, FUN=xegaBNF::readBNF)
txt<-""
for (i in 1:(length(LOM)))
    { txt<-paste0(txt, "% ", all[[i]]$filename, "\n", all[[i]]$BNF)}
writeText(text=txt, fn=fn)
invisible(LOM) }

#' End of experiment.
#'
#' @param name Name of experiment. Default: "Missing".
#' @param type  Output to console (type="console") 
#'              or to latex files (type="latex" | "beamer")?  
#'              Default: "console".
#' @param outpath  Path for latex output files. Default: "".              
#'
#' @return 0 (invisible).
#'
#' @family Report of Experiment
#'
#' @examples
#' experimentEnd()
#' @export
experimentEnd<-function(name="", type="console", outpath=".")
{ if (type=="console") 
  {consoleEndDocument()}
  if (type=="latex") 
  {y<-latexEndDocument()
   writeText(y, newFileName(fn="main", ftype="tex", 
                            expname=name, path=outpath)) 
  buildLatexMain(name=name, path=outpath)}
  if (type=="beamer") 
  {y<-beamerEndDocument()
   writeText(y, newFileName(fn="main", ftype="tex", 
                            expname=name, path=outpath))
  buildLatexMain(name=name, path=outpath)}
  invisible(0)}

#' Sections of an experiment
#'
#' @param name      Name of experiment. (
#' @param secname   Name of section.
#' @param level     Latex markup of the document element.
#'                  Default: "section". Latex supports:
#'                  "section", "subsection", "subsubsection".
#'                  Used for indentation of output.
#' @param clearpage Boolean. Default: TRUE. 
#'                  Ignored.
#' @param type  Output to console (type="console") 
#'              or to latex files (type="latex" | "beamer")?  
#'              Default: "console".
#' @param miniframe Boolean. Default: TRUE.
#'                 If false, turns of miniframes for this section.
#' @param outpath  Path for latex output files. Default: "".              
#'                 
#' @return 0 (invisible).
#'
#' @family Report of Experiment
#'
#' @examples
#' experimentSection(name="Experiment A", 
#'                   secname="Design", level="section", clearpage=FALSE)
#'                  
#' @export
experimentSection<-function(name="Missing experiment", 
                   secname="Missing section", 
                   level="section", clearpage=TRUE, 
                   type="console", miniframe=TRUE, outpath=".")
{ if (type=="console") 
  {consoleSection(secname, level, clearpage)}
  if (type=="latex") 
  {y<-latexSection(secname, level, clearpage)
  writeText(y, newFileName(fn="main", ftype="tex", expname=name, path=outpath))}
  if (type=="beamer") 
  {y<-beamerSection(secname, level, clearpage, miniframe)
  writeText(y, newFileName(fn="main", ftype="tex", expname=name, path=outpath))}
  invisible(0)}

#
# Textblock
#

#' Description (a block of text).
#'
#' @param ...       A list of comma separated strings.
#' @param header Name of the block.
#' @param block  Default: TRUE: frame(block(text)) else
#'                       FALSE: frame(text).
#' @param name   Default: "".
#' @param type  Output to console (type="console") 
#'              or to latex files (type="latex" | "beamer")?  
#'              Default: "console".
#' @param outpath  Path for latex output files. Default: "".              
#'                 
#' @return 0 (invisible).
#'
#' @family Report of Experiment
#'
#' @examples
#' experimentText(
#' "This text describes my experiment.", 
#' "However, it is really neat.") 
#'                  
#' @export
experimentText<-function(..., header="", name="", block=TRUE, 
                         type="console", outpath=".")
{ if (type=="console") 
  {consoleText(..., header=header)}
  if (type=="latex") 
  {y<-latexText(..., header=header)
  writeText(y, newFileName(fn="main", ftype="tex", expname=name, path=outpath))}
  if (type=="beamer") 
  {y<-beamerText(..., header=header, block=block)
  writeText(y, newFileName(fn="main", ftype="tex", expname=name, path=outpath))}
  invisible(0)}

#
# Documentation of elements of an experiment.
# 

#' Get list of files with treatment results.
#'
#' @param pattern  A regular expression for selecting filenames of treatments.
#'                 Assumption: One rds file per treatment.
#'                 Default: "*\\.rds".
#' @param inpath   Path for treatment files. Default: ".". 
#'
#' @return List of filenames with treatments.
#'
#' @family Report of Experiment
#' 
#' @examples
#' experimentTreatments()
#'
#' @export
experimentTreatments<-function(pattern="*\\.rds", inpath=".")
{
return(list.files(path=inpath, pattern=pattern))
}

#' Produces box-and-whisker plots of treatments of an experiment.
#'
#' See the documentation of base::boxplot.
#'
#' @param treatments     List of filenames of treatments.
#' @param variable       Name of variable. 
#' @param title          Title of Boxplot. Default: "".
#' @param name           Name of experiment. Default: "".
#' @param outline        Boolean. If FALSE, remove outliers. Default: TRUE.
#' @param notch          Boolean. If TRUE, notches are drawn. Default: FALSE.
#'                       Nonoverlapping notches indicate different 
#'                       medians of treatments.
#' @param horizontal     Boolean. Default: TRUE. Plots are horizontal.
#' @param varwidth       Boolean. Default: FALSE.
#'                       If TRUE,  width of the plot indicates sample size.
#' @param inpath         Path to treatment files of experiment.
#'                       Default: ".".
#' @param outpath        Path to report files.
#' @param type  Output to console (type="console") 
#'              or to latex files (type="latex" | "beamer")?  
#'              Default: "console".
#'
#' @return 0 (invisible).
#' 
#' @family Report of Experiment
#'
#' @examples
#' cat("Example: TODO.\n")
#'
#' @export
experimentBoxPlot<-function(treatments, variable, title="",
                outline=TRUE, notch=FALSE, horizontal=TRUE, varwidth=FALSE,
                name="", inpath=".", outpath=".", type="console")
{ if (type=="console") 
  { 
    consoleBoxPlot(treatments=treatments, variable=variable, title="",
                outline=outline, notch=notch, 
                horizontal=horizontal, varwidth=varwidth,
                name=name, inpath=inpath)

    consoleFigure(name=name, caption=title)}
  if (type=="latex") 
  {fnName<-latexBoxPlot(treatments=treatments, variable=variable, title="",
                        outline=outline, notch=notch, 
                        horizontal=horizontal, varwidth=varwidth,
                name=name, inpath=inpath, outpath=outpath)
    y<-latexFigure(name, fnName, caption=title, label=fnName)  
  writeText(y, newFileName(fn="main", ftype="tex", expname=name, path=outpath))}
  if (type=="beamer") 
  {fnName<-beamerBoxPlot(treatments=treatments, variable=variable, title="",
                        outline=outline, notch=notch, 
                        horizontal=horizontal, varwidth=varwidth,
                name=name, inpath=inpath, outpath=outpath)
    y<-beamerFigure(name, fnName, caption=title, label=fnName)  
  writeText(y, newFileName(fn="main", ftype="tex", expname=name, path=outpath))}

  invisible(0)}

#' Produces a plot of population statistics of \code{xegaRun}.
#'
#' @description See the documentation xegaPlotPopStats.
#'
#' @param treatmentname  Name of rds-file of treatment.
#' @param name           Name of experiment. Default: "".
#' @param inpath         Path to treatment files of experiment.
#'                       Default: ".".
#' @param outpath        Path to report files.
#' @param type  Output to console (type="console") 
#'              or to latex files (type="latex" | "beamer")?  
#'              Default: "console".
#'
#' @return 0 (invisible).
#' 
#' @family Report of Experiment
#'
#' @examples
#' cat("Example: TODO.\n")
#'
#' @export
experimentPlotPopStats<-function(treatmentname, 
                name="", inpath=".", outpath=".", type="console")
{ obj<-getTreatment(treatmentname=treatmentname, inpath=inpath)
  description<-paste0("Treatment ", obj$tArgs$treatmentName, 
                      " of Experiment ", name) 
  if (type=="console") 
  { 
    xegaPlotPopStats(xegaResult=obj$lastGAResult, 
                              description=description)
    consoleFigure(name=name, caption=description)}
  if (type=="latex")
  { fn<-newFileName(fn="PlotPopStatsFigure", ftype="eps", 
                    expname=name, path=outpath)
    xegaPlotPopStats(xegaResult=obj$lastGAResult, 
                              epsfile = TRUE, 
                              filename = fn,
                              description=description)
    title<-paste0("Plot of last xegaRun for ", description)
    newfn<-unlist(strsplit(fn, .Platform$file.sep))
    newfn<-newfn[length(newfn)]
    y<-latexFigure(name, newfn, caption=title, label=fn)
  writeText(y, newFileName(fn="main", ftype="tex", expname=name, path=outpath))}
  if (type=="beamer")
  { fn<-newFileName(fn="PlotPopStatsFigure", ftype="eps", 
                    expname=name, path=outpath)
    xegaPlotPopStats(xegaResult=obj$lastGAResult, 
                              epsfile = TRUE, 
                              filename = fn,
                              description=description)
    title<-paste0("Plot of last xegaRun for ", description)
    newfn<-unlist(strsplit(fn, .Platform$file.sep))
    newfn<-newfn[length(newfn)]
    y<-beamerFigure(name, newfn, caption=title, label=fn)
  writeText(y, newFileName(fn="main", ftype="tex", expname=name, path=outpath))}

  invisible(0)}

#' Plots the derivation tree of a solution.
#'
#' @description For \code{algorithm=="sgp"}, the genotype of a solution
#'              is a complete derivation tree.  
#'
#' @param treatmentname  Name of rds-file of treatment.
#' @param name           Name of experiment. Default: "".
#' @param inpath         Path to treatment files of experiment.
#'                       Default: ".".
#' @param outpath        Path to report files.
#' @param type  Output to console (type="console") 
#'              or to latex files (type="latex" | "beamer")?  
#'              Default: "console".
#'
#' @return 0 (invisible).
#' 
#' @family Report of Experiment
#'
#' @examples
#' cat("Example: TODO.\n")
#'
#' @importFrom grDevices cairo_pdf
#' @importFrom grDevices dev.off
##' @importFrom graphics  lines
##' @importFrom graphics  mtext
##' @importFrom graphics  title
#' @importFrom igraph graph_from_data_frame
#' @importFrom igraph layout_as_tree
#' @importFrom xegaDerivationTrees treeToDataFrames
#' @export
experimentSolutionGenotype<-function(treatmentname, 
                name="", inpath=".", outpath=".", type="console")
{ obj<-getTreatment(treatmentname=treatmentname, inpath=inpath)
  if (!(obj$lastGAResult$GAenv$algorithm=="sgp")) { return(invisible(0))}
  grammar<-obj$lastGAResult$GAenv$grammar
  gene<-obj$lastGAResult$solution$genotype$gene1
  description<-paste0("The Derivation Tree of a Solution of Treatment ", 
                      obj$tArgs$treatmentName, " of Experiment ", name) 
  x<-xegaDerivationTrees::treeToDataFrames(gene, grammar, verbose=FALSE)
  g1<-igraph::graph_from_data_frame(x$E, directed=TRUE, vertices=x$V)
  if (type=="console") 
  { 
    plot(g1, layout=igraph::layout_as_tree)
    consoleFigure(name=name, caption=description)}
  if (type=="latex")
  { fn<-newFileName(fn="DerivationTreeFigure", ftype="pdf", 
                    expname=name, path=outpath)
    rc<-grDevices::cairo_pdf(filename=fn)
    plot(g1, layout=layout_as_tree)
    rc<-grDevices::dev.off()
    newfn<-unlist(strsplit(fn, .Platform$file.sep))
    newfn<-newfn[length(newfn)]
    y<-latexFigure(name, newfn, caption=description, label=fn, 
                   width=0.9, angle=0)
  writeText(y, newFileName(fn="main", ftype="tex", expname=name, path=outpath))}
  if (type=="beamer")
  { fn<-newFileName(fn="DerivationTreeFigure", ftype="pdf", 
                    expname=name, path=outpath)
    rc<-grDevices::cairo_pdf(filename=fn)
    plot(g1, layout=layout_as_tree)
    rc<-grDevices::dev.off()
    newfn<-unlist(strsplit(fn, .Platform$file.sep))
    newfn<-newfn[length(newfn)]
    y<-beamerFigure(name, newfn, caption=description, label=fn, 
                   width=0.5, angle=0)
  writeText(y, newFileName(fn="main", ftype="tex", expname=name, path=outpath))}

  invisible(0)}

### Tables.

#' Produces tables of statistics of experiment.
#'
#'
#' @description Specify either treatment name or variable name or 
#'              none. Produces tables with statistical result 
#'              of experiment. 
#'
#' @param treatments      List of treatment names of experiment.
#' @param treatment       Name of treatment. Default: NULL.
#' @param variable        Name of variable.  Default: NULL.
#' @param inpath          Path to treatment rds-files of experiment.
#'                          Default: ".".
#' @param outpath          Path to report of experiment. Default: "." 
#' @param caption         Caption of table. Default: "".
#'                        Default: ".".
#' @param name            Name of experiment. Default: "".
#' @param type  Output to console (type="console") 
#'              or to latex files (type="latex" | "beamer")?  
#'              Default: "console".
#'
#' @return 0 (invisible). 
#'
#' @family Report of Experiment
#' 
#' @examples 
#' cat("TBD! \n")
#'
#' @export
experimentStatistic<-function(treatments, treatment=NULL, variable=NULL,
              inpath=".", outpath=".", caption="", name="", type="console")
{ df<-selectFromExperiment(treatments=treatments, treatment=treatment, 
   variable=variable, inpath=inpath)
if (type=="console") 
   {consoleTable(df=df, caption=caption)}
if (type=="latex") 
  { latexTables(df=df, name=name, fnName="StatsTable", mfnName="main",
              caption=caption, tableLength=latexTableLength(), path=outpath)}
if (type=="beamer") 
  { beamerTables(df=df, name=name, fnName="StatsTable", mfnName="main",
              caption=caption, tableLength=beamerTableLength(), path=outpath)}

invisible(0) } 

#' Produces matrix of mean vectors  of statistics of experiment.
#'
#'
#' @description Specify either treatment name or variable name or 
#'              none. Produces tables with statistical result 
#'              of experiment. 
#'
#' @param treatments      List of treatment names of experiment.
#' @param tpvec           Vector of treatment patterns.
#' @param variable        Name of variable.  Default: NULL.
#' @param statistic       Name of statistic. Default: "mean".
#' @param inpath          Path to treatment rds-files of experiment.
#'                          Default: ".".
#' @param outpath          Path to report of experiment. Default: "." 
#' @param caption         Caption of table. Default: "".
#'                        Default: ".".
#' @param name            Name of experiment. Default: "".
#' @param type  Output to console (type="console") 
#'              or to latex files (type="latex" | "beamer")?  
#'              Default: "console".
#'
#' @return 0 (invisible). 
#'
#' @family Report of Experiment
#' 
#' @examples 
#' cat("TBD! \n")
#'
#'@export
experimentMeanMatrix<-function(treatments, tpvec="", variable=NULL, statistic="mean",
              inpath=".", outpath=".", caption="", name="", type="console")
{ df<-selectFromExperiment(treatments=treatments, treatment=NULL, 
   variable=variable, inpath=inpath)
  meanDF<-as.data.frame(df[grepl(treatments, pattern=tpvec[1]), statistic])  
  for (i in (2:length(tpvec)))
  { meanDF<-cbind(meanDF, df[grepl(treatments, pattern=tpvec[i]),statistic]) }
  colnames(meanDF)<-tpvec
if (type=="console")
   {consoleTable(df=meanDF, caption=caption)}
if (type=="latex")
  { latexTables(df=meanDF, name=name, fnName="MeanMatrixTable", mfnName="main",
              caption=caption, tableLength=latexTableLength(), path=outpath)}
if (type=="beamer")
  { beamerTables(df=meanDF, name=name, fnName="MeanMatrixTable", mfnName="main",
              caption=caption, tableLength=beamerTableLength(), path=outpath)}

invisible(0)
}

#' A table of the parameters and their values of a treatment.
#'
#' @param treatmentName    Name of treatment.
#' @param inpath          Path to treatment rds-files of experiment.
#'                          Default: ".".
#' @param outpath          Path to report of experiment. Default: "." 
#' @param caption         Caption of table. Default: "".
#'                        Default: ".".
#' @param name            Default: "".
#' @param type  Output to console (type="console") 
#'              or to latex files (type="latex" | "beamer")?  
#'              Default: "console".
#'
#' @return Invisible 0.
#'
#' @family Report of Experiment
#' 
#' @examples
#' cat("TBD!\n")
#'
#' @export
experimentTreatmentParameters<-function(treatmentName, 
          inpath=".", outpath=".", caption="", name="", type="console")
{  treatmentdf<-readRDS(file.path(inpath, treatmentName))
   df<-pl2df(treatmentdf$tArgs)
   ncaption<-paste(caption, "Parameters of treatment:", 
                   treatmentdf$tArgs$treatmentName, "\n")
if (type=="console")
   {consoleTable(df=df, caption=ncaption)}
if (type=="latex")
  { latexTables(df=df, name=name, fnName="tParmTable", mfnName="main",
              caption=ncaption, tableLength=latexTableLength(), path=outpath)}
if (type=="beamer")
  { beamerTables(df=df, name=name, fnName="tParmTable", mfnName="main",
              caption=ncaption, tableLength=beamerTableLength(), path=outpath)}
invisible(0) }

#' A table of the xega parameters and their values of a treatment.
#'
#' @param treatmentName    Name of treatment.
#' @param inpath          Path to treatment rds-files of experiment.
#'                          Default: ".".
#' @param outpath          Path to report of experiment. Default: "." 
#' @param caption         Caption of table. Default: "".
#'                        Default: ".".
#' @param name            Default: "".
#' @param type  Output to console (type="console") 
#'              or to latex files (type="latex" | "beamer")?  
#'              Default: "console".
#'
#' @return Invisible 0.
#'
#' @family Report of Experiment
#' 
#' @examples
#' cat("TBD!\n")
#'
#' @export
experimentXegaParameters<-function(treatmentName, 
          inpath=".", outpath=".", caption="", name="", type="console")
{  treatmentdf<-readRDS(file.path(inpath, treatmentName))
   df<-pl2df(treatmentdf$xegaArgs)
   ncaption<-paste(caption, "Parameters of treatment", 
                   treatmentdf$tArgs$treatmentName, "passed to xegaRun\n")
if (type=="console")
   {consoleTable(df=df, caption=ncaption)}
if (type=="latex")
  { latexTables(df=df, name=name, fnName="tParmTable", mfnName="main",
              caption=ncaption, tableLength=latexTableLength(), path=outpath)}
if (type=="beamer")
  { beamerTables(df=df, name=name, fnName="tParmTable", mfnName="main",
              caption=ncaption, tableLength=beamerTableLength(), path=outpath)}
invisible(0) }

#' A table of all xega parameters and their values of the a treatment.
#'
#' @param treatmentName    Name of treatment.
#' @param inpath          Path to treatment rds-files of experiment.
#'                          Default: ".".
#' @param outpath          Path to report of experiment. Default: "." 
#' @param caption         Caption of table. Default: "".
#'                        Default: ".".
#' @param name            Default: "".
#' @param type  Output to console (type="console") 
#'              or to latex files (type="latex" | "beamer")?  
#'              Default: "console".
#'
#' @return Invisible 0.
#'
#' @family Report of Experiment
#' 
#' @examples
#' cat("TBD!\n")
#'
#' @export
experimentAllXegaParameters<-function(treatmentName, 
          inpath=".", outpath=".", caption="", name="", type="console")
{  treatmentdf<-readRDS(file.path(inpath, treatmentName))
   df<-pl2df(treatmentdf$lastGAResult$GAenv)
   ncaption<-paste(caption, "All parameters of xegaRun of treatment", 
                   treatmentdf$tArgs$treatmentName, "\n")
if (type=="console")
   {consoleTable(df=df, caption=ncaption)}
if (type=="latex")
  { latexTables(df=df, name=name, fnName="tParmTable", mfnName="main",
              caption=ncaption, tableLength=latexTableLength(), path=outpath)}
if (type=="beamer")
  { beamerTables(df=df, name=name, fnName="tParmTable", mfnName="main",
              caption=ncaption, tableLength=beamerTableLength(), path=outpath)}
invisible(0) }

#' Produces tables of common and different parameters of treatments.
#'
#'
#' @description The table of different parameters of treatments
#'              and the table of common parameters of treatments
#'              are traditionally called the design of the experiment.
#'              The common parameters should make the experiment replicable,
#'              The behaviorally relevant different parameters provide
#'              the basis of testing hypothesis about the effects 
#'              caused by these parameters.
#'
#' @param treatments      List of treatment names of experiment.
#' @param inpath          Path to treatment rds-files of experiment.
#'                          Default: ".".
#' @param outpath          Path to report of experiment. Default: "." 
#' @param caption         Caption of table. Default: "".
#'                        Default: ".".
#' @param name            Name of experiment. Default: "".
#' @param type  Output to console (type="console") 
#'              or to latex files (type="latex" | "beamer")?  
#'              Default: "console".
#' 
#'
#' @return 0 (invisible). 
#'
#' @family Report of Experiment
#' 
#' @examples 
#' cat("TBD! \n")
#'
#' @importFrom xegaBNF dataframePT
#' @export
experimentDesign<-function(treatments,
          inpath=".", outpath=".", caption="", name="", type="console")
{ dfs<-designOfExperiment(treatments=treatments, inpath=inpath)
if (type=="console") 
   { consoleTable(df=t(dfs$common), 
         caption=paste0("Common Parameters of Experiment ", name))
    consoleTable(df=t(dfs$different), 
         caption=paste0("Different Parameters of Experiment ", name))
   }
if (type=="latex") 
  { newdf<-t(dfs$common)
    colnames(newdf)<-c("Parameter Value")
    latexTables(df=newdf, name=name, fnName="CommonTable", mfnName="main",
         caption=paste0("Common Parameters of Experiment ", name),
         tableLength=latexTableLength(), path=outpath)
    newdfdif<-t(dfs$different)
    rownames(newdfdif)<-1:nrow(newdfdif)
    latexTables(df=newdfdif, name=name, fnName="DifferentTable", mfnName="main",
              caption=paste0("Parameters of Treatments of Experiment ", name),
              tableLength=latexTableLength(), path=outpath)}

if (type=="beamer") 

  { newdf<-t(dfs$common)
    colnames(newdf)<-c("Parameter Value")
    beamerTables(df=newdf, name=name, fnName="CommonTable", mfnName="main",
         caption=paste0("Common Parameters of Experiment ", name),
         tableLength=beamerTableLength(), path=outpath)
    newdfdif<-t(dfs$different)
    rownames(newdfdif)<-1:nrow(newdfdif)
    beamerTables(df=newdfdif, name=name, fnName="DifferentTable", mfnName="main",
              caption=paste0("Parameters of Treatments of Experiment ", name),
              tableLength=beamerTableLength(), path=outpath)}

experimentGrammarTable(treatmentname=treatments[1],
                       name=name, common=TRUE,
                       inpath=inpath, outpath=outpath, type=type)

invisible(0) } 

#' A solution table.
#'
#' @param treatmentname   Name of rds-file of treatment.
#' @param name            Name of experiment. Default: "".
#' @param inpath          Path to treatment rds-files of experiment.
#'                          Default: ".".
#' @param outpath          Path to report of experiment. Default: "." 
#' @param type  Output to console (type="console") 
#'              or to latex files (type="latex" | "beamer")?  
#'              Default: "console".
#' @param n               Number of solutions. Default: 5.
#' 
#' @return 0 (invisible).
#'
#' @family Report of Experiment
#'
#' @examples
#' cat("TBD!\n")
#'
#' @export
experimentSolutionTable<-function(treatmentname="", name="",
                     inpath=".", outpath=".", type="console", n=5)
{ obj<-getTreatment(treatmentname=treatmentname, inpath=inpath)
  if (is.na(obj$resultDF$Solution[1])) {return(invisible(0))}
  newdf<-obj$resultDF
  fit<-newdf$Fit
  minfit<-min(fit)
  newdf<-newdf[(minfit==fit),]
  newdf<-newdf$Solution
  solvec<-unique(newdf)
  nsol<-length(solvec)
  l<-unlist(lapply(solvec, FUN=nchar))
  solvec<-solvec[(l==min(l))]
  solvec<-solvec[min(n,length(solvec))]
  newsolvec<-unlist(lapply(solvec, FUN=function(x) {gsub(",", ", ", x)}))
  df<-data.frame(Solution=newsolvec)
  caption<-paste0("The Solution Table of Treatment ", obj$tArgs$treatmentName,  
                  " of Experiment ", name, 
                  ". Fit: ", minfit, ". Unique Shortest Solutions: ", nsol, ".")
if (type=="console") 
    {consoleTable(df=df, 
     caption=caption, name)}
if (type=="latex")
  { latexTables(df=df, name=name, fnName="SolutionTable", mfnName="main",
              caption=caption, align="rp{12cm}", 
              tableLength=latexTableLength(), path=outpath)}
if (type=="beamer")
  { beamerTables(df=df, name=name, fnName="SolutionTable", mfnName="main",
              caption=caption, align="rp{9cm}", 
              tableLength=beamerTableLength(), path=outpath)}
invisible(0) } 

#' The solution of a feedforward NN.
#'
#' @param treatmentname   Name of rds-file of treatment.
#' @param name            Name of experiment. Default: "".
#' @param inpath          Path to treatment rds-files of experiment.
#'                          Default: ".".
#' @param outpath          Path to report of experiment. Default: "." 
#' @param type  Output to console (type="console") 
#'              or to latex files (type="latex" | "beamer")?  
#'              Default: "console".
#' @param n               Number of solutions. Default: 5.
#' 
#' @return 0 (invisible).
#'
#' @family Report of Experiment
#'
#' @examples
#' cat("TBD!\n")
#'
#' @export
experimentNNTables<-function(treatmentname="", name="",
                     inpath=".", outpath=".", type="console", n=1)
{ obj<-getTreatment(treatmentname=treatmentname, inpath=inpath)
  penv<-obj$lastGAResult$GAenv$penv
if (!("nNNparms" %in% names(obj$lastGAResult$GAenv$penv))) 
                                          {return(invisible(0))}
  Solution<-list(Fitness=obj$lastGAResult$solution$fitness, 
       Errors=penv$error(obj$lastGAResult$solution$phenotype))
  solutionDF<-as.data.frame(Solution)
  caption<-paste("Solution of treatment", obj$tArgs$treatmentName, sep=" ")
  if (penv$k()<6)
  {casesDF<-cbind((penv$kSymmetryTable(penv$k())[,1:penv$k()]), 
                  penv$errorTable(obj$lastGAResult$solution$phenotype))}
  else
  {casesDF<-penv$errorTable(obj$lastGAResult$solution$phenotype)}
  caption1<-paste("Cases of treatment", obj$tArgs$treatmentName, sep=" ")
  NNweights<-P2NN(obj$lastGAResult$solution$phenotype, penv$topology())
  NNdepth<-length(penv$topology())-1
  NNtop<-penv$topology()
if (type=="console") 
    {consoleTable(df=solutionDF, caption=caption, name)
     consoleTable(df=casesDF, caption=caption1, name)
     for (j in (1:NNdepth))
     { cat("Layer:", j, "Neurons:", NNtop[j], " (b|W)^T: \n")
       print(NNweights[[j]])
       pause() } }

if (type=="latex")
  { latexTables(df=solutionDF, name=name, 
    fnName="SolutionTable", mfnName="main",
              caption=caption,
              tableLength=latexTableLength(), path=outpath)
    latexTables(df=casesDF, name=name, 
    fnName="CasesTable", mfnName="main",
              caption=caption1,
              tableLength=latexTableLength(), path=outpath)
     for (j in (1:NNdepth))
     { captionNN<-paste("Layer:", j, "Neurons:", NNtop[j], " $(b|W)^T$: \n")
       layerDF<-as.data.frame(NNweights[[j]])
    latexTables(df=layerDF, name=name, 
    fnName="NNWeightTable", mfnName="main",
              caption=captionNN,
              tableLength=latexTableLength(), path=outpath) }
    }
if (type=="beamer")
  { beamerTables(df=solutionDF, name=name, 
              fnName="SolutionTable", mfnName="main",
              caption=caption,
              tableLength=beamerTableLength(), path=outpath)
   beamerTables(df=casesDF, name=name, 
              fnName="SolutionTable", mfnName="main",
              caption=caption1,
              tableLength=beamerTableLength(), path=outpath)
     for (j in (1:NNdepth))
     { captionNN<-paste("Layer:", j, "Neurons:", NNtop[j], " $(b|W)^T$: \n")
       # cat(captionNN)
       layerDF<-as.data.frame(NNweights[[j]])
    beamerTables(df=layerDF, name=name, 
    fnName="NNWeightTable", mfnName="main",
              caption=captionNN,
              tableLength=beamerTableLength(), path=outpath) }
    }
invisible(0) } 

#' A  production table of a grammar.
#'
#' @param treatmentname   Name of rds-file of treatment.
#' @param name            Name of experiment. Default: "".
#' @param common          Boolean. Default: TRUE. 
#'                        Used in common parameters or 
#'                        in analysis if treatment part?
#' @param inpath          Path to treatment rds-files of experiment.
#'                          Default: ".".
#' @param outpath          Path to report of experiment. Default: "." 
#' @param type  Output to console (type="console") 
#'              or to latex files (type="latex" | "beamer")?  
#'              Default: "console".
#' 
#' @return 0 (invisible).
#'
#' @family Report of Experiment
#'
#' @examples
#' cat("TBD!\n")
#'
#' @export
experimentGrammarTable<-function(treatmentname="", name="", common=TRUE,
                     inpath=".", outpath=".", type="console")
{ # Checks on grammar. 
   treatments<-experimentTreatments(inpath=inpath)
   dfs<-designOfExperiment(treatments=treatments, inpath=inpath)
   gcommon<-"Grammar" %in% names(dfs$common)
   if ((common==TRUE) && (!gcommon)) {return(invisible(0))}
   if ((common==FALSE) && gcommon) {return(invisible(0))}
  # Do the grammar.
  obj<-getTreatment(treatmentname=treatmentname, inpath=inpath)
  g<-obj$lastGAResult$GAenv$grammar
  if (is.null(g)) {return(invisible(0))}
  df<-xegaBNF::dataframePT(g$PT, g) 
  if (common==TRUE)
       {caption<-paste0("The Production Table of Experiment ", name)}
  else
       {caption<-paste0("The Production Table of Treatment ", 
                          obj$tArgs$treatmentName, 
                        " of Experiment ", name)}    
if (type=="console")
{   consoleTable(df=df,
    caption=caption) }
if (type=="latex") 
  { latexTables(df=df, name=name, fnName="GrammarTable", mfnName="main",
              caption=caption, tableLength=latexTableLength(), path=outpath)}
if (type=="beamer") 
  { beamerTables(df=df, name=name, fnName="GrammarTable", mfnName="main",
              caption=caption, tableLength=beamerTableLength(), path=outpath)}

invisible(0) } 

#' Format a number.
#'.
#' @param x A real.
#'
#' @return A string.
#' 
#' @examples
#' tformat(1/3)
#'
#'@export
tformat<-function(x) {format(round(x,5), digits=3, scientific=FALSE)}

#' Testing for normality of a data vector.
#'
#' @description A wrapper for the Shapiro-Wilk test. 
#'              See \code{stat::shapiro.test}. 
#'
#' @param treatmentname   Name of rds-file of treatment.
#' @param name            Name of experiment. Default: "".
#' @param variable        Name of variable. Default: "".
#' @param alpha           Level of significance. Default: 0.05. 
#' @param coef            Scaling of window for removing outliers.
#'                        Default: 1.5 (as in boxplot.stats). 
#'                        0 means all data are retained.
#'                        The windox size is given by the length of the box
#'                        plus/minus coef times the length of the box.
#' @param silent          If TRUE, no output.
#' @param inpath          Path to treatment rds-files of experiment.
#'                          Default: ".".
#' @param outpath          Path to report of experiment. Default: "." 
#' @param type  Output to console (type="console") 
#'              or to latex files (type="latex" | "beamer")?  
#'              Default: "console".
#' 
#' @return Boolean (invisible).
#'
#' @family Report of Experiment
#'
#' @examples
#' cat("TBD!\n")
#'
#' @importFrom stats shapiro.test
#' @export
experimentIsNormal<-function(treatmentname="", name="", variable="",
                     alpha=0.05, coef=1.5, silent=TRUE,
                     inpath=".", outpath=".", type="console")
{ tformat<-function(x) {format(round(x,5), digits=3, scientific=FALSE)}
  obj<-getTreatment(treatmentname=treatmentname, inpath=inpath)
  treatmentName<-obj$tArgs$treatmentName
  z<-obj$resultDF[, variable]
  x<-rmOutliers(z, coef=coef)
  x[1]<-x[1]+1e-14
  if ((length(x)>2) && (length(x)<5000))
  {
  test<-shapiro.test(x)
  iText<-paste0("{\\bf Normality Test.} The normality of variable ", variable, 
               " of treatment ", treatmentName, 
               " of experiment ", name, 
               " is tested at a significance level ", alpha, ".\n") 
  if (coef==0) {iText<-paste0(iText,"\n Outliers are not removed (coef=0).\n")}
       else {iText<-paste0(iText, "\n ", (length(z)-length(x)), 
                           " outliers removed (coef=",coef,").\n")}
  iText<-paste(iText, "\n The test-statistic W of the", 
        test$method, "is", 
        tformat(test$statistic), "with a p-value of", 
        tformat(test$p.value), ".\n", sep=" ") 
  isNormal<-test$p.value>alpha
  if (isNormal==TRUE) 
  {iText<-paste(iText, "Since the p-value", tformat(test$p.value), 
                "is above the significance level $\\alpha$", alpha, ",\n", 
                "variable", variable, "of treatment", treatmentName, 
                "of experiment", name,   
                " is not significantly different from", 
                "a normal distribution.\n\n", sep=" ")}
  else                
  {iText<-paste(iText, "Since the p-value", tformat(test$p.value), 
         "is below or equal the significance level $\\alpha=", alpha, "$,\n", 
                "variable {\\bf ", variable, "} of treatment {\\bf ", 
                 treatmentName, 
                "} of experiment", name,   
                " is {\\bf significantly different} from", 
                "a normal distribution.\n\n", sep=" ")}
  } else
  {iText<-paste("{\\bf Warning:} For the Shapiro-Wilks test, ",
                "sample size must be between 3 and 5000.\n\n") 
   isNormal<-FALSE}
if (!silent)
{
   if (type=="console")
   {    cat(iText) }
   if (type=="latex") { 
       fn<-newFileName(fn="main", ftype="tex", expname=name, path=outpath)
       writeText(iText, fn) }
   if (type=="beamer") { 
       fn<-newFileName(fn="main", ftype="tex", expname=name, path=outpath)
       iText<-paste("\\begin{frame}[t]\n", 
               "\\frametitle{Is variable", variable,"of treatment", treatmentName, "normal?}\n", 
               iText, 
               "\\end{frame}\n", sep=" ") 
       writeText(iText, fn) }
}
invisible(isNormal) } 

