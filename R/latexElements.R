# Latex elements for the documentation of statistical experiments.
# (c) 2025 Andreas Geyer-Schulz

# Part I. Start and end of latex document.

#' Prolog of minimal latex document. 
#'
#' @description Contains title, author and purpose of experiment.
#'
#' @param name    Name of experiment. Default: "Missing".
#' @param title   Title of the experiment. Default: "Undefined".
#' @param author  Author of document. Default: "Unknown".
#' @param purpose Purpose of experiment. A short description. 
#'                Default: "Unknown"
#'
#' @return A text string with LaTex markup.
#'
#' @family Latex Elements
#'
#' @examples
#' a<-latexProlog(name="Experiment A.", 
#'                title="First test", author="U.N. Owen",
#'                purpose="To show the impossible.\n")
#' cat(a)
#' @export
latexProlog<-function(name="Missing", 
    title="Undefined", author="Unknown", purpose="Unknown")
{ y<-"\\documentclass[12pt]{article}\n"
  y<-paste0(y, "\\usepackage{a4}\n")
  y<-paste0(y, "\\usepackage{graphicx}\n")
  y<-paste0(y, "\\renewcommand{\\topfraction}{1.0}\n")
  y<-paste0(y, "\\renewcommand{\\floatpagefraction}{1.0}\n")
  y<-paste0(y, "\\begin{document}\n")
  y<-paste0(y, "\\title{Report of experiment ",name, ". ", title, "}\n")
  y<-paste0(y, "\\author{", author, "}\n")
  y<-paste0(y, "\\date{\\today}\n")
  y<-paste0(y, "\\begin{titlepage}\n")
  y<-paste0(y, "\\maketitle\n")
  y<-paste0(y, "\\begin{abstract}\n")
  y<-paste0(y, purpose)
  y<-paste0(y, "\\end{abstract}\n")
  y<-paste0(y, "\\end{titlepage}\n")
  y<-paste0(y, "\\tableofcontents\n")
  y<-paste0(y, "\\clearpage\n")
  y<-paste0(y, "\\listoffigures\n")
  y<-paste0(y, "\\clearpage\n")
  y<-paste0(y, "\\listoftables\n")
  return(y)}

#' End of document (LaTeX)
#'
#' @return A text string with latex markup.
#'
#' @family Latex Elements
#'
#' @examples
#' a<-latexEndDocument()
#' print(a)
#' cat(a)
#' @export
latexEndDocument<-function()
{
y<-"\\end{document}\n"
return(y)
}

#' Text.
#'
#' @param ...  A list of text strings.
#' @param header  Name of the text.
#'
#' @return A text string.
#'
#' @family Latex Elements
#'
#' @examples
#' a<-latexText("Here", "we",  "are")
#' print(a)
#' cat(a)
#' @export
latexText<-function(..., header="")
{return(paste("{\\bf", header, "}.", ..., "\n", sep="\n"))}

# Part II. Sectioning.

#' Sections of a latex document.
#'
#' @param name      Name of the document element.
#' @param level     Latex markup of the document element.
#'                  Default: "section". Latex supports:
#'                  "section", "subsection", "subsubsection".
#' @param clearpage Boolean. Default: TRUE. 
#'                  Clearpage before start of document element.
#'                  Forces output of all floating elements of 
#'                  the previous document element.
#'                 
#' @return A text string with latex markup.
#'
#' @family Latex Elements
#'
#' @examples
#' latexSection(name="Design", level="section", clearpage=FALSE)
#' a<-latexSection(name="Design", level="section", clearpage=TRUE)
#' print(a)
#' cat(a)
#' @export
latexSection<-function(name, level="section", clearpage=TRUE)
{ y<-""
if (clearpage) {y<-"\\clearpage\n"} 
return(paste0(y,"\\", level, "{",name, "}\n"))
}

# Meta data tables.



# Meta data figures.

#' The figure environment for a latex document.
#'
#' @param name      Name of the experiment.
#' @param fnName     File name of the eps file of the graphic.
#' @param caption   Caption of the figure.
#' @param label     Label of the figure.
#' @param width     Proportion of textwidth. Default: 0.6.
#' @param angle     Angle. Default: -90.
#'                 
#' @return A text string with latex markup.
#'
#' @family Latex Elements
#'
#' @examples
#' a<-latexFigure(name="Ex A", fnName="test.eps", caption="test", label="ltest",
#'                width=0.6, angle=-90)
#' print(a)
#' cat(a)
#' @export
latexFigure<-function(name, fnName, caption="No caption defined", label="", width=0.6, angle=-90)
{ if (label=="") {l<-label} 
     else {l<-paste0("\\label{", label, "} ")}
  y<-paste("% ", name, "\n", sep="")
  y<-paste(y, "% Figure: ", caption, "\n", sep="")
  y<-paste(y, "% ", date() ,"\n", sep="")
  y<-paste(y, "\\begin{figure}[ht]\n")
  y<-paste(y, "\\begin{center}\n")
  y<-paste(y, "\\includegraphics[width=", width, "\\textwidth, angle=", angle,"]\n", sep="")
  y<-paste(y, "{", fnName, "}\n", sep="")
  y<-paste(y, "\\end{center}\n")
  y<-paste(y, "\\caption{", caption, l, "}\n")
  y<-paste(y, "\\end{figure}\n\n")
  # writeText(y, newFileName("main", expname=expname))
  return(y)}

#' Produces box-and-whisker plots of treatments of an experiment (eps-file).
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
#'
#' @return Name of eps file with output.
#' 
#' @family Latex Elements
#'
#' @examples
#' cat("Example: TODO.\n")
#'
#' @importFrom grDevices postscript
#' @importFrom grDevices dev.off
#' @export
latexBoxPlot<-function(treatments, variable, title="",
                outline=TRUE, notch=FALSE, horizontal=TRUE, varwidth=FALSE,
                name="", inpath=".", outpath=".")
{full<-unlist(lapply(treatments, 
        FUN=function(x, path) {paste0(path, .Platform$file.sep, x)}, 
        path=inpath))
OL<-lapply(full, FUN=readRDS)
#pnames<-unlist(lapply(OL, FUN=function(x) {x$treatmentname}))
#pname<-Reduce(pnames, f=paste0)
#pname<-gsub(".", "", pname, fixed=TRUE)
pname<-"t"
pname<-paste0(pname, variable)
fnName<-paste0("boxplot", pname)
synthfn<-newFileName(fn=fnName, ftype="eps", expname=name, path=outpath)
rc<-grDevices::postscript(file=synthfn)
consoleBoxPlot(treatments, variable, title=title,
               outline=outline, notch=notch, 
               horizontal=horizontal, varwidth=varwidth,
               name=name, inpath=inpath, epsfilename=synthfn)
rc<-grDevices::dev.off()
new<-unlist(strsplit(synthfn, .Platform$file.sep))
return(new[length(new)]) }

## Tables.

#' The meta table environment for a latex document.
#'
#' @param name      Name of the experiment.
#' @param fnName     File name of the exported table element.
#' @param caption   Caption of the table.
#' @param label     Label of the table.
#'                 
#' @return A text string with latex markup.
#'
#' @family Latex Elements
#'
#' @examples
#' a<-latexMetaTable(name="Ex A", fnName="test.tex", caption="test", label="ltest")
#' print(a)
#' cat(a)
#'
#' @export
latexMetaTable<-function(name, fnName, caption="No caption defined", label="")
{ if (label=="") {l<-label} 
     else {l<-paste0("\\label{", label, "} ")}
  y<-paste("% ", name, "\n", sep="")
  y<-paste(y, "% Table: ", caption, "\n", sep="")
  y<-paste(y, "% ", date() ,"\n", sep="")
  y<-paste(y, "\\input{", fnName, "}\n", sep="")
  y<-paste(y, "% Label: ", l, "\n")
  # writeText(y, newFileName("main", expname=expname))
  return(y)}

#' The table environment for a latex document.
#'
#' @param df        Dataframe or matrix.
#' @param caption   Caption of the table. Default: "".
#' @param align     Alignment of columns. Default: "".
#' @param label     Label of the table.   Default: "".
#' @param center    Center table. Default: "center".
#' @param sanitize  Boolean. Default: FALSE.
#' @param tableLength  Integer. Default: 60.
#'                 
#' @return A text string with latex markup.
#'
#' @family Latex Elements
#'
#' @examples
#' df<-data.frame(Name=c("Alice", "Bob"), Age=c(25, 27))
#' a<-latexTable(df, caption="Alice and Bob", label="AB")
#' print(a)
#' 
#' @importFrom xtable xtable
#' @export
latexTable<-function(df, caption="", label="", align="", center="center",
                     sanitize=FALSE, tableLength=60)
{ if (align=="") {a<-Reduce(rep("r", (ncol(df)+1)), f=paste0)}
                 else {a<-align}
  if (label=="") {l<-label}
     else {l<-paste0("\\label{", label, "} ")}

y<-list()
i<-1
line<-0
ntab<-floor(nrow(df)/tableLength)+(!0==nrow(df)%%tableLength)

while (i<=ntab)
{
newdf<-as.data.frame(df[line+(1:min((nrow(df)-line),tableLength)),])
colnames(newdf)<-colnames(df)
newcaption<-caption
if (ntab>1) {newcaption<-paste0(caption, " (Part ", i,")")}
x<-xtable::xtable(newdf, caption=paste0(newcaption, l), align=a,
         latex.environments = center)
if (sanitize==TRUE)
{y[[i]]<-print(x, print.results=FALSE, sanitize.text.function=function(x) {x})}
else
{y[[i]]<-print(x, print.results=FALSE)}
i<-i+1
line<-line+tableLength
}

return(y)}


#' Write table(s) and meta-table(s) for a latex document.
#'
#' @param df        Dataframe.
#' @param name      Name of the experiment.
#' @param fnName    File name of the exported table element.
#' @param mfnName   File name of the exported meta table element.
#'                  Default: "main".
#' @param caption   Caption of the table. Default: "".
#' @param align     Alignment of columns. Default: "".
#' @param label     Label of the table.   Default: "".
#' @param center    Center table. Default: "center".
#' @param sanitize  Boolean. Default: FALSE.
#' @param tableLength  Integer. Default: 60.
#' @param path      Path. Default: outpath.
#'                 
#' @return Invisible 0.
#'
#' @family Latex Elements
#'
#' @examples
#' df<-data.frame(Name=c("Alice", "Bob"), Age=c(25, 27))
#' a<-latexTable(df, caption="Alice and Bob", label="AB")
#' print(a)
#' 
#' @importFrom xtable xtable
#' @export
latexTables<-function(df, name, fnName, mfnName="main", 
                     caption="", label="", align="", center="center",
                     sanitize=FALSE, tableLength=60, path="")
  {
  y<-latexTable(df=df, caption=caption, label=label,
                 align=align, center=center, sanitize=sanitize,
                 tableLength=tableLength)
  for (i in (1:length(y)))
     {fn<-newFileName(fn=fnName, ftype="tex", expname=name, path=path)
      writeText(y[[i]], fn)
      newfn<-unlist(strsplit(fn, .Platform$file.sep))
      newfn<-newfn[length(newfn)]
      ym<-latexMetaTable(name=name, fnName=newfn, caption=caption, label=newfn)
      writeText(ym, newFileName(fn=mfnName, ftype="tex",
                    expname=name, path=path))
       i<-i+1}
  return(invisible(0))
  }

