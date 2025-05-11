# Latex elements for the documentation of statistical experiments.
# (c) 2025 Andreas Geyer-Schulz

# Part I. Start and end of latex document.

#' Prolog of minimal latex document for beamer class. 
#'
#' @description Contains title, author and purpose of experiment.
#'
#' @param name    Name of experiment. Default: "Missing".
#' @param title   Title of the experiment. Default: "Undefined".
#' @param author  Author of document. Default: "Unknown".
#' @param purpose Purpose of experiment. A short description. 
#'                Default: "Unknown"
#' @param beamertheme   Beamer theme. Default: "default".
#' @param beamercolor   Beamer color. Default: "electricultramarine".
#'
#' @return A text string with LaTex markup.
#'
#' @family beamer Elements
#'
#' @examples
#' a<-beamerProlog(name="Experiment A.", 
#'                title="First test", author="U.N. Owen",
#'                purpose="To show the impossible.\n", beamertheme="AnnArbor")
#' cat(a)
#' @export
beamerProlog<-function(name="Missing", 
    title="Undefined", author="Unknown", purpose="Unknown", 
    beamertheme="default", beamercolor="electricultramarine")
{ y<-"\\documentclass[18pt,c]{beamer}\n"
y<-paste0(y, "\\makeatletter\n")
y<-paste0(y, "\\let\\beamer@writeslidentry@miniframeson=\\beamer@writeslidentry\n")
y<-paste0(y, "\\def\\beamer@writeslidentry@miniframesoff{%\n")
y<-paste0(y, "  \\expandafter\\beamer@ifempty\\expandafter{\\beamer@framestartpage}{}% does not happen normally\n")
y<-paste0(y, "  {%else\n")
y<-paste0(y, "    % removed \\addtocontents commands\n")
y<-paste0(y, "   \\clearpage\\beamer@notesactions%\n")
y<-paste0(y, "  }\n")
y<-paste0(y, "}\n")
y<-paste0(y, "\\newcommand*{\\miniframeson}{\\let\\beamer@writeslidentry=\\beamer@writeslidentry@miniframeson}\n")
y<-paste0(y, "\\newcommand*{\\miniframesoff}{\\let\\beamer@writeslidentry=\\beamer@writeslidentry@miniframesoff}\n")
y<-paste0(y, "\\makeatother\n")
y<-paste0(y,"% yellow\n")
y<-paste0(y,"\\definecolor{goldenyellow}{rgb}{1.0, 0.87, 0.0}\n")
y<-paste0(y,"\\definecolor{electricyellow}{rgb}{1.0, 1.0, 0.0}\n")
y<-paste0(y,"\\definecolor{icterine}{rgb}{0.99, 0.97, 0.37}\n")
y<-paste0(y,"\\definecolor{flavescent}{rgb}{0.97, 0.91, 0.56}\n")
y<-paste0(y,"\\definecolor{lemon}{rgb}{1.0, 0.97, 0.0}\n")
y<-paste0(y,"% orange\n")
y<-paste0(y,"\\definecolor{amber}{rgb}{1.0, 0.75, 0.0}\n")
y<-paste0(y,"\\definecolor{cadmiumorange}{rgb}{0.93, 0.53, 0.18}\n")
y<-paste0(y,"\\definecolor{internationalorange}{rgb}{1.0, 0.31, 0.0}\n")
y<-paste0(y,"% red\n")
y<-paste0(y,"\\definecolor{ferrarired}{rgb}{1.0, 0.11, 0.0}\n")
y<-paste0(y,"\\definecolor{fireenginered}{rgb}{0.81, 0.09, 0.13}\n")
y<-paste0(y,"\\definecolor{cadmiumred}{rgb}{0.89, 0.0, 0.13}\n")
y<-paste0(y,"% blue\n")
y<-paste0(y,"\\definecolor{ao}{rgb}{0.0, 0.5, 0.0}\n")
y<-paste0(y,"\\definecolor{babyblueeyes}{rgb}{0.63, 0.79, 0.95}\n")
y<-paste0(y,"\\definecolor{bleudefrance}{rgb}{0.19, 0.55, 0.91}\n")
y<-paste0(y,"\\definecolor{blue}{rgb}{0.0, 0.0, 1.0}\n")
y<-paste0(y,"\\definecolor{cobalt}{rgb}{0.0, 0.28, 0.67}\n")
y<-paste0(y,"\\definecolor{darkmidnightblue}{rgb}{0.0, 0.2, 0.4}\n")
y<-paste0(y,"\\definecolor{brandeisblue}{rgb}{0.0, 0.44, 1.0}\n")
y<-paste0(y,"\\definecolor{deepskyblue}{rgb}{0.0, 0.75, 1.0}\n")
y<-paste0(y,"\\definecolor{iris}{rgb}{0.35, 0.31, 0.81}\n")
y<-paste0(y,"\\definecolor{navyblue}{rgb}{0.0, 0.0, 0.5}\n")
y<-paste0(y,"\\definecolor{ultramarine}{rgb}{0.07, 0.04, 0.56}\n")
y<-paste0(y,"\\definecolor{electricultramarine}{rgb}{0.25, 0.0, 1.0}\n")
y<-paste0(y,"% green\n")
y<-paste0(y,"\\definecolor{cadmiumgreen}{rgb}{0.0, 0.42, 0.24}\n")
y<-paste0(y,"\\definecolor{darkpastelgreen}{rgb}{0.01, 0.75, 0.24}\n")
  y<-paste0(y, "\\usetheme{", beamertheme, "}\n")
  y<-paste0(y, "\\usecolortheme{default}\n")
  y<-paste0(y, "\\usefonttheme{default}\n")
if (!(beamercolor=="default"))
{  y<-paste0(y, "\\setbeamercolor{structure}{fg=",beamercolor,"}\n")}
  y<-paste0(y, "\\setbeamerfont{frametitle}{size=\\footnotesize}\n")
  y<-paste0(y, "\\usepackage{graphicx}\n")
  y<-paste0(y, "\\renewcommand{\\topfraction}{1.0}\n")
  y<-paste0(y, "\\renewcommand{\\floatpagefraction}{1.0}\n")
  y<-paste0(y, "\\begin{document}\n")
  y<-paste0(y, "\\title{Report of Experiment ",name, ". ", title, "}\n")
  y<-paste0(y, "\\author{", author, "}\n")
  y<-paste0(y, "\\date{\\today}\n")
  y<-paste0(y, "\\begin{frame}\n")
  y<-paste0(y, "\\titlepage\n")
  y<-paste0(y, "\\end{frame}\n")
  y<-paste0(y, "\\begin{frame}\n")
  y<-paste0(y, "\\frametitle{Abstract}\n")
  y<-paste0(y, purpose)
  y<-paste0(y, "%\\end{abstract}\n")
  y<-paste0(y, "\\end{frame}\n")
  y<-paste0(y, "\\begin{frame}[t, allowframebreaks]\n")
  y<-paste0(y, "\\frametitle{Contents}\n")
  y<-paste0(y, "\\tableofcontents[subsubsectionstyle=hide]\n")
  y<-paste0(y, "\\vfill\n\\end{frame}\n")
  return(y)}

#' End of document (Latex beamer)
#'
#' @return A text string with latex markup.
#'
#' @family beamer Elements
#'
#' @examples
#' a<-beamerEndDocument()
#' print(a)
#' cat(a)
#' @export
beamerEndDocument<-function()
{
y<-"\\end{document}\n"
return(y)
}

#' Text.
#'
#' @param ...      A list of text strings.
#' @param header   Name of the block.
#' @param block    Default: TRUE.
#'
#' @return A text string.
#'
#' @family Console Elements
#'
#' @examples
#' a<-beamerText("Here", "we", "are")
#' print(a)
#' cat(a)
#' @export
beamerText<-function(..., header="Comment", block=TRUE)
{
if (block==TRUE)
{ y<-paste("\\begin{frame}\n\\vspace*{2mm}",
         "\\begin{block}{", header, "}",
          ...,
         "\\end{block}\n\\end{frame}",sep="\n") } 
else
{ y<-paste("\\begin{frame}",
         "\\frametitle{", header, "}",
          ...,
         "\\end{frame}",sep="\n") } 
return(y)}

# Part II. Sectioning.

#' Sections of a latex beamer document.
#'
#' @param name      Name of the document element.
#' @param level     Latex markup of the document element.
#'                  Default: "section". Latex supports:
#'                  "section", "subsection", "subsubsection".
#' @param clearpage Boolean. Default: TRUE. 
#'                  Clearpage before start of document element.
#'                  Forces output of all floating elements of 
#'                  the previous document element.
#' @param miniframe Boolean. Default: TRUE.
#'                  If FALSE, miniframes are turned off.
#'                 
#' @return A text string with latex markup.
#'
#' @family Latex Elements
#'
#' @examples
#' beamerSection(name="Design", level="section", clearpage=FALSE)
#' a<-beamerSection(name="Design", level="section", clearpage=TRUE)
#' print(a)
#' cat(a)
#' @export
beamerSection<-function(name, level="section", clearpage=FALSE, miniframe=TRUE)
{ y<-""
if (clearpage) {y<-"\\clearpage\n"} 
if (miniframe) {y<-"\\miniframeson\n"} else {y<-"\\miniframesoff\n"} 
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
#' @family beamer Elements
#'
#' @examples
#' a<-beamerFigure(name="Ex A", fnName="test.eps", caption="test", label="ltest",
#'                width=0.6, angle=-90)
#' print(a)
#' cat(a)
#' @export
beamerFigure<-function(name, fnName, caption="No caption defined", label="", width=0.5, angle=-90)
{ if (label=="") {l<-label} 
     else {l<-paste0("\\label{", label, "} ")}
  y<-paste("% ", name, "\n", sep="")
  y<-paste(y, "% Figure: ", caption, "\n", sep="")
  y<-paste(y, "% ", date() ,"\n", sep="")
  y<-paste(y, "\\begin{frame}\n")
  y<-paste(y, "\\frametitle{",caption,"}\n")
  y<-paste(y, "\\begin{center}\n")
  y<-paste(y, "\\includegraphics[width=", width, "\\textwidth, angle=", angle,"]\n", sep="")
  y<-paste(y, "{", fnName, "}\n", sep="")
  y<-paste(y, "\\end{center}\n")
  y<-paste(y, l, "\n")
  y<-paste(y, "\\end{frame}\n\n")
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
#' @family beamer Elements
#'
#' @examples
#' cat("Example: TODO.\n")
#'
#' @importFrom grDevices postscript
#' @importFrom grDevices dev.off
#' @export
beamerBoxPlot<-function(treatments, variable, title="",
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
#' @family beamer Elements
#'
#' @examples
#' a<-beamerMetaTable(name="Ex A", fnName="test.tex", caption="test", label="ltest")
#' print(a)
#' cat(a)
#'
#' @export
beamerMetaTable<-function(name, fnName, caption="No caption defined", label="")
{ if (label=="") {l<-label} 
     else {l<-paste0("\\label{", label, "} ")}
  y<-paste("% ", name, "\n", sep="")
  y<-paste(y, "% Table: ", caption, "\n", sep="")
  y<-paste(y, "% ", date() ,"\n", sep="")
  y<-paste(y, "\\begin{frame}\n")
  y<-paste(y, "\\fontsize{8pt}{9pt}\\selectfont\n")
  y<-paste(y, "\\frametitle{",caption,"}\n")
  y<-paste(y, "\\input{", fnName, "}\n", sep="")
  y<-paste(y, l, "\n")
  y<-paste(y, "\\end{frame}\n\n")
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
#' @param tableLength  Integer. Default: 25.
#'                 
#' @return A list of text strings with latex markup.
#'         Each list element is a table. 
#' 
#' @family beamer Elements
#'
#' @examples
#' df<-data.frame(Name=c("Alice", "Bob"), Age=c(25, 27))
#' a<-beamerTable(df, caption="Alice and Bob", label="AB")
#' print(a)
#' 
#' @importFrom xtable xtable
#' @export
beamerTable<-function(df, caption="", label="", align="", center="center",
                     sanitize=FALSE, tableLength=20)
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
newdf<-as.data.frame(df[line+(1:min((nrow(df)-line),tableLength)),], 
      optional=TRUE)
# cat("newdf\n"); print(newdf); 
# cat("df\n"); print(df); 
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

#' Write table(s) and meta-table(s) for a latex beamer document.
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
#' @family beamer Elements
#'
#' @examples
#' df<-data.frame(Name=c("Alice", "Bob"), Age=c(25, 27))
#' a<-latexTable(df, caption="Alice and Bob", label="AB")
#' print(a)
#' 
#' @importFrom xtable xtable
#' @export
beamerTables<-function(df, name, fnName, mfnName="main", 
                     caption="", label="", align="", center="center",
                     sanitize=FALSE, tableLength=20, path="")
  {
  y<-beamerTable(df=df, caption=caption, label=label, 
                 align=align, center=center, sanitize=sanitize, 
                 tableLength=tableLength)
  for (i in (1:length(y)))
     {fn<-newFileName(fn=fnName, ftype="tex", expname=name, path=path)
      writeText(y[[i]], fn)
      newfn<-unlist(strsplit(fn, .Platform$file.sep))
      newfn<-newfn[length(newfn)]
      ym<-beamerMetaTable(name=name, fnName=newfn, caption=caption, label=newfn)
      writeText(ym, newFileName(fn=mfnName, ftype="tex",
                    expname=name, path=path))
       i<-i+1}
  return(invisible(0))
  }


