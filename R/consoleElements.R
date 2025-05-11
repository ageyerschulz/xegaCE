
# console output for elements of a computational experiment.
# (c) 2025 Andreas Geyer-Schulz

#' Wait for input.
#' 
#' @return Keyboard input of user (invisible).
#'
#' @family Console Elements
#'
#' @examples
#' pause()
#' a<-pause()
#' print(a)
#'
#' @export
pause<-function()
{
       {invisible(readline(prompt="\n Press [enter] to continue"))}
}

#' Prolog of experiment. 
#'
#' @description Shows name, title, author, and purpose of experiment.
#'
#' @param name     Name of experiment. Default: "Missing".
#' @param title    Title of the experiment. Default: "Undefined".
#' @param author   Author of experimental report. Default: "Unknown".
#' @param purpose  Purpose of experiment. Default: "Unknown".
#'
#' @return A text string (invisible).
#'
#' @family Console Elements
#'
#' @examples
#' consoleProlog(name="Experiment A.", 
#'     title="First test", author="U.N. Owen", 
#'     purpose=paste0("The experiment shows that algorithm A ", 
#'                    "is faster than algorithm B.\n"))
#' 
#' @export
consoleProlog<-function(name="Missing", 
   title="Undefined", author="Unknown", purpose="Unknown")
{ y<-paste0(name," ", title, ". ", date(), "\n")
  y<-paste0(y,"   ", author, "\n\n")
  y<-paste0(y, purpose, "\n\n")
  cat(y) 
  pause()
  invisible(y)}

#' End of document (LaTeX)
#'
#' @return A text string (invisible).
#'
#' @family Console Elements
#'
#' @examples
#' a<-consoleEndDocument()
#' print(a)
#' cat(a)
#' @export
consoleEndDocument<-function()
{
y<-paste0("End of Experiment (", date(), ").\n")
cat(y)
pause()
invisible(y) }

#' Text.
#'
#' @param ...  A list of text strings.
#' @param header Name of the block.
#'
#' @return A text string (invisible).
#'
#' @family Console Elements
#'
#' @examples
#' a<-consoleText("here", "we", "are")
#' print(a)
#' cat(a)
#' @export
consoleText<-function(..., header="")
{
y<-paste(header, ..., sep="\n")
cat(y)
pause()
invisible(y) }

#' Shows the section of an experiment.
#'
#' @param name      Name of the document element. Default: "Missing".
#' @param level     Latex markup of the document element.
#'                  Default: "section". Latex supports:
#'                  "section", "subsection", "subsubsection".
#'                  Used for indentation of output.
#' @param clearpage Boolean. Default: TRUE. 
#'                  Ignored.
#'                 
#' @return A text string (invisible).
#'
#' @family Console Elements
#'
#' @examples
#' consoleSection(name="Design", level="section", clearpage=FALSE)
#' consoleSection(name="Design", level="subsection", clearpage=TRUE)
#' @export
consoleSection<-function(name="Missing", level="section", clearpage=TRUE)
{ y<-""
if (level=="section") {y<-paste0(y," - ")} 
if (level=="subsection") {y<-paste0(y," -- ")} 
if (level=="subsubsection") {y<-paste0(y," --- ")} 
y<-paste0(y,name, "(", date(), ")\n")
cat(y)
pause()
invisible(y) }

### Figures

#' The figure output on a console.
#'
#' @param name      Name of the experiment.
#' @param fnName     File name of the eps file of the graphic.
#' @param caption   Caption of the figure.
#' @param label     Label of the figure.
#'                 
#' @return A text string (invisible).
#'
#' @family Console Elements
#'
#' @examples
#' consoleFigure(name="Ex A", fnName="test.eps", caption="test", label="ltest")
#' 
#' @export
consoleFigure<-function(name="", fnName="", 
               caption="No caption defined", label="")
{ 
  y<-paste0("Experiment:", name, " Figure: ", caption, "\n")
  cat(y)
  pause()
  invisible(y)}

#' Produces box-and-whisker plots of treatments of an experiment.
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
#' @param epsfilename    File name of epsfile. Default: NULL.
#'
#' @return 0 (invisible).
#' 
#' @family Console Elements
#'
#' @examples
#' cat("Example: TODO.\n")
#'
#' @importFrom grDevices postscript
#' @importFrom grDevices dev.off
#' @importFrom graphics boxplot
#' @importFrom graphics par
#' @importFrom graphics axis
#' @importFrom graphics text
#' @export
consoleBoxPlot<-function(treatments, variable, title="",
                outline=TRUE, notch=FALSE, horizontal=TRUE, varwidth=FALSE,
                name="", inpath=".", epsfilename=NULL)
{ full<-fullFileNames(treatments=treatments, inpath=inpath)
OL<-lapply(full, FUN=readRDS)
names<-unlist(lapply(OL, FUN=function(x) {x$tArgs$treatmentName}))
data<-lapply(OL, FUN=function(x, variable) {x$resultDF[,variable]}, variable=variable)
df<-data.frame()
for (i in (1:length(OL)))
  { vec<-OL[[i]]$resultDF[, variable]
    df<-rbind(df, vec) }
df<-t(df)
colnames(df)<-names
if (horizontal==TRUE)
   {# ylab<-"Treatments"
    ylab<-""
    xlab<-variable} 
else
   {ylab<-variable 
    xlab<-"Treatments"}
newtitle<-title
if (outline==FALSE) {newtitle<-paste0(newtitle," (Without outliers).")} else
                    {newtitle<-newtitle}
graphics::par(cex.axis=1.5)
graphics::par(cex.lab=1.5)
graphics::par(cex.main=2.0)
graphics::boxplot(df, main=newtitle, ylab=ylab, xlab=xlab, 
                  varwidth=varwidth, pars=list(yaxt = "n"),
                  outline=outline, notch=notch, horizontal=horizontal)
if (horizontal==TRUE)
   { graphics::axis(2, labels=FALSE)
    graphics::text(y=(seq(1:length(names))), 
                      graphics::par("usr")[1], 
                      labels=names, srt= 75, pos=2, xpd=TRUE)
   }
rc<-0
invisible(rc)
}


### Tables.

#' The table environment for the console.
#'
#' @param df        Dataframe or matrix.
#' @param caption   Caption of the table. Default: "".
#' @param align     Alignment of columns. Default: "".
#' @param label     Label of the table.   Default: "".
#' @param center    Center table. Default: "center".
#'                 
#' @return A dataframe (invisible).
#'
#' @family Console Elements
#'
#' @examples
#' df<-data.frame(Name=c("Alice", "Bob"), Age=c(25, 27))
#' consoleTable(df, caption="Alice and Bob", label="AB")
#' 
#' @importFrom xtable xtable
#' @export
consoleTable<-function(df, caption="", label="", align="", center="center")
{
cat("Table: ", caption, "\n")
print(df)
pause()
return(df)}

