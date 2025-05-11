
# filer-I/O
# (c) 2025 Andreas Geyer-Schulz

#' Generate a new filename with a version number.
#'
#' @description The filename is built by catenating the following elements:
#'     \enumerate{
#'     \item \code{path} The path of the file.
#'     \item \code{expname} is the name of the experiment. May be empty.
#'     \item \code{fn} is the filename.
#'     \item \code{i}  is a number. Is formatted to the 3 positions 
#'                     (with leading \code{0}s). 
#'     \item \code{type} indicates the filetype (e.g. tex or rds). 
#'     } 
#'     If a filename exits, \code{i} is incremented by \code{1}.
#'     \strong{Warning:} The operation is not atomic.
#'
#' @param fn   A filename.
#' @param ftype A filetype. Default: "tex".
#' @param expname Name of an Experiment. Default: "".
#' @param path    A filepath. Default: "".
#'
#' @return A filename 
#'
#' @family File I/O
#' 
#' @examples
#' path<-tempdir()
#' a<-newFileName(fn="test", ftype="rds", path=path)
#' saveRDS(a, file=a)
#' cat("Filename 1:",a, "\n")
#' b<-newFileName(fn="test", ftype="rds", path=path)
#' cat("Filename 2:",b, "\n")
#' saveRDS(b, file=b)
#' c<-newFileName(fn="test", ftype="rds", path=path)
#' cat("Filename 3:",c, "\n")
#'
#' @export
newFileName<-function(fn, ftype="tex", expname="", path=".")
{  i<-0
   newpath<-paste0(path, .Platform$file.sep)
   newi<-formatC(i, width = 3, format = "d", flag = "0")
   newfn<-paste0(newpath, expname, fn, newi, ".", ftype)
   while   (file.exists(newfn))
   {i<-i+1;
   newi<-formatC(i, width = 3, format = "d", flag = "0")
    newfn<-paste0(newpath, expname, fn, newi, ".", ftype)}
return(newfn)}
   
#' Write a text file.
#'
#' @param text  A text string.
#' @param fn    File name. Default: NULL.
#'
#' @return A text string (invisible)
#' 
#' @family File I/O
#' 
#' @examples
#' path<-tempdir()
#' a<-newFileName(fn="test", ftype="txt", path=path)
#' b<-writeText(a, fn=a)
#'
#' @export
writeText<-function(text, fn=NULL)
{ fname<-fn
  if (is.null(fn))
   {fname<-newFileName("tmp")}
  con<-file(fname, "wb")
  b<-writeChar(text, con, eos=NULL)
  close(con)
invisible(text)
}

#' A list of paths to treatment rds-files.
#'
#' @param treatments List of treatment names of experiments.
#' @param inpath   Path for treatment files. Default: ".". 
#'
#' @return A list of paths to treatment rds-files.
#' 
#' @family File I/O
#'
#' @examples
#' cat("TBD!\n")
#' 
#' @export
fullFileNames<-function(treatments, inpath=".")
{ unlist(lapply(treatments, 
    FUN=function(x, path) {paste0(path, .Platform$file.sep, x)}, 
    path=inpath)) }
