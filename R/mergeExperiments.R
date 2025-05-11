
# MergeExperiments.R 
# (c) 2025 Andreas Geyer-Schulz

#' Merge treatment files of an experiment.
#'
#' @description \code{inpath} is supposed to contain all rds-files of all treatments of an experiment.
#'              The filenames of a treatment are supposed to have the format "<treatmentname><serial>.rds".
#'              Serial is a number with three decimal places.
#'              \code{mergeTreatments()} reads all rds-files of a treatment, builds a new dataframe, 
#'              and writes the result to a rds-file with the name "merge<treatmentname>.rds" in 
#'              \code{outpath}.
#'
#' @param inpath Path of treatment files.
#' @param outpath Path for merged files.
#'
#' @return Invisible zero.
#'
#' @family Merge of Treatments
#'
#' @examples
#' ## dir.create(tempfile())
#' ## dir.create(tempfile())
#' ## tmpPath<-tempdir()
#' ## subdirs<-list.files(path=tmpPath)
#' ## dataTmpPath<-paste0(tmpPath, .Platform$file.sep, subdirs[1])
#' ## mergeTmpPath<-paste0(tmpPath, .Platform$file.sep, subdirs[2])
#' tmpPath<-tempdir()
#' dataTmpPath<-tmpPath
#' mergeTmpPath<-tmpPath
#' unlink(paste0(tmpPath, "/*"))
#' list.files(path=dataTmpPath)
#' sym4replay19<-kSym(treatmentname="BoolT1SGPreplayXIX",
#'               experimentname="Ex", k=2, trials=3,
#'               grammarfn="AndOrNotTuned1.txt", algorithm="sgp",
#'               popsize=20, generations=10,
#'               executionModel="Sequential", verbose=0,
#'               semantics="byValue", replay=19,
#'               Gpath=kSymmetryGrammarPath, outpath=dataTmpPath, everyk=1)
#' sym4replay0<-kSym(treatmentname="BoolT1SGPreplayZ",
#'               experimentname="Ex", k=2, trials=3,
#'               grammarfn="AndOrNotTuned1.txt", algorithm="sgp",
#'               popsize=20, generations=10,
#'               executionModel="Sequential", verbose=0,
#'               semantics="byValue", replay=0,
#'               Gpath=kSymmetryGrammarPath, outpath=dataTmpPath, everyk=1)
#' list.files(path=dataTmpPath)
#' mergeTreatments(inpath=dataTmpPath, outpath=mergeTmpPath) 
#' list.files(path=dataTmpPath)
#' list.files(path=mergeTmpPath)
#' 
#' @export
mergeTreatments<-function(inpath=".", outpath=".")
{ # Local functions
treatnm<-function(x) {gsub("\\.rds", "", gsub("[0-9][0-9][0-9]", "", x))}
isInList<-function(flist, pat)
   {unlist(lapply(flist, FUN=function(fn, pat) {grepl(pat, fn)}, pat=pat))}
mergeFiles<-function(fileList, fileName="")
{  DFL<-lapply(fileList, FUN=readRDS)
   DF<-data.frame()
   for (i in (1:length(DFL))) {  DF<-rbind(DF, DFL[[i]]$resultDF) }
   newDF<-list(lastGAResult=DFL[[1]]$lastGAResult,
               resultDF=DF,
               tArgs=DFL[[1]]$tArgs, 
               xegaArgs=DFL[[1]]$xegaArgs, 
               fileName=fileName)
   return(newDF) }
# function start:
fns<-list.files(path=inpath, pattern="*\\.rds")
fullfns<-fullFileNames(treatments=fns, inpath=inpath)
treatnms<-unique(unlist(lapply(fns, FUN=treatnm)))
for (i in (1:length(treatnms)))
   { ix<-isInList(fns, treatnms[i])
     mfiles<-fullfns[ix]
    fileName<-paste0(outpath, .Platform$file.sep, "merge", treatnms[i], ".rds")
    sav<-mergeFiles(mfiles, fileName)
    saveRDS(sav, file=fileName) }
return(invisible(0))
}

