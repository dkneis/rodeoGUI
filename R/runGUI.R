#' Prepare files needed by the GUI
#'
#' Prepare files needed to run a rodeo-based model in the shiny GUI.
#'
#' @param dirRodeo Directory containing the model definition. See notes.
#' @param dirScenarios Directory containing scenario definitions. See notes.
#' @param dirIntro Directory containing material for the
#'   model's introduction pages. See notes.
#' @param colsep Column separator used in delimited text files.
#' @param useTemp If \code{TRUE} (default), the produced files are created in
#'   the session's temporary folder. If \code{FALSE}, the files are creared in
#'   the current working directory (which is rarely useful if the GUI is to be
#'   run on the local machine).
#'
#' @return The function does not return anything but it creates two files in the
#'   current working directory. The first file with the fixed name
#'   'rodeoGuiData.rda' holds various data needed by the GUI. The second file is
#'   a system-specific shared library. The file name starts with 'rodeo'
#'   followed by a random pattern and the extension according to the
#'   \code{dynlib.ext} component of \code{\link[base]{.Platform}}.
#'   
#' @note Detailed help text still missing.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(rodeoGUI)
#' preGUI(
#'   dirRodeo=system.file("examples/tank2/rodeo", package="rodeoGUI"),
#'   dirScenarios=system.file("examples/tank2/scenarios", package="rodeoGUI"),
#'   dirIntro=system.file("examples/tank2/intro", package="rodeoGUI"),
#'   colsep="\t"
#' )
#' }

preGUI <- function(
  dirRodeo = "./rodeo",
  dirScenarios = "./scenarios",
  dirIntro = "./intro",
  colsep="\t",
  useTemp = TRUE
) {

  # Set/check file names
  chkDir <- function(d) {
    d <- gsub(pattern="\\", replacement="/", x=suppressWarnings(normalizePath(d)), fixed=TRUE)
    if (!dir.exists(d))
      stop(paste0("directory '",d,"' does not exist"))
    d
  }
  dirRodeo <- chkDir(dirRodeo)
  dirScenarios <- chkDir(dirScenarios)
  dirIntro <- chkDir(dirIntro)

  # Function to read delimited text files
  rd <- function(f, ...) {
    if (!file.exists(f))
      stop("input table '",f,"' not found")
    utils::read.table(file=f, header=TRUE, sep=colsep, ...)
  }

  # Set file names
  vars <- paste0(dirRodeo, "/vars.txt")
  pars <- paste0(dirRodeo, "/pars.txt")
  funs <- paste0(dirRodeo, "/funs.txt")
  pros <- paste0(dirRodeo, "/pros.txt")
  stoi <- paste0(dirRodeo, "/stoi.txt")
  funsR <- paste0(dirRodeo, "/functions.r")
  funsF <- list.files(path=dirRodeo, pattern=".+[.]f95$", full.names=TRUE)

  # Check existence of function definitions
  if (length(funsR) == 0)
    stop("no file with R functions supplied, need a dummy file at least")
  if (!file.exists(funsR))
    stop("file with function definitions in R not found ('",funsR,"')")
  rCode <- paste(readLines(funsR), collapse="\n")
  tryCatch({
    parse(text=rCode)
  }, error = function(e) {
    stop(paste("attempt to parse contents of file '",funsR,"' failed: ",e))
  })
  if (length(funsF) == 0)
    stop("no Fortran file supplied, need a dummy file with module 'functions' at least")
  if (!all(file.exists(funsF)))
    stop("file(s) with function definitions in Fortran not found ('",
      paste(funsF, collapse="', "),"')")

  # Create object
  model <- rodeo::rodeo$new(
    vars=rd(vars), pars=rd(pars), funs=rd(funs), pros=rd(pros),
    stoi=as.matrix(rd(stoi, row.names=1)), asMatrix=TRUE,
    dim=c(1))  # GUI handles 0D models only

  # Build library
  if (useTemp) {
    libFile <- tempfile(pattern="rodeo", tmpdir=gsub(x=tempdir(), pattern="\\", replacement="/", fixed=TRUE))
  } else {
    libFile <- tempfile(pattern="rodeo", tmpdir=".")
  }
  model$compile(sources=funsF, lib=libFile)

  ##############################################################################

  # Read scenario files
  files <- c(titles="titles.txt", descriptions="descriptions.txt", defaults="defaults.txt")
  files <- stats::setNames(paste(dirScenarios, files, sep="/"), names(files))
  for (i in 1:length(files)) {
    if (!file.exists(files[i]))
      stop(paste("file with scenario ",names(files)[i]," ('",files[i],"') not found"))
  }
  scenTitles <- as.matrix(rd(files["titles"], row.names=1, check.names=FALSE))
  scenDescriptions <- as.matrix(rd(files["descriptions"], row.names=1, check.names=FALSE))
  scenDefaults <- as.matrix(rd(files["defaults"], row.names=1, check.names=FALSE))

  # Check contents of scenario files
  if (!identical(sort(rownames(scenTitles)), sort(rownames(scenDescriptions))) || 
    !identical(sort(colnames(scenTitles)), sort(colnames(scenDescriptions))))
    stop("Matrix of scenario titles doesn't match with matrix of scenario descriptions")
  if (!identical(sort(rownames(scenTitles)), sort(colnames(scenDefaults))))
    stop("Matrix of scenario titles doesn't match with matrix of scenario default values")

  ##############################################################################
  
  # Save data in .rda file to be loaded in server/ui
  XDATA <- list(
    model= model,
    lib= if (useTemp) libFile else basename(libFile),
    rCode= rCode,
    dirIntro= dirIntro,
    scenTitles= scenTitles,
    scenDescriptions= scenDescriptions,
    scenDefaults= scenDefaults
  )

  # NOTE: File name must be consistent with path at top of 'global.R'
  if (useTemp) {
    rdaFile <- paste0(gsub(x=tempdir(), pattern="\\", replacement="/", fixed=TRUE),"/rodeoGuiData.rda")
  } else {
    rdaFile <- "rodeoGuiData.rda"
  }
  save(XDATA, file=rdaFile, ascii=FALSE)

  invisible(NULL)
}


#' Locally run rodeo-based model in GUI
#'
#' Locally run a rodeo-based model in a specialized shiny GUI.
#' 
#' @param useTemp If \code{TRUE} (default), the function expects to find the
#'   two files created by \code{preGUI} in the sessions's temporary folder. If
#'   \code{FALSE}, the function attempts to find the files in the
#'   current working directory.
#'
#' @return \code{NULL}
#' 
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(rodeoGUI)
#' preGUI(
#'   dirRodeo=system.file("examples/tank2/rodeo", package="rodeoGUI"),
#'   dirScenarios=system.file("examples/tank2/scenarios", package="rodeoGUI"),
#'   dirIntro= =system.file("examples/tank2/intro", package="rodeoGUI"),
#'   colsep="\t"
#' )
#' runGUI()
#' }

runGUI <- function(
  useTemp = TRUE
) {
  if (useTemp) {
    rdaFile <- paste0(gsub(x=tempdir(), pattern="\\", replacement="/", fixed=TRUE),"/rodeoGuiData.rda")
  } else {
    rdaFile <- "rodeoGuiData.rda"
  }
  if (!file.exists(rdaFile))
    stop("file '",rdaFile,"' not found")
  load(rdaFile)
  lib <- paste0(XDATA$lib,.Platform$dynlib.ext)
  if (!file.exists(lib))
    stop(paste0("file '",lib,"' not found in working directory"))
  rm(XDATA)
  shiny::runApp(system.file("shiny", package="rodeoGUI"))
}
