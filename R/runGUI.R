#' Prepare files needed by the GUI
#'
#' Prepare files needed to run a rodeo-based model in the shiny GUI.
#'
#' @param appName Application name displayed in top left corner of GUI.
#' @param vars Input table.
#' @param pars Input table.
#' @param funs Input table.
#' @param pros Input table.
#' @param stoi Input table.
#' @param sourcesR R source files.
#' @param sourcesF Fortran source files.
#' @param dirScenarios Directory containing scenario definitions. See notes.
#' @param dirIntro Directory containing material for the
#'   model's introduction pages. See notes.
#' @param colsep Column separator used in delimited text files.
#' @param useTemp If \code{TRUE} (default), the produced files are created in
#'   the session's temporary folder. If \code{FALSE}, the files are creared in
#'   the current working directory (which is rarely useful if the GUI is to be
#'   run on the local machine).
#' @param maxNumberOfScenarios Maximum number of scenarios that can be active
#'   at a time. Reasonable values are between 2 and 4.
#' @param maxNumberOfTimeSteps Maximum allowed number of time steps in
#'   dynamic simulations. The chosen value should take into account the
#'   resources of the machine as well as the number of parallel sessions in a
#'   multi-user environment.
#' @param tStart Start of dynamic simulation period. Default value.
#' @param tFinal End of dynamic simulation period. Default value.
#' @param tStep Time step for dynamic simulation. Default value.
#' @param tShow Time after which dynamic results are displayed. Default value.
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
#' # see 'runGUI'

preGUI <- function(
  appName = "rodeoGUI",
  vars = "vars.txt",
  pars = "pars.txt",
  funs = "funs.txt",
  pros = "pros.txt",
  stoi = "stoi.txt",
  sourcesR = "functions.r",
  sourcesF = "functions.f95",
  dirScenarios = "./scenarios",
  dirIntro = "./intro",
  colsep="\t",
  useTemp = TRUE,
  maxNumberOfScenarios= 3,
  maxNumberOfTimeSteps= 10000,
  tStart= 0,
  tFinal= 10,
  tStep= 0.1,
  tShow= 0
) {

  # Function to read delimited text files
  rd <- function(f, ...) {
    if (!file.exists(f))
      stop("input file '",f,"' not found")
    utils::read.table(file=f, header=TRUE, sep=colsep, ...)
  }

  # Check existence of function definitions
  if (length(sourcesR) == 0)
    stop("no R source file supplied, need a dummy file at least")
  rCode <- ""
  for (f in sourcesR) {
    if (!file.exists(f))
      stop("R source file not found ('",f,"')")
    rCode <- paste0(rCode, "\n", paste(readLines(f), collapse="\n"))
  }
  tryCatch({
    parse(text=rCode)
  }, error = function(e) {
    stop(paste("attempt to parse code from R source file(s) failed: ",e))
  })

  if (length(sourcesF) == 0)
    stop("no Fortran source file supplied, need a dummy file with module 'functions' at least")
  for (f in sourcesF) {
    if (!file.exists(f))
      stop("Fortran source file not found ('",f,"')")
  }

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
  model$compile(sources=sourcesF, lib=libFile)

  ##############################################################################

  # Set/check folder names
  chkDir <- function(d) {
    d <- gsub(pattern="\\", replacement="/", x=d, fixed=TRUE)
    if (!dir.exists(d))
      stop(paste0("directory '",d,"' does not exist"))
    d
  }
  dirScenarios <- chkDir(dirScenarios)
  dirIntro <- chkDir(dirIntro)
  
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
    stop("matrix of scenario titles doesn't match with matrix of scenario descriptions")
  if (!identical(sort(rownames(scenTitles)), sort(colnames(scenDefaults))))
    stop("matrix of scenario titles doesn't match with matrix of scenario default values")

  # Check user-specified limits
  tryCatch({
    maxNumberOfScenarios <- as.integer(maxNumberOfScenarios)
    stopifnot(maxNumberOfScenarios > 0)
    stopifnot(maxNumberOfScenarios < 6)
  }, error = function(x) {
    stop("max. number of scenarios is invalid or outside reasonable range")
  })
  tryCatch({
    maxNumberOfTimeSteps <- as.integer(maxNumberOfTimeSteps)
    stopifnot(maxNumberOfTimeSteps > 0)
    stopifnot(maxNumberOfTimeSteps <= 1e7)
  }, error = function(x) {
    stop("max. number of time steps is invalid or outside reasonable range")
  })

  # Check user-specified time settings
  tryCatch({
    tStart <- as.numeric(tStart)
    tFinal <- as.numeric(tFinal)
    tStep <- as.numeric(tStep)
    tShow <- as.numeric(tShow)
    stopifnot(tStart < tFinal)
    stopifnot(length(seq(from=tStart, to=tFinal, by=tStep)) >= 2)
    stopifnot((tShow >= tStart) && (tShow < tFinal))
  }, error = function(x) {
    stop("bad specification of dynamic simulation period")
  })

  ##############################################################################
  
  # Save data in .rda file to be loaded in server/ui
  XDATA <- list(
    model= model,
    lib= if (useTemp) libFile else basename(libFile),
    rCode= rCode,
    appName= appName,
    dirIntro= dirIntro,
    scenTitles= scenTitles,
    scenDescriptions= scenDescriptions,
    scenDefaults= scenDefaults,
    maxNumberOfScenarios= maxNumberOfScenarios,
    maxNumberOfTimeSteps= maxNumberOfTimeSteps,
    tStart= tStart,
    tFinal= tFinal,
    tStep= tStep,
    tShow= tShow
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
#'   vars=system.file("examples/tank2/rodeo/vars.txt", package="rodeoGUI"),
#'   pars=system.file("examples/tank2/rodeo/pars.txt", package="rodeoGUI"),
#'   funs=system.file("examples/tank2/rodeo/funs.txt", package="rodeoGUI"),
#'   pros=system.file("examples/tank2/rodeo/pros.txt", package="rodeoGUI"),
#'   stoi=system.file("examples/tank2/rodeo/stoi.txt", package="rodeoGUI"),
#'   sourcesR=system.file("examples/tank2/rodeo/functions.r", package="rodeoGUI"),
#'   sourcesF=system.file("examples/tank2/rodeo/functions.f95", package="rodeoGUI"),
#'   dirScenarios=system.file("examples/tank2/scenarios", package="rodeoGUI"),
#'   dirIntro=system.file("examples/tank2/intro", package="rodeoGUI"),
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
