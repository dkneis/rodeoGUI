#' Run/create GUI for a rodeo-based model
#'
#' Run GUI for a rodeo-based model or prepare GUI for use on server.
#'
#' @param dirRodeo Directory containing the model definition. See notes.
#' @param dirScenarios Directory containing scenario definitions. See notes.
#' @param dirIntro Directory containing material for the model's 
#'   introduction page. See notes.
#' @param colsep Column separator used in delimited text files.
#' @param lib File path for the generated shared library. If \code{NULL}
#    this will be a file with a random name in R's temporary folder.
#' @param serverMode Defaults to \code{FALSE}. If set to \code{TRUE}, data
#'   required by the GUI are saved to disk but the shiny app is
#'   \emph{not} run.
#'
#' @return If \code{serverMode} is \code{FALSE}, the function returns
#'   \code{NULL}. Otherwise it returns the name of the file holding all
#'   information needed to run the GUI.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   library(rodeoGUI)
#' runGUI(
#' dirRodeo=system.file("examples/tank2/rodeo", package="rodeoGUI"),
#'   dirScenarios=system.file("examples/tank2/scenarios", package="rodeoGUI"),
#'   dirIntro=system.file("examples/tank2/intro", package="rodeoGUI")
#' )
#' }

runGUI <- function(
  dirRodeo = "./rodeo",
  dirScenarios = "./scenarios",
  dirIntro = "./startpages",
  colsep="\t",
  lib=NULL,
  serverMode=FALSE
) {

  ##############################################################################

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
  if (is.null(lib)) {
    libFile <- tempfile(pattern="rodeo")
  } else {
    libFile <- gsub(pattern="\\", replacement="/",
        x=suppressWarnings(normalizePath(lib)), fixed=TRUE)
    libFile <- paste(dirname(libFile), tolower(basename(libFile)), sep="/")
  }
  model$compile(sources=funsF, lib=libFile)

  ##############################################################################

  # Read scenario files
  files <- c(titles="titles.txt", descriptions="descriptions.txt", defaults="defaults.txt")
  files <- setNames(paste(dirScenarios, files, sep="/"), names(files))
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
  
  # Prepare start pages
  filecontents <- function(f) {
    if (!file.exists(f))
      stop("file '",f,"' not found")
    paste(readLines(con=f), collapse="\n")
  }
  intro <- c(
    EN=filecontents(paste(dirIntro,"EN",sep="/")),
    DE=filecontents(paste(dirIntro,"DE",sep="/"))
  )
  # replace <img ... src=filename.svg ...> by inline svg
  inlineSVG <- function(html, svgDir) {
    pattern <- "[<]img[ ][^<]+[>]"
    out <- html
    while (grepl(x=out, pattern=pattern)) {
      start <- regexpr(text=out, pattern=pattern)
      end <- start + as.integer(attr(start, "match.length")) - 1
      tmp <- substr(out, start, end)
      tmp <- gsub(x=tmp, pattern="^[<]img[ ].*src=\"", replacement="")
      tmp <- gsub(x=tmp, pattern="\".*>$", replacement="")
      svgFile <- paste0(svgDir,"/",tmp)
      if (file.exists(svgFile))
        out <- paste0(substr(out, 1, start-1), "\n",
          filecontents(svgFile), "\n", substr(out, end+1, nchar(out)), "\n")
      else
        warning(paste0("failed to put contents of file '",svgFile,"' inline"))
    }
    out
  }
  intro <- sapply(intro, inlineSVG, svgDir=dirIntro)

  ##############################################################################

  # Save data in .rda file to be loaded in server/ui
  XDATA <- list(
    model= model,
    lib= if (serverMode) basename(libFile) else libFile,
    funsR= if (serverMode) basename(funsR) else funsR,
    intro= intro,
    scenTitles= scenTitles,
    scenDescriptions= scenDescriptions,
    scenDefaults= scenDefaults
  )

  # NOTE: File/path name must be consistent with path at top of 'global.R'
  filename <- paste0(gsub(pattern="\\", replacement="/", x=tempdir(),
    fixed=TRUE), "/rodeoGuiData.rda")
  save(XDATA, file=filename, ascii=FALSE)
  rm(XDATA)

  # Start shiny app
  if (serverMode) {
    return(filename)
  } else {
    shiny::runApp(system.file("shiny", package="rodeoGUI"))
    return(invisible(NULL))
  }
}
