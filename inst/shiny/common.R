library(rodeo)
# library(rodeoGUI)  # don't load here as this would prevent function overloading
library(deSolve)
library(rootSolve)
library(reshape2)
library(svglite)

########################################################################

# Load XDATA list object from file with expected name
dirs <- c(".", "shiny.app", gsub(pattern="\\", replacement="/", x=tempdir(), fixed=TRUE))
f <- "rodeoGuiData.rda"
loaded <- FALSE
for (d in dirs) {
  if (file.exists(paste(d,f,sep="/"))) {
    load(file=paste(d,f,sep="/"))
    loaded <- TRUE
    break
  }
}
if (!loaded)
  stop(paste0("file '",f,
    "' not found in folder(s): '",paste(dirs,collapse="', '"),"'"))

# Load R functions possibly needed to run model$stoichiometry()
eval(parse(text=XDATA$rCode))

########################################################################

# Labels of GUI in multiple languages
translate <- as.matrix(read.table(system.file("translations/translations.txt",
  package="rodeoGUI"), sep="\t", header=TRUE, colClasses="character", quote="", row.names=1))

########################################################################
########################################################################
# ORDINARY FUNCTIONS
########################################################################
########################################################################

lastErrMsg <- function(msg) {
  gsub(x=geterrmessage(), pattern="Error in [^:]+: ", replacement="")
}

framedMessage <- function(msg) {
  paste0("<p style='background-color:",guiPink(),
    ";border:2px; border-style:solid; border-color:",guiGrey(dark=TRUE),
    "; padding:1em;'>",msg,"</p>")  
}

########################################################################
# Updates parameters and initial values for a specific scenario

updateInputs <- function(
  model,        # rodeo object
  scenDefaults, # matrix with defaults for all scenarios
  scenDefaultId,# Id of selected default scenario
  scenEdits,    # user input for this scenario (character string)
  lang
) {
  # get default pars and vars
  if (is.null(scenDefaultId))
    scenDefaultId <- colnames(scenDefaults)[1]
  dflt <- setNames(sapply(scenDefaults[,scenDefaultId],
      function(x) {eval(parse(text=as.character(x)))}), rownames(scenDefaults))
  pars <- dflt[model$namesPars()]
  vars <- dflt[model$namesVars()]
  # get user inputs for this scenario
  tryCatch({
    val <- eval(parse(text=paste("c(",scenEdits,")")))
    if (length(val) > 0) {
      stopifnot(!is.null(names(val)))
      stopifnot(all(names(val) != ""))
    }
  }, error = function(e) {
    stop(paste0(translate["invalidUserInput",lang],": '",
      scenEdits,"'. ",translate["expectingNamedVector",lang],"."))
  })
  if (length(val) > 0) {
    unknown <- names(val)[!names(val) %in% c(names(pars),names(vars))]
    if (length(unknown) > 0)
      stop(paste0(translate["assignmentToUnknownName",lang],": '",
        paste(unknown,collapse="', '"),"'"))
  }
  # update pars vector
  user.pars <- val[names(val) %in% names(pars)]
  if (length(user.pars) > 0)
    pars[names(user.pars)] <- unname(user.pars)
  # update vars vector
  user.vars <- val[names(val) %in% names(vars)]
  if (length(user.vars) > 0)
    vars[names(user.vars)] <- unname(user.vars)
  # update in object
  model$setPars(pars)
  model$setVars(vars)
  invisible(NULL)
}


########################################################################
# Returns the stoichiometry matrix as colored HTML table 

stoiAsHTML <- function(model, selectedVars, selectedPros, lang) {
  signAsColor <- function(x) {
    if (as.numeric(x) > 0) return(paste0('<div style="background-color:',guiOrange(),';">',
      formatC(as.numeric(x), digits=2, format="e"),'</div>'))
    if (as.numeric(x) < 0) return(paste0('<div style="background-color:',guiBlue(),';">',
      formatC(as.numeric(x), digits=2, format="e"),'</div>'))
    return("")  # zero not printed
  }
  m <- model$stoichiometry(box=1, time=0)[selectedPros,selectedVars,drop=FALSE]
  tbl <- cbind(data.frame(process=rownames(m), stringsAsFactors=FALSE),
    as.data.frame(m, check.names=FALSE))
  exportDF(x=tbl, tex=FALSE,
    align=setNames(rep("center", ncol(m)), colnames(m)),
    colnames= setNames(translate["process",lang], "process"),
    funCell= setNames(replicate(ncol(m), signAsColor), colnames(m))
  )
}

########################################################################
# Returns the process rates as a HTML table including stoi. factors for a single variable

prosTable <- function(model, selectedVar, hide, lang) {
  if (!paste0("descr.",lang) %in% names(model$getProsTable()))
    descrCol <- "description"
  else
    descrCol <- paste0("descr.",lang)
  tbl <- merge(x=model$getProsTable()[,c("name","unit",descrCol,"expression")],
    y=data.frame(process=rownames(model$stoichiometry()),
      factor=model$stoichiometry()[,selectedVar], stringsAsFactors=FALSE),
    by.x="name", by.y="process")
  italic <- function(x) {paste0('<div style="font-style:italic;">',
    x,'</div>')}
  tbl <- data.frame(lapply(tbl, as.character),stringsAsFactors=FALSE)
  if (hide) {
    tmp <- tbl[tbl[,"factor"] != "0",]
    if (nrow(tmp) > 0) {
      tbl <- tmp
    } else {
      tbl <- tbl[1,]
      for (i in 1:ncol(tbl))
        tbl[,i] <- "--"
    }
  }
  exportDF(x=tbl, tex=FALSE,
    width=setNames(c(15,10,30,30,15), c("name","unit",descrCol,"expression","factor")),
    align=setNames(rep("left", ncol(tbl)), colnames(tbl)),
    colnames= setNames(
      c(translate["process",lang], translate["unit",lang], translate["description",lang],
       translate["expression",lang], translate["factor",lang]),
      c("name", "unit", descrCol, "expression", "factor")
    ),
    funCell= setNames(replicate(2, italic), c("expression", "factor"))
  )
}

########################################################################
# Returns the functions as a HTML table

funsTable <- function(model, lang) {
  if (!paste0("descr.",lang) %in% names(model$getFunsTable()))
    descrCol <- "description"
  else
    descrCol <- paste0("descr.",lang)
  tbl <- model$getFunsTable()[,c("name","unit",descrCol)]
  tbl <- data.frame(lapply(tbl, as.character),stringsAsFactors=FALSE)
  exportDF(x=tbl, tex=FALSE,
    width=setNames(c(15,15,70), c("name","unit",descrCol)),
    align=setNames(rep("left", ncol(tbl)), colnames(tbl)),
    colnames= setNames(
      c(translate["function_",lang], translate["unit",lang], translate["description",lang]),
      c("name", "unit", descrCol)
    )
  )
}

########################################################################
# Creates scenario summary table in HTML for chosen item type

scenDescrTable <- function(scenTitles, scenDefaults, model, lang,
  what=c("variable","parameter"), group="", sort=FALSE) {
  if (what == "variable") {
    items <- model$namesVars()
    tbl <- model$getVarsTable()
  } else if (what == "parameter") {
    items <- model$namesPars()
    tbl <- model$getParsTable()
  } else {
    stop("inappropriate value passed to 'what'")
  }
  if (!paste0("descr.",lang) %in% names(tbl)) {
    descrCol <- "description"
  } else {
    descrCol <- paste0("descr.",lang)
  }
  if (!paste0("group.",lang) %in% names(tbl)) {
    groupCol <- "group"
  } else {
    groupCol <- paste0("group.",lang)
  }
  out <- tbl[match(items, tbl[,"name"]), c("name","unit",descrCol,groupCol)]
  val <- as.data.frame(scenDefaults[items,,drop=FALSE])
  val <- data.frame(lapply(val, as.character),stringsAsFactors=FALSE)
  whichDiffer <- which(apply(val, 1, function(x){!all(x == x[1])}))
  if (length(whichDiffer) > 0) {
    for (i in whichDiffer) {
      val[i,] <- paste0('<div style="background-color:',guiOrange(),';">',val[i,],'</div>')
    }
  }
  val <- cbind(item=items, val)
  out <- merge(x=out, y=val, by.x="name", by.y="item", sort=sort)
  out <- data.frame(lapply(out, as.character),stringsAsFactors=FALSE)
  names(out)[1] <- translate[what,lang]
  names(out)[2:3] <- c(translate["unit",lang],translate["description",lang])
  # filter by group
  if (group != "")
    out <- out[out[,groupCol] == group,]
  out <- out[, names(out) != groupCol]
  
  if (nrow(out) > 0) { # check avoids warning when language is changed while group selection is active
    exportDF(out,
      width=setNames(c(15, 15, 100-15-ncol(scenDefaults)*10-15, rep(10, ncol(scenDefaults))), names(out)),
      align=setNames(c(rep("left",3), rep("right", ncol(scenDefaults))), names(out)),
      colnames=scenTitles[colnames(scenDefaults),lang],
      tex=FALSE
    )
  }
}

########################################################################
# SVG icons used by GUI

# Help open/close button
symbolHelpOpen <- paste0('
  <svg viewBox="0 0 110 110" height="25px">
    <circle cx="55" cy="55" r="50" style="fill:',guiBlue(dark=TRUE),
      '; stroke:',guiBlue(dark=TRUE),'; stroke-width:5;"/>
    <text x="37" y="77" style="font-size:70px; fill:white;">?</text>
  </svg>
')
symbolHelpClose <- paste0('
  <svg viewBox="0 0 110 110" height="25px">
    <circle cx="55" cy="55" r="50" style="fill:',guiBlue(dark=TRUE),
      '; stroke:',guiBlue(dark=TRUE),'; stroke-width:5;"/>
  <path d="M 20,55 75,30 75,80 z" style="fill:white; stroke:none"/>
  </svg>
')

########################################################################
# Read help pages

readHelp <- function(lang, topics) {
  out <- ""
  for (top in topics) {
    out <- paste(out,
      paste(readLines(paste0(system.file("uiHelp", package="rodeoGUI"),"/",
      top,".",lang)), collapse="\n"), collapse="\n")
  }
  out
}

langs <- c("EN","DE","UA")

help <- rbind(
  missing= c(
    EN="A help page is currently not available for the selected view.",
    DE="Für die gewählte Ansicht ist noch keine Hilfe verfügbar.",
    UA="Сторінка довідки наразі не доступна для вибраного представлення даних."
  ),
  intro= sapply(langs, readHelp, "introduction"),
  stoi= sapply(langs, readHelp, "stoichiometry"),
  pros= sapply(langs, readHelp, "tableOfProcesses"),
  funs= sapply(langs, readHelp, "tableOfFunctions"),
  scenDesc= sapply(langs, readHelp, "tableOfScenarios"),
  scenVars= sapply(langs, readHelp, "tableOfVariables"),
  scenPars= sapply(langs, readHelp, "tableOfParameters"),
  dyn= sapply(langs, readHelp, c("defineScenarios","startComputation","detailsDynamic")),
  std= sapply(langs, readHelp, c("defineScenarios","startComputation","detailsSteady"))
)
