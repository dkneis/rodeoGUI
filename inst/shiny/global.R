library(rodeo)
library(deSolve)
library(rootSolve)
#library(xtable)
library(reshape2)
library(svglite)

########################################################################

# Load XDATA list object from file with expected name
dirs <- c(".",gsub(pattern="\\", replacement="/", x=tempdir(), fixed=TRUE))
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

# Labels in GUI
translate <- rbind(
  and = c(EN="and", DE="und"),
  assignmentToUnknownName = c(EN="Cannot assign value to unknown parameter or variable",
    DE="Wertzuweisung nicht möglich da Parameter oder Variable unbekannt"),
  description = c(EN="Description", DE="Beschreibung"),
  dynamics = c(EN="Dynamics", DE="Dynamik"),
  effectOnSteadyState = c(EN="Effect on steady st.", DE="Effekt auf Gleichgew."),
  everything = c(EN="Everything", DE="Alles"),
  expectingNamedVector = c(EN="Expecting assignment statement like 'pi = 3.1415' or a comma delimited list of such statements",
                           DE="Erwarte Zuweisung der Form 'pi = 3.1415' oder eine durch Komma getrennte Liste solcher Zuweisungen"),
  expectingUnnamedVector = c(EN="Expecting a comma delimited list of numeric values",
                           DE="Erwarte eine durch Komma getrennte Liste numerischer Werte"),
  expression = c(EN="Expression", DE="Ausdruck"),
  factor = c(EN="Factor", DE="Faktor"),
  failedToSetModelInputsForScenario = c(EN="Failed to set input values for scenario", DE="Fehler beim Setzen der Eingangsdaten für Szenario"),
  failedToComputeSolutionFor = c(EN="Failed to compute solution for", DE="Fehler beim Berechnen für"),
  function_ = c(EN="Function", DE="Funktion"),
  functions = c(EN="Functions", DE="Funktionen"),
  hideInactiveProcesses = c(EN="Hide inactive processes", DE="Inaktive Prozesse ausblenden"),
  identifier = c(EN="Short name", DE="Bezeichnung"),
  initialValues = c(EN="Initial values", DE="Anfangswerte"),
  introduction = c(EN="Introduction", DE="Einführung"),
  introPageMissing = c(EN="Introduction page not available",
                       DE="Einstiegsseite nicht verfügbar"),
  invalidUserInput = c(EN="Invalid user input", DE="Ungültige Eingabe"),
  invalidVectorOfTimes = c(EN="Invalid specification of time period and/or time step",
    DE="Ungültige Spezifikation von Zeitfenster und/oder Zeitschritt"),
  needsUpdate = c(EN="Press the 'Compute' button to update results!",
    DE="Drücke 'Berechnen' um das Ergebnis zu aktualisieren!"),
  numberOfScenarios = c(EN="Number of scenarios", DE="Anzahl Szenarios"),
  overview = c(EN="Overview", DE="Übersicht"),
  parameter = c(EN="Parameter", DE="Parameter"),
  parameters = c(EN="Parameters", DE="Parameter"),
  patternNotMatchingAnyVariable = c(EN="Pattern does not match any variable name", DE="Kein Variablenname entspricht diesem Muster"),
  patternNotMatchingAnyProcess = c(EN="Pattern does not match any process name", DE="Kein Prozessname entspricht diesem Muster"),
  patternToSelectVariables = c(EN="Variable name matches pattern", DE="Variablenname folgt Muster"),
  patternToSelectProcesses = c(EN="Process name matches pattern", DE="Prozessname folgt Muster"),
  patternNotValid = c(EN="Pattern is not a valid regular expression.", DE="Muster ist kein gültiger regulärer Ausdruck."),
  process = c(EN="Process", DE="Prozess"),
  processes = c(EN="Processes", DE="Prozesse"),
  run = c(EN="Compute", DE="Berechnen"),
  scenario = c(EN="Scenario", DE="Szenario"),
  scenarios = c(EN="Scenarios", DE="Szenarios"),
  selectByPattern = c(EN="Select by pattern", DE="Selektiere nach Muster"),
  selectView = c(EN="Select view", DE="Ansicht wählen"),
  showStoichiometryFactorFor = c(EN="Show stoichiometric factor for", DE="Zeige Stöchiometriefaktor für"),  
  simulation = c(EN="Simulation", DE="Simulation"),
  steadystate = c(EN="Steady state", DE="Gleichgewicht"),
  stoichiometry = c(EN="Stoichiometry", DE="Stöchiometrie"),
  time = c(EN="Time", DE="Zeit"),
  tStart = c(EN="From time", DE="Beginn"),
  tFinal = c(EN="To time", DE="Ende"),
  tStep = c(EN="Time step", DE="Zeitschritt"),
  tShow = c(EN="Display from", DE="Zeige ab"),
  unit = c(EN="Unit", DE="Einheit"),
  useAsMultipliers = c(EN="Multiply with default", DE="Multipliziere mit Standard"),
  usesTheRPackages = c(EN="uses the R-packages", DE="nutzt die R-Pakete"),
  values = c(EN="Values", DE="Werte"),
  variable = c(EN="Variable", DE="Variable"),
  variables = c(EN="Variables", DE="Variablen"),
  variedItem = c(EN="Varied item", DE="Variierte Größe")
)

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
  if (!lang %in% names(model$getProsTable()))
    descrCol <- "description"
  else
    descrCol <- lang
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
  if (!lang %in% names(model$getFunsTable()))
    descrCol <- "description"
  else
    descrCol <- lang
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

scenDescrTable <- function(scenTitles, scenDefaults, model, lang, what=c("variable","parameter")) {
  if (what == "variable") {
    items <- model$namesVars()
    tbl <- model$getVarsTable()
  } else if (what == "parameter") {
    items <- model$namesPars()
    tbl <- model$getParsTable()
  } else {
    stop("inappropriate value passed to 'what'")
  }
  if (!lang %in% names(tbl))
    descrCol <- "description"
  else
    descrCol <- lang
  out <- tbl[match(items, tbl[,"name"]), c("name","unit",descrCol)]
  val <- as.data.frame(scenDefaults[items,,drop=FALSE])
  val <- data.frame(lapply(val, as.character),stringsAsFactors=FALSE)
  whichDiffer <- which(apply(val, 1, function(x){!all(x == x[1])}))
  if (length(whichDiffer) > 0) {
    for (i in whichDiffer) {
      val[i,] <- paste0('<div style="background-color:',guiOrange(),';">',val[i,],'</div>')
    }
  }
  val <- cbind(item=items, val)
  out <- merge(x=out, y=val, by.x="name", by.y="item")
  out <- data.frame(lapply(out, as.character),stringsAsFactors=FALSE)
  names(out)[1] <- translate[what,lang]
  names(out)[2:3] <- c(translate["unit",lang],translate["description",lang])
  exportDF(out,
    width=setNames(c(15, 15, 100-15-ncol(scenDefaults)*10-15, rep(10, ncol(scenDefaults))), names(out)),
    align=setNames(c(rep("left",3), rep("right", ncol(scenDefaults))), names(out)),
    colnames=scenTitles[colnames(scenDefaults),lang],
    tex=FALSE
  )
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

langs <- c("EN","DE")

help <- rbind(
  missing= c(
    EN="A help page is currently not available for the selected view.",
    DE="Für die gewählte Ansicht ist noch keine Hilfe verfügbar."
  ),
  intro= sapply(langs, readHelp, "introduction"),
  stoi= sapply(langs, readHelp, "stoichiometry"),
  pros= sapply(langs, readHelp, "tableOfProcesses"),
  funs= sapply(langs, readHelp, "tableOfFunctions"),
  scenDesc= sapply(langs, readHelp, "tableOfScenarios"),
  scenVars= sapply(langs, readHelp, "tableOfVariables"),
  scenPars= sapply(langs, readHelp, "tableOfParameters"),
  dyn= sapply(langs, readHelp, c("defineScenarios","startComputation","detailsDynamic")),
  std= sapply(langs, readHelp, c("defineScenarios","startComputation","detailsSteady")),
  eff= sapply(langs, readHelp, "effects")
)
