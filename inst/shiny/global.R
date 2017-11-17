library(rodeo)
library(deSolve)
library(rootSolve)
library(xtable)

########################################################################

# Load data created by runGUI
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
  stop(paste0("GUI initialization file '",f,
    "' not found in folder(s): '",paste(dirs,collapse="', '"),"'"))

# Load R functions
source(rodeoGuiData$funsR)

# Dissolve list
model <- rodeoGuiData$model
lib <- rodeoGuiData$lib
intro <- rodeoGuiData$intro
scenTitles <- rodeoGuiData$scenTitles
scenDescriptions <- rodeoGuiData$scenDescriptions
scenDefaults <- rodeoGuiData$scenDefaults
rm(rodeoGuiData)

########################################################################

# Labels in GUI
translate <- rbind(
  and = c(EN="and", DE="und"),
  done = c(EN="Done", DE="Fertig"),
  description = c(EN="Description", DE="Beschreibung"),
  dynamics = c(EN="Dynamics", DE="Dynamik"),
  expression = c(EN="Expression", DE="Ausdruck"),
  factor = c(EN="Factor", DE="Faktor"),
  identifier = c(EN="Short name", DE="Bezeichnung"),
  initialValues = c(EN="Initial values", DE="Anfangswerte"),
  needsUpdate = c(EN="Please (re)run", DE="Bitte (neu)berechnen"),  
  onLeftAxis = c(EN="On left axis", DE="Auf linker Achse"),
  onRightAxis = c(EN="On right axis", DE="Auf rechter Achse"),
  overview = c(EN="Overview", DE="Übersicht"),
  parameter = c(EN="Parameter", DE="Parameter"),
  parameters = c(EN="Parameters", DE="Parameter"),  
  process = c(EN="Process", DE="Prozess"),
  processes = c(EN="Processes", DE="Prozesse"),
  run = c(EN="Compute", DE="Berechnen"),
  scenario = c(EN="Scenario", DE="Szenario"),
  scenarios = c(EN="Scenarios", DE="Szenarios"),
  selectView = c(EN="Select view", DE="Ansicht wählen"),
  showStoichiometryFactorFor = c(EN="Show stoichiometric factor for", DE="Zeige Stöchiometriefaktor für"),  
  simulation = c(EN="Simulation", DE="Simulation"),
  introduction = c(EN="Introduction", DE="Einführung"),
  steadystate = c(EN="Steady state", DE="Gleichgewicht"),
  stoichiometry = c(EN="Stoichiometry", DE="Stöchiometrie"),  
  tStart = c(EN="From time", DE="Beginn"),
  tFinal = c(EN="To time", DE="Ende"),
  tStep = c(EN="Time step", DE="Zeitschritt"),
  tShow = c(EN="Display from", DE="Zeige ab"),
  unit = c(EN="Unit", DE="Einheit"),
  usesTheRPackages = c(EN="uses the R-packages", DE="nutzt die R-Pakete"),
  variable = c(EN="Variable", DE="Variable"),
  variables = c(EN="Variables", DE="Variablen")
)

# Colors used for labels, bottons, etc
guiColors <- c(blue="#9fbfdf", blueDark="#3973ac",
  orange="#ffd480", orangeDark="#e69900",
  greyLight="#E5E5E5", greyDark="#7F7F7F")

########################################################################
########################################################################
# ORDINARY FUNCTIONS
########################################################################
########################################################################

########################################################################
# Updates parameters and initial values for a specific scenario

updateInputs <- function(
  model,        # rodeo object
  scenDefaults, # matrix with defaults for all scenarios
  scenDefaultId,# Id of selected default scenario
  scenEdits     # user input for this scenario (character string)
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
    scenEdits <- eval(parse(text=paste("c(",scenEdits,")")))
  }, error = function(e) {
    stop(paste0("Invalid user edit: '",scenEdits,"'"))
  })
  # update pars vector
  user.pars <- scenEdits[names(scenEdits) %in% names(pars)]
  if (length(user.pars) > 0)
    pars[names(user.pars)] <- unname(user.pars)
  # update vars vector
  user.vars <- scenEdits[names(scenEdits) %in% names(vars)]
  if (length(user.vars) > 0)
    vars[names(user.vars)] <- unname(user.vars)
  # update in object
  model$setPars(pars)
  model$setVars(vars)
  invisible(NULL)
}

########################################################################
# Plots dymanic outputs

visualizeDynamic <- function (
  out,       # data frame as output by dynamic solver with additional column 'scenario'
  display,   # list with elements 'L' and 'R', each holding a vector of variable names to show on left/right axis
  varsTable  # table of state variables as returned by model$getVarsTable()
) {
  # Constants
  const <- list(
    addAxisSpace = 0.5,
    multAxisSpace = 3.5
  )
  # Color function
  if ("color" %in% names(varsTable)) {
    clr <- function(x) {
      as.character(varsTable[match(x,varsTable[,"name"]),"color"])
    }
  } else {
    clrVect <- colorRampPalette(c("royalblue4","cyan","seagreen","red4"))(nrow(varsTable))
    clr <- function(x) {
      clrVect[match(x,varsTable[,"name"])]
    }
  }
  # Split display
  layout(matrix(1:2, nrow=2), heights=c(0.08, 0.92))
  omar <- par("mar")
  # Legend
  par(mar=c(0.2,max(4, 4*length(display[["L"]])),0.2,max(4, 4*length(display[["R"]]))))
  plot(x=0, y=0, type="n", bty="n", axes=FALSE, ann=FALSE)
  rect(xleft=par("usr")[1], ybottom=par("usr")[3], xright=par("usr")[2], ytop=par("usr")[4],
    col="#F7F6E7", border="lightgrey")
  dispAll <- unique(c(display[["L"]], display[["R"]]))
  if (length(dispAll) > 0) {
    legend("topleft", bty="n", horiz=TRUE, fill=clr(dispAll), legend=dispAll)
  }
  # Base of actual plot
  par(mar=c(4,max(4, 4*length(display[["L"]])),0.2,max(4, 4*length(display[["R"]]))))
  plot(range(out[,"time"]), c(-1,1), bty="n", type="n", axes=FALSE, ann=FALSE)
  rect(xleft=par("usr")[1], ybottom=par("usr")[3], xright=par("usr")[2], ytop=par("usr")[4],
    col="#F7F6E7", border="lightgrey")
  axis(side=1, line=const[["addAxisSpace"]])
  # Individual graphs
  for (ax in c("L","R")) {
    if (length(display[[ax]]) > 0) {
      for (k in 1:length(display[[ax]])) {
        par(new=TRUE)
        plot(range(out[,"time"]), range(out[,display[[ax]][k]]), type="n",
          axes=FALSE, ann=FALSE)
        axis(side=c(L=2,R=4)[ax], line=(k-1) * const[["multAxisSpace"]] + const[["addAxisSpace"]],
          col=clr(display[[ax]][k]))
        for (is in 1:length(unique(out[,"scenario"]))) {
          rows <- which(out[,"scenario"] == is)
          lines(out[rows,"time"], out[rows,display[[ax]][k]], lty=is,
            col=clr(display[[ax]][k]))
        }
      }
    }
  }
  # Reset
  par(mar=omar)
  layout(1)
}

########################################################################
# Returns the stoichiometry matrix as colored HTML table 

stoiAsHTML <- function(model, selectedVars, selectedPros, lang) {
  signAsColor <- function(x) {
    if (as.numeric(x) > 0) return(paste0('<div style="background-color:',guiColors["orange"],';">',
      formatC(as.numeric(x), digits=2, format="e"),'</div>'))
    if (as.numeric(x) < 0) return(paste0('<div style="background-color:',guiColors["blue"],';">',
      formatC(as.numeric(x), digits=2, format="e"),'</div>'))
    return("")
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

prosTable <- function(model, selectedVar, lang) {
  tbl <- merge(x=model$getProsTable()[,c("name","unit","description","expression")],
    y=data.frame(process=rownames(model$stoichiometry()),
      factor=model$stoichiometry()[,selectedVar], stringsAsFactors=FALSE),
    by.x="name", by.y="process")
  italic <- function(x) {paste0('<div style="font-style:italic;">',
    x,'</div>')}
  exportDF(x=tbl, tex=FALSE,
    width=setNames(c(15,10,30,30,15), c("name","unit","description","expression","factor")),
    align=setNames(rep("left", ncol(tbl)), colnames(tbl)),
    colnames= c(name=translate["process",lang], unit=translate["unit",lang],
      description=translate["description",lang],
      expression=translate["expression",lang],
      factor=translate["factor",lang]),
    funCell= setNames(replicate(2, italic), c("expression", "factor"))
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
  descrCol <- if (lang %in% names(tbl)) lang else "description"
  out <- tbl[match(items, tbl[,"name"]), c("name","unit",descrCol)]
  val <- as.data.frame(scenDefaults[items,,drop=FALSE])
  val <- data.frame(lapply(val, as.character),stringsAsFactors=FALSE)
  whichDiffer <- which(apply(val, 1, function(x){!all(x == x[1])}))
  if (length(whichDiffer) > 0) {
    for (i in whichDiffer) {
      val[i,] <- paste0('<div style="background-color:',guiColors["orange"],';">',val[i,],'</div>')
    }
  }
  val <- cbind(item=items, val)
  out <- merge(x=out, y=val, by.x="name", by.y="item")
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
# HTML table with steady state results

steadyTable <- function(m, lang) {
  if (ncol(m) >= 2) {
    tmp <- m
    tmp <- apply(tmp, 1:2, as.character)
    for (i in 2:ncol(m)) {
      greater <- which(m[,i] > m[,1])
      if (length(greater) > 0)
        tmp[greater, i] <- paste0('<div style="background-color:',
          guiColors["orange"],';">',tmp[greater,i],'</div>')
      smaller <- which(m[,i] < m[,1])
      if (length(smaller) > 0)
        tmp[smaller, i] <- paste0('<div style="background-color:',
          guiColors["blue"],';">',tmp[smaller,i],'</div>')
    }
    m <- tmp
  } else {
    m <- apply(m, 1:2, as.character)
  }
  tbl <- cbind(rownames(m), data.frame(m, check.names=FALSE))
  names(tbl)[1] <- translate["variable",lang]
  exportDF(tbl, align=setNames(c("left", rep("right", ncol(tbl)-1)),names(tbl)),
    tex=FALSE)
}

########################################################################
# SVG icons used by GUI

# Help open/close button
symbolHelpOpen <- paste0('
  <svg viewBox="0 0 110 110" height="25px">
    <circle cx="55" cy="55" r="50" fill="',guiColors["blueDark"],
      '" stroke="',guiColors["blueDark"],'" stroke-width="5"/>
    <text x="37" y="77" font-size=70 fill="','white','">?</text>
  </svg>
')
symbolHelpClose <- paste0('
  <svg viewBox="0 0 110 110" height="25px">
    <circle cx="55" cy="55" r="50" fill="',guiColors["blueDark"],
      '" stroke="',guiColors["blueDark"],'" stroke-width="5"/>
  <path d="M 20,55 75,30 75,80 z" fill="white" stroke="none"/>
  </svg>
')

########################################################################
# Read help pages

readHelp <- function(basename) {
  paste(readLines(paste0(system.file("uiHelp", package="rodeoGUI"),"/",basename)), collapse="\n")
}

help <- rbind(
  missing= c(
    EN="A help page is currently not available for the selected view.",
    DE="Für die gewählte Ansicht ist noch keine Hilfe verfügbar."
  ),
  intro= c(
    EN="English help page still missing.",
    DE= readHelp("DE_introduction.html")
  ),
  stoi= c(
    EN="English help page still missing.",
    DE= readHelp("DE_stoichiometry.html")
  ),
  pros= c(
    EN="English help page still missing.",
    DE= readHelp("DE_tableOfProcesses.html")
  ),
  scenDesc= c(
    EN="English help page still missing.",
    DE= readHelp("DE_tableOfScenarios.html")
  ),
  scenVars= c(
    EN="English help page still missing.",
    DE= readHelp("DE_tableOfVariables.html")
  ),
  scenPars= c(
    EN="English help page still missing.",
    DE= readHelp("DE_tableOfParameters.html")
  ),
  dyn= c(
    EN="English help page still missing.",
    DE= paste(
      readHelp("DE_defineScenarios.html"),
      readHelp("DE_startComputation.html"),
      readHelp("DE_detailsDynamic.html"),
      collapse="\n")
  ),
  std= c(
    EN="English help page still missing.",
    DE= paste(
      readHelp("DE_defineScenarios.html"),
      readHelp("DE_startComputation.html"),
      readHelp("DE_detailsSteady.html"),
      collapse="\n")
  )
)
