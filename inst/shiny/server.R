shinyServer <- function(input, output) {

  ##############################################################################
  ##############################################################################
  # PART 1: ELEMENTS OF THE DYNAMIC UI
  ##############################################################################
  ##############################################################################
  
  # Text strings appearing in the UI
  output$uiHTML.selectView <- renderText({
    paste("<b>",translate["selectView",input$language],"</b>")
  })
  output$uiHTML.usedPackages <- renderText({
    paste0(translate["usesTheRPackages",input$language],
      " <a href='https://CRAN.R-project.org/package=rodeo'><u>rodeo</u></a>,",
      " <a href='https://CRAN.R-project.org/package=deSolve'><u>deSolve</u></a>,",
      " <a href='https://CRAN.R-project.org/package=rootSolve'><u>rootSolve</u></a> ",
      translate["and",input$language],
      " <a href='https://CRAN.R-project.org/package=shiny'><u>shiny</u></a>")
  })

  ##############################################################################
  # CONTROLS FOR SELECTING THE VIEW
  ##############################################################################

  output$uiElem.view <- renderUI({
    lst <- list(
      overview= setNames(c("intro","pros","stoi"),
        translate[c("introduction","processes","stoichiometry"),input$language]),
      scenarios= setNames(c("scenDesc","scenPars","scenVars"),
        translate[c("description","parameters","initialValues"),input$language]),
      simulation= setNames(c("dyn","std","eff"),
        translate[c("dynamics", "steadystate", "effectOnSteadyState"),input$language])
    )
    for (n in names(lst))
      names(lst)[names(lst)==n] <- translate[n,input$language]
    tagList(
      selectInput(inputId="view", label=NULL, multiple=FALSE,
        choices=lst,  selected="start", selectize=FALSE)
    )
  })

  ##############################################################################
  # CONTROLS FOR THE PRESENTATION OF THE STOICHIOMETRY MATRIX
  ##############################################################################

  output$uiElem.stoiScen <- renderUI({ tagList(selectInput(inputId="stoiScen",
    label=translate["scenario",input$language], multiple=FALSE,
    choices=setNames(rownames(XDATA$scenTitles),XDATA$scenTitles[,input$language]),
    selected=rownames(XDATA$scenTitles)[1], selectize=FALSE)) })
  output$uiElem.stoiVars <- renderUI({ tagList(selectInput(inputId="stoiVars",
    label=translate["variables",input$language], multiple=TRUE, choices=XDATA$model$namesVars(),
    selected=XDATA$model$namesVars()[1:min(5,XDATA$model$lenVars())], selectize=FALSE)) })
  output$uiElem.stoiPros <- renderUI({ tagList(selectInput(inputId="stoiPros",
    label=translate["processes",input$language], multiple=TRUE, choices=XDATA$model$namesPros(),
    selected=XDATA$model$namesPros()[1:min(5:XDATA$model$lenPros())], selectize=FALSE)) })

  ##############################################################################
  # CONTROLS FOR THE PRESENTATION OF PROCESS RATES
  ##############################################################################

  output$uiElem.prosVar <- renderUI({
    tagList(selectInput(inputId="prosVar",
      label=translate["showStoichiometryFactorFor",input$language], multiple=FALSE,
      choices=XDATA$model$namesVars(), selected=XDATA$model$namesVars()[1], selectize=FALSE))
  })
  
  ##############################################################################
  # COMMON CONTROLS FOR DYNAMIC AND STEADY STATE SIMULATION
  ##############################################################################

  # Generate input fields for number of scenarios in dynamic/steady comp.
  output$uiElem.nScen <- renderUI({ tagList(numericInput(inputId='nScen',
    label=translate["numberOfScenarios",input$language], value=1, min=1, max=3, step=1)) })

  # Generate input fields for scenarios in dynamic/steady comp.
  output$uiElem.scenSpecs <- renderUI({
    nScen <- if (is.null(input$nScen)) 1 else input$nScen
    code <- ""
    for (i in 1:nScen) {
      code <- paste0(code, "selectInput(inputId=\'scenDefaultId.",i,"\', label=\'",
        translate["scenario",input$language]," ",i,"\', multiple=FALSE, choices=setNames(",
        "c(",paste(paste0("'",rownames(XDATA$scenTitles),"'"), collapse=","),"),",
        "c(",paste(paste0("'",XDATA$scenTitles[,input$language],"'"), collapse=","),")",
        "), selected='",rownames(XDATA$scenTitles)[min(i, nrow(XDATA$scenTitles))],"', selectize=FALSE),")
      code <- paste0(code, "textInput(inputId=\'scenEdits.",i,"\', label=NULL, value=\'\')")
      if (i < nScen)
        code <- paste0(code, ",")
    }
    code <- paste0("tagList(list(",code, "))")
    eval(parse(text=code))
  })

  ##############################################################################
  # SPECIFIC CONTROLS FOR DYNAMIC SIMULATION
  ##############################################################################

  # Time control
  output$uiElem.tStart <- renderUI({ tagList(textInput(inputId='tStart',
    label=translate["tStart",input$language], value=0)) })
  output$uiElem.tFinal <- renderUI({ tagList(textInput(inputId='tFinal',
    label=translate["tFinal",input$language], value=10))})
  output$uiElem.tStep <- renderUI({ tagList(textInput(inputId='tStep',
    label=translate["tStep",input$language], value=.1)) })
  output$uiElem.tShow <- renderUI({ tagList(textInput(inputId='tShow',
    label=translate["tShow",input$language], value=0)) })
  
  # Variables to be displayed
  output$uiElem.dynVar1 <- renderUI({ tagList(selectInput(inputId="dynVar1",
    label=NULL, multiple=FALSE,
    choices=if(is.numeric(sim[["dyn"]])) dimnames(sim[["dyn"]])[[2]] else "?",
    selected=if(is.numeric(sim[["dyn"]])) dimnames(sim[["dyn"]])[[2]][min(1,dim(sim[["dyn"]])[2])] else "?", selectize=FALSE)) })
  output$uiElem.dynVar2 <- renderUI({ tagList(selectInput(inputId="dynVar2",
    label=NULL, multiple=FALSE,
    choices=if(is.numeric(sim[["dyn"]])) dimnames(sim[["dyn"]])[[2]] else "?",
    selected=if(is.numeric(sim[["dyn"]])) dimnames(sim[["dyn"]])[[2]][min(2,dim(sim[["dyn"]])[2])] else "?", selectize=FALSE)) })
  output$uiElem.dynVar3 <- renderUI({ tagList(selectInput(inputId="dynVar3",
    label=NULL, multiple=FALSE,
    choices=if(is.numeric(sim[["dyn"]])) dimnames(sim[["dyn"]])[[2]] else "?",
    selected=if(is.numeric(sim[["dyn"]])) dimnames(sim[["dyn"]])[[2]][min(3,dim(sim[["dyn"]])[2])] else "?", selectize=FALSE)) })
  output$uiElem.dynVar4 <- renderUI({ tagList(selectInput(inputId="dynVar4",
    label=NULL, multiple=FALSE,
    choices=if(is.numeric(sim[["dyn"]])) dimnames(sim[["dyn"]])[[2]] else "?",
    selected=if(is.numeric(sim[["dyn"]])) dimnames(sim[["dyn"]])[[2]][min(4,dim(sim[["dyn"]])[2])] else "?", selectize=FALSE)) })

  # Run button
  output$uiElem.runDyn <- renderUI({ tagList(actionButton(inputId="runDyn",
    translate["run",input$language],
    style=paste0("color: white; background-color: ",guiColors["blueDark"]))) })

  ##############################################################################
  # SPECIFIC CONTROLS FOR STEADY STATE SIMULATION
  ##############################################################################

  output$uiElem.runStd <- renderUI({
    tagList(
      actionButton(inputId="runStd", translate["run",input$language],
        style=paste0("color: white; background-color: ",guiColors["blueDark"]))
    )
  })

  ##############################################################################
  # CONTROLS FOR EFFECT ANALYSIS
  ##############################################################################
  
  # Scenario, varied item, list of values
  output$uiElem.effScen <- renderUI({ tagList(selectInput(inputId="effScen",
    label=translate["scenario",input$language], multiple=FALSE,
    choices=setNames(rownames(XDATA$scenTitles),XDATA$scenTitles[,input$language]),
    selected=rownames(XDATA$scenTitles)[1], selectize=FALSE)) })
  output$uiElem.effItem <- renderUI({ tagList(selectInput(inputId="effItem",
    label=translate["variedItem",input$language], multiple=FALSE,
    choices=c(XDATA$model$namesPars(), XDATA$model$namesVars()),
    selected=XDATA$model$namesPars()[1], selectize=FALSE)) })
  output$uiElem.effValues <- renderUI({ tagList(textInput(inputId="effValues",
    label=translate["values",input$language], value="0.5, 1, 2")) })
  output$uiElem.effMultiply <- renderUI({ tagList(checkboxInput(inputId="effMultiply",
    label=translate["useAsMultipliers",input$language], value=TRUE)) })

  # Variables to be displayed
  output$uiElem.effVar1 <- renderUI({ tagList(selectInput(inputId="effVar1",
    label=NULL, multiple=FALSE,
    choices=if(is.numeric(sim[["eff"]])) dimnames(sim[["eff"]])[[1]] else "?",
    selected=if(is.numeric(sim[["eff"]])) dimnames(sim[["eff"]])[[1]][min(1,dim(sim[["eff"]])[1])] else "?", selectize=FALSE)) })
  output$uiElem.effVar2 <- renderUI({ tagList(selectInput(inputId="effVar2",
    label=NULL, multiple=FALSE,
    choices=if(is.numeric(sim[["eff"]])) dimnames(sim[["eff"]])[[1]] else "?",
    selected=if(is.numeric(sim[["eff"]])) dimnames(sim[["eff"]])[[1]][min(2,dim(sim[["eff"]])[1])] else "?", selectize=FALSE)) })
  output$uiElem.effVar3 <- renderUI({ tagList(selectInput(inputId="effVar3",
    label=NULL, multiple=FALSE,
    choices=if(is.numeric(sim[["eff"]])) dimnames(sim[["eff"]])[[1]] else "?",
    selected=if(is.numeric(sim[["eff"]])) dimnames(sim[["eff"]])[[1]][min(3,dim(sim[["eff"]])[1])] else "?", selectize=FALSE)) })
  output$uiElem.effVar4 <- renderUI({ tagList(selectInput(inputId="effVar4",
    label=NULL, multiple=FALSE,
    choices=if(is.numeric(sim[["eff"]])) dimnames(sim[["eff"]])[[1]] else "?",
    selected=if(is.numeric(sim[["eff"]])) dimnames(sim[["eff"]])[[1]][min(4,dim(sim[["eff"]])[1])] else "?", selectize=FALSE)) })
  
  # Run button
  output$uiElem.runEff <- renderUI({ tagList( actionButton(inputId="runEff",
    translate["run",input$language],
    style=paste0("color: white; background-color: ",guiColors["blueDark"]))) })
  
  ##############################################################################
  # CONTROLS TO SHOW/HIDE HELP
  ##############################################################################
  
  # Generate help open/close buttons
  output$uiElem.helpOpen <- renderUI({ tagList(actionLink(inputId="helpOpen",
    label=HTML(symbolHelpOpen))) })
  output$uiElem.helpClose <- renderUI({ tagList(actionLink(inputId="helpClose",
    label=HTML(symbolHelpClose))) })

  # Controls display of help pages using conditionalPanel
  # https://stackoverflow.com/questions/38895710/passing-reactive-values-to-conditionalpanel-condition
  output$showHelp <- reactive({
    if (is.null(input$helpClose) || is.null(input$helpOpen))
      return(FALSE)
    else
      return(input$helpOpen > input$helpClose)
  })
  outputOptions(output, "showHelp", suspendWhenHidden = FALSE)


  ##############################################################################
  ##############################################################################
  # PART 2: ACTUAL SERVER CODE
  ##############################################################################
  ##############################################################################

  ##############################################################################
  # Compute / invalidate model outputs
  ##############################################################################

  # Initialize model outputs
  sim <- reactiveValues(dyn=NULL, std=NULL, eff=NULL)

  # Update model outputs when run button was pressed
  observeEvent(input$runDyn, { tryCatch({ sim$dyn <- computeDynamic() },
      error = function(e) { sim$dyn <- as.character(e) }) })
  observeEvent(input$runStd, { tryCatch({ sim$std <- computeSteady() },
    error = function(e) { sim$std <- as.character(e) }) })
  observeEvent(input$runEff, { tryCatch({ sim$eff <- computeEffect() },
    error = function(e) { sim$eff <- as.character(e) }) })

  # Dynamics and steady state: Invalidate outputs if inputs were changed
  inputs_dyn_std <- reactive({
#    tmp <- unlist(reactiveValuesToList(input))
#    tmp[grepl(x=names(tmp), pattern="^scenDefaultId[.][0123456789]+$")]
    # possibly one can use the leading dot with reactiveValuesToList
    # THIS IS A WORKAROUND: TO BE ON THE SAVE SIDE, WE QUERY THE SETTINGS FOR
    # MORE SCENARIOS THAN THE USER CAN ACTUALLY SELECT
    c(input$nScen,
      input$scenDefaultId.1,input$scenDefaultId.2,input$scenDefaultId.3,
      input$scenDefaultId.4,input$scenDefaultId.5,input$scenDefaultId.6,
      input$scenEdits.1,input$scenEdits.2,input$scenEdits.3,
      input$scenEdits.4,input$scenEdits.5,input$scenEdits.6
    )
  })
  observeEvent(inputs_dyn_std(), {
    sim$dyn <- NULL
    sim$std <- NULL
  })
  
  # Effect analysis: Invalidate outputs if inputs were changed
  inputs_eff <- reactive({
    c(input$effScen, input$effItem, input$effValues, input$effMultiply)
  })
  observeEvent(inputs_eff(), {
    sim$eff <- NULL
  })

  ##############################################################################
  # Dynamic computation / results
  ##############################################################################
  
  # Compute dynamics when button was pressed
  computeDynamic <- function() {
    dyn.load(paste0(XDATA$lib, .Platform$dynlib.ext))
    out <- NULL
    for (is in 1:input$nScen) {
      # get inputs for current scenario
      inp <- reactiveValuesToList(input)
      tryCatch({
        updateInputs(XDATA$model, scenDefaults=XDATA$scenDefaults,
          scenDefaultId=inp[[paste0("scenDefaultId.",is)]],
          scenEdits=inp[[paste0("scenEdits.",is)]],
          lang=input$language)
      }, error = function(e) {
        stop(paste0(e, "\n", translate["failedToSetModelInputsForScenario",input$language]," ",is,"."))
      })
      # run model
      tryCatch({
        t0 <- as.numeric(input$tStart)
        t1 <- as.numeric(input$tFinal)
        dt <-as.numeric(input$tStep)
        stopifnot(t1 > t0)
        stopifnot(dt > 0)
        times <- seq(from=t0, to=t1, by=dt)
      }, error = function(e) {
        stop(paste0(translate["invalidVectorOfTimes",input$language],"."))
      })
      tryCatch({
        this <- deSolve::ode(y=XDATA$model$getVars(), parms=XDATA$model$getPars(),
          times=times,
          hmax=as.numeric(input$tStep),
          func=XDATA$model$libFunc(),
          dllname=basename(XDATA$lib),
          nout=XDATA$model$lenPros(),          # works for 0D model only
          outnames=XDATA$model$namesPros()     # works for 0D model only
        )
      }, error = function(e) {
        stop(paste0(translate["failedToComputeSolutionFor",input$language]," ",
          translate["scenario",input$language]," ",is,"."))
      })
      this <- cbind(scenario=rep(is, nrow(this)) ,this)
      # add to results of other scenarios
      out <- rbind(out, this)
    }
    dyn.unload(paste0(XDATA$lib, .Platform$dynlib.ext))
    out <- out[out[,"time"] >= min(as.numeric(input$tShow), max(out[,"time"])), ,drop=FALSE]
    # turn into array (dim1 = time, dim2 = variables, dim3 = scenarios)
    out <- melt(data=as.data.frame(out), id.vars=c("scenario","time"),
      variable.name="variable", value.name="value")
    out <- acast(data=out, formula=time~variable~scenario, value.var="value")
    out
  }

  # Render dynamic results
  empty <- function() {
    plot(0, 0, type="n", axes=FALSE, ann=FALSE)
    legend("center", bty="n", legend=translate["needsUpdate",input$language])
    NULL
  }
  resultDyn <- function(var) {
    if (is.null(sim[["dyn"]])) {
      out <- empty()
    } else if (is.character(sim[["dyn"]])) {
      validate(lastErrMsg())
    } else {
      tryCatch({
        out <- visualizeDynamic(out=sim[["dyn"]], var=var, lang=input$language)
      }, error = function(e) {
        validate(lastErrMsg())
      })
    }
    out
  }
  output$resultDyn1 <- renderPlot({ resultDyn(var=input$dynVar1) })
  output$resultDyn2 <- renderPlot({ resultDyn(var=input$dynVar2) })
  output$resultDyn3 <- renderPlot({ resultDyn(var=input$dynVar3) })
  output$resultDyn4 <- renderPlot({ resultDyn(var=input$dynVar4) })

  ##############################################################################
  # Steady state computation / results
  ##############################################################################
  
  # Compute steady state when button was pressed
  computeSteady <- function() {
    dyn.load(paste0(XDATA$lib, .Platform$dynlib.ext))
    out <- NULL
    for (is in 1:input$nScen) {
      # get inputs for current scenario
      inp <- reactiveValuesToList(input)
      tryCatch({
        updateInputs(XDATA$model, scenDefaults=XDATA$scenDefaults,
          scenDefaultId=inp[[paste0("scenDefaultId.",is)]],
          scenEdits=inp[[paste0("scenEdits.",is)]],
          lang=input$language)
      }, error = function(e) {
        stop(paste0(e, "\n", translate["failedToSetModelInputsForScenario",input$language]," ",is,"."))
      })
      # run model
      tryCatch({
        this <- rootSolve::runsteady(y=XDATA$model$getVars(), times=c(0,Inf), func=XDATA$model$libFunc(),
          parms=XDATA$model$getPars(), dllname=basename(XDATA$lib), nout=XDATA$model$lenPros(),
          outnames=XDATA$model$namesPros())
      }, error = function(e) {
        stop(paste0(translate["failedToComputeSolutionFor",input$language],
          " ",translate["scenario",input$language]," ",is,"."))
      })
      if (!attr(this, "steady")) {
        this <- rep(NA,length(this$y)+length(this[[2]]))
      } else {
        this <- signif(c(this$y, this[[2]]), 3)
      }
      # add to results of other scenarios
      out <- cbind(out, this)
      colnames(out)[ncol(out)] <- paste(translate["scenario",input$language],is)
    }
    dyn.unload(paste0(XDATA$lib, .Platform$dynlib.ext))
    rownames(out) <- c(XDATA$model$namesVars(), XDATA$model$namesPros())
    out
  }
  
  # Render steady state results
  output$resultsSteady <- renderText({
    if (is.null(sim[["std"]])) {
      out <- translate["needsUpdate",input$language]
    } else if (is.character(sim[["std"]])) {
      validate(lastErrMsg())
    } else {
      tryCatch({
        out <- steadyTable(m=sim[["std"]], lang=input$language)
      }, error = function(e) {
        validate(lastErrMsg())
      })
    }
    out
  })

  ##############################################################################
  # Single-effect computation / results
  ##############################################################################
  
  # Compute effect of parameter on steady state when button was pressed
  computeEffect <- function() {
    dyn.load(paste0(XDATA$lib, .Platform$dynlib.ext))
    out <- NULL
    tryCatch({
      values <- strsplit(input$effValues, split=",", fixed=TRUE)[[1]]
      stopifnot(length(values) >= 1)
      values <- as.numeric(values)
      stopifnot(all(is.finite(values)))
      if (input$effMultiply) {
        values <- values * eval(parse(text=XDATA$scenDefaults[input$effItem, input$effScen]))
      }
    }, error = function(e) {
      stop(paste0(translate["invalidUserInput",input$language],": '",input$effValues,
        "'. ",translate["expectingUnnamedVector",input$language]))
    })
    for (i in 1:length(values)) {
      # get inputs for current scenario
      inp <- reactiveValuesToList(input)
      updateInputs(XDATA$model, scenDefaults=XDATA$scenDefaults,
        scenDefaultId=input$effScen,
        scenEdits=paste0(input$effItem,"=",values[i]),
        lang=input$language)
      # run model
      tryCatch({
        this <- rootSolve::runsteady(y=XDATA$model$getVars(), times=c(0,Inf), func=XDATA$model$libFunc(),
          parms=XDATA$model$getPars(), dllname=basename(XDATA$lib), nout=XDATA$model$lenPros(),
          outnames=XDATA$model$namesPros())
      }, error = function(e) {
        stop(paste0(translate["failedToComputeSolutionFor",input$language]," ",input$effItem,"=",values[i],"."))
      })
      if (!attr(this, "steady")) {
        this <- rep(NA,length(this$y)+length(this[[2]]))
      } else {
        this <- signif(c(this$y, this[[2]]), 3)
      }
      # add to results
      out <- cbind(out, this)
      colnames(out)[ncol(out)] <- as.character(values[i])
    }
    dyn.unload(paste0(XDATA$lib, .Platform$dynlib.ext))
    rownames(out) <- c(XDATA$model$namesVars(), XDATA$model$namesPros())
    out
  }

  # Render effect results
  resultEff <- function(var) {
    if (is.null(sim[["eff"]])) {
      out <- empty()
    } else if (is.character(sim[["eff"]])) {
      validate(lastErrMsg())
    } else {
      tryCatch({
        out <- visualizeEffect(out=sim[["eff"]], var=var, lang=input$language)
      }, error = function(e) {
        validate(lastErrMsg())
      })
    }
    out
  }
  output$resultEff1 <- renderPlot({ resultEff(var=input$effVar1) })
  output$resultEff2 <- renderPlot({ resultEff(var=input$effVar2) })
  output$resultEff3 <- renderPlot({ resultEff(var=input$effVar3) })
  output$resultEff4 <- renderPlot({ resultEff(var=input$effVar4) })
  
  ##############################################################################
  # Intro page, process table, stoichiometry matrix
  ##############################################################################

  output$intro <- renderText({
    XDATA$intro[input$language]
  })

  output$stoichiometry <- renderText({
    # get inputs for current scenario
    updateInputs(XDATA$model, scenDefaults=XDATA$scenDefaults,
      scenDefaultId=input$stoiScen,  scenEdits="", lang=input$language)
    v <- if(is.null(input$stoiVars)) XDATA$model$namesVars()[1] else input$stoiVars
    p <- if(is.null(input$stoiPros)) XDATA$model$namesPros()[1] else input$stoiPros
    stoiAsHTML(XDATA$model, selectedVars=v, selectedPros=p, lang=input$language)
  })

  output$processes <- renderText({
    v <- if(is.null(input$prosVar)) XDATA$model$namesVars()[1] else input$prosVar
    prosTable(XDATA$model, selectedVar=v, lang=input$language)
  })

  ##############################################################################
  # Scenario descriptions
  ##############################################################################

  output$scenShowDesc <- renderText({
    x <- data.frame(title=XDATA$scenTitles[,input$language],
      descr=XDATA$scenDescriptions[rownames(XDATA$scenTitles),input$language],
      stringsAsFactors=TRUE)
    colnames(x) <- translate[c("identifier","description"), input$language]
    exportDF(x, width=setNames(c(25,75), names(x)), tex=FALSE)
  })
  output$scenShowVars <- renderText({
    scenDescrTable(XDATA$scenTitles, XDATA$scenDefaults, XDATA$model, lang=input$language, what="variable")
  })
  output$scenShowPars <- renderText({
    scenDescrTable(XDATA$scenTitles, XDATA$scenDefaults, XDATA$model, lang=input$language, what="parameter")
  })
  
  ##############################################################################
  # Help text
  ##############################################################################

  output$helpText <- renderText({
    if (!is.null(input$view)) {
      if (! input$view %in% rownames(help))
        help["missing", input$language]
      else
        help[input$view, input$language]
    } else {
      HTML("") 
    }
  })
}
