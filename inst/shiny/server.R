source(system.file("shiny/common.R", package="rodeoGUI"))

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
      overview= setNames(c("intro","pros","stoi","funs"),
        translate[c("introduction","processes","stoichiometry","functions"),input$language]),
      scenarios= setNames(c("scenDesc","scenPars","scenVars"),
        translate[c("description","parameters","initialValues"),input$language]),
      simulation= setNames(c("dyn","std"),
        translate[c("dynamics", "steadystate"),input$language])
    )
    for (n in names(lst))
      names(lst)[names(lst)==n] <- translate[n,input$language]
    tagList(
      selectInput(inputId="view", label=NULL, multiple=FALSE,
        choices=lst,  selected=NULL, selectize=FALSE)
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
  output$uiElem.stoiUsePatterns <- renderUI({ tagList(checkboxInput(inputId="stoiUsePatterns",
    label=translate["selectByPattern",input$language], value=FALSE)) })
  output$uiElem.stoiPatternVars <- renderUI({ tagList(textInput(inputId='stoiPatternVars',
    label=translate["patternToSelectVariables",input$language], value="^.+$")) })
  output$uiElem.stoiPatternPros <- renderUI({ tagList(textInput(inputId='stoiPatternPros',
    label=translate["patternToSelectProcesses",input$language], value="^.+$")) })

  
  ##############################################################################
  # CONTROLS FOR THE PRESENTATION OF PROCESS RATES
  ##############################################################################

  output$uiElem.prosVar <- renderUI({
    tagList(selectInput(inputId="prosVar",
      label=translate["showStoichiometryFactorFor",input$language], multiple=FALSE,
      choices=XDATA$model$namesVars(), selected=XDATA$model$namesVars()[1], selectize=FALSE))
  })
  output$uiElem.prosHide <- renderUI({ tagList(checkboxInput(inputId="prosHide",
    label=translate["hideInactiveProcesses",input$language], value=TRUE))
  })

  ##############################################################################
  # CONTROLS FOR THE PRESENTATION OF PARAMETERS AND INITIAL VALUES
  ##############################################################################

  output$uiElem.sortVars <- renderUI({ tagList(checkboxInput(inputId="sortVars",
    label=translate["sortAlphatecically",input$language], value=FALSE))
  })
  output$uiElem.sortPars <- renderUI({ tagList(checkboxInput(inputId="sortPars",
    label=translate["sortAlphatecically",input$language], value=FALSE))
  })
  
  ##############################################################################
  # COMMON CONTROLS FOR DYNAMIC AND STEADY STATE SIMULATION
  ##############################################################################

  # Generate input fields for number of scenarios in dynamic/steady comp.
  output$uiElem.nScen <- renderUI({ tagList(numericInput(inputId='nScen',
    label=translate["numberOfScenarios",input$language], value=1, min=1,
    max=XDATA$maxNumberOfScenarios, step=1)) })

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
      code <- paste0(code, "textInput(inputId=\'scenEdits.",i,"\', label=NULL,
        value=\'\', placeholder=\'",translate["scenarioModifications",input$language],"\')")
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
    label=translate["tStart",input$language], value=XDATA$tStart)) })
  output$uiElem.tFinal <- renderUI({ tagList(textInput(inputId='tFinal',
    label=translate["tFinal",input$language], value=XDATA$tFinal))})
  output$uiElem.tStep <- renderUI({ tagList(textInput(inputId='tStep',
    label=translate["tStep",input$language], value=XDATA$tStep)) })
  output$uiElem.tShow <- renderUI({ tagList(textInput(inputId='tShow',
    label=translate["tShow",input$language], value=XDATA$tShow)) })
  # Item to be displayed
  output$uiElem.itemDyn <- renderUI({
    tagList(selectInput(inputId="itemDyn",
      label=NULL, multiple=FALSE,
      choices=if(is.data.frame(sim[["dyn"]])) sim[["dyn"]][,"label"] else "?",
      selected=lastShown[["dyn"]], selectize=FALSE))
  })
  # Run button
  output$uiElem.runDyn <- renderUI({ tagList(actionButton(inputId="runDyn",
    translate["run",input$language],
    style=paste0("color: white; background-color: ",guiBlue(dark=TRUE)))) })

  ##############################################################################
  # SPECIFIC CONTROLS FOR STEADY STATE SIMULATION
  ##############################################################################

  # Item to be displayed
  output$uiElem.itemStd <- renderUI({
    tagList(selectInput(inputId="itemStd",
      label=NULL, multiple=FALSE,
      choices=if(is.data.frame(sim[["std"]])) sim[["std"]][,"label"] else "?",
      selected=lastShown[["std"]], selectize=FALSE))
  })
  # Run button
  output$uiElem.runStd <- renderUI({ tagList( actionButton(inputId="runStd",
    translate["run",input$language],
    style=paste0("color: white; background-color: ",guiBlue(dark=TRUE)))
    )
  })

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
  # Initialize model outputs
  ##############################################################################

  sim <- reactiveValues(dyn=NULL, std=NULL)
  up2date <- reactiveValues(dyn=FALSE, std=FALSE)

  ##############################################################################
  # Update model outputs when run buttons were pressed
  ##############################################################################

  observeEvent(input$runDyn, {
    tryCatch({
      sim[["dyn"]] <- computeDynamic()
      up2date[["dyn"]] <- TRUE
    }, error = function(e) { sim[["dyn"]] <- as.character(e) }) })
  observeEvent(input$runStd, {
    tryCatch({
      sim[["std"]] <- computeSteady()
      up2date[["std"]] <- TRUE
    }, error = function(e) { sim[["std"]] <- as.character(e) }) })

  ##############################################################################
  # Invalidate results if inputs were changed
  ##############################################################################

  # NOTE: We query the inputs for more scenarios than the user can actually
  #       select, just to be on the save side when the upper limit is changed
  #       somewhere else in the code. There seems to be no simple way to make
  #       this code dynamic (TODO: check whether the issue can be solved using
  #       the leading dot with 'reactiveValuesToList').

  if (XDATA$maxNumberOfScenarios > 6) {
    stop("max. number of scenarios passed via XDATA cannot be handled by current code")
    # Note: Code in rodeoGUI::preGUI() should make sure that this message never appears
  }
  
  invalidate_dyn <- reactive({
    c(input$language,
      input$nScen,
      input$scenDefaultId.1,input$scenDefaultId.2,input$scenDefaultId.3,
      input$scenDefaultId.4,input$scenDefaultId.5,input$scenDefaultId.6,
      input$scenEdits.1,input$scenEdits.2,input$scenEdits.3,
      input$scenEdits.4,input$scenEdits.5,input$scenEdits.6,
      input$tStart,
      input$tFinal,
      input$tStep,
      input$tShow
    )
  })
  observeEvent(invalidate_dyn(), { up2date[["dyn"]] <- FALSE })

  invalidate_std <- reactive({
    c(input$language,
      input$nScen,
      input$scenDefaultId.1,input$scenDefaultId.2,input$scenDefaultId.3,
      input$scenDefaultId.4,input$scenDefaultId.5,input$scenDefaultId.6,
      input$scenEdits.1,input$scenEdits.2,input$scenEdits.3,
      input$scenEdits.4,input$scenEdits.5,input$scenEdits.6
    )
  })
  observeEvent(invalidate_std(), { up2date[["std"]] <- FALSE })
  
  ##############################################################################
  # Remember last item selected for plotting
  ##############################################################################
  
  lastShown <- reactiveValues(dyn=NULL, std=NULL)
  observeEvent(input$itemDyn, { if (input$itemDyn != "?") lastShown[["dyn"]] <- input$itemDyn })
  observeEvent(input$itemStd, { if (input$itemStd != "?") lastShown[["std"]] <- input$itemStd })

  ##############################################################################
  # Dynamic simulation
  ##############################################################################

  # Computation  
  computeDynamic <- function() {
    dyn.load(paste0(XDATA$lib, .Platform$dynlib.ext))
    out <- NULL
    prm <- NULL
    # get time settings
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
    if ((length(times) - 1) > XDATA$maxNumberOfTimeSteps)
      stop(paste0(translate["numberOfTimeStepsExceedsLimitOf",input$language],
        " ",XDATA$maxNumberOfTimeSteps,"."))
    # process scenarios
    withProgress(message=translate["runningDynamicSimulation",input$language], value = 0, {
      for (is in 1:input$nScen) {
        incProgress(1/input$nScen, detail = paste(translate["scenario",input$language], is))
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
          this <- deSolve::ode(y=XDATA$model$getVars(), parms=XDATA$model$getPars(),
            times=times,
            hmax=as.numeric(input$tStep),
            func=XDATA$model$libFunc(),
            dllname=basename(XDATA$lib),
            nout=XDATA$model$lenPros(),          # works for 0D model only
            outnames=XDATA$model$namesPros()     # works for 0D model only
          )
        }, warning = function(x) {
          stop(paste0(translate["failedToComputeSolutionFor",input$language]," ",
            translate["scenario",input$language]," ",is,"."))
        }, error = function(x) {
          stop(paste0(translate["failedToComputeSolutionFor",input$language]," ",
            translate["scenario",input$language]," ",is,"."))
        })
        this <- cbind(scenario=rep(is, nrow(this)) ,this)
        # add to results of other scenarios
        out <- rbind(out, this)
        prm <- cbind(prm, XDATA$model$getPars())
        colnames(prm)[ncol(prm)] <- paste(translate["scenario",input$language],is)
      }
    })
    dyn.unload(paste0(XDATA$lib, .Platform$dynlib.ext))
    out <- out[out[,"time"] >= min(as.numeric(input$tShow), max(out[,"time"])), ,drop=FALSE]
    # turn into array (dim1 = time, dim2 = variables, dim3 = scenarios)
    out <- melt(data=as.data.frame(out), id.vars=c("scenario","time"),
      variable.name="variable", value.name="value")
    out <- acast(data=out, formula=time~variable~scenario, value.var="value")
    dimnames(out)[[3]] <- paste(translate["scenario",input$language], 1:input$nScen)
    rownames(prm) <- XDATA$model$namesPars()
    showDynamic(out, prm, input$language)
  }

  # Presentation
  resultDyn <- function(item) {
    if (is.null(sim[["dyn"]])) {
      out <- framedMessage(translate["resultsMissing",input$language])
    } else if (is.character(sim[["dyn"]])) {
      validate(lastErrMsg())
    } else {
      row <- match(item, sim[["dyn"]][,"label"])
      out <- sim[["dyn"]][row, "content"]
      if (!up2date[["dyn"]])
        out <- paste0(framedMessage(translate["resultsOutdated",input$language]), out)
    }
    out
  }
  output$resultDyn <- renderText({ resultDyn(item=input$itemDyn) })

  ##############################################################################
  # Steady state simulation
  ##############################################################################
  
  # Computation
  computeSteady <- function() {
    dyn.load(paste0(XDATA$lib, .Platform$dynlib.ext))
    out <- NULL
    prm <- NULL
    # process scenarios
    withProgress(message=translate["runningSteadySimulation",input$language], value = 0, {    
      for (is in 1:input$nScen) {
        incProgress(1/input$nScen, detail = paste(translate["scenario",input$language], is))
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
            parms=XDATA$model$getPars(), dllname=basename(XDATA$lib),
            nout=XDATA$model$lenPros(),       # works for 0D model only
            outnames=XDATA$model$namesPros()  # works for 0D model only
          )
        }, warning = function(x) {
          stop(paste0(translate["failedToComputeSolutionFor",input$language],
            " ",translate["scenario",input$language]," ",is,"."))
        }, error = function(x) {
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
        prm <- cbind(prm, XDATA$model$getPars())
        colnames(prm)[ncol(prm)] <- paste(translate["scenario",input$language],is)
      }
    })
    dyn.unload(paste0(XDATA$lib, .Platform$dynlib.ext))
    rownames(out) <- c(XDATA$model$namesVars(), XDATA$model$namesPros())
    rownames(prm) <- XDATA$model$namesPars()
    showSteady(out, prm, input$language)
  }
  
  # Presentation
  resultStd <- function(item) {
    if (is.null(sim[["std"]])) {
      out <- framedMessage(translate["resultsMissing",input$language])
    } else if (is.character(sim[["std"]])) {
      validate(lastErrMsg())
    } else {
      row <- match(item, sim[["std"]][,"label"])
      out <- sim[["std"]][row, "content"]
      if (!up2date[["std"]])
        out <- paste0(framedMessage(translate["resultsOutdated",input$language]), out)
    }
    out
  }
  output$resultStd <- renderText({ resultStd(item=input$itemStd) })


  ##############################################################################
  # Intro page
  ##############################################################################

  output$intro <- renderUI({
    addResourcePath("dirIntro", XDATA$dirIntro)
    fLocal <- paste0(XDATA$dirIntro,"/",input$language,".html")
    fServer <- paste0("dirIntro/",input$language,".html")
    validate(
      need(file.exists(fLocal), translate["introPageMissing",input$language])
    )
    tags$iframe(src=fServer, height="900px", width="100%", seamless="seamless")
  })

  ##############################################################################
  # Stoichiometry matrix
  ##############################################################################

  output$stoichiometry <- renderText({
    # get inputs for current scenario
    updateInputs(XDATA$model, scenDefaults=XDATA$scenDefaults,
      scenDefaultId=input$stoiScen,  scenEdits="", lang=input$language)
    # set variables and processes being displayed
    if (is.null(input$stoiUsePatterns) || (!input$stoiUsePatterns)) {
      v <- if(is.null(input$stoiVars)) XDATA$model$namesVars()[1] else input$stoiVars
      p <- if(is.null(input$stoiPros)) XDATA$model$namesPros()[1] else input$stoiPros
    } else {
      tryCatch({
        v <- XDATA$model$namesVars()[grepl(x=XDATA$model$namesVars(), pattern=input$stoiPatternVars)]
        p <- XDATA$model$namesPros()[grepl(x=XDATA$model$namesPros(), pattern=input$stoiPatternPros)]
      }, error = function(e) {
        validate(translate["patternNotValid",input$language])
      })
      validate(need(length(v) > 0, translate["patternNotMatchingAnyVariable",input$language]))
      validate(need(length(p) > 0, translate["patternNotMatchingAnyProcess",input$language]))
    }
    stoiAsHTML(XDATA$model, selectedVars=v, selectedPros=p, lang=input$language)
  })

  ##############################################################################
  # Table of processes
  ##############################################################################

  output$processes <- renderText({
    v <- if(is.null(input$prosVar)) XDATA$model$namesVars()[1] else input$prosVar
    h <- if(is.null(input$prosHide)) TRUE else input$prosHide
    prosTable(XDATA$model, selectedVar=v, hide=h, lang=input$language)
  })

  ##############################################################################
  # Table of functions
  ##############################################################################
  
  output$functions <- renderText({
    funsTable(XDATA$model, lang=input$language)
  })
  
  ##############################################################################
  # Table of scenarios
  ##############################################################################

  output$scenShowDesc <- renderText({
    x <- data.frame(title=XDATA$scenTitles[,input$language],
      descr=XDATA$scenDescriptions[rownames(XDATA$scenTitles),input$language],
      stringsAsFactors=TRUE)
    colnames(x) <- translate[c("identifier","description"), input$language]
    exportDF(x, width=setNames(c(25,75), names(x)), tex=FALSE)
  })

  ##############################################################################
  # Tables of parameters and initial values
  ##############################################################################
  
  output$scenShowVars <- renderText({
    scenDescrTable(XDATA$scenTitles, XDATA$scenDefaults, XDATA$model,
      lang=input$language, what="variable",
      sort=if (is.null(input$sortVars)) FALSE else input$sortVars)
  })
  output$scenShowPars <- renderText({
    scenDescrTable(XDATA$scenTitles, XDATA$scenDefaults, XDATA$model,
      lang=input$language, what="parameter",
      sort=if (is.null(input$sortPars)) FALSE else input$sortPars)
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
