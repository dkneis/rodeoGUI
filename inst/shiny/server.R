shinyServer <- function(input, output) {

  # Text strings appearing in the UI
  output$uiHTML.selectView <- renderText({
    paste("<b>",translate["selectView",input$language],"</b>")
  })
  output$uiHTML.usedPackages <- renderText({
    paste0(translate["usesTheRPackages",input$language],
      " <a href='https://CRAN.R-project.org/package=rodeo'>rodeo</a>,",
      " <a href='https://CRAN.R-project.org/package=deSolve'>deSolve</a>,",
      " <a href='https://CRAN.R-project.org/package=rootSolve'>rootSolve</a> ",
      translate["and",input$language],
      " <a href='https://CRAN.R-project.org/package=shiny'>shiny</a>")
  })

  # Generate input field to select a view
  output$uiElem.view <- renderUI({
    lst <- list(
      overview= setNames(c("intro","pros","stoi"),
        translate[c("introduction","processes","stoichiometry"),input$language]),
      scenarios= setNames(c("scenDesc","scenPars","scenVars"),
        translate[c("description","parameters","initialValues"),input$language]),
      simulation= setNames(c("dyn","std"),
        translate[c("dynamics", "steadystate"),input$language])
    )
    for (n in names(lst))
      names(lst)[names(lst)==n] <- translate[n,input$language]
    tagList(
      selectInput(inputId="view", label=NULL, multiple=FALSE,
        choices=lst,  selected="start", selectize=FALSE)
    )
  })

  # Generate input fields for number of scenarios
  output$uiElem.nScen <- renderUI({
    tagList(
      numericInput(inputId='nScen',
        label=translate["scenarios",input$language], value=1, min=1, max=3, step=1)
    )
  })

  # Generate input fields for scenarios
  output$uiElem.scenSpecs <- renderUI({
    nScen <- if (is.null(input$nScen)) 1 else input$nScen
    code <- ""
    code <- paste0(code, "HTML('<div style=\"width:100%; overflow:hidden;\">'),")
    for (i in 1:nScen) {
      code <- paste0(code, "HTML('<div style=\"width:",floor(100/nScen) ,"%; float:left\">'),")
      code <- paste0(code, "selectInput(inputId=\'scenDefaultId.",i,"\', label=\'",
        translate["scenario",input$language]," ",i,"\', multiple=FALSE, choices=setNames(",
        "c(",paste(paste0("'",rownames(scenTitles),"'"), collapse=","),"),",
        "c(",paste(paste0("'",scenTitles[,input$language],"'"), collapse=","),")",
        "), selected='",rownames(scenTitles)[1],"', selectize=FALSE),")
      code <- paste0(code, "textInput(inputId=\'scenEdits.",i,"\', label=NULL, value=\'\'),")
      code <- paste0(code, "HTML('</div>'),")
    }
    code <- paste0(code, "HTML('</div>')")
    code <- paste0("tagList(list(",code, "))")
    eval(parse(text=code))
  })

  # Generate fields control plotting of the stoichiometry matrix
  output$uiElem.stoiSpecs <- renderUI({
    code <- ""
    code <- paste0(code, "HTML('<div style=\"width:100%; overflow:hidden;\">'),")
    code <- paste0(code, "HTML('<div style=\"width:33%; float:left\">'),")
    code <- paste0(code, "selectInput(inputId=\'scenStoi', label=\'",
      translate["scenario",input$language],"\', multiple=FALSE, choices=setNames(",
      "c(",paste(paste0("'",rownames(scenTitles),"'"), collapse=","),"),",
      "c(",paste(paste0("'",scenTitles[,input$language],"'"), collapse=","),")",
      "), selected='",rownames(scenTitles)[1],"', selectize=FALSE),")
    code <- paste0(code, "HTML('</div>'),")
    code <- paste0(code, "HTML('<div style=\"width:33%; float:left\">'),")
    code <- paste0(code, "selectInput(inputId=\'varsStoi', label=\'",
      translate["variables",input$language],"\', multiple=TRUE, choices=c(",
      paste(paste0("'",model$namesVars(),"'"), collapse=","),")",
      ",selected=c('",paste(model$namesVars()[1:min(5,model$lenVars())],
        collapse="','"),"'), selectize=FALSE),")
    code <- paste0(code, "HTML('</div>'),")
    code <- paste0(code, "HTML('<div style=\"width:33%; float:left\">'),")
    code <- paste0(code, "selectInput(inputId=\'prosStoi', label=\'",
      translate["processes",input$language],"\', multiple=TRUE, choices=c(",
      paste(paste0("'",model$namesPros(),"'"), collapse=","),")",
      ",selected=c('",paste(model$namesPros()[1:min(5:model$lenPros())],
        collapse="','"),"'), selectize=FALSE),")
    code <- paste0(code, "HTML('</div>'),")
    code <- paste0(code, "HTML('</div>')")
    code <- paste0("tagList(list(",code, "))")
    eval(parse(text=code))
  })

  # Generate fields control presentation of process rates
  output$uiElem.prosSpecs <- renderUI({
    code <- ""
    code <- paste0(code, "selectInput(inputId=\'varPros', label=\'",
      translate["showStoichiometryFactorFor",input$language],"\', multiple=FALSE, choices=c(",
      paste(paste0("'",model$namesVars(),"'"), collapse=","),")",
      ",selected='",model$namesVars()[1],"', selectize=FALSE)")
    code <- paste0("tagList(list(",code, "))")
    eval(parse(text=code))
  })
    
  # Generate selectors for items to be displayed in dynamics plot
  output$uiElem.dynVars <- renderUI({
    tagList(
      selectInput(inputId="dynVars",
        label=translate["variables",input$language], multiple=TRUE,
        choices=model$namesVars(),
        selected=model$namesVars()[1:min(4,model$lenVars())], selectize=FALSE)
    )
  })

  # Generate input fields for time control
  output$uiElem.tStart <- renderUI({
    tagList(
      textInput(inputId='tStart',
        label=translate["tStart",input$language], value=0)
    )
  })
  output$uiElem.tFinal <- renderUI({
    tagList(
      textInput(inputId='tFinal',
        label=translate["tFinal",input$language], value=10)
    )
  })
  output$uiElem.tStep <- renderUI({
    tagList(
      textInput(inputId='tStep',
        label=translate["tStep",input$language], value=.1)
    )
  })
  output$uiElem.tShow <- renderUI({
    tagList(
      textInput(inputId='tShow',
        label=translate["tShow",input$language], value=0)
    )
  })

  # Generate run buttons
  output$uiElem.runDyn <- renderUI({
    tagList(
      actionButton(inputId="runDyn", translate["run",input$language],
        style=paste0("color: white; background-color: ",guiColors["blueDark"]))
    )
  })
  output$uiElem.runStd <- renderUI({
    tagList(
      actionButton(inputId="runStd", translate["run",input$language],
        style=paste0("color: white; background-color: ",guiColors["blueDark"]))
    )
  })

  # Generate help open/close buttons
  output$uiElem.helpOpen <- renderUI({
    tagList(actionLink(inputId="helpOpen", label=HTML(symbolHelpOpen)))
  })
  output$uiElem.helpClose <- renderUI({
    tagList(actionLink(inputId="helpClose", label=HTML(symbolHelpClose)))
  })

  ##############################################################################
  # Controls display of help pages using conditionalPanel
  # https://stackoverflow.com/questions/38895710/passing-reactive-values-to-conditionalpanel-condition
  ##############################################################################

  output$showHelp <- reactive({
    if (is.null(input$helpClose) || is.null(input$helpOpen))
      return(FALSE)
    else
      return(input$helpOpen > input$helpClose)
  })
  outputOptions(output, "showHelp", suspendWhenHidden = FALSE)


  ##############################################################################
  # Controls display of computed results
  ##############################################################################
  
  upToDate <- reactiveValues(dyn=FALSE, std=FALSE)
  # Mark as valid after computation
  observeEvent(input$runDyn, {
    upToDate$dyn <- TRUE
  })
  observeEvent(input$runStd, {
    upToDate$std <- TRUE
  })
  # Mark as invalid if number of scenarios was changed
  observeEvent(input$nScen, {
    upToDate$dyn <- FALSE
    upToDate$std <- FALSE
  })
  # Mark as invalid if scenario data were changed
  scenarioSettings <- reactive({
#    tmp <- unlist(reactiveValuesToList(input))
#    tmp[grepl(x=names(tmp), pattern="^scenDefaultId[.][0123456789]+$")]
    # possibly one can use the leading dot with reactiveValuesToList
    # THIS IS A WORKAROUND: TO BE ON THE SAVE SIDE, WE QUERY THE SETTINGS FOR
    # MORE SCENARIOS THAN THE USER CAN ACTUALLY SELECT
    c(input$scenDefaultId.1,input$scenDefaultId.2,input$scenDefaultId.3,
      input$scenDefaultId.4,input$scenDefaultId.5,input$scenDefaultId.6,
      input$scenEdits.1,input$scenEdits.2,input$scenEdits.3,
      input$scenEdits.4,input$scenEdits.5,input$scenEdits.6)
  })
  observeEvent(scenarioSettings(), {
    upToDate$dyn <- FALSE
    upToDate$std <- FALSE
  })


  ##############################################################################
  # Dynamic computation / results
  ##############################################################################
  
  # Compute dynamics when button was pressed
  computeDynamic <- eventReactive(input$runDyn, {
    dyn.load(paste0(lib, .Platform$dynlib.ext))
    out <- NULL
    for (is in 1:input$nScen) {
      # get inputs for current scenario
      inp <- reactiveValuesToList(input)
      updateInputs(model, scenDefaults=scenDefaults,
        scenDefaultId=inp[[paste0("scenDefaultId.",is)]],
        scenEdits=inp[[paste0("scenEdits.",is)]])
      # run model
      this <- deSolve::ode(y=model$getVars(), parms=model$getPars(),
        times=seq(from=as.numeric(input$tStart), to=as.numeric(input$tFinal),
          by=as.numeric(input$tStep)),
        hmax=as.numeric(input$tStep),
        func=model$libFunc(),
        dllname=basename(lib),
        nout=model$lenPros(),          # works for 0D model only
        outnames=model$namesPros()     # works for 0D model only
      )
      this <- cbind(scenario=rep(is, nrow(this)) ,this)
      # add to results of other scenarios
      out <- rbind(out, this)
    }
    dyn.unload(paste0(lib, .Platform$dynlib.ext))
    out <- out[out[,"time"] >= min(as.numeric(input$tShow), max(out[,"time"])), ,drop=FALSE]
    out
  })

  # Render dynamic results
  output$resultsDynamic <- renderPlot({
    if (!upToDate[["dyn"]]) {
      plot(0, 0, type="n", axes=FALSE, ann=FALSE)
      legend("center", bty="n", legend=translate["needsUpdate",input$language])
    } else {
      visualizeDynamic(out=computeDynamic(),
        vars=input$dynVars, lang=input$language)
    }
  })

  ##############################################################################
  # Steady state computation / results
  ##############################################################################
  
  # Compute steady state when button was pressed
  computeSteady <- eventReactive(input$runStd, {
    dyn.load(paste0(lib, .Platform$dynlib.ext))
    out <- NULL
    for (is in 1:input$nScen) {
      # get inputs for current scenario
      inp <- reactiveValuesToList(input)
      updateInputs(model, scenDefaults=scenDefaults,
        scenDefaultId=inp[[paste0("scenDefaultId.",is)]],
        scenEdits=inp[[paste0("scenEdits.",is)]])
      # run model
      this <- rootSolve::runsteady(y=model$getVars(), times=c(0,Inf), func=model$libFunc(),
        parms=model$getPars(), dllname=basename(lib), nout=model$lenPros(),
        outnames=model$namesPros())
      if (!attr(this, "steady")) {
        this <- rep(NA,length(this$y))
      } else {
        this <- signif(this$y, 3)
      }
      # add to results of other scenarios
      out <- cbind(out, this)
      colnames(out)[ncol(out)] <- paste(translate["scenario",input$language],is)
    }
    dyn.unload(paste0(lib, .Platform$dynlib.ext))
    rownames(out) <- model$namesVars()
    out
  })
  
  # Render steady state results
  output$resultsSteady <- renderText({
    if (!upToDate[["std"]]) {
      translate["needsUpdate",input$language]
    } else {
      steadyTable(m=computeSteady(), lang=input$language)
    }
  })

  ##############################################################################
  # Intro page, process table, stoichiometry matrix
  ##############################################################################

  output$intro <- renderText({
    intro[input$language]
  })

  output$stoichiometry <- renderText({
    # get inputs for current scenario
    updateInputs(model, scenDefaults=scenDefaults,
      scenDefaultId=input$scenStoi,  scenEdits="")
    v <- if(is.null(input$varsStoi)) model$namesVars()[1] else input$varsStoi
    p <- if(is.null(input$prosStoi)) model$namesPros()[1] else input$prosStoi
    stoiAsHTML(model, selectedVars=v, selectedPros=p, lang=input$language)
  })

  output$processes <- renderText({
    v <- if(is.null(input$varPros)) model$namesVars()[1] else input$varPros
    prosTable(model, selectedVar=v, lang=input$language)
  })

  ##############################################################################
  # Scenario descriptions
  ##############################################################################

  output$scenShowDesc <- renderText({
    x <- data.frame(title=scenTitles[,input$language],
      descr=scenDescriptions[rownames(scenTitles),input$language],
      stringsAsFactors=TRUE)
    colnames(x) <- translate[c("identifier","description"), input$language]
    exportDF(x, width=setNames(c(25,75), names(x)), tex=FALSE)
  })
  output$scenShowVars <- renderText({
    scenDescrTable(scenTitles, scenDefaults, model, lang=input$language, what="variable")
  })
  output$scenShowPars <- renderText({
    scenDescrTable(scenTitles, scenDefaults, model, lang=input$language, what="parameter")
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
