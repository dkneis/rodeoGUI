styleDefs <- paste0('
  table, th, td {
    padding: 2px;
    border: 2px solid white;
  }
  th {
    color: ',guiColors["greyDark"],';
    background-color: white;
  }
  td {
    background-color: ',guiColors["greyLight"],';
  }
')

shinyUI <- fluidPage(
  HTML(paste0("<style>",styleDefs,"</style>")),
  
  ##############################################################################
  # HORIZONTAL PANEL AT TOP OF PAGE
  ##############################################################################
  
  fluidRow(
    column(6,
      HTML(paste0("<h2><font color='",guiColors["blueDark"],"'>","rodeoGUI","</font></h2>")),
      htmlOutput("uiHTML.usedPackages")
    ),
    conditionalPanel(
      condition = "output.showHelp == false",
      column(3,
        br(),
        htmlOutput("uiHTML.selectView"),
        uiOutput("uiElem.view")
      ),
      column(2,
        br(),
        HTML("<b>EN | DE | ...</b>"),
        selectInput(inputId="language", label=NULL, multiple=FALSE,
          choices=colnames(translate), selected="EN", selectize=FALSE)
      )
    ),
    conditionalPanel(
      condition = "output.showHelp == false",
      column(1,
        br(),
        uiOutput("uiElem.helpOpen")
      )
    ),
    conditionalPanel(
      condition = "output.showHelp == true",
      column(3+2, HTML("")),
      column(1,
        br(),
        uiOutput("uiElem.helpClose")
      )
    )
  ),
  hr(),

  ##############################################################################
  conditionalPanel(
    condition = "(input.view == 'dyn' || input.view == 'std') && (output.showHelp == false)" ,
    fluidRow(
      column(2, uiOutput("uiElem.nScen")),
      column(8, uiOutput("uiElem.scenSpecs")),
      column(2,      
        conditionalPanel(
          condition = "input.view == 'dyn'",
          fluidRow(column(12, br(), uiOutput("uiElem.runDyn")))
        ),
        conditionalPanel(
          condition = "input.view == 'std'",
          fluidRow(column(12, br
            (), uiOutput("uiElem.runStd")))
        )
      )
    ),
    hr()
  ),
  
  ##############################################################################
  conditionalPanel(
    condition = "(input.view == 'dyn') && (output.showHelp == false)",
    fluidRow(
      column(3, uiOutput("uiElem.tStart")),
      column(3, uiOutput("uiElem.tFinal")),
      column(3, uiOutput("uiElem.tStep")),
      column(3, uiOutput("uiElem.tShow"))
    ),
    hr(),
    fluidRow(
      column(6,
        fluidRow(column(8, NULL), column(4, uiOutput("uiElem.dynVar1"))),
        fluidRow(column(12, plotOutput("resultDyn1", height=paste0(plotHeight,"px"))))
      ),
      column(6,
        fluidRow(column(8, NULL), column(4, uiOutput("uiElem.dynVar2"))),
        fluidRow(column(12, plotOutput("resultDyn2", height=paste0(plotHeight,"px"))))
      )
    ),
    fluidRow(
      column(6,
        fluidRow(column(8, NULL), column(4, uiOutput("uiElem.dynVar3"))),
        fluidRow(column(12, plotOutput("resultDyn3", height=paste0(plotHeight,"px"))))
      ),
      column(6,
        fluidRow(column(8, NULL), column(4, uiOutput("uiElem.dynVar4"))),
        fluidRow(column(12, plotOutput("resultDyn4", height=paste0(plotHeight,"px"))))
      )
    )
  ),

  ##############################################################################
  conditionalPanel(
    condition = "(input.view == 'std') && (output.showHelp == false)",
    fluidRow(
      column(10, tableOutput("resultsSteady"))
    )
  ),

  ##############################################################################
  conditionalPanel(
    condition = "(input.view == 'eff') && (output.showHelp == false)",
    fluidRow(
      column(3, uiOutput("uiElem.effItem")),
      column(6, uiOutput("uiElem.effValues")),
      column(6, uiOutput("uiElem.effMultiply"))
    ),
    fluidRow(
      column(3, uiOutput("uiElem.effScen")),
      column(9, uiOutput("uiElem.runEff"))
    ),
    hr(),
    fluidRow(
      column(6,
        fluidRow(column(8, NULL), column(4, uiOutput("uiElem.effVar1"))),
        fluidRow(column(12, plotOutput("resultEff1", height=paste0(plotHeight,"px"))))
      ),
      column(6,
        fluidRow(column(8, NULL), column(4, uiOutput("uiElem.effVar2"))),
        fluidRow(column(12, plotOutput("resultEff2", height=paste0(plotHeight,"px"))))
      )
    ),
    fluidRow(
      column(6,
        fluidRow(column(8, NULL), column(4, uiOutput("uiElem.effVar3"))),
        fluidRow(column(12, plotOutput("resultEff3", height=paste0(plotHeight,"px"))))
      ),
      column(6,
        fluidRow(column(8, NULL), column(4, uiOutput("uiElem.effVar4"))),
        fluidRow(column(12, plotOutput("resultEff4", height=paste0(plotHeight,"px"))))
      )
    )
  ),

  ##############################################################################
  conditionalPanel(
    condition = "(input.view == 'intro') && (output.showHelp == false)",
    fluidRow(
      column(12, htmlOutput("intro"))
    )
  ),

  ##############################################################################
  conditionalPanel(
    condition = "(input.view == 'stoi') && (output.showHelp == false)",
    fluidRow(column(12, uiOutput("uiElem.stoiSpecs"))),
    hr(),
    fluidRow(column(12, htmlOutput("stoichiometry")))
  ),

  ##############################################################################
  conditionalPanel(
    condition = "(input.view == 'pros') && (output.showHelp == false)",
    fluidRow(column(12, uiOutput("uiElem.prosSpecs"))),
    hr(),
    fluidRow(column(12, htmlOutput("processes")))
  ),
  
  ##############################################################################
  conditionalPanel(
    condition = "(input.view == 'scenDesc') && (output.showHelp == false)",
    fluidRow(column(12, htmlOutput("scenShowDesc")))
  ),

    ##############################################################################
  conditionalPanel(
    condition = "(input.view == 'scenVars') && (output.showHelp == false)",
    fluidRow(column(12, htmlOutput("scenShowVars")))
  ),

  ##############################################################################
  conditionalPanel(
    condition = "(input.view == 'scenPars') && (output.showHelp == false)",
    fluidRow(column(12, htmlOutput("scenShowPars")))
  ),

  ##############################################################################
  # Show help page for the currently selected view
  conditionalPanel(
    condition = "output.showHelp == true",
    fluidRow(column(12, htmlOutput("helpText")))
  )
  

)
