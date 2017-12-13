styleDefs <- paste0('
  table, th, td {
    padding: 2px;
    border: 2px solid white;
  }
  th {
    font-weight: normal;
    color: ',guiGrey(dark=TRUE),';
    background-color: white;
  }
  td {
    background-color: ',guiGrey(),';
  }
  h2 {
    color: ',guiBlue(dark=TRUE),';
  }
  h3 {
    color: ',guiGrey(dark=TRUE),';
  }
  label {
    font-weight: normal;
    margin-bottom: 2px;
    color: ',guiBlue(dark=TRUE),';
  }
')

widthSide <- 3
widthMain <- 9

shinyUI <- fluidPage(
  HTML(paste0("<style>",styleDefs,"</style>")),
  
  ##############################################################################
  # HORIZONTAL PANEL AT TOP OF PAGE
  ##############################################################################
  
  fluidRow(
    column(6,
      HTML("<h2> rodeoGUI </h2>"),
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
    column(widthSide,
      uiOutput("uiElem.nScen"),
      uiOutput("uiElem.scenSpecs"),
      conditionalPanel(
        condition = "input.view == 'dyn'",
        uiOutput("uiElem.tStart"),
        uiOutput("uiElem.tFinal"),
        uiOutput("uiElem.tStep"),
        uiOutput("uiElem.tShow"),
        uiOutput("uiElem.runDyn")
      ),
      conditionalPanel(
        condition = "input.view == 'std'",
        uiOutput("uiElem.runStd")
      )
    )
  ),
  
  ##############################################################################
  conditionalPanel(
    condition = "(input.view == 'dyn') && (output.showHelp == false)",
    column(widthMain,
      fluidRow(column(8, NULL), column(4, uiOutput("uiElem.itemDyn"))),
      fluidRow(style="vertical-align:top;", column(12, htmlOutput("resultDyn")))
    )
  ),

  ##############################################################################
  conditionalPanel(
    condition = "(input.view == 'std') && (output.showHelp == false)",
    column(widthMain,
      fluidRow(column(8, NULL), column(4, uiOutput("uiElem.itemStd"))),
      fluidRow(column(12, htmlOutput("resultStd")))
    )
  ),

  ##############################################################################
  conditionalPanel(
    condition = "(input.view == 'eff') && (output.showHelp == false)",
    column(widthSide,
      uiOutput("uiElem.effScen"),
      uiOutput("uiElem.effItem"),
      uiOutput("uiElem.effValues"),
      uiOutput("uiElem.effMultiply"),
      uiOutput("uiElem.runEff")
    ),
    column(widthMain,
      fluidRow(column(8, NULL), column(4, uiOutput("uiElem.itemEff"))),
      fluidRow(column(12, htmlOutput("resultEff")))
    )
  ),

  ##############################################################################
  conditionalPanel(
    condition = "(input.view == 'intro') && (output.showHelp == false)",
    fluidRow(
      column(12, uiOutput("intro"))
    )
  ),

  ##############################################################################
  conditionalPanel(
    condition = "(input.view == 'stoi') && (output.showHelp == false)",
    column(widthSide,
      uiOutput("uiElem.stoiScen"),
      uiOutput("uiElem.stoiUsePatterns"),
      conditionalPanel(
        condition = "input.stoiUsePatterns == true",
        uiOutput("uiElem.stoiPatternVars"),
        uiOutput("uiElem.stoiPatternPros")
      ),
      conditionalPanel(
        condition = "input.stoiUsePatterns == false",
        uiOutput("uiElem.stoiVars"),
        uiOutput("uiElem.stoiPros")
      )
    ),
    column(widthMain,
      htmlOutput("stoichiometry")
    )
  ),

  ##############################################################################
  conditionalPanel(
    condition = "(input.view == 'pros') && (output.showHelp == false)",
    uiOutput("uiElem.prosVar"),
    uiOutput("uiElem.prosHide"),
    htmlOutput("processes")
  ),
  
  ##############################################################################
  conditionalPanel(
    condition = "(input.view == 'funs') && (output.showHelp == false)",
    htmlOutput("functions")
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
