source(system.file("shiny/common.R", package="rodeoGUI"))

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

widthSide = 3   # width of margin with input elements (1...12)
widthMain = 9   # width of main display area (1...12)

shinyUI <- fluidPage(
  HTML(paste0("<style>",styleDefs,"</style>")),
  
  # Page header
  fluidRow(
    column(12,
      HTML(if (!is.null(XDATA$header)) XDATA$header else "")
    )
  ),

  ##############################################################################
  # HORIZONTAL PANEL AT TOP OF PAGE
  ##############################################################################
  
  fluidRow(
    column(6,
      HTML("<h1>",XDATA$appName,"</h1>"),
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
  # INPUTS AND OUTPUTS FOR THE USER-SELECTED VIEWS
  ##############################################################################

  ##############################################################################
  # help page for currently selected view
  conditionalPanel(
    condition = "output.showHelp == true",
    fluidRow(column(12, htmlOutput("helpText")))
  ),
  ##############################################################################
  # welcome page
  conditionalPanel(
    condition = "(input.view == 'intro') && (output.showHelp == false)",
    fluidRow(
      column(12, uiOutput("intro"))
    )
  ),
  ##############################################################################
  # stoichiometry
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
  # processes
  conditionalPanel(
    condition = "(input.view == 'pros') && (output.showHelp == false)",
    uiOutput("uiElem.prosVar"),
    uiOutput("uiElem.prosHide"),
    htmlOutput("processes")
  ),
  ##############################################################################
  # functions
  conditionalPanel(
    condition = "(input.view == 'funs') && (output.showHelp == false)",
    htmlOutput("functions")
  ),
  ##############################################################################
  # scenario descriptions
  conditionalPanel(
    condition = "(input.view == 'scenDesc') && (output.showHelp == false)",
    fluidRow(column(12, htmlOutput("scenShowDesc")))
  ),
  ##############################################################################
  # state variables
  conditionalPanel(
    condition = "(input.view == 'scenVars') && (output.showHelp == false)",
    fluidRow(column(12,
      uiOutput("uiElem.filterVars"),
      uiOutput("uiElem.sortVars"),
      htmlOutput("scenShowVars")
    ))
  ),
  ##############################################################################
  # parameters
  conditionalPanel(
    condition = "(input.view == 'scenPars') && (output.showHelp == false)",
    fluidRow(column(12,
      uiOutput("uiElem.filterPars"),
      uiOutput("uiElem.sortPars"),
      htmlOutput("scenShowPars")
    ))
  ),
  ##############################################################################
  # simulation inputs
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
  # dynamic simulation outputs
  conditionalPanel(
    condition = "(input.view == 'dyn') && (output.showHelp == false)",
    column(widthMain,
      fluidRow(
        column(6, uiOutput("uiElem.itemDynUpper")),
        column(6, uiOutput("uiElem.itemDynLower"))
      ),
      fluidRow(style="vertical-align:top;",
        column(12,
          HTML("<div style='width:600px; margin:auto; float:left;'>"),
          htmlOutput("displayDynUpper"),
          HTML("</div>")
        )
      ),
      hr(),
      fluidRow(style="vertical-align:top;",
        column(12,
          HTML("<div style='width:600px; margin:auto; float:left;'>"),
          htmlOutput("displayDynLower"),
          HTML("</div>")
        )
      ),
      fluidRow(
        column(8, NULL), column(4,downloadButton("downloadDyn", "Download"))
      )
    )
  ),

  ##############################################################################
  # steady-state simulation outputs
  conditionalPanel(
    condition = "(input.view == 'std') && (output.showHelp == false)",
    column(widthMain,
      fluidRow(column(8, NULL), column(4, uiOutput("uiElem.itemStd"))),
      fluidRow(column(12, htmlOutput("displayStd"))),
      fluidRow(column(8, NULL), column(4,downloadButton("downloadStd", "Download")))
    )
  ),
  
  ##############################################################################
  # footer
  fluidRow(
    column(12,
      HTML(if (!is.null(XDATA$footer)) XDATA$footer else "")
    )
  )
)
