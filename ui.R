library(shinydashboard)
library(plotly)
library(data.table)
library(DT)
library(lubridate)
library(shinythemes)
library(leaflet)
library(shinyWidgets)

navbarPage(
  HTML('<div> <img width="98" height="88" style="float:left; margin-left: 2px; margin-right: 5px; margin-top: -10px" src="img.png"> </div>'),
  theme=shinytheme("cosmo"),
  header = tagList(
    useShinydashboard()
  ),
  tabPanel(title = "Coronavirus Tracker",
           box(width="100%",
               tags$head(HTML('<link href="https://fonts.googleapis.com/css?family=Roboto+Mono" rel="stylesheet">')),
               tags$head(HTML('<style>* {font-size: 98%; font-family: Roboto Mono;}</style>')),
               wellPanel(style = "background-color: #FFFAFA; border-color: #2c3e50;",
                         uiOutput("Metrics"),
                         conditionalPanel(condition="input.Metric == 'Deaths Per Million'",leafletOutput('chloro', height="600px")),
                         conditionalPanel(condition="input.Metric == 'Cases Per Million'", leafletOutput('chloro2', height="600px")),
                         HTML('<div align="center">'),
                         radioButtons("Metric", "", c("Deaths Per Million", "Cases Per Million"), inline=T, width="100%"),
                         HTML('</div>')),
               
               wellPanel(style = "background-color: #fff; border-color: #2c3e50;",
                         conditionalPanel(condition="input.package == 'Map'",leafletOutput('map', height="600px")),
                         conditionalPanel(condition="input.package == 'Satellite'", leafletOutput('lightmap', height="600px")),
                         HTML('<div align="center">'),
                         radioButtons("package", "", c("Map", "Satellite"), inline=T, width="100%"),
                         HTML('</div>'))
           )
           
  ),
  tabPanel(title="Simulate", icon=icon('gears'),
           box(width="100%",
               wellPanel(style = "background-color: #FFFAFA; border-color: #2c3e50;",
                         fluidRow(
                           
                           column(4, 
                                  selectInput("State", "Select State", choices=c(state.name, "US", "Italy", "United Kingdom"), selected="US")),
                           column(4, numericInput("fatal", label="Fatality Rate", value=1.0)),
                           column(4, numericInput("deathTime", label = "Time Delay Between Infection and Death (Days)", value = 21))
                           
                         ),
                         hr(),
                         plotlyOutput('StateChart')
               ),

               
               wellPanel(style = "background-color: #FFFAFA; border-color: #2c3e50;",
                         plotlyOutput('deaths'),
                         DTOutput('Table'))),
               box(width="100%",
                   fluidRow(
                     valueBoxOutput("cases"),
                     valueBoxOutput("death"),
                     valueBoxOutput("recovered")))),
  tabPanel(title="About", icon=icon("book"),
           uiOutput("paper")
  )
)
