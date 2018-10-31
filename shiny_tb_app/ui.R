library(shiny)
library(data.table)
library(DT)

shinyUI(fluidPage(
  h3("Data toolbox"),
  tabsetPanel(
    tabPanel("Matching de referentiel",
             sidebarPanel(h3("Input:"),
                          actionButton("run_matching","MATCH",width="100%",icon("paper-plane"), 
                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),       
                          
                          wellPanel(
                            #tags$style(".shiny-file-input-progress {display: none}"),
                            fileInput("data_input","Fichier à matcher :"),
                            uiOutput("col_to_match_selection"),
                            fileInput("referential_input","Fichier référentiel") ,
                            uiOutput("col_matched")
                          ),
                          # wellPanel(
                          #   checkboxGroupInput("options1","Auswertung:",c("Grundmaterial","Schweissnähte")),
                          #   conditionalPanel(condition="$.inArray('Schweissnähte',input.options1) > -1", 
                          #                    sliderInput("filter", "Filter:", 0.75, min = 0, max = 1))
                          # ),
                          wellPanel(
                            radioButtons("method", "methode de rapprochement:", c("qgram","lv", "jw")),
                            conditionalPanel(condition="input.method == 'qgram'",selectInput("param", "param",c(1,2,3),selected = 2)) 
                          ),
                          wellPanel(
                            downloadButton("downloadData", "Download")
                          ),width = 3
             ),
             mainPanel(h3("Output:"),width="30%",
                       fluidRow(
                         column(2,fluidPage(dataTableOutput('data_loaded'))),
                         # column(0),
                         column(2,fluidPage(dataTableOutput('ref_loaded'))),
                         # column(0),
                         column(3,fluidPage(dataTableOutput('res')))
                       )
             )
    ), 
    tabPanel("tool2"),
    tabPanel("tool3")
    # tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap1.css")
  )
))