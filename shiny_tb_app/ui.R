library(shiny)
library(data.table)
library(DT)
library(networkD3)
library(purrr)
shinyUI(fluidPage(
  h3("Data toolbox"),
  tabsetPanel(
    tabPanel("Matching de referentiel",
             sidebarPanel(h3("Input:"),
                          actionButton("MR_run_matching","MATCH",width="100%",icon("paper-plane"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),       
                          wellPanel(
                            fileInput("MR_data_input","Fichier à matcher :"),
                            uiOutput("MR_col_to_match_selection"),
                            fileInput("MR_referential_input","Fichier référentiel") ,
                            uiOutput("MR_col_matched")
                          ),
                          wellPanel(
                            radioButtons("MR_method", "methode de rapprochement:", c("qgram","lv", "jw")),
                            conditionalPanel(condition="input.MR_method == 'qgram'",selectInput("MR_param", "param",c(1,2,3),selected = 2)) 
                          ),
                          wellPanel(
                            downloadButton("MR_downloadData", "Download")
                          ),width = 3
             ),
             mainPanel(h3("Output:"),width="30%",
                       fluidRow(
                         column(2,fluidPage(dataTableOutput('MR_data_loaded'))),
                         column(2,fluidPage(dataTableOutput('MR_ref_loaded'))),
                         column(3,fluidPage(dataTableOutput('MR_res')))
                       )
             )
    ),
    tabPanel("Hierarchy explorer",
             fluidRow(
               column(2,fileInput("HE_file_input","Input file")),
               column(2,uiOutput("HE_input_1")),
               column(2,uiOutput("HE_input_2")),
               column(2,uiOutput("HE_input_3")),
               column(2,uiOutput("HE_input_4")),
               column(2,uiOutput("HE_input_5"))
             ),
             # fluidRow(
             #   sankeyNetworkOutput("HE_sankey_plot",width ="100%",height = "700px")
             # )
             # ,
             # fluidRow(
             #   column(2,checkboxInput("HE_tri","tri", value = FALSE, width = NULL)),
             #   column(2,sliderInput("HE_iteration", label = "iteration", min = 0,max = 20000, value = 32))
             #   ),
             fluidRow(
               uiOutput("HE_sankey_ui")
             )

    ),
    tabPanel("tool2"),
    tabPanel("tool3")
    # tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap1.css")
  )
))