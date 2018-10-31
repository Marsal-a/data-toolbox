library(shiny)

# Define UI ----
shinyUI(fluidPage(
  h3("SAS Toolbox"),
  
  tabsetPanel(
    tabPanel("SASFat",
             sidebarPanel(h2("Input:"),
                          actionButton("runSASFat","Run Job",width="100%",icon("paper-plane"), 
                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),       
                          
                          wellPanel(
                            #tags$style(".shiny-file-input-progress {display: none}"),
                            fileInput("FEInp","Pfad FE input Deck:"), 
                            fileInput("FERes","Pfad FE Results:") 
                          ),
                          wellPanel(
                            checkboxGroupInput("options1","Auswertung:",c("Grundmaterial","Schweissnähte")),
                            conditionalPanel(condition="$.inArray('Schweissnähte',input.options1) > -1", 
                                             sliderInput("filter", "Filter:", 0.75, min = 0, max = 1))
                          ),
                          wellPanel(
                            radioButtons("solver", "Solver:", c("Ansys","Abaqus", "Optistruct")),
                            conditionalPanel(condition="input.solver == 'Ansys'",selectInput("lic", "Lizenz",c("preppost","stba","meba"))) 
                          ),
                          wellPanel(
                            checkboxGroupInput("options2","Optionen:",c("Schreibe LCFiles"))
                          )
             ),
             mainPanel(br(),h2("Output:"),width="30%")
    ), 
    tabPanel("Nietauswertung"),
    tabPanel("Spannungskonzept EN12663")
  )
))