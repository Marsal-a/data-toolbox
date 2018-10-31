library(shiny)
library(data.table)
library(dygraphs)
library(ggplot2)
library(scales)
library(caret)
library(nlme)
library(mboost)
library(gbm)
library(purrr)
library(kernlab)
library(glmnet)

Logged = FALSE
my_username <- ""
my_password <- ""

# setwd("C:/PROJET/GRTGAZ/SHINY/Prevision_Nomination_GRTgaz - test_adam")
load(file="datashiny.Rdata")
# load(file="list_modeles_qua.Rdata")

ui1 <- function(){
  
  tagList(
    div(id = "login",
        wellPanel(textInput("userName", "Username"),
                  passwordInput("passwd", "Password"),
                  br(),actionButton("Login", "Log in"))),
    tags$style(type="text/css", "#login {font-size:10px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
  )}

ui2 <- function(){
  
  tagList(
    
    tabPanel("Data Visualisation",
             fluidPage(
               fluidRow(
                 column(3,
                        selectInput("CheckCodePCR", "Selection du PCR :", data_pcr$ident,selected ="PITS_Total-Sud" )
                 ),
                 column(3,
                        checkboxGroupInput(inputId="CheckHorizon",label="Selection de l horizon :",inline=TRUE, choices=c("J0"="J0","J1"="J1","J2"="J2","J3"="J3","J4"="J4","J5"="J5"),selected="J0")
                 ),
                 column(3,
                        uiOutput("list_var_expl")
                 ),
                 column(2,
                        a(img(src="logo_grt.png",width="60%",height="60%",type="image/png"),href="http://www.grtgaz.com/", target="_blank")
                 ),
                 column(1,
                        a(img(src="logo_cm.png",width="110%",height="110%",type="image/png"),href="http://www.climpact.com/", target="_blank")
                 )
               ),
               fluidRow(
                 
                 hr(),
                 dygraphOutput("plot",width="100%",height="500px"),
                 hr(),
                 column(6,
                        tableOutput("table2")),
                 column(6,
                        tableOutput("table3"))
               )
             )
    ),
    tabPanel("Variable Selection",
             fluidPage(
               fluidRow(
                 column(3,
                        selectInput("CheckCodePCR1", "Selection du PCR :", data_pcr$ident[-grep("_repart",data_pcr$ident)],selected ="PITS_Total-Sud" )
                 ),
                 column(3,
                        radioButtons(inputId="CheckHorizon1",label="Selection de l horizon :",inline=TRUE, choices=c("J0"="J0","J1"="J1","J2"="J2","J3"="J3","J4"="J4","J5"="J5"),selected="J0")
                 ),
                 column(3,""),
                 column(2,
                        a(img(src="logo_grt.png",width="60%",height="60%",type="image/png"),href="http://www.grtgaz.com/", target="_blank")
                 ),
                 column(1,
                        a(img(src="logo_cm.png",width="110%",height="110%",type="image/png"),href="http://www.climpact.com/", target="_blank")
                 )
               ),
               fluidRow(
                 hr(),
                 column(9,
                        plotOutput("plot1",height = "700px")
                 ),
                 column(3,
                        plotOutput("plot2",height = "700px")
                 )
               )
             )
    ),
    tabPanel("Data Downloading",
             fluidPage(
               
               fluidRow(
                 column(3,
                        downloadButton("downloadData","Download")
                 ),
                 column(3,""),
                 column(3,""),
                 column(2,
                        a(img(src="logo_grt.png",width="60%",height="60%",type="image/png"),href="http://www.grtgaz.com/", target="_blank")
                 ),
                 column(1,
                        a(img(src="logo_cm.png",width="110%",height="110%",type="image/png"),href="http://www.climpact.com/", target="_blank")
                 )
                 ),
               fluidRow(
                 hr(),
                 dataTableOutput("table")
               ) 
             )
    ),
    tabPanel("Proto prevision",
             fluidPage(
               fluidRow(
                 column(3,dateInput("date1", "Date de l echeance J0 : ", value = Sys.Date())),
                 column(3,fileInput('file_input', 'Choose CSV File',accept=c('text/csv','text/comma-separated-values,text/plain.csv'))),
                  # column(3,actionButton("start_model", "Lancement des modeles")),
                 column(3,fluidRow(
                   actionButton("start_model", "Lancement des modeles"),
                   downloadButton("download_prev", "download_prev"))
                 ),
                 column(2,a(img(src="logo_grt.png",width="60%",height="60%",type="image/png"),href="http://www.grtgaz.com/", target="_blank")),
                 column(1,a(img(src="logo_cm.png",width="110%",height="110%",type="image/png"),href="http://www.climpact.com/", target="_blank")                 )
               ),
               fluidRow(
                 
                 hr(),
                 tableOutput('contents')
               ) 
             )
    ),
    tabPanel("Glossary",
             fluidPage(
               title = 'Regles de calcul des indicateurs statistiques',
               withMathJax(),
               helpText('Forecast Accuracy (FA) d\'un echantillon contenant n date : $$(1-\\frac{\\sum_{i}^n|prev_i-obs_i|}{\\sum_{i}^n|obs_i|})*100$$'),
               helpText('Mean Absolute Error (MAE) d\'un echantillon contenant n date : $$\\frac{\\sum_{i}^n|prev_i-obs_i|}{n}$$'),
               helpText('Root Mean Squared Error (RMSE) d\'un echantillon contenant n date : $$\\sqrt{\\sum_{i}^n(prev_i-obs_i)^2}$$'),
               helpText('Correlation (COR) : $$cor(prev,obs)*100$$'),
               helpText('Decalage : $$cor(prev,obs)-cor(prev_{-1j},obs)$$'),
               
               tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")
             )
    )
  )
  
}

ui = (htmlOutput("page"))

