# Define server logic ----
shinyServer(function(input, output) {
  observeEvent(input$runSASFat, {
    FEInp <- input$FEInp
    FERes <- input$FERes
    opt1 <- input$options1 
    opt2 <- input$options2
    filter <- input$filter
    solver <- input$solver
    lic <- input$lic
    
    write(c(FEInp$datapath,FERes$datapath,opt1,opt2,filter,solver,lic),"ghhh.inp")
  })
})