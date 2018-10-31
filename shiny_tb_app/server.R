source("ui.R")
lancement=F
options(shiny.maxRequestSize = 500*1024^2)
server=function(session,input,output){
  input_data=reactive({
    # browser()
    if(is.null(input$data_input)) return(NULL)
    infile=input$data_input
    dt=fread(infile$datapath,encoding = "Latin-1",check.names = T)
    return(dt)
  })
  output$data_loaded <- renderDataTable({
    if(is.null(input$col_to_match)) return(NULL)
    if(input$col_to_match=="") return(NULL)
    col=eval(input$col_to_match)
    DT=datatable(input_data()[,..col],options = list(
      autoWidth = TRUE,
      dom="ftp",
      pageLength = 25,
      columnDefs = list(list(width = '200%', targets = "_all"))
      ),rownames = F)%>%
      formatStyle( 0, target= 'row', lineHeight='50%')
    return(DT)
  })
  col_data_to_match_selection=reactive({
    cols=names(input_data())
  })
  output$col_to_match_selection=renderUI({
    if(is.null(input$data_input)) return(NULL)
    selectInput("col_to_match","Colonne à matcher ? ",choices=c(col_data_to_match_selection()))
  })
  
  
  input_ref=reactive({
  
    if(is.null(input$referential_input)) return(NULL)
    infile=input$referential_input
    dt=fread(infile$datapath,encoding = "Latin-1",check.names = T)
    return(dt)
  })
  output$ref_loaded <- renderDataTable({
    if(is.null(input$col_matched)) return(NULL)
    if(input$col_matched=="") return(NULL)
    col=eval(input$col_matched)
    DT=datatable(input_ref()[,..col],options = list(autoWidth = TRUE,dom="ftp", pageLength = 25),rownames = F)%>%
      formatStyle( 0, target= 'row', lineHeight='50%')
    return(DT)
  })
  col_referential_to_be_matched=reactive({
    cols=names(input_ref())
  })
  output$col_matched=renderUI({
    if(is.null(input$referential_input)) return(NULL)
    selectInput("col_matched","Colonne du référentiel à utiliser?",choices=col_referential_to_be_matched())
  })
  
  match_ref_model=function(data_in,
                           ref_in,
                           col_name_ref_model,
                           col_data_to_match,
                           max_dist=NULL,
                           string_dist_method){
    
    library(stringdist)
    # browser()
    list_ref_model=unique(ref_in[,get(col_name_ref_model)])
    list_data_unique=data_in[,unique(get(col_data_to_match))]
    
    #calcualte best matched model
    match_ref_model=CJ(data_unique=list_data_unique,ref_model=list_ref_model)
    if(input$method=="qgram"){
      match_ref_model[,dist:=stringdist(data_unique,ref_model,method ="qgram",q=input$param)]  
    }
    if(input$method=="lv"){
      match_ref_model[,dist:=stringdist(data_unique,ref_model,method ="lv")]  
    }
    if(input$method=="jw"){
      match_ref_model[,dist:=stringdist(data_unique,ref_model,method ="jw")]  
    }
    setorder(match_ref_model,data_unique,dist)
    
    #keep only matching for distance 
    if(!is.null(max_dist)){
      match_ref_model=match_ref_model[dist<=max_dist]
    }
    
    res=match_ref_model[,.SD[1],by=data_unique]
    data_in[,order_col:=1:.N]
    setkey(res,data_unique)
    setkeyv(data_in,col_data_to_match)
    data_in[res,ref_model:=i.ref_model]
    data_in=data_in[order(order_col)]
    data_in[,order_col:=NULL]
    return(data_in)
    
  }
  
  dt_out_reac=eventReactive(input$run_matching,{
    
    dt_out=match_ref_model(input_data(),input_ref(),input$col_matched,input$col_to_match)
    showModal(modalDialog(title = "","matching ok",easyClose = TRUE,footer = tagList(modalButton("Ok"))))
    return(dt_out)
    
  })
  output$res=renderDataTable({
    cols=c(eval(input$col_to_match),"ref_model")
    dt=dt_out_reac()[,..cols]
    DT=datatable(dt,options = list(autoWidth = F,dom="ftp", pageLength = 25),rownames = F)%>%
      formatStyle( 0, target= 'row', lineHeight='50%')
    return(DT)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      # browser()
      paste(gsub("\\..*","",input$data_input$name),"_matched",".csv", sep = "")
    },
    content = function(file) {
      fwrite(dt_out_reac(), file, row.names = FALSE)
    }
  )
  
}