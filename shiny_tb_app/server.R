source("ui.R")
lancement=F
options(shiny.maxRequestSize = 1000*1024^2)
server=function(session,input,output){
  
  MR_input_data=reactive({
    # browser()
    if(is.null(input$MR_data_input)) return(NULL)
    infile=input$MR_data_input
    
    if(grepl(".Rdata",infile$datapath)){
      l=load(infile$datapath)
      dt=get(l)
    }else{
      dt=fread(infile$datapath,encoding = "Latin-1",check.names = T)  
    }
    
    return(dt)
  })
  output$MR_data_loaded <- renderDataTable({
    if(is.null(input$MR_col_to_match)) return(NULL)
    if(input$MR_col_to_match=="") return(NULL)
    col=eval(input$MR_col_to_match)
    # browser()
    MR_input_data()[]
    DT=datatable(unique(MR_input_data()[,col,with=F]),options = list(
      autoWidth = TRUE,
      dom="ftp",
      pageLength = 25,
      columnDefs = list(list(width = '200%', targets = "_all"))
    ),rownames = F)%>%
      formatStyle( 0, target= 'row', lineHeight='50%')
    return(DT)
  },server = T)
  MR_col_data_to_match_selection=reactive({
    cols=names(MR_input_data())
  })
  output$MR_col_to_match_selection=renderUI({
    if(is.null(input$MR_data_input)) return(NULL)
    selectInput("MR_col_to_match","Colonne à matcher ? ",choices=c(MR_col_data_to_match_selection()))
  })
  
  MR_input_ref=reactive({
    if(is.null(input$MR_referential_input)) return(NULL)
    infile=input$MR_referential_input
    if(grepl(".Rdata",infile$datapath)){
      l=load(infile$datapath)
      dt=get(l)
    }else{
      dt=fread(infile$datapath,encoding = "Latin-1",check.names = T)  
    }
    return(dt)
  })
  output$MR_ref_loaded <- renderDataTable({
    if(is.null(input$MR_col_matched)) return(NULL)
    if(input$MR_col_matched=="") return(NULL)
    col=eval(input$MR_col_matched)
    DT=datatable(unique(MR_input_ref()[,col,with=F]),options = list(autoWidth = TRUE,dom="ftp", pageLength = 25),rownames = F)%>%
      formatStyle( 0, target= 'row', lineHeight='50%')
    return(DT)
  })
  MR_col_referential_to_be_matched=reactive({
    cols=names(MR_input_ref())
  })
  output$MR_col_matched=renderUI({
    if(is.null(input$MR_referential_input)) return(NULL)
    selectInput("MR_col_matched","Colonne du référentiel à utiliser?",choices=MR_col_referential_to_be_matched())
  })
  
  MR_match_ref_model=function(data_in,ref_in,col_name_ref_model,col_data_to_match,max_dist=NULL,string_dist_method){
    
    library(stringdist)
    # browser()
    list_ref_model=unique(ref_in[,get(col_name_ref_model)])
    list_data_unique=data_in[,unique(get(col_data_to_match))]
    
    #calcualte best matched model
    match_ref_model=CJ(data_unique=list_data_unique,ref_model=list_ref_model)
    if(string_dist_method=="qgram"){
      match_ref_model[,dist:=stringdist(data_unique,ref_model,method ="qgram",q=input$MR_param)]  
    }
    if(string_dist_method=="lv"){
      match_ref_model[,dist:=stringdist(data_unique,ref_model,method ="lv")]  
    }
    if(string_dist_method=="jw"){
      match_ref_model[,dist:=stringdist(data_unique,ref_model,method ="jw")]  
    }
    setorder(match_ref_model,data_unique,dist,na.last = T)
    
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
  
  MR_dt_out_reac=eventReactive(input$MR_run_matching,{
    browser()
    dt_out=MR_match_ref_model(MR_input_data(),MR_input_ref(),input$MR_col_matched,input$MR_col_to_match,NULL,input$MR_method)
    showModal(modalDialog(title = "","matching ok",easyClose = TRUE,footer = tagList(modalButton("Ok"))))
    return(dt_out)
    
  })
  
  output$MR_res=renderDataTable({
    cols=c(eval(input$MR_col_to_match),"ref_model")
    dt=unique(MR_dt_out_reac()[,..cols])
    DT=datatable(dt,options = list(autoWidth = F,dom="ftp", pageLength = 25),rownames = F)%>%
      formatStyle( 0, target= 'row', lineHeight='50%')
    return(DT)
  })
  
  output$MR_downloadData <- downloadHandler(
    filename = function() {
      # browser()
      paste(gsub("\\..*","",input$MR_data_input$name),"_matched",".csv", sep = "")
    },
    content = function(file) {
      fwrite(MR_dt_out_reac(), file, row.names = FALSE)
    }
  )
  
  
  
  
  
  HE_input_data=reactive({
    # browser()
    if(is.null(input$HE_file_input)) return(NULL)
    infile=input$HE_file_input
    dt=fread(infile$datapath,encoding = "UTF-8",check.names = T)
    return(dt)
  })
  HE_col_names=reactive({
    cols=c("",names(HE_input_data()))
  })
  output$HE_input_1=renderUI({
    if(is.null(input$HE_file_input)) return(NULL)
    selectInput("HE_col_1","input 1",HE_col_names())
  })
  output$HE_input_2=renderUI({
    if(is.null(input$HE_file_input)) return(NULL)
    selectInput("HE_col_2","input 1",HE_col_names())
  })
  output$HE_input_3=renderUI({
    if(is.null(input$HE_file_input)) return(NULL)
    selectInput("HE_col_3","input 3",HE_col_names())
  })
  output$HE_input_4=renderUI({
    if(is.null(input$HE_file_input)) return(NULL)
    selectInput("HE_col_4","input 4",HE_col_names())
  })
  output$HE_input_5=renderUI({
    if(is.null(input$HE_file_input)) return(NULL)
    selectInput("HE_col_5","input 5",HE_col_names())
  })
  
  sankey_diagram_plot=function(dt,columns,prefixes=c("c1_","c2_","c3_","c4_","c5_","c6_"),removeNAs=T){
    
    # browser()
    levels=length(columns)-1
    links=rbindlist(map(1:levels,function(x){
      res=dt[,.N,by=.(source=get(columns[x]),target=get(columns[x+1]))][order(source,target)]
      if(removeNAs){
        res=res[!is.na(source) & !is.na(target)]
        res=res[source!="" & target!=""]
      }
      res[,source:=paste0(prefixes[x],source)]
      res[,target:=paste0(prefixes[x+1],target)]
    }))
    
    # if(input$HE_tri){
      links[,pos1:=1:.N-1]
      links[,pos2:=(.N):(2*.N-1)]
      nodes=rbind(links[,.(names=source,pos=pos1)],links[,.(names=target,pos=pos2)])
      nodes=nodes[order(pos)]
      nodes=unique(nodes,by="names")
      nodes=nodes[order(pos)]
      nodes[,pos:=1:.N-1]
      
    # }else{
    #   nodes=rbind(links[,.(names=source)],links[,.(names=target)])
    #   nodes=nodes[order(names),.(names=unique(names))]
    #   nodes[,pos:=1:.N-1]
    #   nodes=nodes[order(pos)]
    # }
    
    setkey(links,source)
    setkey(nodes,names)
    links[nodes,int_source:=i.pos]
    setkey(links,target)
    links[nodes,int_target:=i.pos]
    nodes=nodes[order(pos)]
    nodes[,names:=gsub("c[0-9]_","",names)]
    nodes[,names:=gsub("[^[:alnum:] _-]","",names)]
    
    sk=sankeyNetwork(Links=links,Nodes=nodes,
                     Source = "int_source",Target = "int_target",Value="N",
                     NodeID = "names",sinksRight = F,
                     units = " machines",fontSize = 12, nodeWidth = 30,
                     iterations = 500
                     # iterations = input$HE_iteration
                     )
    
    return(sk)
  }
  
  output$HE_sankey_plot=renderSankeyNetwork({
    
    cols=c(input$HE_col_1,input$HE_col_2,input$HE_col_3,
           input$HE_col_4,input$HE_col_5,input$HE_col_6)
    cols=cols[which(!cols=="")]
    if(length(cols)<2){
      res=NULL
    }else{
      res=sankey_diagram_plot(HE_input_data(),columns = cols,removeNAs =F)  
    }
    
    return(res)
  })
  
  HE_sankey_height=reactive({
    # browser()
    cols=c(input$HE_col_1,input$HE_col_2,input$HE_col_3,
           input$HE_col_4,input$HE_col_5,input$HE_col_6)
    cols=cols[which(!cols=="")]
    if(is.null(cols)) return(NULL)
    nrows=max(unlist(map(cols,function(x) length(HE_input_data()[,unique(get(x))]))))
    return(nrows)
  })
  
  output$HE_sankey_ui=renderUI(
    sankeyNetworkOutput("HE_sankey_plot",height = paste0("",max(HE_sankey_height()*15,500),"px",""))
    # sankeyNetworkOutput("HE_sankey_plot",height = "500px")
  )
  
  
  ftosql_input_data=reactive({
    # browser()
    if(is.null(input$ftosql_file_input)) return(NULL)
    infile=input$ftosql_file_input
    dt=fread(infile$datapath,check.names = T,encoding = "Latin-1")
    return(dt)
  })
  # ftosql_input_schema=reactive({})
  create_sql_table=function(az_scheme,az_table,R_table,col_to_delete){
    
    walk(col_to_delete,function(x){
      R_table[,(x):=NULL]
    })
    
    names(R_table) <- gsub("\\.", "_", names(R_table))
    
    # browser()
    class=data.table(name=names(R_table),type=unlist(map(R_table,~base::class(.)[[1]]),use.names = T),max_char=map(R_table,~max(nchar(.,keepNA = F))))
    class[,dec:=paste0(name,fcase(type=="character",paste0(" nvarchar(",max_char,")"),
                                  type=="integer"," integer",
                                  type=="numeric"," decimal(18,5)",
                                  type=="POSIXct"," date",
                                  type=="POSIXtt"," date",
                                  type=="POSIXlt"," date"))]
    
    drop_table=paste0(
      "IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_NAME = '",az_table,"' AND TABLE_SCHEMA = '",az_scheme,"')
    DROP TABLE ",az_scheme,".",az_table
    )
    
    
    create_table_start=paste0(
      paste0("CREATE TABLE ",az_scheme,".",az_table,"\n(\n"),  ##START
      paste0(class$dec,collapse = ",\n"),
      paste0("\n)")
    )
    
    walk(names(R_table),function(x){
      # print(x)
      if(class(R_table[,get(x)])[[1]]=="character"){
        # sapply(R_table,function(x) class(x)=="character"),with=F])
        R_table[,(x):=paste0("'",gsub("'"," ",get(x)),"'")]
      }
      if(class(R_table[,get(x)])[[1]]%in%c("integer","numeric")){
        # sapply(R_table,function(x) class(x)=="character"),with=F])
        R_table[is.na(get(x)),(x):=0]
      }
      if(class(R_table[,get(x)])[[1]]%in%c("POSIXct","POSIXt","POSIXlt")){
        R_table[,(x):=paste0("'",format(get(x),"%Y/%m/%d"),"'")]
      }
      
    })
    R_table[, key_ := do.call(paste, c(.SD, sep = ",")), .SDcols = names(R_table)]
    
    
    inserts=R_table[,paste0("SELECT ",key_," UNION ALL")]
    inserts=replace(inserts,length(inserts),gsub("UNION ALL","",tail(inserts,1)))
    res=c(drop_table,"\n",create_table_start,"\n",paste0("INSERT INTO ",az_scheme,".",az_table),"\n",inserts)
    return(res)
    
    
    
  }
  
  
  output$ftosql_output <- renderUI({
    if(!is.null(input$ftosql_file_input)){
      # browser()
      # f=create_sql_table("lab","table",ftosql_input_data(),NULL)
      # HTML(paste(create_sql_table(input$ftosql_file_schema,input$ftosql_file_table,ftosql_input_data(),NULL),collapse = '<br/>'))
      query=create_sql_table(input$ftosql_file_schema,input$ftosql_file_table,ftosql_input_data(),NULL)
      if (length(query)>50){
        res=c(query[1:49],"...",tail(query,1))  
      }else{
        res=query
      }
      
      HTML(
        paste(
          res,
          collapse = '<br/>')
        )
    }
  })
  
  output$ftosql_download <- downloadHandler(
    filename = function() {
      paste0(input$ftosql_file_schema,".",input$ftosql_file_table,".sql")
    },
    content = function(file) {
      res=create_sql_table(input$ftosql_file_schema,input$ftosql_file_table,ftosql_input_data(),NULL)
      write(res,file)
    }
  )
  
  # observeEvent(input$do, {
  #   if(!is.null(input$ftosql_file_input)){
  #     write.table(
  #       create_sql_table(input$ftosql_file_schema,input$ftosql_file_table,ftosql_input_data(),NULL),
  #       row.names = F,quote=F,col.names = F,"clipboard-100000")
  #   }
  # })
  
  
}