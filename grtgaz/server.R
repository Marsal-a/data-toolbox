source("ui.R")
Trigger_prev_ok=FALSE
Trigger_all_var_build_ok=FALSE
Trigger_file_exist_ok=FALSE
Trigger_file_format_ok=FALSE
Trigger_prev_calcule_ok=FALSE

allvar_build <- function(data_i){
  
  var_lag <- c("IND_H_NORD","Alloc_PL_H_TOTAL_NORD_S","Alloc_PL_H_TOTAL_TIGFSUD_S","Alloc_PL_H_TOTAL_SUD_S","NIVEAU_PITS_H_TOTAL_NORD","NIVEAU_PITS_H_PS000NB_NORD","NIVEAU_PITS_H_TOTAL_TIGFSUD","NIVEAU_PITS_H_PS000SC_SUD","NIVEAU_PITS_H_TOTAL_SUD",
               "Alloc_PITS_H_TOTAL_NORD_ES","Alloc_PITS_H_TOTAL_TRSSUD_ES","Alloc_PITS_H_PS000NA_NORD_ES","Alloc_PITS_H_PS000NC_NORD_ES","Alloc_PIR_H_IR0006_NORD_ES","Alloc_PIR_H_IR0011_NORD_ES","Alloc_PIR_H_IR0010_NORD_ES","Alloc_PIR_H_IR0013_14_59_NORD_ES",
               "Alloc_PIR_H_TOTAL_NORD_ES")
  ind_lag <- 1:13
  data_f <- copy(data_i)
  
  col_int=names(data_f[,sapply(data_f,is.integer),with=F])
  data_f[,(col_int):=lapply(.SD,as.numeric),.SDcols=(col_int)]
  
  #Calendrier
  data_f[,Date:=as.Date(Date,format="%d/%m/%Y")]
  data_f[,":="( day1_n=as.numeric(format(Date, format="%u")),
                day=as.factor(format(Date, format="%u")),
                week_n=as.numeric(format(Date, format="%W")),
                day2_n=as.numeric(format(Date, format="%j")),
                month_n=as.numeric(format(Date, format="%m")),
                month=as.factor(format(Date, format="%m")))]
  
  data_f[,":="(sais.h=0,sais.e=0)]
  data_f[month_n%in%c(11,12,1,2,3),sais.h:=1]
  data_f[month_n%in%c(4:10),sais.e:=1]
  
  data_f[,month_ch:=0]
  data_f[as.numeric(format(Date,"%d"))==1,month_ch:=1]
  data_f[,sais_ch:=ifelse((month(Date)==4 & mday(Date)==1) | (month(Date)==11 & mday(Date)==1),1,0)]
  
  dummy_day=dummyVars(~day,data_f)
  
  # dummy_month=dummyVars(~month,data_f)
  m_dum=data.table(month=as.character(seq(1:12)))
  m_dum=data.table(month=as.factor(sprintf("%02d",seq(1:12))))
  dummy_month=dummyVars(~month,m_dum)

  data_f <- as.data.table(cbind(data_f,predict(dummy_day,newdata=data_f),predict(dummy_month,newdata = data_f)))
  
  full_month <- paste("month.",1:12,sep="")
  month_indata <- names(data_f)[grep("month.[0-9]{1,2}$",names(data_f))]
  month_toadd <- full_month[!(full_month %in% month_indata)]
  data_f[,(month_toadd):=0]
  
  
  
  # Variables metier
  data_f[,":="(COE_PIP_H_TOTAL_SUD_E=-COE_PIP_H_TOTAL_NORD_S,
               COE_PIR_H_IR0006_NORD_ES=COE_PIR_H_IR0006_NORD_E+COE_PIR_H_IR0006_NORD_S,
               Alloc_PL_H_TOTAL_NORD_S=Alloc_PITD_H_TOTAL_NORD_S+Alloc_PLC_H_TOTAL_NORD_S,
               Alloc_PL_H_TOTAL_TRSSUD_S=Alloc_PL_H_TOTAL_TIGFSUD_S+Alloc_PL_H_TOTAL_SUD_S,
               COE_PIR_H_TOTAL_NORD_E=COE_PIR_H_IR0011_NORD_E+COE_PIR_H_IR0010_NORD_E+COE_PIR_H_IR0006_NORD_E+COE_PIR_H_IR0013_14_59_NORD_E,
               Alloc_PIR_H_TOTAL_NORD_ES=Alloc_PIR_H_IR0011_NORD_ES+Alloc_PIR_H_IR0010_NORD_ES+Alloc_PIR_H_IR0006_NORD_ES+Alloc_PIR_H_IR0013_14_59_NORD_ES,
               Alloc_PITTM_H_IT0002_NORD_E=Alloc_PITTM_H_TOTAL_NORD_E)]
  
  data_f[,":="(IND_H_NORD=COE_PIR_H_TOTAL_NORD_E+Alloc_PL_H_TOTAL_NORD_S+Alloc_PITTM_H_TOTAL_NORD_E,
               Alloc_PL_FR_S=Alloc_PL_H_TOTAL_NORD_S+Alloc_PL_H_TOTAL_SUD_S+Alloc_PL_B_TOTAL_NORD_S,
               Alloc_PITD_FR_S=Alloc_PITD_H_TOTAL_NORD_S+Alloc_PITD_H_TOTAL_SUD_S,
               tmin_delta=tmin_nord-tmin_sud,tmax_delta=tmax_nord-tmax_sud)]
  
  data_f[,":="(AllocDelta_PL_FR_S=Alloc_PL_FR_S-shift(Alloc_PL_FR_S,1),
               AllocDelta_PITD_FR_S=Alloc_PITD_FR_S-shift(Alloc_PITD_FR_S,1),
               tminDelta_delta=tmin_delta-shift(tmin_delta,1),
               tmaxDelta_delta=tmax_delta-shift(tmax_delta,1))]
  
  
  var_coese=grep("^COESE_",names(data_f),value = TRUE)
  var_coe=gsub(pattern = "COESE",replacement = "COE",var_coese)
  data_f[,gsub(pattern = "COESE",replacement = "COEGS",var_coese):=lapply(seq_along(var_coe),function(x) pmin(get(var_coese[x]),get(var_coe[x]),na.rm=TRUE))]
  
  
  
  data_f[, ":="(COEGS2_PITS_H_PS000SC_SUD_ES=COEGS_PITS_H_PS000SC_SUD_E,
                COEGS2_PITS_H_PS000SA_SUD_ES=COEGS_PITS_H_PS000SA_SUD_E,
                COEGS2_PITS_H_PS000NA_NORD_ES=COEGS_PITS_H_PS000NA_NORD_E,
                COEGS2_PITS_H_PS000NB_NORD_ES=COEGS_PITS_H_PS000NB_NORD_E,
                COEGS2_PITS_H_PS000NC_NORD_ES=COEGS_PITS_H_PS000NC_NORD_E)]
  
  
  data_f[sais.h==0, ":="(COEGS2_PITS_H_PS000SC_SUD_ES=COEGS_PITS_H_PS000SC_SUD_S,
                         COEGS2_PITS_H_PS000SA_SUD_ES=COEGS_PITS_H_PS000SA_SUD_S,
                         COEGS2_PITS_H_PS000NA_NORD_ES=COEGS_PITS_H_PS000NA_NORD_S,
                         COEGS2_PITS_H_PS000NB_NORD_ES=COEGS_PITS_H_PS000NB_NORD_S,
                         COEGS2_PITS_H_PS000NC_NORD_ES=COEGS_PITS_H_PS000NC_NORD_S)]
  
  # Lag
  for(lag in ind_lag){
    var_lag_shifted <- apply(expand.grid(var_lag,lag),1,paste,collapse="_Lag_")
    data_f[,(var_lag_shifted):=lapply(.SD,function(x) shift(x,n=lag)),.SDcols=var_lag]
  }
  return(data_f)
}

check_input_file <- function(data_i,date_launch=as.Date(Sys.time())){
  
  data_i=as.data.table(data_i)
  if(ncol(data_i) != 52) return("Les données d'entrée doivent avoir 52 colonnes...")
  if(nrow(data_i) != 13) return("Les données d'entrée doivent avoir 13 lignes...")
  
  mandatory_var=c("Date","Alloc_PL_B_TOTAL_NORD_S","Alloc_PLC_H_TOTAL_NORD_S","Alloc_PITD_H_TOTAL_NORD_S","Alloc_PITD_H_TOTAL_SUD_S", "Alloc_PL_H_TOTAL_SUD_S",
                  "Alloc_PL_H_TOTAL_TIGFSUD_S","Alloc_PITTM_H_TOTAL_NORD_E","Alloc_PITTM_H_TOTAL_SUD_E","COE_PIR_H_IR0010_NORD_E","COE_PIR_H_IR0011_NORD_E","COE_PIR_H_IR0013_14_59_NORD_E",   
                  "COE_PIR_H_IR0006_NORD_E","COE_PIR_H_IR0006_NORD_S","Alloc_PIR_H_IR0006_NORD_ES","Alloc_PIR_H_IR0011_NORD_ES","Alloc_PIR_H_IR0010_NORD_ES","Alloc_PIR_H_IR0013_14_59_NORD_ES",
                  "COE_PITS_H_PS000NB_NORD_E","COE_PITS_H_PS000NB_NORD_S","COE_PITS_H_PS000NC_NORD_E","COE_PITS_H_PS000NC_NORD_S","COE_PITS_H_PS000NA_NORD_S","COE_PITS_H_PS000NA_NORD_E",
                  "COE_PITS_H_PS000SC_SUD_E", "COE_PITS_H_PS000SC_SUD_S", "COE_PITS_H_PS000SA_SUD_S","COE_PITS_H_PS000SA_SUD_E", "COESE_PITS_H_PS000NB_NORD_E","COESE_PITS_H_PS000NB_NORD_S",
                  "COESE_PITS_H_PS000NC_NORD_E","COESE_PITS_H_PS000NC_NORD_S","COESE_PITS_H_PS000NA_NORD_S","COESE_PITS_H_PS000NA_NORD_E","COESE_PITS_H_PS000SC_SUD_E","COESE_PITS_H_PS000SC_SUD_S",
                  "COESE_PITS_H_PS000SA_SUD_S","COESE_PITS_H_PS000SA_SUD_E","NIVEAU_PITS_H_TOTAL_NORD","NIVEAU_PITS_H_PS000NB_NORD","NIVEAU_PITS_H_TOTAL_TIGFSUD","NIVEAU_PITS_H_PS000SC_SUD",
                  "NIVEAU_PITS_H_TOTAL_SUD","Alloc_PITS_H_TOTAL_NORD_ES","Alloc_PITS_H_TOTAL_TRSSUD_ES","Alloc_PITS_H_PS000NA_NORD_ES","Alloc_PITS_H_PS000NC_NORD_ES","COE_PIP_H_TOTAL_NORD_S",
                  "tmin_sud", "tmin_nord","tmax_sud","tmax_nord")
  if(!identical(sort(mandatory_var),sort(names(data_i)))) return("Les colonnes des données d'entrée ne sont pas correctement nommées...")
  
  test_format_date <- try(as.Date(data_i$Date,format="%d/%m/%Y"))
  if(class(test_format_date)=="try-error" || is.na(test_format_date))  return("La date doit avoir le format jj/mm/aaaa..." )
  
  if(sum(with(data_i, as.Date(Date,format="%d/%m/%Y") >= as.Date(date_launch-7) & as.Date(Date,format="%d/%m/%Y") <= as.Date(date_launch+5))) != 13) return("Le fichier d'entrée doit contenir des données allant de J-7 à J+5...")
  
  if(sum(data_i[,apply(.SD,2,is.numeric),.SDcols=-"Date"])!=51) return("Les donnÃ©es du fichier d'entrÃ©e doivent Ãªtre des donnÃ©es de type numerique...")
  
  if(sum(data_i[,apply(.SD,2,function(x) sum(is.na(x))),.SDcols=-"Date"])>78) return("Le fichier d'entrÃ©e contient trop de valeurs manquantes...")
  
  # browser()
  
  signes=rbindlist(lapply(seq_along(names(data_i))[-grep("Date",names(data_i))],function(x){
    dt=data.table(name=names(data_i)[x],signe=sign(data_i[,x,with=F]))
    names(dt)=c("name","signe")
    dt
  }))
  signes=signes[,.N,by=.(name,signe)]
  signes=dcast(signes,name~signe,value.var="N")
  names(signes)=c("var","neg","nul","pos","err")
  for (j in names(signes)){set(signes,which(is.na(signes[[j]])),j,0)}
  signes[,":="(pos=pos+nul,neg=neg+nul,nul=NULL)]
  setkey(signes,var)
  
  expected_sign=data.table(var=names(data_i)[-grep("Date",names(data_i))],sign=c(rep(-1,6),rep(1,2),1,NA,1,1,NA,rep(NA,4),1,
                                                                                 -1,1,-1,-1,1,1,-1,-1,1,1,-1,1,-1,
                                                                                 -1,1,1,-1,-1,1,rep(NA,9),-1,rep(NA,4)))    
  setkey(expected_sign,var)
  signes[expected_sign,expected:=i.sign]
  signes[expected==1,diff:=!pos==13]
  signes[expected==-1,diff:=!neg==13]
  
  if(nrow(signes[(diff)])!=0) return(paste0("Probleme de signes pour les colonnes :\n",paste0(signes[(diff),unique(var)],collapse = ", ")))

  return(NULL)
  
}

server = (function(input, output,session) {
  
  input_model <- reactive({
    
    inFile <- input$file_input
    
    if (is.null(inFile))
      return(NULL)
    
    dt=data.table(read.csv2(inFile$datapath,dec=",",sep=";"))
    # browser()
    if(!is.null(check_input_file(dt,input$date1))){
      showModal(modalDialog(
        title = "Import des donnees",
        check_input_file(dt,input$date1),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Ok")
        )
      ))
    }
    return(dt)
  })
  
  output$contents <- renderTable({
    input_model()
  })
  
  declencheur_modeles=eventReactive(input$start_model,{
    
    if(!USER$Trigger_file_exist_ok){
      showModal(modalDialog(title = "Erreur","Veuillez importer un fichier",easyClose = TRUE,footer = tagList(modalButton("Ok"))))
      return(NULL)
    }
    
    if(!USER$Trigger_file_format_ok){
      showModal(modalDialog(title = "Erreur","Erreur de format du fichier ou du choix de la date J0",easyClose = TRUE,footer = tagList(modalButton("Ok"))))
      return(NULL)
    }
    
    if(!USER$Trigger_all_var_build_ok){
      showModal(modalDialog(title = "Erreur","Probleme de construction des variables, contact Adam/Julien",easyClose = TRUE,footer = tagList(modalButton("Ok"))))
      return(NULL)
    }
    
    # load(file="list_modeles_qua.Rdata")
    
    list_model_str=list("P_PIP","P_PITS_NORD_TOTAL","P_PIR_PIRINEOS","P_PITS_LUSSAGNET","P_PITS_SUD_ATLANTIQUE","P_PITS_SUD_EST","P_PITS_SUD_TOTAL",
                          "P_EBJ_FR_EH", "P_EBJ_FR_H",
                          "P_PITS_NORD_EST","P_PITS_NORD_OUEST","P_PITS_NORD_ATLANTIQUE",
                          "P_PIR_N_TOTAL","P_PIR_DUNKERQUE","P_PIR_OBERGAILBACH","P_PIR_TAISNIERES","P_PIR_OLTINGUE")
    
    dt=allvar_build(input_model())
    dt[,J:=paste0("J",Date-input$date1)]
    horizon=0:5
    list_prev=list()
  
    for (PCR in list_model_str){
      # load(file=paste0("C:/PROJET/GRTGAZ/SHINY/Prevision_Nomination_GRTgaz_v03/list_model_",PCR,".Rdata"),verbose = T)
      load(file=paste0(PCR,".Rdata"),verbose = T)
      for (hzn in horizon){
        print(paste0(hzn," ",PCR))
        dt[J==paste0("J",hzn),
                  paste0(model[[paste0("J_",hzn)]]$cible,paste0("_J",hzn)):=predict(model[[paste0("J_",hzn)]]$model,newdata= dt[J==paste0("J",hzn)])]
        list_prev=c(list_prev,paste0(model[[paste0("J_",hzn)]]$cible,paste0("_J",hzn)))
      }
      rm(model)
    }

    dt=melt(dt,id.vars = c("Date","J"),measure.vars = unlist(list_prev))
    dt[,variable:=as.character(variable)]
    dt[,J_var:=substr(variable, nchar(variable)-2+1, nchar(variable))]
    dt=dt[J%in%c("J0","J1","J2","J3","J4","J5")]
    dt[,variable:=gsub("_J[0-9]$","",variable)]
    dt=dt[J==J_var]
    dt[,":="(J_var=NULL)]
    setnames(dt,c("value","J"),c("Prev","Horizon"))
    dt=dcast(dt,Date+Horizon~variable,value.var = "Prev")
    showModal(modalDialog(title = "","Calcul des prévisions sans erreur",easyClose = TRUE,footer = tagList(modalButton("Ok"))))
    
    # browser()
    
    
    output$download_prev <- downloadHandler(
      filename = function() {
        paste("Prev_", Sys.Date(),".csv", sep="")
        paste("Prev_", input$date1,".csv", sep="")
      },
      # browser(),
      # load("prev.Rdata"),
      # dt=declencheur_modeles(),
      content = function(file) {
        write.table(dt, file, sep=";", dec=",",row.names=FALSE)
      }
    )
    
    return(dt)
  })
  
  observeur_modele <- observe({ declencheur_modeles()})
  
  data <- reactive({
    data_f=datashiny[Horizon%in%input$CheckHorizon & CodePCR==input$CheckCodePCR]
    return(data_f)
  })
  
  data05 <- reactive({
    data_f=datashiny[Horizon%in%input$CheckHorizon1 & CodePCR==input$CheckCodePCR1]
    return(data_f)
  })
  
  data_var_expl_f=reactive({
    if(!is.null(input$var_selected) & input$var_selected!=""){
      # data_exp=data_var_expl_adam[hzn%in%input$CheckHorizon & CodePCR==input$CheckCodePCR & variable==input$var_selected]
      data_exp=data_var_expl[variable==input$var_selected]
      # print(data_exp)
      return(data_exp)
    }else{
      return(NULL)
    }
  })
  
  data1 <- reactive({
    data_f=varimp[CodePCR==input$CheckCodePCR1 & Horizon==input$CheckHorizon1]
    return(data_f)
  })
  
  output$list_var_expl <- renderUI({
    selectInput("var_selected", "Variables explicatives", c("None",unique(varimp[Horizon%in%input$CheckHorizon & CodePCR==input$CheckCodePCR]$Variable)))
  })
  
  output$plot <- renderDygraph({
    data0=data()
    data1=dcast(data0,formula=Date+CodePCR+is_train+inertie+Obs~Horizon,value.var = "Prev")
    setnames(data1,"Obs","OBS")
    # browser()
    data2=data1[,c("Date","OBS",input$CheckHorizon),with=F]

    if(!is.null(input$var_selected)){
      # if (input$var_selected!=" "){
        var_exp_data=copy(data_var_expl_f())
        # data_var_expl_adam=data_var_expl_adam[PCR==,.(Date,var_exp=get(input$var_selected))]
        setkey(data2,Date)
        setkey(var_exp_data,Date)
        data2[var_exp_data,var_exp:=i.value]
      # }
    }
    
    dygraph=dygraph(data2,main=paste0("Point : ",input$CheckCodePCR," ------ Modele : ",ifelse(unique(data1$inertie),"Avec Inertie","Sans Inertie")),
            ylab="GWh")    %>%
      dyRangeSelector() %>%
      # dyOptions(drawPoints = TRUE, pointSize = 2,strokeWidth=2) %>%
      dyOptions(drawPoints = TRUE, pointSize =1.5,strokeWidth=1,colors =c("#BF2878","#00008B","#0000FF","#1E90FF","#2A7D50","#3CB371","#8CD3A9","#A0522D")) %>%
      dyUnzoom() %>%
      dyLegend(width = 800) %>%
      dyShading(from =data1[is_train=="Test"][1]$Date, to =data1[is_train=="Test"][.N]$Date,color="#E0E0E0") %>%
      dyAnnotation("2013-11-1", text = "Train", attachAtBottom = TRUE, width = 60) %>%
      dyAnnotation("2016-3-1", text = "Test", attachAtBottom = TRUE, width = 60)

    if(!is.null(input$var_selected)){
      if (input$var_selected!=" "){
        dygraph=dygraph%>%dySeries("var_exp",axis = 'y2')
      }
    }

    return(dygraph)
    
  })
  
  output$plot1 <- renderPlot({
    gg=data1()
    
    ggplot(gg,aes(x=reorder(gg$label_var,gg$Importance),y=Importance,fill=Importance))+
      geom_bar(stat="identity")+
      coord_flip()+
      labs(y="Importance",x="Variables")+
      theme_minimal()+
      labs(x="",y="Importance")+
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=12))+
      scale_fill_gradient2(low = muted("white"),
                           high = muted("blue"), midpoint =-20)
  })
  
  output$plot2 <- renderPlot({
    gg1=data05()
    gg=copy(gg1)
    gg[,":="(err=Prev-Obs,month=month(Date),sais.h=0)]
    gg[as.numeric(as.character(month))%in%c(11,12,1,2,3),sais.h:=1]
    gg$is_train <- factor(gg$is_train,levels = c("Train","Test"))
    gg$sais.h=ifelse(gg$sais.h==0,"Hiver Gazier","Ete Gazier")
    
    ggplot(gg,aes(x=is_train,y=err))+
      geom_boxplot(aes(fill=factor(sais.h)),size=1,notch=TRUE)+
      geom_hline(yintercept = 0, lty=2)+
      theme_minimal()+
      labs(y="Erreur (GWh)",x="Echantillon")+
      theme(legend.position = "top")+
      scale_fill_discrete("")+
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=12),
            legend.text=element_text(size=14))+
      scale_y_continuous(breaks=seq(-50000,50000,25))
    
  })
  
  output$table2 <- renderTable({
    gg=data()
    gga=gg[is_train=="Train",.(MAE=round(mean(abs(Prev-Obs),na.rm=TRUE),1),
                               FA=round((1-(sum(abs(Prev-Obs),na.rm=TRUE)/sum(abs(Obs))))*100,1),
                               RMSE=round(sqrt(mean((Prev-Obs)^2,na.rm=TRUE)),1),
                               COR=round(mean(cor(Prev,Obs,use="pairwise.complete.obs"))*100,1),
                               Decalage=round((cor(Prev,Obs)-cor(shift(Obs,1),Prev,use = "complete.obs"))*100,1)),
           by=.(CodePCR,Horizon,is_train)]
    setnames(gga,c("is_train","MAE","FA","RMSE","COR"),c("Echantillon","MAE(GWh)","FA(%)","RMSE(GWh)","COR(%)"))
    return(gga)
  },align="r",digits=1)
  
  output$table3 <- renderTable({
    gg=data()
    gga=gg[is_train=="Test",.(MAE=round(mean(abs(Prev-Obs),na.rm=TRUE),1),
                              FA=round((1-(sum(abs(Prev-Obs),na.rm=TRUE)/sum(abs(Obs))))*100,1),
                              RMSE=round(sqrt(mean((Prev-Obs)^2,na.rm=TRUE)),1),
                              COR=round(mean(cor(Prev,Obs,use="pairwise.complete.obs"))*100,1),
                              Decalage=round((cor(Prev,Obs)-cor(shift(Obs,1),Prev,use = "complete.obs"))*100,1)),
           by=.(CodePCR,Horizon,is_train)]
    setnames(gga,c("is_train","MAE","FA","RMSE","COR"),c("Echantillon","MAE(GWh)","FA(%)","RMSE(GWh)","COR(%)"))
    return(gga)
  },align="r",digits=1)
  
  output$table <- renderDataTable({
    data_ii=copy(datashiny)
    data_ii[,Date:=as.character(Date)]
  },options = list(pageLength = 12))
  
  # output$download_prev <- downloadHandler(
  #   filename = function() {
  #     paste("Donnees_", Sys.Date(),".csv", sep="")
  #   },
  #   browser(),
  #   # load("prev.Rdata"),
  #   # dt=declencheur_modeles(),
  #   content = function(file) {
  #     write.table(dt, file, sep=";", dec=",",row.names=FALSE)
  #   }
  # )
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Donnees_", Sys.Date(),".csv", sep="")
    },
    content = function(file) {
      write.table(datashiny, file, sep=";", dec=",",row.names=FALSE)
    }
  )  
  
  
  USER <- reactiveValues(Logged = Logged,
                         Trigger_file_exist_ok=Trigger_file_exist_ok,
                         Trigger_file_format_ok=Trigger_file_format_ok,
                         Trigger_all_var_build_ok=Trigger_all_var_build_ok,
                         Trigger_prev_ok=Trigger_prev_ok)
  
  observe({ 
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          Id.username <- which(my_username == Username)
          Id.password <- which(my_password == Password)
          if (length(Id.username) > 0 & length(Id.password) > 0) {
            if (Id.username == Id.password) {
              USER$Logged <- TRUE
            } 
          }
        } 
      }
    }    
  })
  
  observe({
    if (USER$Logged == FALSE){
      output$page <- renderUI({
        div(class="outer",do.call(bootstrapPage,c("",ui1())))
      })
    }
    if (USER$Logged == TRUE) {
      output$page <- renderUI({
        div(class="outer",do.call(navbarPage,c(inverse=TRUE,id="nav",title = "Prevision de nomination",ui2())))
      })
      print(ui)
    }
  })
  
  observe({
    if(!is.null(input_model())){
      USER$Trigger_file_exist_ok=TRUE
    }
    if(USER$Trigger_file_exist_ok){
      if(is.null(check_input_file(input_model(),input$date1))){
        USER$Trigger_file_format_ok=TRUE
      }
    }
    if(USER$Trigger_file_format_ok){
      safe_allvar_build=safely(allvar_build)
      if(!is.null(safe_allvar_build(input_model())$result)){
        USER$Trigger_all_var_build_ok=TRUE
      }
    }
    
    if(USER$Trigger_all_var_build_ok){
      # browser()
    }
  })
})
