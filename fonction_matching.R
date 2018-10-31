args = commandArgs(trailingOnly=TRUE)
print(args)
if (length(args)==0) {
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
} else if (length(args)<9){
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
} else {
    ref_model_path_arg=args[1]
    ref_model_csv_arg=args[2]
    col_name_ref_model_arg=args[3]
    scrapped_db_input_path_arg=args[4]
    scrapped_db_input_csv_arg=args[5]
    col_scrapped_model_arg=args[6]
    scrapped_db_output_path_arg=args[7]
    scrapped_db_output_csv_arg=args[8]
    max_dist_arg=args[9]
    string_dist_method_arg=args[10]
}

match_ref_model=function(ref_model_path,
                         ref_model_csv,
                         col_name_ref_model,
                         scrapped_db_input_path,
                         scrapped_db_input_csv,
                         col_scrapped_model,
                         scrapped_db_output_path,
                         scrapped_db_output_csv,
                         max_dist=NULL,
                         string_dist_method="qgram"){
  
  library(data.table)
  library(stringdist)
  
  #Import referentiel 
  file_ref_model=paste0(ref_model_path,ref_model_csv)
  ref_model=fread(file_ref_model,check.names = T)
  liste_ref_model=unique(ref_model[,get(col_name_ref_model)])
  
  #Import scrapped_data
  scrapped_db_file=paste0(scrapped_db_input_path,scrapped_db_input_csv)
  scrapped_db=fread(scrapped_db_file)
  list_scrapped_model=scrapped_db[,unique(get(col_scrapped_model))]
  
  #calcualte best matched model
  match_ref_model=CJ(scrapped_model=list_scrapped_model,ref_model=liste_ref_model)
  match_ref_model[,dist:=stringdist(scrapped_model,ref_model,method =string_dist_method,q=2)]
  setorder(match_ref_model,scrapped_model,dist)
  
  #keep only matching for distance 
  if(!is.null(max_dist)){
    match_ref_model=match_ref_model[dist<=max_dist]
  }
  
  res=match_ref_model[,.SD[1],by=scrapped_model]
  
  setkey(res,scrapped_model)
  setkeyv(scrapped_db,col_scrapped_model)
  scrapped_db[res,ref_model:=i.ref_model]
  
  fwrite(scrapped_db,file=paste0(scrapped_db_output_path,scrapped_db_output_csv))
  return(scrapped_db)
  
}


match_ref_model(ref_model_path=ref_model_path_arg,
                ref_model_csv=ref_model_csv_arg,
                col_name_ref_model=col_name_ref_model_arg,
                scrapped_db_input_path=scrapped_db_input_path_arg,
                scrapped_db_input_csv=scrapped_db_input_csv_arg,
                col_scrapped_model=col_scrapped_model_arg,
                scrapped_db_output_path=scrapped_db_output_path_arg,
                scrapped_db_output_csv=scrapped_db_output_csv_arg,
                max_dist=ifelse(max_dist_arg=="NULL",NULL,max_dist_arg),
                string_dist_method=string_dist_method_arg)
