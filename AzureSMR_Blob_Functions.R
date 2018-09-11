save_chunks<-function(df,tenant_id,client_id,auth_key,auth_type,resource_group,storage_account,container_name,blob_folder,blob_name){
  write.csv.str <- function(df, row.names = F) {
    filepath <- tempfile()
    fwrite(df, filepath, row.names = row.names)
    s <- readr::read_file(filepath)
    s
  }
  end <- 0
  id <- 1
  start <- 1
  num_rows<-round(64000000/((object.size(df)[1])/nrow(df)),0)  
  end <- start+num_rows
  sc <- createAzureContext(tenantID =tenant_id
                           #application id = clientID
                           ,clientID = client_id
                           #the password 
                           ,authKey= auth_key
                           ,authType = auth_type)
  
  azureSAGetKey(sc,resourceGroup = resource_group, storageAccount = storage_account )
  while (end < nrow(df) ) {
    #get size of one row and figure out how many rows per chunk, limiting to 64 MB
    #id will save multiple files
    chunk<-denials_data_valid[start:end,]
    
    azurePutBlob(sc
                 ,storageAccount = storage_account
                 ,container = container_name
                 ,contents = write.csv.str(chunk)
                 ,blob = paste(blob_folder,'/',blob_name,'_',id,sep='')) 
    id<- id+1
    start <- end+1
    end <- start+num_rows
    print(start)
    print(end)
  }
}
get_blob_data <- function(tenant_id,client_id,auth_key,auth_type,resource_group,storage_account,container_name,blob_folder,blob_name) {
  sc <- createAzureContext(tenantID =tenant_id
                           #application id = clientID
                           ,clientID = client_id
                           #the password 
                           ,authKey= auth_key
                           ,authType = auth_type)
  
  azureSAGetKey(sc,resourceGroup = resource_group, storageAccount = storage_account )
  
  all_files<-azureBlobLS(sc,directory = blob_folder,recursive = F,storageAccount = storage_account,resourceGroup = resource_group,container = container_name)
  total_files <- nrow(all_files)
  print(total_files)
  start <- 1
  while (start <=total_files){
    print (start)
    data<-azureGetBlob(azureActiveContext = sc,storageAccount = storage_account, directory = blob_folder,container = container_name,resourceGroup = resource_group,blob=paste(blob_name,'_',start,sep=''))
    filepath<-tempfile()
    write_lines(data,filepath)
    if (!exists('blob_df')){
      blob_df<-fread(filepath,showProgress = F)
      start <- start+1
    } else {
      blob_df %<>% rbind(.,fread(filepath))
      start<-start+1
    }
  }
  return(blob_df)
}
