create_embed_data<- function(embed_data,col_name_embedding='cats',cols_to_embed,max_words=F, max_words_num=20000,save_location = NA){
  #embed_data is the dataframe containing the embedding date
  #col_name_embedding is the column name you can use for the embeddings, this should never change
  #cols_to_embed = column names that are going to be included in the embeding layer
  #max words is the max number of unqiue valeus you want to have in the layer

  df_X_embeddings <- embed_data %>% select(one_of(cols_to_embed))%>% ungroup()

  df_X_embeddings %<>%
    mutate_at(vars(one_of(cols_to_embed)), funs(str_replace_all(.,"[^[:alnum:]]",''))) %>%
    mutate_at(vars(one_of(cols_to_embed)), funs(str_replace_all(.,'[[:space:]]',''))) %>%
    unite(cats, colnames(.), sep = ' ') %>%
    mutate(cats=tolower(cats))
  guess<-max_words_num
  tokenizer <- text_tokenizer(num_words = guess) %>% fit_text_tokenizer(df_X_embeddings$cats)
  num_words <- length(tokenizer$word_index)
  if (num_words > guess)
  {warning('the total embeddeding layer size > the guessed words, consider increasing')
    warning(paste('setting embeddings to to ',num_words, sep =' '))
  }
  if (max_words == T ){
    num_words <- max_words_num
    warning(paste('setting embeddings to',max_words_num, sep =' '))
  }
  tokenizer <- text_tokenizer(num_words = num_words) %>% fit_text_tokenizer(df_X_embeddings$cats)
  max_length <- length(cols_to_embed)
  if (max_length == 0){
    max_length <- 1
  }

  embed_seq <- get_encoded_data(tokenizer = tokenizer,x = df_X_embeddings,max_length = max_length, colname = 'cats')
  #num_words = tokenizer$num_words
  if (!is.na(save_location)){
    #take first 50 characters of the embedding layer names
    cols_to_embed_vec<-substr(paste(cols_to_embed,collapse = '_'),start=1,stop=50)
    loc_save_data_info <- paste('embed',cols_to_embed_vec,'Train.rds', sep='_')
    loc_save_location_full <-paste(embedding_loc_single,loc_save_data_info,sep = '/')
    loc_save_tokenizer <- paste('embed',cols_to_embed_vec,'tokenizer',sep = '_')
    loc_save_tokenizer_full <- paste(save_location,loc_save_tokenizer,sep = '/')
    saveRDS(embed_seq,loc_save_location_full)
    save_text_tokenizer(tokenizer, loc_save_tokenizer_full)
  }
  return_list <- list(embed_seq = embed_seq,tokenizer = tokenizer)#,max_length = max_length, num_words = num_words)
}

get_embed_data<- function(embed_data,col_name_embedding='cats',cols_to_embed,tokenizer=NA,save_location = NA,dataset_identifier = 'Test'){
  #save_location should be the location of the Training tokenizer to load
  cols_to_embed_vec<-substr(paste(cols_to_embed,collapse = '_'),start=1,stop=50)
  #takes data, a tokenizer and the lenght and will create the embeddings for new data
  if (is.na(tokenizer)){
    if(is.na(save_location)){
      stop('no tokenizer or saved location found')
    }
    loc_save_tokenizer <- paste('embed',cols_to_embed_vec,'tokenizer',sep = '_')
    loc_save_tokenizer_full <- paste(save_location,loc_save_tokenizer,sep = '/')
    tokenizer<-load_text_tokenizer(loc_save_tokenizer_full)
  }
  df_X_embeddings <-embed_data %>% select(one_of(cols_to_embed))%>% ungroup()
  df_X_embeddings <- df_X_embeddings %>% mutate_at(vars(one_of(cols_to_embed)), funs(str_replace_all(.,"[^[:alnum:]]",'')))
  df_X_embeddings <- df_X_embeddings %>% mutate_at(vars(one_of(cols_to_embed)), funs(str_replace_all(.,'[[:space:]]','')))

  df_X_embeddings <- df_X_embeddings %>% unite(cats, colnames(.), sep = ' ')
  df_X_embeddings<- df_X_embeddings %>% mutate(cats=tolower(cats))

  embed_seq<-get_encoded_data(tokenizer = tokenizer,x = df_X_embeddings,max_length = length(cols_to_embed), colname = 'cats')
  if (!is.na(save_location)){
    loc_save_data_info <- paste('embed',cols_to_embed_vec,paste(dataset_identifier,'.rds',sep=''), sep='_')
    loc_save_location_full <- paste(save_location,loc_save_data_info,sep = '/')
    saveRDS(embed_seq,loc_save_location_full)
  } else {
    warning('Embedding Data Not Saved')
  }
  return(embed_seq)
}

run_embedding_actions <- function(generate_embedding_data = T,embed_cols,data_location,data=NA,generate_test_data =F,test_save_location=NA){
  #return a list of embedding data and tokenizer
  #generate embedding data will need to eb set to T or F. if it is F, the function will load the embedding data and tokenizer at the location data_location
  #data is the data you want to run the embedding layer on
  #if data is na then this will just load the data and the tokenizer
  #when you are ready to set up the test data, pass the data location of the saved embedding data and tokenizer, the test data, change generate_test_data to T and the location to save the RDS of the test data
  #looks like this function will be useful for running on the Train set and get_embed_data will be for loading these toeknizers to the Test Validation Set of data
  if (generate_embedding_data == T){
    if(is.na(data)){
      stop('data needed to generated embedding data is not provided')
    }
    embed_list <- lapply(embed_cols, function(x){
      create_embed_data(data %>%ungroup() %>%select(x)
                        ,cols_to_embed = x,save_location = data_location)})
  } else {
    if (generate_test_data == T){

      embed_list<- lapply(embed_cols, function(x){
        cols_to_embed_vec<-substr(paste(x,collapse = '_'),start=1,stop=50)
        get_embed_data(embed_data = data,cols_to_embed = x,tokenizer =  load_text_tokenizer(paste(data_location,'/embed_',cols_to_embed_vec,'_tokenizer',sep='')),save_loc=test_save_location)

      })

      #embed_test_list<- get_embed_data(embed_data = denials_data_test,cols_to_embed = embed_cols[[1]],tokenizer = embed_list[[1]][[2]],save_loc=embedding_loc_test_single)

    } else if (is.na(test_save_location)) {
      embed_list<-lapply(embed_cols, function(x){
        cols_to_embed_vec<-substr(paste(x,collapse = '_'),start=1,stop=50)
        list(data=readRDS(paste(data_location,'/embed_',cols_to_embed_vec,'_Train.rds',sep='')),  tokenizer=load_text_tokenizer(paste(data_location,'/embed_',cols_to_embed_vec,'_tokenizer',sep='')))
      })
    } else {
      embed_list<-lapply(embed_cols, function(x){
        cols_to_embed_vec<-substr(paste(x,collapse = '_'),start=1,stop=50)
        list(data=readRDS(paste(test_save_location,'/embed_',x,'_Test.rds',sep='')), tokenizer= load_text_tokenizer(paste(data_location,'/embed_',x,'_tokenizer',sep='')))
      })
    }
  }
  return(embed_list)
}

get_encoded_data <- function(tokenizer=tokenizer, x,max_length=max_length, colname='combined_embed'){
  embed_seq<-texts_to_sequences(tokenizer,x %>% select(contains(colname))%>% pull(.))
  embed_seq<-pad_sequences(embed_seq,maxlen=max_length)
  return(embed_seq)
}
