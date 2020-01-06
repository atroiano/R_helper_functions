pacman::p_load(tidyverse,xml2,XML)
# Credit to dantonnoriega at https://github.com/dantonnoriega/xmltools/blob/master/R/xml_get_paths.R. 
# I ripped this function off 100% from his code for this purpose and added the term_value to be required

#this will create a list of terminal nodes from a nodeset, so you do something like
#mark_terminal_value = '/term'
#path_dig(xml_find_all(xml_data,'//Location/Location2', mark_terminal = mark_terminal_value
path_dig <- function(nodeset, term_value,...) {
  
  args <- list(...)
  mark_terminal <- term_value
  
  node_len <- nodeset %>%
    xml2::xml_length()
  
  # top level are nodes with ln == 0
  terminal <- node_len == 0
  
  if(sum(terminal) == 0) { # no top level data
    x <- xml2::xml_path(nodeset)
    nodeset <- lapply(nodeset, xml2::xml_children)
    mapply(function(i, j) list(i, path_dig(j, ...)),
           i = x, j = nodeset, USE.NAMES = FALSE, SIMPLIFY = FALSE)
  } else {
    if(sum(terminal) == length(terminal)) {
      terminal_nodes <- nodeset[terminal] # subset to only terminal nodes
      if(!is.null(mark_terminal)) {
        mark_terminal <- as.character(mark_terminal)
        x <- trimws(paste0(mark_terminal, xml2::xml_path(terminal_nodes)))
      } else {
        x <- xml2::xml_path(terminal_nodes)
      }
      return(x)
    } else {
      terminal_nodes <- nodeset[terminal]
      if(!is.null(mark_terminal)) {
        mark_terminal <- as.character(mark_terminal)
        x <- trimws(paste0(mark_terminal, xml2::xml_path(terminal_nodes)))
      } else {
        x <- xml2::xml_path(terminal_nodes)
      }
      nodeset <- nodeset[!terminal]
      y <- xml2::xml_path(nodeset)
      nodeset <- lapply(nodeset, xml2::xml_children)
      append(x, mapply(function(i, j) list(i, path_dig(j, ...)),
                       i = y, j = nodeset, USE.NAMES = FALSE, SIMPLIFY = FALSE))
    }
  }
}

#After that is run, pass all the paths and the document you used above to this function to get a DF that supports 7 levels of traversing and arrays.
# Term_name is the name of the terminal values you passed above so the object mark_terminal_value
#NodeXLoop is the loop number for that terminal value if it's located within an array.
get_terminal_data <- function(xml_paths, xml_doc,term_name){
  paths = xml_paths %>% unlist
  terminal_nodes <- paths %>% keep(~str_detect(.x,term_name))
  terminal_stuff <- terminal_nodes %>% enframe()
  terminal_stuff %<>%  mutate(
    trav = str_replace(value,'/term','')
    ,values = map_chr(trav,~xml_text(xml_find_all(xml_doc,.x)))
    ,codes = map_chr(trav,~get_attrs(x=.x,xml_doc = xml_doc))
  ) 
  terminal_stuff %<>% separate(trav,into=c('Drop','Root','Node1','Node2','Node3','Node4','Node5','Node6','Node7'),sep = '/',remove = F) %>% 
    mutate(
      Node1Loop = str_replace_all(str_extract(Node1,'\\[.\\]'),'[^[:alnum:] ]','')
      ,Node2Loop = str_replace_all(str_extract(Node2,'\\[.\\]'),'[^[:alnum:] ]','')
      ,Node3Loop = str_replace_all(str_extract(Node3,'\\[.\\]'),'[^[:alnum:] ]','')
      ,Node4Loop = str_replace_all(str_extract(Node4,'\\[.\\]'),'[^[:alnum:] ]','')
      ,Node5Loop = str_replace_all(str_extract(Node5,'\\[.\\]'),'[^[:alnum:] ]','')
      ,Node6Loop = str_replace_all(str_extract(Node6,'\\[.\\]'),'[^[:alnum:] ]','')
      ,Node7Loop = str_replace_all(str_extract(Node7,'\\[.\\]'),'[^[:alnum:] ]','')
      ,Node1 = str_replace_all(Node1,'\\[.\\]','')
      ,Node2 = str_replace_all(Node2,'\\[.\\]','')
      ,Node3 = str_replace_all(Node3,'\\[.\\]','')
      ,Node4 = str_replace_all(Node4,'\\[.\\]','')
      ,Node5 = str_replace_all(Node5,'\\[.\\]','')
      ,Node6 = str_replace_all(Node6,'\\[.\\]','')
      ,Node7 = str_replace_all(Node7,'\\[.\\]','')
    )
  return(terminal_stuff)
}

