get_means_recipe<- function(rec){
  map(rec$steps,.f = function(.x){
    if(class(.x)[[1]] =='step_center')
      return(.x$means)
  }) %>% compact %>% reduce(1)
}
  
get_sd_recipe <- function(rec){
  map(rec$steps,.f = function(.x){
    if(class(.x)[[1]] =='step_scale')
      return(sds=.x$sds)
  }) %>% compact %>% reduce(1)
}
