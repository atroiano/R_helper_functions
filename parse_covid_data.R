# files should be the fs::dir_lis(<loc of files>)
covid_data <- read_csv(files[1])

for (i in files[2:length(files)]){
  print(i)
  if (ncol(read_csv(i,n_max = 1)) == 1){
    data<- read_tsv(i,col_types = cols(
      `Province/State` = col_character(),
      `Country/Region` = col_character(),
      `Last Update` = col_character(),
      Confirmed = col_double(),
      Deaths = col_double(),
      Recovered = col_double()
    ))
  } else{
    data<- read_csv(i,col_types = cols(
      `Province/State` = col_character(),
      `Country/Region` = col_character(),
      `Last Update` = col_character(),
      Confirmed = col_double(),
      Deaths = col_double(),
      Recovered = col_double()
    ))
  }
  covid_data <- bind_rows(covid_data,data)
}
