fn_get_death_data_spain <- function(){

death_spain <-  suppressWarnings(read_excel("data-raw/INE/Spain_2020.xlsx",sheet=1,range="A8:AK119") %>%
                                   select(starts_with("...1" ) | starts_with("Total")) %>%
                                   rename(Age="...1",
                                          January=Total...4,
                                          February=Total...7,
                                          March=Total...10,
                                          April=Total...13,
                                          May=Total...16,
                                          June=Total...19,
                                          July=Total...22,
                                          August=Total...25,
                                          September=Total...28,
                                          October=Total...31,
                                          November=Total...34,
                                          December=Total...37) %>%
                                   mutate(January=as.integer(January),
                                          February=as.integer(February),
                                          March =as.integer(March ),
                                          April =as.integer(April ),
                                          May =as.integer(May ),
                                          June =as.integer(June ),
                                          July =as.integer(July ),
                                          August =as.integer(August),
                                          September =as.integer(September),
                                          October =as.integer(October),
                                          November =as.integer(November),
                                          December =as.integer(December),
                                          Age=str_remove_all(Age, " years old"),
                                          Age=str_remove_all(Age, " year old"),
                                          Age=str_remove_all(Age, " and over"))%>%
                                   mutate_if(is.integer, replace_na, 0))

death_spain_tmp <- death_spain %>%
  mutate(Age=as.numeric(Age)) %>%
  filter(Age >99)%>%
  mutate(Age=as.factor(Age))%>%
  adorn_totals("row")%>%
  filter(Age=="Total")%>%
  mutate(Age=replace(Age, Age=="Total","100"))

death_spain <- death_spain %>%
  mutate(Age=as.numeric(Age)) %>%
  filter(Age <100)%>%
  mutate(Age=as.factor(Age)) %>%
  rbind(death_spain_tmp)

return(death_spain)

}