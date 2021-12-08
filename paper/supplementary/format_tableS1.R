library(tidyverse)

t = readxl::read_xlsx("paper/Table_S1.xlsx")

ft = function(a,b,c) {
  return(paste0(a,))
}

t %>%
  mutate(freq=paste0(`excess_year_Global Serfling`),
         bay1)
