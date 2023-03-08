
## Add all RDS files to one list
library(tidyverse)
lista.df <- lapply(list.files(path = "1_data/raw_data/",
                              pattern = "*.rds",
                              full.names = TRUE), 
                   readRDS)

fnames_fecha <- list.files(path = "1_data/raw_data/",
                           pattern = "*.rds",
                           full.names = TRUE) %>% 
  gsub(".rds","",.) %>% 
  gsub("-"," Q",.) %>%
  paste("20",., sep="") %>% 
  as.yearqtr("%Y Q%q") 

lista.df <- lapply(lista.df, function.minus)

lista.df <- mapply(function.identificar, lista.df, fnames_fecha)

saveRDS(lista.df, "1_data/processed_data/lista_df.rds")
lista.df <- readRDS("1_data/processed_data/lista_df.rds")


lista.df <- lapply(lista.df, function.minus)


varlist <- c("clase2","r_def", "c_res", "eda", "l_nac_c", "rama", "TIEMPO", "fac", "rama_est2", "ent", "sex", "clase1", "eda5c", "eda7c", "ent", "anios_esc", "niv_ins")

select_columns <- function(data_list, varlist) {
  lapply(data_list, function(df) {
    df[, varlist, drop = FALSE]
  })
}

lista.df <- select_columns(lista.df, varlist)

bd.ocupacion <- Reduce(full_join,lista.df) %>% mutate(fecha = as.yearqtr(TIEMPO))