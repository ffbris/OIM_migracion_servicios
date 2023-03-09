
## Add all RDS files to one list
library(tidyverse)
lista.df <- lapply(list.files(path = "1_data/raw_data/",
                              pattern = "*.rds",
                              full.names = TRUE), 
                   readRDS)

fnames_fecha <- list.files(path = "1_data/raw_data",
                           pattern = "*.rds",
                           full.names = TRUE) %>% 
                gsub(".rds","",.) %>% 
                gsub(".*_data/","",.) %>%
                gsub("-"," Q",.) %>%
                paste("20",., sep="") %>% 
                as.yearqtr("%Y Q%q") 

function.minus <- function(df){
  names(df) <- tolower(names(df))
  return(df)
}

lista.df <- lapply(lista.df, function.minus)


# Corrige para algunos años en los que solamente se presenta factor de expansión trimestral y no total
for (i in 1:6){
  lista.df[[59+i]] <- lista.df[[59+i]] %>% mutate(fac = fac_tri)
}

function.identificar <- function(df,x){
  cbind(df, TIEMPO=rep(x,nrow(df)))
}

lista.df <- mapply(function.identificar, lista.df, fnames_fecha)

saveRDS(lista.df, "1_data/processed_data/lista_df.rds")
