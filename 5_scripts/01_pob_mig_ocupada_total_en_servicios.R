setwd("")
lista.df <- readRDS("1_data/processed_data/lista_df.rds")

function.total.serv <- function(df) {
  df %>% filter(clase2 == 1, 
                r_def == 0, 
                (c_res == 1 | c_res == 3), 
                eda >= 15, 
                eda <= 98,
                rama == 4) %>% 
    mutate(migrante = ifelse(l_nac_c > 33, TRUE,FALSE)) %>%
    group_by(rama, migrante, TIEMPO) %>%
    summarise(total.ocupada = sum(fac)) %>% ungroup()
}

lista.pob.ocup.total.serv <- lapply(lista.df, function.total.serv)

bd.ocupacion.total.serv <- Reduce(full_join,lista.pob.ocup.total.serv) %>% mutate(fecha = as.yearqtr(TIEMPO))

bd.ocupacion.total.serv %>% saveRDS("1_data/processed_data/bd_ocupacion_total_serv.rds")


# bd.ocupacion.total.serv  <-  readRDS("1_data/processed_data/bd_ocupacion_total_serv.rds")
