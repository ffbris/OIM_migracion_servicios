
lista.df <- readRDS("~/Desktop/23 Migración ASI servicios/OIM_migracion_servicios/1_data/processed_data/lista_df.rds")


function.migrante.jov <- function(df) {
  df %>% filter(clase1 == 1, 
                r_def == 0, 
                (c_res == 1 | c_res == 3), 
                eda5c == 1
  ) %>% 
    group_by(TIEMPO) %>%
    summarise(total.ocupada.jov = sum(fac)) %>% ungroup()
}

lista.migrante.jov <- lapply(lista.df, function.migrante.jov)

bd.ocupacion.jov <- Reduce(full_join,lista.migrante.jov) %>% mutate(fecha = as.yearqtr(TIEMPO))


bd.ocupacion.jov  %>% saveRDS("1_data/processed_data/bd_ocupacion_jov.rds")
