


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