### PENDIENTE

function.migrante.origen <- function(df) {
  df %>% filter(clase2 == 1, 
                r_def == 0, 
                (c_res == 1 | c_res == 3), 
                eda >= 15, 
                eda <= 98,
                rama == 4) %>% 
    mutate(migrante = ifelse(l_nac_c > 33, TRUE,FALSE)) %>%
    filter(migrante == TRUE) %>%
   mutate(origen = l_nac_c) %>%
    group_by(origen, TIEMPO) %>%
    summarise(total.ocupada.origen = sum(fac)) %>% ungroup()
}



lista.pob.ocup.mig.orig <- lapply(lista.df, function.migrante.origen)

bd.ocupacion.orig <- Reduce(full_join,lista.pob.ocup.mig.orig) %>% mutate(fecha = as.yearqtr(TIEMPO))