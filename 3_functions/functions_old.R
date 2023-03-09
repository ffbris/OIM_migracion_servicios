function.identificar <- function(df,x){
  cbind(df, TIEMPO=rep(x,nrow(df)))
}

function.minus <- function(df){
  names(df) <- tolower(names(df))
  names(df) <- gsub(x = names(df), pattern = "_tri", replacement = "")  
  return(df)
}





function.migrante.interno <- function(df) {
  df %>% filter(clase2 == 1, 
                r_def == 0, 
                (c_res == 1 | c_res == 3), 
                eda >= 15, 
                eda <= 98,
                rama_est2 == 3
  ) %>% 
    mutate(migrante.ext = ifelse(l_nac_c > 33, TRUE,FALSE),
           migrante.int = ifelse(l_nac_c != ent, TRUE, FALSE)) %>%
    group_by(migrante.ext, migrante.int, ent, TIEMPO) %>%
    summarise(total.ocupada.manufactura = sum(fac)) %>% ungroup()
}

function.migrante.sex <- function(df) {
  df %>% filter(clase2 == 1, 
                r_def == 0, 
                (c_res == 1 | c_res == 3), 
                eda >= 15, 
                eda <= 98,
                rama_est2 == 3
  ) %>% 
    mutate(migrante = ifelse(l_nac_c > 33, TRUE,FALSE)) %>%
    group_by(migrante, sex, TIEMPO) %>%
    summarise(total.ocupada.sex = sum(fac)) %>% ungroup()
}

function.migrante.jov <- function(df) {
  df %>% filter(clase1 == 1, 
                r_def == 0, 
                (c_res == 1 | c_res == 3), 
                eda5c == 1
  ) %>% 
    group_by(TIEMPO) %>%
    summarise(total.ocupada.jov = sum(fac)) %>% ungroup()
}

function.migrante.eda <- function(df) {
  df %>% filter(clase2 == 1, 
                r_def == 0, 
                (c_res == 1 | c_res == 3), 
                eda >= 15, 
                eda <= 98,
                rama_est2 == 3
  ) %>% 
    mutate(migrante = ifelse(l_nac_c > 33, TRUE,FALSE)) %>%
    group_by(migrante, eda7c, TIEMPO) %>% filter(migrante ==TRUE) %>% 
    summarise(total.ocupada.eda = sum(fac)) %>% ungroup()
}



function.migrante.eda2 <- function(df) {
  df %>% filter(clase2 == 1, 
                r_def == 0, 
                (c_res == 1 | c_res == 3), 
                eda >= 15, 
                eda <= 98,
                rama_est2 == 3
  ) %>% 
    mutate(migrante = ifelse(l_nac_c > 33, TRUE,FALSE)) %>% filter(migrante ==TRUE) %>% 
    group_by(TIEMPO) %>% 
    summarise(Media = weighted.mean(eda, fac), 
              Mediana = weighted.median(eda, fac),
              Cuartil_1 = weighted.quantile(eda, fac, prob = .25, na.rm= TRUE),
              Cuartil_3 = weighted.quantile(eda, fac, prob = .75, na.rm= TRUE))
}

function.migrante.origen <- function(df) {
  df %>% filter(clase2 == 1, 
                r_def == 0, 
                (c_res == 1 | c_res == 3), 
                eda >= 15, 
                eda <= 98,
                rama_est2 == 3
  ) %>% 
    mutate(migrante = ifelse(l_nac_c > 33, TRUE,FALSE)) %>%
    filter(migrante == TRUE) %>%
    # mutate(origen = ifelse(
    #   (l_nac_c >= 100 & l_nac_c < 200), "AFRICA",
    #   ifelse(
    #   (l_nac_c >= 200 & l_nac_c < 300), "AMERICA",
    #   ifelse(
    #     (l_nac_c >= 300 & l_nac_c < 400), "ASIA",
    #     ifelse(
    #       (l_nac_c >= 400 & l_nac_c < 500), "EUROPA","OTROS"
    #     )
    #   )
    #   )
  # ))%>%
  mutate(origen = l_nac_c) %>%
    group_by(origen, TIEMPO) %>%
    summarise(total.ocupada.origen = sum(fac)) %>% ungroup()
}

function.migrante.estado <- function(df) {
  df %>% filter(clase1 == 1, 
                r_def == 0, 
                (c_res == 1 | c_res == 3), 
                eda >= 15, 
                eda <= 98,
                rama_est2 == 3
  ) %>% 
    mutate(migrante = ifelse(l_nac_c > 33, TRUE,FALSE)  ) %>% 
    group_by(TIEMPO, ent, migrante) %>%
    summarise(total.ocupada.estado = sum(fac)) %>% ungroup()
}

function.migrante.esc <- function(df) {
  df %>% filter(clase2 == 1, 
                r_def == 0, 
                (c_res == 1 | c_res == 3), 
                eda >= 15, 
                eda <= 98,
                rama_est2 == 3
  ) %>% 
    mutate(migrante = ifelse(l_nac_c > 33, TRUE,FALSE)) %>% filter(migrante ==TRUE) %>% 
    group_by(TIEMPO) %>% 
    summarise(Media = weighted.mean(anios_esc, fac), 
              Mediana = weighted.median(anios_esc, fac, na.rm = TRUE),
              Cuartil_1 = weighted.quantile(anios_esc, fac, prob = .25, na.rm= TRUE),
              Cuartil_3 = weighted.quantile(anios_esc, fac, prob = .75, na.rm= TRUE))
}


function.migrante.niv.inst <- function(df) {
  df %>% filter(clase1 == 1, 
                r_def == 0, 
                (c_res == 1 | c_res == 3), 
                eda >= 15, 
                eda <= 98,
                rama_est2 == 3
  ) %>% 
    filter(l_nac_c > 33) %>%
    mutate( origen = l_nac_c )%>%
    group_by(TIEMPO, niv_ins, origen) %>%
    summarise(total.ocupada.estado = sum(fac)) %>% ungroup()
}