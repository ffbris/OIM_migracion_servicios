lista.df <- readRDS("~/Desktop/23 Migración ASI servicios/OIM_migracion_servicios/1_data/processed_data/lista_df.rds")


function.migrante.interno <- function(df) {
  df %>% filter(clase2 == 1, 
                r_def == 0, 
                (c_res == 1 | c_res == 3), 
                eda >= 15, 
                eda <= 98,
                rama == 4
  ) %>% 
    mutate(migrante.ext = ifelse(l_nac_c > 33, TRUE,FALSE),
           migrante.int = ifelse(l_nac_c != ent, TRUE, FALSE)) %>%
    group_by(migrante.ext, migrante.int, ent, TIEMPO) %>%
    summarise(total.ocupada.manufactura = sum(fac)) %>% ungroup()
}


lista.pob.ocup.mig.int <- lapply(lista.df, function.migrante.interno)

bd.ocupacion.int <- Reduce(full_join,lista.pob.ocup.mig.int) %>% mutate(fecha = as.yearqtr(TIEMPO))

bd.ocupacion.int  %>% saveRDS("1_data/processed_data/bd_ocupacion_miginterna.rds")


bd.ocupacion.int %>% filter(migrante.ext == FALSE & migrante.int == TRUE) %>%
  group_by(fecha, migrante.int) %>% 
  summarise(Total = sum(total.ocupada.manufactura)) %>% 
  ggplot(aes(x=fecha, y=Total)) +
  geom_line() + 
  xlab("") + 
  labs(y="Población ocupada", 
       x = "Fecha", 
       # title = "Personas migrantes nacionales ocupadas en la manufactura (total)",
       # subtitle = "2005-2022 Trimestral",
       caption = "Fuente: INEGI ENOE") +
  # theme_bw() + 
  scale_y_continuous(labels = scales::label_number(suffix = "M", decimal.mark = ",", big.mark = ".", scale = 1e-6)) +
  theme(text=element_text(size=10,  family="Gill Sans Nova Book"))

ggsave("6_output/01_plot.pob_mig_interna_total.svg", 
       device = "svg",
       height = 15,
       width = 24,
       units = "cm",
       dpi = 300)


bd.ocupacion.int %>% filter(migrante.ext == FALSE) %>%
  mutate(migrante.int2 = ifelse(migrante.int == TRUE, "SI", "NO")) %>%
  group_by(fecha, migrante.int2) %>% 
  summarise(Total = sum(total.ocupada.manufactura)) %>% 
  spread(migrante.int2, Total) %>% 
  mutate(Porc = SI / (SI + NO)) %>% 
  ggplot(aes(x=fecha, y=Porc)) +
  geom_line() + 
  xlab("") + 
  labs(y="Porcentaje", 
       x = "Fecha", 
       # title = "Personas migrantes nacionales como proporción del total\nocupadas en la manufactura",
       # subtitle = "2005-2022 Trimestral",
       # caption = "Fuente: INEGI ENOE"
  ) + 
  # theme_bw() + 
  scale_y_continuous(labels = scales::label_percent(decimal.mark = ","), limits = c(0,.3)) +
  theme(text=element_text(size=10,  family="Gill Sans Nova Book"))

ggsave("plot.pob_mig_interna_porporcion.svg", 
       device = "svg",
       height = 15,
       width = 24,
       units = "cm",
       dpi = 300)