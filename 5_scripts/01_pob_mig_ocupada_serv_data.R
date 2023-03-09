# lista.df <- readRDS("~/Desktop/23 Migración ASI servicios/OIM_migracion_servicios/1_data/processed_data/lista_df.rds")


# function.migrante <- function(df) {
#   df %>% filter(clase2 == 1, 
#                 r_def == 0, 
#                 (c_res == 1 | c_res == 3), 
#                 eda >= 15, 
#                 eda <= 98,
#                 rama == 4 ) %>% 
#     mutate(migrante = ifelse(l_nac_c > 33, TRUE,FALSE)) %>%
#     group_by(migrante, ent, rama_est2, TIEMPO) %>%
#     summarise(total.ocupada.servicios = sum(fac)) %>% ungroup()
# }
# 
# lista.pob.ocup.mig <- lapply(lista.df, function.migrante)
# 
# bd.ocupacion <- Reduce(full_join,lista.pob.ocup.mig) %>% mutate(fecha = as.yearqtr(TIEMPO))
# 
# bd.ocupacion %>% saveRDS("1_data/processed_data/bd.ocupacion.rds")

bd.ocupacion  <-  readRDS("1_data/processed_data/bd.ocupacion.rds")

bd.ocupacion %>% 
  group_by(fecha) %>% 
  summarise(Total = sum(total.ocupada.servicios))%>%
  ggplot(aes(x=fecha, y=Total)) +
  geom_line() + 
  xlab("") + 
  labs(y="Población ocupada", 
       x = "Fecha") +
  scale_y_continuous(labels = scales::label_number(suffix = "M", decimal.mark = ",", big.mark = ".", scale = 1e-6)) +
  theme(text=element_text(size=10,  family="Gill Sans Nova Book"))

ggsave("6_output/01_plot.pob_migserv_total.svg", 
       device = "svg",
       height = 15,
       width = 24,
       units = "cm",
       dpi = 300)

bd.ocupacion %>% filter(migrante == TRUE) %>%
  group_by(fecha) %>% 
  summarise(Total = sum(total.ocupada.servicios))%>%
  ggplot(aes(x=fecha, y=Total)) +
  geom_line() + 
  geom_smooth(alpha=.2, fill = "gray") + 
  xlab("") + 
  labs(y="Población ocupada", 
       x = "Fecha") +
  scale_y_continuous(labels = scales::label_number(decimal.mark = ",", big.mark = ".")) +          
  theme(text=element_text(size=10,  family="Gill Sans Nova Book"))

ggsave("6_output/01_plot.pob_migserv_total_mig.svg", 
       device = "svg",
       height = 15,
       width = 24,
       units = "cm",
       dpi = 300)


bd.ocupacion %>% group_by(fecha, migrante) %>%
  summarise(Total = sum(total.ocupada.servicios)) %>% 
  ungroup() %>% 
  spread(migrante, Total) %>%
  rename(Migrante= 3, No.Migrante =2) %>%
  mutate(prop = Migrante/No.Migrante) %>%
  ggplot(aes(x=fecha, y=prop)) +
  geom_line() + 
  geom_smooth(alpha=.2, fill = "gray") + 
  xlab("") + 
  labs(y="Porcentaje", 
       x = "Fecha", 
       # title = "Proporción entre población migrante internacional y población \ntotal ocupada en la manufactura",
       # subtitle = "2005-2022 Trimestral",
       # caption = "Fuente: INEGI ENOE."
  ) +
  # theme_bw() + 
  scale_y_continuous(labels = scales::label_percent(decimal.mark = ",")) +          theme(text=element_text(size=10,  family="Gill Sans Nova Book"))

ggsave("6_output/01_plot.pob_migserv_total_proporcion.svg", 
       device = "svg",
       height = 15,
       width = 24,
       units = "cm",
       dpi = 300)  

clasificacion <- c("Restaurantes y servicios \nde alojamiento", "Transportes, comunicaciones,\n correo y almacenamiento", "Servicios profesionales, \nfinancieros y corporativos", 
"Servicios sociales", "Servicios diversos", "Gobierno y organismos \ninternacionales")

clasif.df <- data.frame(rama_est2 = c(6,7,8,9,10,11), clasificacion)

bd.ocupacion %>% filter(migrante == TRUE) %>% 
  left_join(clasif.df, by= "rama_est2") %>%
  group_by(fecha, clasificacion) %>% 
  summarise(Total = sum(total.ocupada.servicios))%>%
  mutate(clasificacion = fct_reorder(clasificacion, Total, .desc = FALSE)) %>%
  ggplot(aes(x=fecha, y=Total, fill = clasificacion)) +
  geom_bar(position="stack", stat="identity") +
    xlab("") + 
  labs(y="Población ocupada", 
       x = "Fecha") +
  scale_y_continuous(labels = scales::label_number(decimal.mark = ",", big.mark = ".")) +          
  theme(text=element_text(size=10,  family="Gill Sans Nova Book"))

ggsave("6_output/01_plot.pob_migserv_total_mig_subsector.svg", 
       device = "svg",
       height = 15,
       width = 24,
       units = "cm",
       dpi = 300)