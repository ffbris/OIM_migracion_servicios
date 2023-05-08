
lista.df <- readRDS("~/Desktop/23 Migración ASI servicios/OIM_migracion_servicios/1_data/processed_data/lista_df.rds")

sectores_rama <- c("Agropecuario",
                   "Construcción",
                   "Industria manufacturera",
                   "Comercio",
                   "Servicio",
                   "Otros",
                   "No especificado")

df_rama <- data.frame(sectores = sectores_rama, rama = c(6,1,2,3,4,5,7))

function.total <- function(df) {
  df %>% filter(clase2 == 1, 
                r_def == 0, 
                (c_res == 1 | c_res == 3), 
                eda >= 15, 
                eda <= 98,
  ) %>% 
    mutate(migrante = ifelse(l_nac_c > 33, TRUE,FALSE)) %>%
    group_by(rama, migrante, TIEMPO) %>%
    summarise(total.ocupada = sum(fac)) %>% ungroup()
}

lista.pob.ocup.total <- lapply(lista.df, function.total)

bd.ocupacion.total <- Reduce(full_join,lista.pob.ocup.total) %>% mutate(fecha = as.yearqtr(TIEMPO))

bd.ocupacion.total %>% saveRDS("1_data/processed_data/bd_ocupacion_total.rds")
bd.ocupacion.total  <-  readRDS("1_data/processed_data/bd_ocupacion_total.rds")
# Total migrant population as % of total workforce

bd.ocupacion.total %>% group_by(fecha, migrante) %>%
  summarise(Total = sum(total.ocupada)) %>% 
  ungroup() %>% 
  spread(migrante, Total) %>%
  rename(Migrante= 3, No.Migrante =2) %>%
  mutate(prop = Migrante/No.Migrante) %>%
  ggplot(aes(x=fecha, y=prop)) +
  geom_line() + 
  geom_smooth(alpha=.2, fill = "gray") + 
  xlab("") + 
  labs(y="Porcentaje", 
       x = "Fecha") +
  scale_x_yearqtr(format = "%YT%q", n = (2022-2005)*2-3) +
  scale_y_continuous(labels = scales::label_percent(decimal.mark = ",")) +          
  theme(text=element_text(size=10,  family="Gill Sans Nova Book"),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 8))

ggsave("6_output/01_pob_migocupada_total.svg", 
       device = "svg",
       height = 15,
       width = 24,
       units = "cm",
       dpi = 300)



#============================== Total occupied migrant population by sector

bd.ocupacion.total  %>% filter(migrante == TRUE) %>%
  group_by(rama, fecha) %>%   
  left_join(df_rama, by= "rama") %>%
  mutate(rama =as.factor(rama), Sectores = sectores, Total = total.ocupada)%>%
  filter(rama != 7)  %>%
  ggplot(aes(x=fecha, y=Total, fill= Sectores)) +
  geom_bar(position="stack", stat="identity") + 
  xlab("") + 
  labs(y="Población ocupada", 
       x = "Fecha") +
  scale_x_yearqtr(format = "%YT%q", n = (2022-2005)*2-3) +
  scale_y_continuous(labels = scales::label_number(decimal.mark = ",", big.mark = ".")) +          
  theme(text=element_text(size=10,  family="Gill Sans Nova Book"),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 8))

ggsave("6_output/01_plot.pob_migocupada_total_sector.svg", 
       device = "svg",
       height = 15,
       width = 24,
       units = "cm",
       dpi = 300)

bd.ocupacion.total  %>% filter(migrante == TRUE) %>%
  group_by(rama, fecha) %>%   
  left_join(df_rama, by= "rama") %>%
  mutate(rama =as.factor(rama), Sectores = sectores, Total = total.ocupada) %>% 
  filter(fecha == "2022 Q4") %>% as.data.frame

#============================== Migrant population as % of total occupied population

bd.ocupacion.total %>%
  group_by(rama, fecha) %>%   spread(migrante, total.ocupada) %>%
  rename(Migrante= 5, No.Migrante =4) %>% mutate(prop = Migrante/(Migrante+No.Migrante)) %>%
  left_join(df_rama, by= "rama") %>%
  mutate(rama =as.factor(rama), Sectores = sectores)%>%
  filter(rama != 7)  %>%
  ggplot(aes(x=fecha, y=prop, group= Sectores, colour = Sectores)) +
  geom_smooth(fill = "gray") +
  xlab("") +
  labs(y="Población ocupada",
       x = "Fecha",
       caption = "Nota: Series suavizadas con método loess.") +
  scale_y_continuous(labels = scales::label_percent(decimal.mark = ",")) +
  theme(text=element_text(size=10,  family="Gill Sans Nova Book"))

ggsave("6_output/01_plot.pob_migocupada_total_sector_proporcion.svg",
       device = "svg",
       height = 15,
       width = 24,
       units = "cm",
       dpi = 300)