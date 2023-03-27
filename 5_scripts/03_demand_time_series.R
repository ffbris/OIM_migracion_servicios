
bd.ocupacion.total <- readRDS("1_data/processed_data/bd_ocupacion_total.rds")

demanda <- bd.ocupacion.total %>% 
  group_by(fecha) %>% 
  filter(rama == 4) %>%   #sector servicios
  summarise(total_o = sum(total.ocupada))

demanda <- rbind(demanda, c(as.yearqtr("2020 Q2"), NA)) %>% arrange(fecha) 


modelo_d <- auto.arima(ts(demanda$total_o, frequency = 4, start= c(2005,3)))
summary(modelo_d)
checkresiduals(modelo_d)

forecast(modelo_d,20,level=95)

as.data.frame(forecast(modelo_d,20,level=95)) %>% 
  add_rownames(var = "fecha") %>% 
  mutate(fecha= as.yearqtr(fecha)) %>% 
  full_join(demanda, by = "fecha") %>% arrange(fecha) %>% 
  mutate(Forecast = `Point Forecast`,
         Lo = `Lo 95`,
         Hi = `Hi 95`) %>%
  ggplot(aes(x=fecha)) +
  geom_line(alpha=.5, aes(y=total_o)) + 
  geom_line(alpha=.5, aes(y= Forecast), linetype = "dotted") + 
  geom_ribbon(aes(ymin = Lo, ymax = Hi), alpha = .2) +
  labs(y="Trabajadores", 
       x = "Fecha"
  ) +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()),
                     limits= c(0, NA)) +
  common_params


ggsave("6_output/3_plot.modelo_Demanda_ARIMA.svg", 
       device = "svg",
       height = 15,
       width = 24,
       units = "cm",
       dpi = 300)

checkresiduals(modelo_d)

as.data.frame(forecast(modelo_d,20,level=95)) %>% 
  add_rownames(var = "fecha") %>% 
  mutate(fecha= as.yearqtr(fecha)) %>% 
  full_join(demanda, by = "fecha") %>% arrange(fecha) %>% 
write.csv("1_data/processed_data/pronosticodemandaARIMA.csv")
