
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
  
  # ggplot(aes(x=fecha, y=Total, fill = Edades)) +
  #                       geom_bar(position="stack", stat="identity") + 
  #                       xlab("") + 
  labs(y="Trabajadores", 
       x = "Fecha", 
       # title = "Población ocupada en manufactura",
       # subtitle = "2005-2022 Trimestral (2022-2027 pronóstico)"
  ) +
  # theme_bw() + 
  scale_y_continuous(labels = scales::label_number(decimal.mark = ",", big.mark = ".")) +
  common_params


ggsave("plot.modelo_Demanda_ARIMA.png", 
       device = "png",
       height = 15,
       width = 25,
       units = "cm",
       dpi = 300)



