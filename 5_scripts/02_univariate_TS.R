library(xts)
library(tidyverse)
library(TTR)
library(forecast)
library(readxl)
library(corrplot)
library(caret)
library(tsDyn)
library(urca)
library(ggthemr)
library(spatstat)
library(shades)
library(extrafont)

setwd("~/Desktop/23 Migración ASI servicios/OIM_migracion_servicios")

ggthemr_reset()


set.seed(12345)
paleta_OIM_base <- c("#0033A0", 
                              "#5B92E5", 
                              "#FFB81C",
                              "#5CB8B2",
                              "#ff671f",
                              "#d22630")
                              
paleta_OIM <- c(paleta_OIM_base,
                brightness(paleta_OIM_base, 0.75),
                brightness(paleta_OIM_base, 0.5),
                brightness(paleta_OIM_base, 0.3))

OIM <- define_palette(
  swatch = paleta_OIM,
  gradient = c(lower = "#5B92E5", upper = "#0033A0"),
  background = "#ffffff",
  text = c("#444444", "#444444"),
  line = c("#696969", "#696969"),
  gridline = "#F5F5F5"
)

ggthemr(OIM)
# font_import(paths = "7_misc/fonts/", prompt = FALSE)
# font.add("Gill_Sans_Nova", "7_misc/fonts/GillSansNova-Book.otf")


bd.ocupacion  <-  readRDS("1_data/processed_data/bd.ocupacion.rds")


bd.ocupacion.ts <- bd.ocupacion %>% 
  filter(migrante == TRUE) %>% 
  group_by(fecha) %>% 
  summarise(Total =sum(total.ocupada.servicios))

# Se interpola el dato faltante por COVID
bd.ocupacion.ts <- rbind(bd.ocupacion.ts, c(as.yearqtr("2020 Q2"), NA)) %>% arrange(fecha) %>% mutate(Total = na.approx(Total))

n_mig <- bd.ocupacion.ts %>% select(Total) %>% ts(frequency = 4, start= c(2005,3))

hist(n_mig) 

tseries::adf.test(n_mig,alternative = "stationary")

acf(n_mig, lag.max=30)
pacf(n_mig, lag.max=30)

modelo <- auto.arima(n_mig)

summary(modelo)

checkresiduals(modelo)

arima1 <- as.data.frame(forecast(modelo,20,level=95))

arima1 %>% 
  add_rownames(var = "fecha") %>% 
  mutate(fecha= as.yearqtr(fecha)) %>% 
  full_join(bd.ocupacion.ts, by = "fecha") %>% arrange(fecha) %>% 
  mutate(Forecast = `Point Forecast`,
         Lo = `Lo 95`,
         Hi = `Hi 95`) %>%
  ggplot(aes(x=fecha)) +
  geom_line(aes(y=Total)) + 
  geom_line(aes(y= Forecast), linetype = "dotted") + 
  geom_ribbon(aes(ymin = Lo, ymax = Hi), alpha = .2) +
  labs(y="Población ocupada", 
       x = "Fecha") + 
  scale_y_continuous(labels = scales::label_number(decimal.mark = ",", big.mark = "."), 
                     limits = c(0,NA)) +
  common_params

ggsave("6_output/plot.modelo_1_ARIMAuniv.svg", 
       device = "svg",
       height = 15,
       width = 24,
       units = "cm",
       dpi = 300)



