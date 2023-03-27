
library(GGally)

bd.ocupacion.ts <- readRDS("1_data/processed_data/bd_ocupacion_ts.rds")
bd.ocupacion.jov <- readRDS("1_data/processed_data/bd_ocupacion_jov.rds")


 datos_adicionales <- read_excel("1_data/datos_1_stocks.xlsx",  sheet = "Hoja 3")

datos_adicionales$fecha <- as.yearqtr(datos_adicionales$year + .5)

base.completa <- left_join(bd.ocupacion.ts, datos_adicionales, by = "fecha")

base.completa$ref_asylum_MEX <- exp(
  na.approx(log(base.completa$ref_asylum_MEX), na.rm = FALSE, rule = 2)
) 

base.completa$ref_asylum_US <- exp(
  na.approx(log(base.completa$ref_asylum_US), na.rm = FALSE, rule = 2)
)

base.completa$US_mig_stock <- exp(
  na.approx(log(base.completa$US_mig_stock), na.rm = FALSE, rule = 2)
)

base.completa$MEX_mig_stock <- exp(
  na.approx(log(base.completa$MEX_mig_stock), na.rm = FALSE, rule = 2)
)

datos_adicionales_2 <- read_excel("1_data/datos_1_stocks.xlsx",  sheet = "UPMigratoria")

datos_adicionales_2$fecha <- as.yearqtr(as.numeric(datos_adicionales_2$year) + .5)

datos_adicionales_2 <- datos_adicionales_2 %>% select(fecha, total_entradas, visitante_permiso_trabajo, extranjeros_residenes_mex)

base.completa <- left_join(base.completa, datos_adicionales_2, by = "fecha")

base.completa$total_entradas <- na.approx(base.completa$total_entradas, na.rm = FALSE, rule = 2)
base.completa$visitante_permiso_trabajo <- na.approx(base.completa$visitante_permiso_trabajo, na.rm = FALSE, rule = 2)     
base.completa$extranjeros_residenes_mex <- na.approx(base.completa$extranjeros_residenes_mex, na.rm = FALSE, rule = 2) 



base.completa <- left_join(base.completa, bd.ocupacion.jov, by = "fecha")
base.completa <- base.completa %>% select(-TIEMPO, -year)




datos_adicionales_3 <- read_excel("1_data/datos_1_stocks.xlsx",  sheet = "econom")

datos_adicionales_3$fecha <- as.yearqtr(as.numeric(datos_adicionales_3$year) + .5)

base.completa <- left_join(base.completa, datos_adicionales_3, by = "fecha")
base.completa <- base.completa %>% select(-year) 

base.completa$PIB_pc <- exp(
  na.approx(log(base.completa$PIB_pc), na.rm = FALSE, rule = 2)
)
base.completa$FBCF <- exp(
  na.approx(log(base.completa$FBCF), na.rm = FALSE, rule = 2)
)

base.completa$migracion_interna <- exp(
  na.spline(log(base.completa$migracion_interna), na.rm = FALSE)
)

base.completa



base.completa <- base.completa %>% as.data.frame() %>% mutate(total.ocupada.jov = na.approx(total.ocupada.jov))

corrplot(cor(base.completa[,2:13]), method = "number")

base.completa %>% select(-fecha) %>% ggpairs

write.csv(base.completa, "1_data/processed_data/2_regresores_multiv.csv")


# Calculate the correlation matrix between the independent variables
cor_matrix <- cor(base.completa[, c(2:13)])

# Identify highly correlated variables with a correlation coefficient greater than 0.8
highly_correlated <- findCorrelation(cor_matrix, cutoff = 0.8)

base.completa[, highly_correlated] %>% colnames

base.completa.modif <- base.completa %>% mutate(
  ref_asylum_MEX = diff(c(ref_asylum_MEX, NA)) %>% na.approx(na.rm = FALSE, rule = 2),
  FBCF = diff(c(FBCF,NA)) %>% na.approx(na.rm = FALSE, rule = 2),
  MEX_mig_stock = diff(c(MEX_mig_stock, NA)) %>% na.approx(na.rm = FALSE, rule = 2),
  migracion_interna = migracion_interna / total_entradas
) %>% select(-extranjeros_residenes_mex, -Total, -ref_asylum_MEX)

base.completa.modif.s.fecha <- base.completa.modif %>% select(-fecha)

cor_matrix2 <- cor(base.completa.modif.s.fecha)

highly_correlated2 <- findCorrelation(cor_matrix2, cutoff = 0.8)

base.completa.modif.s.fecha[, highly_correlated2] %>% colnames

base.completa.modif.s.fecha <- base.completa.modif.s.fecha %>% 
  mutate_at(c("ref_asylum_US"          ,   
              "US_mig_stock"   ,          
              "MEX_mig_stock"      ,       
              "total_entradas"      ,      
              "visitante_permiso_trabajo",
             "total.ocupada.jov"     ,    
             "PIB_pc"           ,         
              "FBCF"), ~(scale(.) %>% as.vector))

base.completa.modif.s.fecha %>% ggpairs
base.completa.modif.c.fecha  <-  cbind("fecha" = base.completa$fecha, base.completa.modif.s.fecha) 
write.csv(base.completa.modif.c.fecha, "1_data/processed_data/2_regresores_multiv.csv")

# Drop highly correlated variables from the model


modelo_2 <- auto.arima(
  ts(base.completa$Total, frequency = 4, start= c(2005,3)), 
  xreg =  as.matrix(base.completa.modif.s.fecha))

summary(modelo_2)


prediccion_regresores <- function(x){
  y <- base.completa.modif.s.fecha %>% select(x) %>% ts(frequency = 4, start= c(2005,3))
  modelo <- auto.arima(y)
  forecast(modelo,20,level=95) %>% as.data.frame() %>% select(`Point Forecast`)
}

predicciones <- sapply(1:ncol(base.completa.modif.s.fecha), prediccion_regresores) %>% as.data.frame()
names(predicciones) <- names(base.completa.modif.s.fecha) 
x_fcst <- as.matrix(
  predicciones
)

matriz_trabajo <- x_fcst %>% as.data.frame
matriz_trabajo$fecha <-   as.data.frame(forecast(modelo_2,5,level=95, xreg=x_fcst)) %>% 
  add_rownames(var = "fecha") %>% 
  mutate(fecha= as.yearqtr(fecha))  %>% pull(fecha)


matriz_trabajo_completa <- rbind(matriz_trabajo, base.completa.modif.c.fecha)

pivot_longer(matriz_trabajo_completa, 
             cols = colnames(matriz_trabajo_completa[,1:8]), 
             names_to = "variable", 
             values_to = "value") %>% 
  ggplot(aes(x= fecha, y= value, colour=variable)) + geom_line() +facet_grid(rows = "variable")

ggsave("6_output/02_plot.modelo_2_forecastvariables.svg", 
       device = "svg",
       height = 15,
       width = 24,
       units = "cm",
       dpi = 300)

  
as.data.frame(forecast(modelo_2,level=95, xreg=x_fcst)) %>% 
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
  scale_y_continuous(labels = scales::label_number(decimal.mark = ",", big.mark = "."), limits = c(0,NA)) +
  common_params + 
  ylim(NA, 230000)

ggsave("6_output/02_plot.modelo_2_ARIMAdinam.svg", 
       device = "svg",
       height = 15,
       width = 24,
       units = "cm",
       dpi = 300)

checkresiduals(modelo_2)

as.data.frame(forecast(modelo_2,level=95, xreg=x_fcst)) %>% 
  add_rownames(var = "fecha") %>% 
  mutate(fecha= as.yearqtr(fecha)) %>% 
  full_join(bd.ocupacion.ts, by = "fecha") %>% arrange(fecha) %>% write.csv("1_data/processed_data/pronosticoARIMAdinamic.csv")

base.tercer.modelo <- cbind("Total" = base.completa$Total, base.completa.modif.s.fecha)

jotest_2 <- ca.jo(base.tercer.modelo)
summary(jotest_2)

beta_tsDyn <- VECM(base.tercer.modelo,
                   lag = 2, r= 1,
                   include = "trend",
                   estim = "ML")
summary(beta_tsDyn)
# 
# 
# predicciones_3 <- predict(beta_tsDyn, n.ahead = 20) %>% 
#   as.data.frame() %>% 
#   select(Total) %>% 
#   mutate(fecha= as.yearqtr(seq(from = 2023, to = 2027.75, by =.25)))
# 
# 
# 
# predicciones_3 %>% 
#   full_join(bd.ocupacion.ts, by = "fecha") %>% arrange(fecha) %>% 
#   mutate(Forecast = `Total.x`) %>%
#   ggplot(aes(x=fecha)) +
#   geom_line(aes(y=Total.y)) + 
#   geom_line(alpha=.5, aes(y= Forecast), linetype = "dotted") + 
#   labs(y="Población ocupada", 
#        x = "Fecha") +
#   scale_y_continuous(labels = scales::label_number(decimal.mark = ",", big.mark = "."), limits = c(0,NA)) +
#   common_params + 
#   ylim(NA, 230000)
# 
# ggsave("6_output/02_plot.modelo_3_VECM.svg", 
#        device = "svg",
#        height = 15,
#        width = 24,
#        units = "cm",
#        dpi = 300)
# 
# 
# # predict(beta_tsDyn, n.ahead = 20) %>% 
# #   as.data.frame() %>% select(total.ocupada.jov) %>% mutate(total.ocupada.jov_2 = exp(total.ocupada.jov)) %>%   mutate(fecha= as.yearqtr(seq(from = 2022, to = 2026.75, by =.25))) %>% 
# #    full_join(base.completa_colin, by = "fecha") %>% arrange(fecha) %>% 
# #   ggplot(aes(x=fecha)) +
# #   geom_line(alpha=.5, aes(y=exp(total.ocupada.jov.y))) + 
# #                           geom_line(alpha=.5, aes(y= total.ocupada.jov_2), linetype = "dotted") 
