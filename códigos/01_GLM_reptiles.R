
# cargamos data frame con todas las variables que se especificaron, tanto 
# ambientales como antropogenicas

load("dat.RData")
list_vars22.06 <- datos

View(list_vars22.06)


df_rep_22.06 <- list_vars22.06[["Reptilia"]]
df_rep_22.06

## los analisis previos al glm ya se realizaron en los anteriores scripts ##


# probamos el glm con todas las variables

gm_rep22.06 <- glm(formula = CONTEO ~.,
                   family = poisson(link = "log"), 
                   data = df_rep_22.06)

summary(gm_rep22.06)
alias(gm_rep22.06)
AIC(gm_rep22.06)
BIC(gm_rep22.06)
gmresrep <- resid(gm_rep22.06)
qqnorm(gmresrep)
hist(gmresrep)
plot(density(gmresrep))


# existe multicolinealidad entre las varibles, por lo que es necesario reducirlas
# lo hare mediante la correlacion, dejando las que tengan mayor relevancia respecto
#  a las otras


cor_rep22.06 <- df_rep_22.06 %>% 
  select(where(is.numeric), -c(CONTEO)) %>% 
  cor(use ="complete.obs", method = "spearman")
View(cor_rep22.06)

cor_rep_vars22.06 <- findCorrelation(cor_rep22.06, 0.9, verbose = T, names = T)
cor_rep_vars22.06

df_rep_red22.06 <- df_rep_22.06 %>% select(-any_of(cor_rep_vars22.06))
df_rep_red22.06
str(df_rep_red22.06)

# una vez con las variables reducidas probaremos nuevamente


# GLM

gm_rep22.06 <- glm(formula = CONTEO ~.,
                   family = poisson(link = "log"), 
                   data = df_rep_red22.06)
summary(gm_rep22.06)
alias(gm_rep22.06)
AIC(gm_rep22.06)
BIC(gm_rep22.06)
gmresrep <- resid(gm_rep22.06)
qqnorm(gmresrep)
hist(gmresrep)
plot(density(gmresrep))





# probaremos reduciendo mas variables aumentando la correlacion


cor_rep_vars22.06 <- findCorrelation(cor_rep22.06, 0.8, verbose = T, names = T)
cor_rep_vars22.06

df_rep_red22.06 <- df_rep_22.06 %>% select(-any_of(cor_rep_vars22.06))
df_rep_red22.06
str(df_rep_red22.06)

# una vez con las variables reducidas probaremos nuevamente


# GLM

gm_rep22.06 <- glm(formula = CONTEO ~.,
                   family = poisson(link = "log"), 
                   data = df_rep_red22.06)
summary(gm_rep22.06)
summary(gm_rep22.06)
alias(gm_rep22.06)
BIC(gm_rep22.06)
gmresrep <- resid(gm_rep22.06)
qqnorm(gmresrep)
hist(gmresrep)
plot(density(gmresrep))






# aun existe multicolinealidad, reduciremos un poco mas


cor_rep_vars22.06 <- findCorrelation(cor_rep22.06, 0.77, verbose = T, names = T)
cor_rep_vars22.06

df_rep_red22.06 <- df_rep_22.06 %>% select(-any_of(cor_rep_vars22.06))
df_rep_red22.06
str(df_rep_red22.06)

# una vez con las variables reducidas probaremos nuevamente





# GLM

gm_rep22.06 <- glm(formula = CONTEO ~.,
                   family = poisson(link = "log"), 
                   data = df_rep_red22.06)
summary(gm_rep22.06)
alias(gm_rep22.06)
BIC(gm_rep22.06)
gmresrep <- resid(gm_rep22.06)
qqnorm(gmresrep)
hist(gmresrep)
plot(density(gmresrep))



# vemos que ya no existe colinealidad entre las variables, lo que indica que 
# son "independientes"






# Modelos multiples
# evaluadas bajo el AIC ---------------------------------------------------

backup_options <- options()
options(na.action = "na.fail")
modelrep22.06aic <- dredge(gm_rep22.06, trace = 2, rank = "AIC")
View(modelrep22.06aic)
options(backup_options)



# Seleccionaremos un subconjunto de modelos (bajo algun criterio) y de este
# subconjunto realizaremos inferencia de las variables que mayor influyen en
# base a la suma de los pesos de akaike





# Seleccion de conjunto de modelos con con delta<4 -------------------
# (recomendado por Burnham y anderson 2002)



view(modelrep22.06aic[modelrep22.06aic$delta <= 4,])

# suma de los pesos de akaike
swrep22.06aic <- sw(modelrep22.06aic[modelrep22.06aic$delta <= 4,])
swrep22.06aic <- tidy(swrep22.06aic)

View(swrep22.06aic)


# seleccion de subconjunto de modelos cuyo peso conjunto sea de <= 0.95
# (tambien recomendado por Burnham y anderson 2002)

view(modelrep22.06aic[cumsum(modelrep22.06aic$weight) <= 0.95,])

# suma de los pesos de akaike
swrep22.06aic2 <- sw(modelrep22.06aic[cumsum(modelrep22.06aic$weight) <= 0.95,])
swrep22.06aic2 <- tidy(swrep22.06aic2)

View(swrep22.06aic2)


# con esto tenemos las variables que mayor inflyuyen en el atropellamiento de 
# los reptiles, como se puede observar los resultados son muy similares en ambos
# criterios (delta<4 y peso conjunto <0.95)



# mostramos el modelo "mejor" evaluado de ambos subconjuntos

# modelo 1
gm_rep1aic <- eval(getCall(modelrep22.06aic[modelrep22.06aic$delta <= 4,], 1))
summary(gm_rep1aic)



# graficos
graficarep1aic <- visreg(gm_rep1aic, xvar = "BARO", gg= T, 
                          scale = "response", partial=T) +
  labs(title = "Reptiles",
       subtitle = "Generalized linear model 1",
       tag = "1",
       x = "Presión Barométrica",
       y = "Número de atropellamientos",
       caption = "AIC rank"
  ) +
  theme_gray()
graficarep1aic


graficarep2aic <- visreg(gm_rep1aic, xvar = "DISTANCIA_ARROYOS_MEAN", gg= T, 
                          scale = "response", partial=T) +
  ylim(0,14) +
  labs(title = "Reptiles",
       subtitle = "Generalized linear model 1",
       tag = "2",
       x = "Distancia a Arroyos",
       y = "Número de atropellamientos",
       caption = "AIC rank"
  ) +
  theme_gray()
graficarep2aic



graficarep3aic <- visreg(gm_rep1aic, xvar = "PENDIENTE_D", gg= T, 
                         scale = "linear", partial=T) +
  labs(title = "Reptiles",
       subtitle = "Generalized linear model 1",
       tag = "3",
       x = "Pendiente Derecha",
       y = "Número de atropellamientos",
       caption = "AIC rank"
  ) +
  theme_gray()
graficarep3aic

graficarep4aic <- visreg(gm_rep1aic, xvar = "PENDIENTE_I", gg= T, 
                         scale = "linear", partial=T) +
  labs(title = "Reptiles",
       subtitle = "Generalized linear model 1",
       tag = "4",
       x = "Pendiente Izquierda",
       y = "Número de atropellamientos",
       caption = "AIC rank"
  ) +
  theme_gray()
graficarep4aic


# AllOk





# Conclusiones AIC

# lista de variables por orden de influencia en el atropellamiento de reptiles
View(swrep22.06aic2)

# modelo 1
summary(gm_rep1aic)

# graficos
graficarep1aic
graficarep2aic
graficarep3aic
graficarep4aic














# evaluados bajo el AICc --------------------------------------------------


backup_options <- options()
options(na.action = "na.fail")
modelrep22.06aicc <- dredge(gm_rep22.06, trace = 2, rank = "AICc")
View(modelrep22.06aicc)
sw(modelrep22.06aicc)
swrep22.06aicc <- sw(modelrep22.06aicc)
swrep22.06aicc <- tidy(swrep22.06aicc)
names(swrep22.06aicc) <- c("Variables", "Suma de los pesos de akaike")
options(backup_options)

View(swrep22.06aicc)


# -------------------------------------------------------------------------
# esto es importante, solo seleccionar un pequelo conjunto menor al num de la muestra
View(modelrep22.06aicc[1:30,])
str(modelrep22.06)
sw(modelrep22.06aicc[1:20,])
swrep22.06aicc1 <- sw(modelrep22.06aicc[1:20,])
swrep22.06aicc1 <- tidy(swrep22.06aicc1)
swrep22.06aicc1
# -------------------------------------------------------------------------











# modelo 1
gm_rep1aicc <- eval(getCall(modelrep22.06aicc, 1))
summary(gm_rep1aicc)


# graficos
graficarep1aicc <- visreg(gm_rep1aicc, xvar = "BARO", gg= T, 
                      scale = "response", partial=T) +
  ylim(0,14) +
  labs(title = "Reptiles",
       subtitle = "Generalized linear model 1",
       tag = "1",
       x = "Presión Barométrica",
       y = "Número de atropellamientos",
       caption = "AICc rank"
  ) +
  theme_gray()
graficarep1aicc

graficarep2aicc <- visreg(gm_rep1aicc, xvar = "DISTANCIA_ARROYOS_MEAN", gg= T, 
                      scale = "response", partial=T) +
  ylim(0,14) +
  labs(title = "Reptiles",
       subtitle = "Generalized linear model 1",
       tag = "2",
       x = "Distancia a Arroyos",
       y = "Número de atropellamientos",
       caption = "AICc rank"
  ) +
  theme_gray()
graficarep2aicc

# AllOk

















# evaluados bajo el BIC ---------------------------------------------------


backup_options <- options()
options(na.action = "na.fail")
modelrep22.06bic <- dredge(gm_rep22.06, trace = 2, rank = "BIC")
View(modelrep22.06bic)
sw(modelrep22.06bic)
swrep22.06bic <- sw(modelrep22.06bic)
swrep22.06bic <- tidy(swrep22.06bic)
names(swrep22.06bic) <- c("Variables", "Suma de los pesos de akaike")
options(backup_options)

View(swrep22.06bic)


# modelo 1
gm_rep1bic <- eval(getCall(modelrep22.06bic, 1))
summary(gm_rep1bic)




# graficos
graficarep1bic <- visreg(gm_rep1bic, xvar = "BARO", gg= T, 
                          scale = "response", partial=T) +
  ylim(0,14) +
  labs(title = "Reptiles",
       subtitle = "Generalized linear model 1",
       tag = "1",
       x = "Presión Barométrica",
       y = "Número de atropellamientos",
       caption = "BIC rank"
  ) +
  theme_gray()
graficarep1bic

graficarep2bic <- visreg(gm_rep1bic, xvar = "DISTANCIA_ARROYOS_MEAN", gg= T, 
                          scale = "response", partial=T) +
  ylim(0,14) +
  labs(title = "Reptiles",
       subtitle = "Generalized linear model 1",
       tag = "2",
       x = "Distancia a Arroyos",
       y = "Número de atropellamientos",
       caption = "BIC rank"
  ) +
  theme_gray()
graficarep2bic

# AllOk





View(swrep22.06)
suppressWarnings(print(graficarep1))
suppressWarnings(print(graficarep2))






