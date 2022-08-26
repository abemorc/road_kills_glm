
# Se construiran todos los modelos posibles con las posibles combinaciones de 
# las variables, y posteriormente se seleccioanra un subconjunto de estos



# Modelos multiples
# evaluadas bajo el AICc ---------------------------------------------------

backup_options <- options()
options(na.action = "na.fail")
modelrep22.06aicc <- dredge(gm_rep22.06, trace = 2, rank = "AICc")
View(modelrep22.06aicc)
options(backup_options)



# Seleccionaremos un subconjunto de modelos (bajo algun criterio) y de este
# subconjunto realizaremos inferencia de las variables que mayor influyen en
# base a la suma de los pesos de akaike
# (unicamente es necesario uno, pero se solicito en que se hicieran varios)




# criterios de seleccion:
# Seleccion de conjunto de modelos con delta<4 (los modelos "mejor" rankeados) ------
# (recomendado por Burnham y anderson 2002)



view(modelrep22.06aicc[modelrep22.06aicc$delta <= 4,])

# suma de los pesos de akaike
swrep22.06aicc <- sw(modelrep22.06aicc[modelrep22.06aicc$delta <= 4,])
swrep22.06aicc <- tidy(swrep22.06aicc)





# seleccion de subconjunto de modelos cuyo peso conjunto sea de <= 0.95
# (tambien recomendado por Burnham y anderson 2002)

view(modelrep22.06aicc[cumsum(modelrep22.06aicc$weight) <= 0.95,])

# suma de los pesos de akaike
swrep22.06aicc2 <- sw(modelrep22.06aicc[cumsum(modelrep22.06aicc$weight) <= 0.95,])
swrep22.06aicc2 <- tidy(swrep22.06aicc2)





# seleccion del 10% de los modelos mejor rankeados (el que ya se habia utilizado)

view(modelrep22.06aicc[1:floor(nrow(modelrep22.06aicc)*0.10),])

# suma de los pesos de akaike
swrep22.06aicc3 <- sw(modelrep22.06aicc[1:floor(nrow(modelrep22.06aicc)*0.10),])
swrep22.06aicc3 <- tidy(swrep22.06aicc3)




# ------------------------------------------------------------


# Veamos los resultados de los tres criterios de seleccion
View(swrep22.06aicc)
View(swrep22.06aicc2)
View(swrep22.06aicc3)


# Se observa que los resultados son bastamte similares, se mantiene mas o 
# menos el mismo orden de relevancia de las variables, cambiando muy poco 
# los lugares

# con esto tenemos las variables que mayor inflyuyen en el atropellamiento de 
# los reptiles, como se puede observar los resultados son muy similares en 
# los tres criterios (delta<4, peso conjunto <0.95, 10% top models)




# ejemplo del modelo final -------------------------------------------
# mostramos el modelo "mejor" evaluado de los tres subconjuntos

# modelo 1
gm_rep1aicc <- eval(getCall(modelrep22.06aicc[modelrep22.06aicc$delta <= 4,], 1))
summary(gm_rep1aicc)



# graficos
graficarep1aicc <- visreg(gm_rep1aicc, xvar = "BARO", gg= T, 
                         scale = "response", partial=T) +
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





# Conclusiones AICc

# lista de variables por orden de influencia en el atropellamiento de reptiles
View(swrep22.06aicc)   #criterio delta<4
View(swrep22.06aicc2)  #criterio suma acumulativa .95
View(swrep22.06aicc3)  #criterio 10% top models



# modelo 1
summary(gm_rep1aicc)

# graficos
graficarep1aicc
graficarep2aicc








