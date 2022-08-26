View(list_vars22.06)

df_anf_22.06 <- list_vars22.06[["Amphibia"]]
df_anf_22.06

# probamos el glm con todas las variables

gm_anf22.06 <- glm(formula = CONTEO ~.,
                   family = poisson(link = "log"), 
                   data = df_anf_22.06)

summary(gm_anf22.06)
alias(gm_anf22.06)
AIC(gm_anf22.06)
BIC(gm_anf22.06)
gmresanf <- resid(gm_anf22.06)
qqnorm(gmresanf)
hist(gmresanf)
plot(density(gmresanf))


# existe multicolinealidad entre las varibles, por lo que es necesario reducirlas
# lo hare mediante la correlacion, dejando las que tengan mayor relevancia respecto
#  a las otras


cor_anf22.06 <- df_anf_22.06 %>% 
  select(where(is.numeric), -c(CONTEO)) %>% 
  cor(use ="complete.obs", method = "spearman")
View(cor_anf22.06)

cor_anf_vars22.06 <- findCorrelation(cor_anf22.06, 0.9, verbose = T, names = T)
cor_anf_vars22.06

df_anf_red22.06 <- df_anf_22.06 %>% select(-any_of(cor_anf_vars22.06))
df_anf_red22.06
str(df_anf_red22.06)

# una vez con las variables reducidas probaremos nuevamente


# GLM

gm_anf22.06 <- glm(formula = CONTEO ~.,
                   family = poisson(link = "log"), 
                   data = df_anf_red22.06)
summary(gm_anf22.06)
alias(gm_anf22.06)
AIC(gm_anf22.06)
BIC(gm_anf22.06)
gmresanf <- resid(gm_anf22.06)
qqnorm(gmresanf)
hist(gmresanf)
plot(density(gmresanf))





# probaremos reduciendo mas variables aumentando la correlacion


cor_anf_vars22.06 <- findCorrelation(cor_anf22.06, 0.8, verbose = T, names = T)
cor_anf_vars22.06

df_anf_red22.06 <- df_anf_22.06 %>% select(-any_of(cor_anf_vars22.06))
df_anf_red22.06
str(df_anf_red22.06)

# una vez con las variables reducidas probaremos nuevamente


# GLM

gm_anf22.06 <- glm(formula = CONTEO ~.,
                   family = poisson(link = "log"), 
                   data = df_anf_red22.06)
summary(gm_anf22.06)
summary(gm_anf22.06)
alias(gm_anf22.06)
BIC(gm_anf22.06)
gmresanf <- resid(gm_anf22.06)
qqnorm(gmresanf)
hist(gmresanf)
plot(density(gmresanf))






# aun existe multicolinealidad, reduciremos un poco mas


cor_anf_vars22.06 <- findCorrelation(cor_anf22.06, 0.73, verbose = T, names = T)
cor_anf_vars22.06

df_anf_red22.06 <- df_anf_22.06 %>% select(-any_of(cor_anf_vars22.06))
df_anf_red22.06
str(df_anf_red22.06)

# una vez con las variables reducidas probaremos nuevamente


# GLM

gm_anf22.06 <- glm(formula = CONTEO ~.,
                   family = poisson(link = "log"), 
                   data = df_anf_red22.06)
summary(gm_anf22.06)
alias(gm_anf22.06)
BIC(gm_anf22.06)
gmresanf <- resid(gm_anf22.06)
qqnorm(gmresanf)
hist(gmresanf)
plot(density(gmresanf))




backup_options <- options()
options(na.action = "na.fail")
modelanf22.06 <- dredge(gm_anf22.06, trace = 2)
View(modelanf22.06)
sw(modelanf22.06)
swanf22.06 <- sw(modelanf22.06)
swanf22.06 <- tidy(swanf22.06)
names(swanf22.06) <- c("Variables", "Suma de los pesos de akaike")
options(backup_options)

View(swanf22.06)



gm_anf1 <- eval(getCall(modelanf22.06, 1))
summary(gm_anf1)


graficaanf1 <- visreg(gm_anf1, xvar = "DIST_ANTRO", gg= T, 
                   scale = "response", partial=T) +
  ylim(0,17) +
  labs(title = "Anfibios",
       tag = "1",
       x = "Distancia a zonas antropogénicas",
       y = "Número de atropellamientos",
       caption = "Generalized linear model"
  ) +
  theme_gray()



graficaanf2 <- visreg(gm_anf1, xvar = "DOSEL_MEDIA", gg= T, 
       partial=T) +
  labs(title = "Anfibios",
       tag = "2",
       x = "Dosel arbóreo",
       y = "Número de atropellamientos",
       caption = "Generalized linear model"
  ) +
  theme_gray()






# Conclusion: tenemos la lista en orden descendente de las variables que mas 
# influyen en el atropellamiento de los anfibios

View(swanf22.06)
suppressWarnings(print(graficaanf1))
suppressWarnings(print(graficaanf2))



