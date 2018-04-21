install.packages("ggfortify")
install.packages("haven")
install.packages("dplyr")
install.packages("memisc")
install.packages("GGally")
install.packages("tidyr")
install.packages("pander")
install.packages("ANOVA")
install.packages("ggpubr")
install.packages("sandwich")
install.packages("ggthemes")
install.packages("Hmisc")
install.packages("huxtable")

library(dplyr)
library(memisc)
library(haven)
library(lmtest)
library(ggplot2)
library(ggfortify)
library(pander)
library(lme4)
library(ggpubr)
library(car)
library(sandwich)
library(ggthemes)
library(Hmisc)
library(huxtable)

df = as.data.frame(read_spss("BD final - estudantes.sav"))



# Models
## Fraude by the Norm
datan = transmute(df, oportunidade = as.numeric(df$tri_oportunidade),
                  pressao = as.numeric(df$losango_motivacao),
                  racionalizacao = as.numeric(df$tri_dist_moral),
                  fraudenctx = as.numeric(df$norma_contexto),
                  fraudencop = as.numeric(df$norma_copiar),
                  fraudenplg = as.numeric(df$norma_plagio),
                  fraudeneud = as.numeric(df$norma_eu_desonesto),
                  fraudenavg = rowMeans(df[68:71], na.rm = TRUE))

### Norma Contexto
modeln1 = lm(data = datan, fraudenctx ~ oportunidade + pressao + racionalizacao) 

### Norma Copiar
modeln2 = lm(data = datan, fraudencop ~ oportunidade + pressao + racionalizacao) 

### Norma Plágio
modeln3 = lm(data = datan, fraudenplg ~ oportunidade + pressao + racionalizacao) 

### Norma Eu desonesto
modeln4 = lm(data = datan, fraudeneud ~ oportunidade + pressao + racionalizacao) 

### Norma Avg
modelnavg = lm(data = datan, fraudenavg ~ oportunidade + pressao + racionalizacao) 

### Comparing Models
AIC(model11, model12, model13, model14, model15)

BIC(model11, model12, model13, model14, model15)

#### Best model - modelnavg

## Fraud by specific behaviours and the gravity assessement 
datak = transmute(df, oportunidade = as.numeric(df$tri_oportunidade),
                  pressao = as.numeric(df$losango_motivacao),
                  racionalizacao = as.numeric(df$tri_dist_moral),
                  kf1 = as.numeric(df$k_especificos_1_2), 
                  kf2 = as.numeric(df$k_especificos_1_5), 
                  kf3 = as.numeric(df$k_especificos_1_7), 
                  kf4 = as.numeric(df$k_especificos_1_9), 
                  kf5 = as.numeric(df$k_especificos_1_10), 
                  kf6 = as.numeric(df$k_especificos_1_12), 
                  kf7 = as.numeric(df$k_especificos_1_57), 
                  kf8 = as.numeric(df$k_especificos_1_13), 
                  kf9 = as.numeric(df$k_especificos_1_15), 
                  kf10 = as.numeric(df$k_especificos_1_16), 
                  kf11 = as.numeric(df$k_especificos_1_17), 
                  kf12 = as.numeric(df$k_especificos_1_18), 
                  kf13 = as.numeric(df$k_especificos_1_20), 
                  kf14 = as.numeric(df$k_especificos_1_21), 
                  kf15 = as.numeric(df$k_especificos_1_24), 
                  kf16 = as.numeric(df$k_especificos_1_28),
                  kfavg = rowMeans(df[34:50], na.rm = TRUE),
                  kg1 = as.factor(df$k_especificos_2_2), 
                  kg2 = as.factor(df$k_especificos_2_5), 
                  kg3 = as.factor(df$k_especificos_2_7), 
                  kg4 = as.factor(df$k_especificos_2_9), 
                  kg5 = as.factor(df$k_especificos_2_10), 
                  kg6 = as.factor(df$k_especificos_2_12), 
                  kg7 = as.factor(df$k_especificos_2_57), 
                  kg8 = as.factor(df$k_especificos_2_13), 
                  kg9 = as.factor(df$k_especificos_2_15), 
                  kg10 = as.factor(df$k_especificos_2_16), 
                  kg11 = as.factor(df$k_especificos_2_17), 
                  kg12 = as.factor(df$k_especificos_2_18), 
                  kg13 = as.factor(df$k_especificos_2_20), 
                  kg14 = as.factor(df$k_especificos_2_21), 
                  kg15 = as.factor(df$k_especificos_2_24), 
                  kg16 = as.factor(df$k_especificos_2_28),
                  kgavg = as.factor(as.integer(rowMeans(df[51:67], na.rm = TRUE))))

## Models
modelk1 = lm(data = datak, 
             kf1 ~ oportunidade + pressao + racionalizacao + kg1)

modelk2 = lm(data = datak, 
             kf2 ~ oportunidade + pressao + racionalizacao + kg2)

modelk3 = lm(data = datak, 
             kf3 ~ oportunidade + pressao + racionalizacao + kg3)

modelk4 = lm(data = datak, 
             kf4 ~ oportunidade + pressao + racionalizacao + kg4)

modelk5 = lm(data = datak, 
             kf5 ~ oportunidade + pressao + racionalizacao + kg5)

modelk6 = lm(data = datak, 
             kf6 ~ oportunidade + pressao + racionalizacao + kg6)

modelk7 = lm(data = datak, 
             kf7 ~ oportunidade + pressao + racionalizacao + kg7)

modelk8 = lm(data = datak, 
             kf8 ~ oportunidade + pressao + racionalizacao + kg8)

modelk9 = lm(data = datak, 
             kf9 ~ oportunidade + pressao + racionalizacao + kg9)

modelk10 = lm(data = datak, 
             kf10 ~ oportunidade + pressao + racionalizacao + kg10)

modelk11 = lm(data = datak, 
             kf11 ~ oportunidade + pressao + racionalizacao + kg11)

modelk12 = lm(data = datak, 
             kf12 ~ oportunidade + pressao + racionalizacao + kg12)

modelk13 = lm(data = datak, 
             kf13 ~ oportunidade + pressao + racionalizacao + kg13)

modelk14 = lm(data = datak, 
             kf14 ~ oportunidade + pressao + racionalizacao + kg14)

modelk15 = lm(data = datak, 
             kf15 ~ oportunidade + pressao + racionalizacao + kg15)

modelk16 = lm(data = datak, 
             kf16 ~ oportunidade + pressao + racionalizacao + kg16)

# Average model
modelkavg = lm(data = datak, 
               kfavg ~ oportunidade + pressao + racionalizacao + kgavg)
summary(modelkavg)
autoplot(modelkavg)

## Comparing models
AIC(modelk1, modelk2, modelk3, modelk4, modelk5, modelk6, modelk7, 
    modelk8, modelk9, modelk10, modelk11, modelk12, modelk13, modelk14, 
    modelk15, modelk16, modelkavg)

BIC(modelk1, modelk2, modelk3, modelk4, modelk5, modelk6, modelk7, 
    modelk8, modelk9, modelk10, modelk11, modelk12, modelk13, modelk14, 
    modelk15, modelk16, modelkavg)

### Best model - modelkavg
# Final Models
dataF = transmute(df, oportunidade = as.numeric(df$tri_oportunidade),
                  pressao = as.numeric(df$losango_motivacao),
                  racionalizacao = as.numeric(df$tri_dist_moral),
                  fraudenavg = rowMeans(df[68:71], na.rm = TRUE),
                  kgavg = as.factor(as.integer(rowMeans(df[51:67], na.rm = TRUE))),
                  kfavg = rowMeans(df[34:50], na.rm = TRUE))

ModelN = lm(data = dataF, fraudenavg ~ oportunidade + pressao + racionalizacao)

ModelC = lm(data = dataF, kfavg ~ oportunidade + pressao + racionalizacao + kgavg)

## Normality Tests
### Shapiro-Wilk's
shapiro.test(dataF$oportunidade)
shapiro.test(dataF$pressao)
shapiro.test(dataF$racionalizacao)
shapiro.test(dataF$fraudenavg)
shapiro.test(dataF$kgavg)
shapiro.test(dataF$kfavg)

#### Graphical confirmation
ggqqplot(dataF$fraudenavg) + 
  theme_gdocs() +
  labs(title="QQPlot FraudeN")
  
ggqqplot(dataF$kgavg) + 
  theme_gdocs() +
  labs(title="QQPlot FraudeC")

## Autocorrelation - No
dwtest(ModelC)
dwtest(ModelN)
bgtest(ModelC)
bgtest(ModelN)

## Multicollinearity - No
vif(ModelC)
vif(ModelN)

## Heteroscedasticity - No
bptest(ModelC)
bptest(ModelN)
gqtest(ModelC)
gqtest(ModelN)

ncvTest(ModelC)

### Graphical Confirmation
autoplot(ModelC)

# Analysing the results
summary(ModelN)
summary(ModelC)

# Sensitivity Analysis
ModelNS = lm(data = dataF, fraudenavg ~ oportunidade + 
               pressao + racionalizacao + df$idade + 
               df$socioeconomico + df$estudos_pai)
summary(ModelNS)

ModelCS = lm(data = dataF, kfavg ~ oportunidade + 
               pressao + racionalizacao + kgavg + df$idade +
               as.factor(df$socioeconomico)) 
summary(ModelCS)

