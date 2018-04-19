install.packages("ggfortify")
install.packages("haven")
install.packages("dplyr")
install.packages("memisc")
install.packages("GGally")
install.packages("tidyr")

library(dplyr)
library(memisc)
library(haven)
library(lmtest)
library(ggplot2)
library(ggfortify)



df = as.data.frame(read_spss("BD final - estudantes.sav"))

# Base variables

data = transmute(df, oportunidade = as.numeric(df$tri_oportunidade),
                 pressao = as.numeric(df$losango_motivacao),
                 racionalizacao = as.numeric(df$tri_dist_moral),
                 fraudenctx = as.numeric(df$norma_contexto),
                 fraudencop = as.numeric(df$norma_copiar),
                 fraudenplg = as.numeric(df$norma_plagio),
                 fraudeneud = as.numeric(df$norma_eu_desonesto),
                 fraudenavg = rowMeans(df[68:71], na.rm = TRUE),
                 fraudkfreq = rowMeans(df[34:50], na.rm = TRUE))


GGally::ggpairs(data)
# Models
## Fraude by the Norm
### Norma Contexto

model11 = lm(data = data, fraudenctx ~ oportunidade + pressao + racionalizacao) 
summary(model11)
autoplot(model11)

### Norma Copiar

model12 = lm(data = data, fraudencop ~ oportunidade + pressao + racionalizacao) 
summary(model12)

### Norma Plágio

model13 = lm(data = data, fraudenplg ~ oportunidade + pressao + racionalizacao) 
summary(model13)

### Norma Eu desonesto

model14 = lm(data = data, fraudeneud ~ oportunidade + pressao + racionalizacao) 
summary(model14)

### Norma Avg

model15 = lm(data = data, fraudenavg ~ oportunidade + pressao + racionalizacao) 
summary(model15)
autoplot(model15) + theme_bw()
autoplot(prcomp(model15), scale = TRUE)

### Comparing Models
AIC(model11, model12, model13, model14, model15)

## Fraud by Frequency

model21 = lm(data = data, fraudkfreq ~ oportunidade + pressao + racionalizacao)
summary(model21)

# Tests
# Multicollinearity - bptest, gqtest
bptest(model11)
bptest(model12)
bptest(model13)
bptest(model14)
bptest(model15)

gqtest(model13)
gqtest(model15)

# Heteroskedascitity - PCA?

