# Análise conjunta para grupos de experimentos casualizados em blocos em fatorial

install.packages("readxl")
library(readxl)
dados <- read_excel("first analysis.xlsx")
View(first_analysis)

str(first_analysis)

library(dplyr)
dados <- dados%>%
  mutate(Block = as.character(Block),
         Treat = as.character(Treat),
         Name_city = as.character(Name_city),
         Season = as.character(Season))
str(dados)
colnames(dados)

# Analise em cada local
library(lmtest)

# Local 1 - Sinop
mod.l1 <- aov(Grain_yield ~ Block + Treat + Season + Treat:Season,
           data = dados,
           subset = Name_city == "Sinop")

shapiro.test(rstandard(mod.l1))

lmtest::bptest(mod.l1)

anova(mod.l1)

# Local 2 - C. Mourão
mod.l2 <- aov(Grain_yield ~ Block + Treat + Season + Treat:Season,
              data = dados,
              subset = Name_city == "C. Mourão")

shapiro.test(rstandard(mod.l2))

lmtest::bptest(mod.l2)

anova(mod.l2)

# Local 3 - Londrina
mod.l3 <- aov(Grain_yield ~ Block + Treat + Season + Treat:Season,
              data = dados,
              subset = Name_city == "Londrina")

shapiro.test(rstandard(mod.l3))

lmtest::bptest(mod.l3)

anova(mod.l3)

# Análise Conjunta

# Razão entre os quadrados médios dos resíduos

(QMResiduo1<- anova(mod.l1)$"Mean Sq"[5])
(QMResiduo2<- anova(mod.l2)$"Mean Sq"[5])
(QMResiduo3<- anova(mod.l3)$"Mean Sq"[5])

QMResiduo<- c(QMResiduo1, QMResiduo2, QMResiduo3)

(max(QMResiduo) / min(QMResiduo))

# ANOVA 
mod.conj <- aov(Grain_yield ~ Name_city + Block%in%Name_city + Treat + Season + 
                  Treat:Season + Name_city:Treat + Name_city:Season + 
                  Name_city:Treat:Season,
                data = dados)

anova(mod.conj)


## Efeito de Tratamentos dentro de cada Local

