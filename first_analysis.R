# Análise conjunta para grupos de experimentos casualizados em blocos
library(readxl)

dados <- read_excel("first analysis.xlsx")
str(dados)

library(dplyr)
dados <- dados %>%
  mutate(Block = as.character(Block), 
         Treat = as.character(Treat), 
         Name_city = as.character(Name_city),
         Grain_yield = as.numeric(Grain_yield, na.rm = TRUE))
str(dados)
colnames(dados)

library(ggplot2)
ggplot(dados,
       aes(x = Treat,
           y = Grain_yield,
           color = Treat)) + 
  geom_point() +
  theme(axis.text.x = element_blank()) +
  facet_wrap(~Name_city) +
  xlab("Tratamentos") +
  ylab("Rendimento de Grao (t/ha)") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_blank())          


ggplot(dados, aes(x = Treat, y = Grain_yield, color = Treat)) + 
  geom_point() +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),       # Reduz o tamanho do texto da legenda
    legend.key.size = unit(0.3, "cm")           # Diminui o tamanho das chaves na legenda
  ) +
  guides(color = guide_legend(nrow = 1)) +      # Organiza a legenda em uma única linha
  facet_wrap(~Name_city) +
  labs(x = "Tratamentos", y = "Rendimento de Grão (t/ha)")


ggplot(dados,
       aes(x = Name_city ,
           y = Grain_yield,
           color = Name_city)) + 
  geom_point() +
  theme(axis.text.x = element_blank()) +
  facet_wrap(~Treat) +
  xlab("Locais") +
  ylab("Rendimento de Grao (t/ha)") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_blank())


ggplot(dados, aes(x = Name_city, y = Grain_yield, color = Name_city)) + 
  geom_point() +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),         # Reduz o tamanho do texto da legenda
    legend.key.size = unit(0.3, "cm")             # Diminui o tamanho das chaves na legenda
  ) +
  guides(color = guide_legend(nrow = 1)) +        # Organiza a legenda em uma única linha
  facet_wrap(~Treat) +
  labs(x = "Locais", y = "Rendimento de Grão (t/ha)")


#Analises Individuais
library(lmtest)

#Local 1 - Planaltina
mod.l1<- aov(Grain_yield ~ Block + Treat, 
             data=dados, 
             subset = c(Name_city=="Planaltina"))

shapiro.test(rstandard(mod.l1))

lmtest::bptest(mod.l1) #studentized Breusch-Pagan test

ggplot(subset(dados, Name_city=="Planaltina"),
       aes(x = Treat,
           y = rstudent(mod.l1))) +
  geom_point()

anova(mod.l1)     

#Local 2 - Sinop
mod.l2<- aov(Grain_yield ~ Block + Treat,
             data = dados,
             subset = c(Name_city=="Sinop"))

shapiro.test(rstandard(mod.l2))

lmtest::bptest(mod.l2) #studentized Breusch-Pagan test

ggplot(subset(dados, Name_city=="Planaltina"),
       aes(x = Treat,
           y = rstudent(mod.l2))) +
  geom_point()


anova(mod.l2)     

#Local 3 - C. Mourão        
mod.l3<- aov(Grain_yield ~ Block + Treat,
             data = dados,
             subset = c(Name_city=="C. Mourão"))

shapiro.test(rstandard(mod.l3))

lmtest::bptest(mod.l3) #studentized Breusch-Pagan test

ggplot(subset(dados, Name_city=="Planaltina"),
       aes(x = Treat,
           y = rstudent(mod.l3))) +
  geom_point()


anova(mod.l3)     

#Local 4 - Londrina
mod.l4<- aov(Grain_yield ~ Block + Treat,
             data = dados,
             subset = c(Name_city=="Londrina"))

shapiro.test(rstandard(mod.l4))

lmtest::bptest(mod.l4) #studentized Breusch-Pagan test

ggplot(subset(dados, Name_city=="Londrina"),
       aes(x = Treat,
           y = rstudent(mod.l4))) +
  geom_point()


anova(mod.l4) 

#Local 5 - 7 Lagoas ($)
mod.l5 <- aov(Grain_yield ~ Block + Treat,
              data = dados,
              subset = c(Name_city == "7 Lagoas ($)"))

shapiro.test(rstandard(mod.l5))

lmtest::bptest(mod.l5) #studentized Breusch-Pagan test

ggplot(subset(dados, Name_city == "7 Lagoas ($)"),
       aes(x = Treat,
           y = rstudent(mod.l5))) +
  geom_point()

anova(mod.l5)

#Local 6 - Goiânia
mod.l6 <- aov(Grain_yield ~ Block + Treat,
              data = dados,
              subset = c(Name_city == "Goiânia"))

shapiro.test(rstandard(mod.l6))

lmtest::bptest(mod.l6)

ggplot(subset(dados, Name_city == "Goiânia"),
       aes(x = Treat,
           y = rstudent(mod.6))) +
  geom_point()

anova(mod.l6)

#Local 7 - Londrina (2)
mod.l7 <- aov(Grain_yield ~ Block + Treat,
              data = dados,
              subset = c(Name_city == "Londrina (2)"))

shapiro.test(rstandard(mod.l7))

lmtest::bptest(mod.l7)

ggplot(subset(dados, Name_city == "Londrina (2)"),
       aes(x = Treat,
           y = rstandard(mod.l7))) +
  geom_point()

anova(mod.l7)

#Local 8 - Dourados
mod.l8 <- aov(Grain_yield ~ Block + Treat,
              data = dados,
              subset = c(Name_city == "Dourados"))

shapiro.test(rstandard(mod.l8))

lmtest::bptest(mod.l8)

ggplot(subset(dados, Name_city == "Dourados"),
       aes(x = Treat,
           y = rstandard(mod.l8))) +
  geom_point()

anova(mod.l8)
#Nao rejeita H0 - media dos tratamentos nao diferem entre si

#Local 9 - S. Rai. das Mangabeiras
mod.l9 <- aov(Grain_yield ~ Block + Treat,
              data = dados,
              subset = c(Name_city == "S. Rai. das Mangabeiras"))

shapiro.test(rstandard(mod.l9))

lmtest::bptest(mod.l9)

ggplot(subset(dados, Name_city == "S. Rai. das Mangabeiras"),
       aes(x = Treat,
           y = rstandard(mod.l9))) +
  geom_point()

anova(mod.l9)

#Local 10 - Vilhena
mod.l10 <- aov(Grain_yield ~ Block + Treat,
               data = dados,
               subset = c(Name_city == "Vilhena"))

shapiro.test(rstandard(mod.l10))

lmtest::bptest(mod.l10)

ggplot(subset(dados, Name_city == "Vilhena"),
       aes(x = Treat,
           y = rstandard(mod.l10))) +
  geom_point()

anova(mod.l10)

#Local 11 - Sinop (2)
mod.l11 <- aov(Grain_yield ~ Block + Treat,
               data = dados,
               subset = c(Name_city == "Sinop (2)"))

shapiro.test(rstandard(mod.l11))

lmtest::bptest(mod.l11)

ggplot(subset(dados, Name_city == "Sinop (2)"),
       aes(x = Treat,
           y = rstandard(mod.l11))) +
  geom_point()

anova(mod.l11)

#Local12 - C. Mourão (2)
mod.l12 <- aov(Grain_yield ~ Block + Treat,
               data = dados,
               subset = c(Name_city == "C. Mourão (2)"))

shapiro.test(rstandard(mod.l12))

lmtest::bptest(mod.l12)

ggplot(subset(dados, Name_city == "C. Mourão (2)"),
       aes(x = Treat,
           y = rstandard(mod.l12))) +
  geom_point()

anova(mod.l12)

#Local13 - N. S. das Dores ($)
mod.l13 <- aov(Grain_yield ~ Block + Treat,
               data = dados,
               subset = c(Name_city == "N. S. das Dores ($)"))

shapiro.test(rstandard(mod.l13))

lmtest::bptest(mod.l13)

ggplot(subset(dados, Name_city == "N. S. das Dores ($)"),
       aes(x = Treat,
           y = rstandard(mod.l13))) +
  geom_point()

anova(mod.l13)

#Local14 - N. S. das Dores
mod.l14 <- aov(Grain_yield ~ Block + Treat,
               data = dados,
               subset = c(Name_city == "N. S. das Dores"))

shapiro.test(rstandard(mod.l14))

lmtest::bptest(mod.l14)

ggplot(subset(dados, Name_city == "N. S. das Dores"),
       aes(x = Treat,
           y = rstandard(mod.l14))) +
  geom_point()

anova(mod.l14)

#Local15 - Janaúba
mod.l15 <- aov(Grain_yield ~ Block + Treat,
               data = dados,
               subset = c(Name_city == "Janaúba"))

shapiro.test(rstandard(mod.l15))

lmtest::bptest(mod.l15)

ggplot(subset(dados, Name_city == "Janaúba"),
       aes(x = Treat,
           y = rstandard(mod.l15))) +
  geom_point()

anova(mod.l15)

# Análise Conjunta

# Razão entre os quadrados médios dos resíduos

(QMResiduo1<- anova(mod.l1)$"Mean Sq"[3])
(QMResiduo2<- anova(mod.l2)$"Mean Sq"[3])
(QMResiduo3<- anova(mod.l3)$"Mean Sq"[3])
(QMResiduo4<- anova(mod.l4)$"Mean Sq"[3])
(QMResiduo5<- anova(mod.l5)$"Mean Sq"[3])
(QMResiduo6<- anova(mod.l6)$"Mean Sq"[3])
(QMResiduo7<- anova(mod.l7)$"Mean Sq"[3])
(QMResiduo8<- anova(mod.l8)$"Mean Sq"[3])
(QMResiduo9<- anova(mod.l9)$"Mean Sq"[3])
(QMResiduo10<- anova(mod.l10)$"Mean Sq"[3])
(QMResiduo11<- anova(mod.l11)$"Mean Sq"[3])
(QMResiduo12<- anova(mod.l12)$"Mean Sq"[3])
(QMResiduo13<- anova(mod.l13)$"Mean Sq"[3])
(QMResiduo14<- anova(mod.l14)$"Mean Sq"[3])
(QMResiduo15<- anova(mod.l15)$"Mean Sq"[3])

QMResiduo<- c(QMResiduo1, QMResiduo2, QMResiduo3, QMResiduo4, QMResiduo5, 
              QMResiduo6, QMResiduo7, QMResiduo8, QMResiduo9, QMResiduo10, 
              QMResiduo11, QMResiduo12, QMResiduo13, QMResiduo14, QMResiduo15)

(max(QMResiduo) / min(QMResiduo))

# ANOVA 
mod.conj <- aov(Grain_yield ~ Name_city + Name_city:Block+
                  Treat + Name_city:Treat, 
                data=dados)
anova(mod.conj)

## Análise exploratória (descritiva)
library(ggplot2)
ggplot(data=dados,
       aes(x = Treat,
           y = Grain_yield,
           color = Name_city,
           group = Name_city)) + 
  geom_point(stat = "summary",
             fun = "mean") +
  geom_line(stat = "summary",
            fun = "mean") +
  xlab("Treatment") +
  ylab("Grain_yield") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_blank())

# Como o efeito da interação entre Locais e Tratamentos foi significativo,
# vamos avaliar o efeito de Tratamentos dentro de cada um dos Locais.

## Efeito de Tratamentos dentro de cada Local

local.m <- model.matrix( ~ Name_city - 1, 
                         dados)
colnames(local.m)

mod.tratd.local<- aov(Grain_yield ~ Name_city + Name_city:Block + 
                        local.m[, "Name_cityPlanaltina"]:Treat + # efeito de Tratamento dentro do Local L1
                        local.m[, "Name_citySinop"]:Treat + 
                        local.m[, "Name_cityC. Mourão"]:Treat +
                        local.m[, "Name_cityLondrina"]:Treat +
                        local.m[, "Name_city7 Lagoas ($)"]:Treat +
                        local.m[, "Name_cityGoiânia"]:Treat +
                        local.m[, "Name_cityLondrina (2)"]:Treat +
                        local.m[, "Name_cityDourados"]:Treat +
                        local.m[, "Name_cityS. Rai. das Mangabeiras"]:Treat +
                        local.m[, "Name_cityVilhena"]:Treat +
                        local.m[, "Name_citySinop (2)"]:Treat +
                        local.m[, "Name_cityC. Mourão (2)"]:Treat +
                        local.m[, "Name_cityN. S. das Dores"]:Treat +
                        local.m[, "Name_cityN. S. das Dores ($)"]:Treat +
                        local.m[, "Name_cityJanaúba"]:Treat,
                        
                      data=dados)

anova(mod.tratd.local)

## Vilhena e C.Mourao(2) nao foram significativo para os tratamentos -> trat nao diferem dentro dos respectivos locais  

## Comparações múltiplas 
library(agricolae)

#Media das tratamentos dentro de cada local

(tukey.treatd.L1 <- with(subset(dados, Name_city == "Planaltina"),
                        HSD.test(Grain_yield, 
                                 Treat,
                                 45,
                                 3.0)))

(tukey.treatd.L2 <- with(subset(dados, Name_city == "Sinop"),
                         HSD.test(Grain_yield, 
                                  Treat,
                                  45,
                                  3.0)))

(tukey.treatd.L3 <- with(subset(dados, Name_city == "C. Mourão"),
                         HSD.test(Grain_yield, 
                                  Treat,
                                  45,
                                  3.0)))

(tukey.treatd.L4 <- with(subset(dados, Name_city == "Londrina"),
                         HSD.test(Grain_yield, 
                                  Treat,
                                  45,
                                  3.0)))

(tukey.treatd.L5 <- with(subset(dados, Name_city == "7 Lagoas ($)"),
                         HSD.test(Grain_yield,
                                  Treat,
                                  45,
                                  3.0)))

(tukey.treatd.L6 <- with(subset(dados, Name_city == "Goiânia"),
                         HSD.test(Grain_yield, 
                                  Treat,
                                  45,
                                  3.0)))

(tukey.treatd.L7 <- with(subset(dados, Name_city == "Londrina (2)"),
                         HSD.test(Grain_yield, 
                                  Treat,
                                  45,
                                  3.0)))

(tukey.treatd.L8 <- with(subset(dados, Name_city == "Dourados"),
                         HSD.test(Grain_yield, 
                                  Treat,
                                  45,
                                  3.0)))

(tukey.treatd.L9 <- with(subset(dados, Name_city == "S. Rai. das Mangabeiras"),
                         HSD.test(Grain_yield, 
                                  Treat,
                                  45,
                                  3.0)))

(tukey.treatd.L10 <- with(subset(dados, Name_city == "Vilhena"),
                         HSD.test(Grain_yield, 
                                  Treat,
                                  45,
                                  3.0)))

(tukey.treatd.L11 <- with(subset(dados, Name_city == "Sinop (2)"),
                          HSD.test(Grain_yield, 
                                   Treat,
                                   45,
                                   3.0)))

(tukey.treatd.L12 <- with(subset(dados, Name_city == "C. Mourão (2)"),
                          HSD.test(Grain_yield, 
                                   Treat,
                                   45,
                                   3.0)))

(tukey.treatd.L13 <- with(subset(dados, Name_city == "N. S. das Dores"),
                          HSD.test(Grain_yield, 
                                   Treat,
                                   45,
                                   3.0)))

(tukey.treatd.L14 <- with(subset(dados, Name_city == "N. S. das Dores ($)"),
                          HSD.test(Grain_yield, 
                                   Treat,
                                   45,
                                   3.0)))

(tukey.treatd.L15 <- with(subset(dados, Name_city == "Janaúba"),
                          HSD.test(Grain_yield, 
                                   Treat,
                                   45,
                                   3.0)))




tukey.treatd.L1$groups$Treat <- rownames(tukey.treatd.L1$groups)
tukey.treatd.L1$groups$Name_city <- "Planaltina"

tukey.treatd.L2$groups$Treat <- rownames(tukey.treatd.L2$groups)
tukey.treatd.L2$groups$Name_city <- "Sinop"

tukey.treatd.L3$groups$Treat <- rownames(tukey.treatd.L3$groups)
tukey.treatd.L3$groups$Name_city <- "C. Mourão"

tukey.treatd.L4$groups$Treat <- rownames(tukey.treatd.L4$groups)
tukey.treatd.L4$groups$Name_city <- "Londrina"

tukey.treatd.L5$groups$Treat <- rownames(tukey.treatd.L5$groups)
tukey.treatd.L5$groups$Name_city <- "7 Lagoas ($)"

tukey.treatd.L6$groups$Treat <- rownames(tukey.treatd.L6$groups)
tukey.treatd.L6$groups$Name_city <- "Goiânia"

tukey.treatd.L7$groups$Treat <- rownames(tukey.treatd.L7$groups)
tukey.treatd.L7$groups$Name_city <- "Londrina (2)"

tukey.treatd.L8$groups$Treat <- rownames(tukey.treatd.L8$groups)
tukey.treatd.L8$groups$Name_city <- "Dourados"

tukey.treatd.L9$groups$Treat <- rownames(tukey.treatd.L9$groups)
tukey.treatd.L9$groups$Name_city <- "S. Rai. das Mangabeiras"

tukey.treatd.L10$groups$Treat <- rownames(tukey.treatd.L10$groups)
tukey.treatd.L10$groups$Name_city <- "Vilhena"

tukey.treatd.L11$groups$Treat <- rownames(tukey.treatd.L11$groups)
tukey.treatd.L11$groups$Name_city <- "Sinop (2)"

tukey.treatd.L12$groups$Treat <- rownames(tukey.treatd.L12$groups)
tukey.treatd.L12$groups$Name_city <- "C. Mourão (2)"

tukey.treatd.L13$groups$Treat <- rownames(tukey.treatd.L13$groups)
tukey.treatd.L13$groups$Name_city <- "N. S. das Dores ($)"

tukey.treatd.L14$groups$Treat <- rownames(tukey.treatd.L14$groups)
tukey.treatd.L14$groups$Name_city <- "N. S. das Dores"

tukey.treatd.L15$groups$Treat <- rownames(tukey.treatd.L15$groups)
tukey.treatd.L15$groups$Name_city <- "Janaúba"

(tukey.treat <- data.frame(rbind(
  tukey.treatd.L1$groups,
  tukey.treatd.L2$groups,
  tukey.treatd.L3$groups,
  tukey.treatd.L4$groups,
  tukey.treatd.L5$groups,
  tukey.treatd.L6$groups,
  tukey.treatd.L7$groups,
  tukey.treatd.L8$groups,
  tukey.treatd.L9$groups,
  tukey.treatd.L10$groups,
  tukey.treatd.L11$groups,
  tukey.treatd.L12$groups,
  tukey.treatd.L13$groups,
  tukey.treatd.L14$groups,
  tukey.treatd.L15$groups
)))


#plots individuais
ggplot(tukey.treatd.L1$groups,
       aes(x = Treat,
           y = Grain_yield,
           label = groups,
           fill = Treat)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(x = Treat,
                    ymin = Grain_yield - tukey.treatd.L1$statistics$MSD/2,
                    ymax = Grain_yield + tukey.treatd.L1$statistics$MSD/2)) +
  facet_grid(~ Name_city) +
  geom_text(aes(x = Treat,
                y = Grain_yield + tukey.treatd.L1$statistics$MSD/2 + 1)) +
  xlab("Tratamentos") +
  ylab("Rendimento de Graos")

#plot para todos juntos
install.packages("GGally")
library(GGally)

ggpairs(tukey.treat, columns= 1:4,
       aes(x = Treat,
           y = Grain_yield,
           label = groups,
           fill = Treat)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(x = Treat,
                    ymin = Grain_yield - tukey.treatd.L1$statistics$MSD/2,
                    ymax = Grain_yield + tukey.treatd.L1$statistics$MSD/2)) +
  facet_grid(~ Name_city) +
  geom_text(aes(x = Treat,
                y = Grain_yield + tukey.treatd.L1$statistics$MSD/2 + 1)) +
  theme(legend.position = "button")
  xlab("Tratamentos") +
  ylab("Rendimento de Graos")

