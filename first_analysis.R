# Análise conjunta para grupos de experimentos casualizados em blocos
library(readxl)

dados <- read_excel("first analysis.xlsx")
str(dados)

library(dplyr)
dados <- dados %>%
  mutate(Block = as.character(Block), 
         Genotype = as.character(Genotype), 
         Environment = as.character(Environment),
         Grain_yield = as.numeric(Grain_yield, na.rm = TRUE),
         Season = as.character(Season))
str(dados)
colnames(dados)

library(ggplot2)

# Distribution per environment ----
# nao precisa ser adicionado ao app

ggplot(dados, aes(x = Genotype, y = Grain_yield, color = Genotype)) + 
  geom_point() +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    legend.position = "none",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),       # Reduz o tamanho do texto da legenda
    legend.key.size = unit(0.3, "cm")
  ) +   
  guides(color = guide_legend(nrow = 1)) +# Organiza a legenda em uma única linha
  facet_wrap(~Environment) +
  labs(x = "Genotype", y = "Grain Yield (t/ha)")



# Interaction plots ----
# sera adicionado ao app

library(tidyr)
# GEI
GEI <- dados %>% 
  group_by(Environment, Genotype) %>% 
  summarise(Grain_yield = mean(Grain_yield, na.rm = TRUE)) %>% 
  drop_na() #remove linhas com valores ausentes em um df
  #todas as linhas que compartilham o mesmo Environment e Genotype serão tratadas juntas.

# Ordering
index = order(GEI$Grain_yield, decreasing = FALSE)#organiza os valores de rendimentos de graos na forma crescente
GEI = GEI[index,]

# Plot 
(p_E <- GEI %>% ggplot(aes(Environment, Grain_yield)) +
    geom_line(linewidth = 0.8, aes(group = Genotype, color = Genotype, 
                                   alpha = ifelse(Genotype %in% c('1', '10', '64', '24', '44', '7'), 1, 0.7))) +
    labs(title = "Interaction between Genotype and Environment",
         x = "Environment",
         y = "Grain Yield (t/ha)") +
    guides(color = guide_legend(title = "Genotype", ncol = 1), alpha = "none") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust=1)) +
    scale_color_manual(values = c("#003350", "#ff6441", "#cc662f", "#2F95CC", "#1F6650", '#4A7F99'),
                       limits = c('1', '10', '64', '24', '44', '7'),
                       breaks = c('1', '10', '64', '24', '44', '7')))



# GEI - outra forma de abordar o grafico feito pela vitoria
library(ggplot2)
ggplot(data=dados,
       aes(x = Environment, 
           y = Grain_yield,
           color = Genotype,
           group = Genotype,
           alpha = ifelse(Genotype %in% c(), 1, 0.7))) + 
  geom_point(stat = "summary",
             fun = "mean") +
  geom_line(linewidth = 0.8,
            stat = "summary",
            fun = "mean") +
  labs(title = "Interaction between Genotype and Environment",
       x = "Environment",
       y = "Grain Yield (t/ha)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        legend.position = "none")+
  guides(color = guide_legend(title = "Genotype", ncol = 2, alpha = "none"))



# Interacao Genotipo vs Season 
# nao precisa colocar no app
library(ggplot2)
ggplot(data=dados,
       aes(x = Season,
           y = Grain_yield,
           color = Environment,
           group = Environment)) + 
  geom_point(stat = "summary",
             fun = "mean") +
  geom_line(stat = "summary",
            fun = "mean") +
  xlab("Season") +
  ylab("Grain yield (t/ha)") +
  theme_bw() +
  theme(legend.position = "botton",
        legend.direction = "horizontal",
        legend.title = element_blank())



# Boxplots ----
# pode ser colocado como analise exploratoria
ggplot(dados, aes(x = Environment, y = Grain_yield)) + 
  geom_boxplot(fill = "#cc662f",
               colour = "black",
               alpha = 0.5,
               outlier.colour = "#003350") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  scale_color_manual() #essa linha faz com que possa escolher as cores manualmente 


#ANOVA - grupo de experimentos com arranjo fatorial
library(lmtest)

mod <- aov(Grain_yield ~ Environment + Environment/Block + Genotype + Season + Genotype:Season + 
             Environment:Genotype + Environment:Season + Environment:Genotype:Season,
           data=dados)

anova(mod)



#Analises Individuais para Cada Local ----
# nao precisa ser colocado no app
library(lmtest)
library(dplyr)

# Local 1 - Planaltina
mod.l1<- aov(Grain_yield ~ Block + Genotype + Season + Genotype:Season, 
             data=dados, 
             subset = c(Environment=="Planaltina"))

shapiro.test(rstandard(mod.l1))

lmtest::bptest(mod.l1) #studentized Breusch-Pagan test

anova(mod.l1)

      #grafico para verificar homogeneidade de variancias 
ggplot(dados %>% filter(Environment == "Planaltina"),
       aes(x = Genotype, 
           y = rstudent(mod.l1), 
           color = Genotype)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Tratamento", y = "Resíduos Padronizados", title = "Resíduos vs Tratamento") +
  theme(legend.position = "none")  


# Local 2 - Sinop 01
mod.l2<- aov(Grain_yield ~ Block + Genotype + Season + Genotype:Season, 
             data=dados, 
             subset = c(Environment=="Sinop 01"))

shapiro.test(rstandard(mod.l2))

lmtest::bptest(mod.l2) #studentized Breusch-Pagan test

anova(mod.l2)

      #grafico para verificar homogeneidade de variancias 
ggplot(dados %>% filter(Environment == "Sinop 01"),
       aes(x = Genotype, 
           y = rstudent(mod.l2), 
           color = Genotype)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Tratamento", y = "Resíduos Padronizados", title = "Resíduos vs Tratamento") +
  theme(legend.position = "none") 

#Local 3 - C. Mourão 02
mod.l3<- aov(Grain_yield ~ Block + Genotype + Season + Genotype:Season, 
             data=dados, 
             subset = c(Environment=="C. Mourão 02"))

shapiro.test(rstandard(mod.l3))

lmtest::bptest(mod.l3) #studentized Breusch-Pagan test

anova(mod.l3)

      #grafico para verificar homogeneidade de variancias 
ggplot(dados %>% filter(Environment == "C. Mourão 02"),
       aes(x = Genotype, 
           y = rstudent(mod.l3), 
           color = Genotype)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Tratamento", y = "Resíduos Padronizados", title = "Resíduos vs Tratamento") +
  theme(legend.position = "none") 

# Local 4 - Londrina
mod.l4<- aov(Grain_yield ~ Block + Genotype + Season + Genotype:Season, 
             data=dados, 
             subset = c(Environment=="Londrina"))

shapiro.test(rstandard(mod.l4))

lmtest::bptest(mod.l4) #studentized Breusch-Pagan test

anova(mod.l4)

      #grafico para verificar homogeneidade de variancias 
ggplot(dados %>% filter(Environment == "Londrina"),
       aes(x = Genotype, 
           y = rstudent(mod.l4), 
           color = Genotype)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Tratamento", y = "Resíduos Padronizados", title = "Resíduos vs Tratamento") +
  theme(legend.position = "none") 

#Local 5 - 7 Lagoas 
mod.l5<- aov(Grain_yield ~ Block + Genotype + Season + Genotype:Season, 
             data=dados, 
             subset = c(Environment=="7 Lagoas"))

shapiro.test(rstandard(mod.l5))

lmtest::bptest(mod.l5) #studentized Breusch-Pagan test

anova(mod.l5)

      #grafico para verificar homogeneidade de variancias 
ggplot(dados %>% filter(Environment == "7 Lagoas"),
       aes(x = Genotype, 
           y = rstudent(mod.l3), 
           color = Genotype)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Tratamento", y = "Resíduos Padronizados", title = "Resíduos vs Tratamento") +
  theme(legend.position = "none") 

#Local 6 - Goiânia
mod.l6<- aov(Grain_yield ~ Block + Genotype + Season + Genotype:Season, 
             data=dados, 
             subset = c(Environment=="Goiânia"))

shapiro.test(rstandard(mod.l6))

lmtest::bptest(mod.l6) #studentized Breusch-Pagan test

anova(mod.l6)

      #grafico para verificar homogeneidade de variancias 
ggplot(dados %>% filter(Environment == "Goiânia"),
       aes(x = Genotype, 
           y = rstudent(mod.l6), 
           color = Genotype)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Tratamento", y = "Resíduos Padronizados", title = "Resíduos vs Tratamento") +
  theme(legend.position = "none") 

#Local 7 - Dourados
mod.l7<- aov(Grain_yield ~ Block + Genotype + Season + Genotype:Season, 
             data=dados, 
             subset = c(Environment=="Dourados"))

shapiro.test(rstandard(mod.l7))

lmtest::bptest(mod.l7) #studentized Breusch-Pagan test

anova(mod.l7)

      #grafico para verificar homogeneidade de variancias 
ggplot(dados %>% filter(Environment == "Dourados"),
       aes(x = Genotype, 
           y = rstudent(mod.l7), 
           color = Genotype)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Tratamento", y = "Resíduos Padronizados", title = "Resíduos vs Tratamento") +
  theme(legend.position = "none")

#Local 8 - S. Rai. das Mangabeiras
mod.l8<- aov(Grain_yield ~ Block + Genotype + Season + Genotype:Season, 
             data=dados, 
             subset = c(Environment=="S. Rai. das Mangabeiras"))

shapiro.test(rstandard(mod.l8))

lmtest::bptest(mod.l8) #studentized Breusch-Pagan test

anova(mod.l8)

      #grafico para verificar homogeneidade de variancias 
ggplot(dados %>% filter(Environment == "S. Rai. das Mangabeiras"),
       aes(x = Genotype, 
           y = rstudent(mod.l8), 
           color = Genotype)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Tratamento", y = "Resíduos Padronizados", title = "Resíduos vs Tratamento") +
  theme(legend.position = "none")

#Local 9 - Vilhena
mod.l9<- aov(Grain_yield ~ Block + Genotype + Season + Genotype:Season, 
             data=dados, 
             subset = c(Environment=="Vilhena"))

shapiro.test(rstandard(mod.l9))

lmtest::bptest(mod.l9) #studentized Breusch-Pagan test

anova(mod.l9)

      #grafico para verificar homogeneidade de variancias 
ggplot(dados %>% filter(Environment == "Vilhena"),
       aes(x = Genotype, 
           y = rstudent(mod.l9), 
           color = Genotype)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Tratamento", y = "Resíduos Padronizados", title = "Resíduos vs Tratamento") +
  theme(legend.position = "none")

#Local 10 - Sinop 02
mod.l10<- aov(Grain_yield ~ Block + Genotype + Season + Genotype:Season, 
             data=dados, 
             subset = c(Environment=="Sinop 02"))

shapiro.test(rstandard(mod.l10))

lmtest::bptest(mod.l10) #studentized Breusch-Pagan test

anova(mod.l10)

      #grafico para verificar homogeneidade de variancias 
ggplot(dados %>% filter(Environment == "Sinop 02"),
       aes(x = Genotype, 
           y = rstudent(mod.l10), 
           color = Genotype)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Tratamento", y = "Resíduos Padronizados", title = "Resíduos vs Tratamento") +
  theme(legend.position = "none")

#Local11 - C. Mourão 02
mod.l11<- aov(Grain_yield ~ Block + Genotype + Season + Genotype:Season, 
             data=dados, 
             subset = c(Environment=="C. Mourão 02"))

shapiro.test(rstandard(mod.l11))

lmtest::bptest(mod.l11) #studentized Breusch-Pagan test

anova(mod.l11)

      #grafico para verificar homogeneidade de variancias 
ggplot(dados %>% filter(Environment == "C. Mourão 02"),
       aes(x = Genotype, 
           y = rstudent(mod.l11), 
           color = Genotype)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Tratamento", y = "Resíduos Padronizados", title = "Resíduos vs Tratamento") +
  theme(legend.position = "none")

#Local12 - N. S. das Dores 01
mod.l12<- aov(Grain_yield ~ Block + Genotype + Season + Genotype:Season, 
             data=dados, 
             subset = c(Environment=="N. S. das Dores 01"))

shapiro.test(rstandard(mod.l12))

lmtest::bptest(mod.l12) #studentized Breusch-Pagan test

anova(mod.l12)

      #grafico para verificar homogeneidade de variancias 
ggplot(dados %>% filter(Environment == "N. S. das Dores 01"),
       aes(x = Genotype, 
           y = rstudent(mod.l12), 
           color = Genotype)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Tratamento", y = "Resíduos Padronizados", title = "Resíduos vs Tratamento") +
  theme(legend.position = "none")

#Local13 - N. S. das Dores 02
mod.l13<- aov(Grain_yield ~ Block + Genotype + Season + Genotype:Season, 
              data=dados, 
              subset = c(Environment=="N. S. das Dores 02"))

shapiro.test(rstandard(mod.l13))

lmtest::bptest(mod.l13) #studentized Breusch-Pagan test

anova(mod.l13)

      #grafico para verificar homogeneidade de variancias 
ggplot(dados %>% filter(Environment == "N. S. das Dores 02"),
       aes(x = Genotype, 
           y = rstudent(mod.l13), 
           color = Genotype)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Tratamento", y = "Resíduos Padronizados", title = "Resíduos vs Tratamento") +
  theme(legend.position = "none")


# Análise Conjunta

# Razão entre os quadrados médios dos resíduos

(QMResiduo1<- anova(mod.l1)$"Mean Sq"[5])
(QMResiduo2<- anova(mod.l2)$"Mean Sq"[5])
(QMResiduo3<- anova(mod.l3)$"Mean Sq"[5])
(QMResiduo4<- anova(mod.l4)$"Mean Sq"[5])
(QMResiduo5<- anova(mod.l5)$"Mean Sq"[5])
(QMResiduo6<- anova(mod.l6)$"Mean Sq"[5])
(QMResiduo7<- anova(mod.l7)$"Mean Sq"[5])
(QMResiduo8<- anova(mod.l8)$"Mean Sq"[5])
(QMResiduo9<- anova(mod.l9)$"Mean Sq"[5])
(QMResiduo10<- anova(mod.l10)$"Mean Sq"[5])
(QMResiduo11<- anova(mod.l11)$"Mean Sq"[5])
(QMResiduo12<- anova(mod.l12)$"Mean Sq"[5])
(QMResiduo13<- anova(mod.l13)$"Mean Sq"[5])


QMResiduo<- c(QMResiduo1, QMResiduo2, QMResiduo3, QMResiduo4, QMResiduo5, 
              QMResiduo6, QMResiduo7, QMResiduo8, QMResiduo9, QMResiduo10, 
              QMResiduo11, QMResiduo12, QMResiduo13)

(max(QMResiduo) / min(QMResiduo))

# ANOVA modelo conjunto - ja colocado acima
library(lmtest)
mod <- aov(Grain_yield ~ Environment + Environment/Block + Genotype + Season + Genotype:Season + 
             Environment:Genotype + Environment:Season + Environment:Genotype:Season,
           data=dados)

anova(mod)

#Como o efeito da interação entre Locais e Tratamentos foi significativo,podemos avaliar o efeito de Tratamentos dentro de cada um dos Locais.



# ------------------------------------------------------------------------------
# Efeito de Tratamentos dentro de cada Local e Comparacoes de Medias- Segundo o Augusto nao sera necessario incluir na analise

local.m <- model.matrix( ~ Environment - 1, 
                         dados)
colnames(local.m)

mod.tratd.local<- aov(Grain_yield ~ Environment + Environment/Block + 
                        local.m[, "EnvironmentPlanaltina"]:Genotype + # efeito de Tratamento dentro do Local L1
                        local.m[, "EnvironmentSinop 01"]:Genotype + 
                        local.m[, "EnvironmentC. Mourão 01"]:Genotype +
                        local.m[, "EnvironmentLondrina"]:Genotype +
                        local.m[, "Environment7 Lagoas"]:Genotype +
                        local.m[, "EnvironmentGoiânia"]:Genotype +
                        local.m[, "EnvironmentDourados"]:Genotype +
                        local.m[, "EnvironmentS. Rai. das Mangabeiras"]:Genotype +
                        local.m[, "EnvironmentVilhena"]:Genotype +
                        local.m[, "EnvironmentSinop 02"]:Genotype +
                        local.m[, "EnvironmentC. Mourão 02"]:Genotype +
                        local.m[, "EnvironmentN. S. das Dores 01"]:Genotype +
                        local.m[, "EnvironmentN. S. das Dores 02"]:Genotype, 
                      data=dados)

anova(mod.tratd.local)

# Duvida em relacao ao modelo acima
# mod <- aov(Grain_yield ~ Environment + Environment/Block + Genotype + Season + Genotype:Season + 
#              Environment:Genotype + Environment:Season + Environment:Genotype:Season,
#            data=dados)


## Comparações múltiplas de medias
library(agricolae)

#Media das tratamentos dentro de cada local - Tukey eh um teste mais concervativo, como nao ha variacoes mto grandes eh normal nao dar tantas diferencas significativas

(tukey.Genotyped.L1 <- with(subset(dados, Environment == "Planaltina"),
                         HSD.test(Grain_yield, 
                                  Genotype,
                                  45,
                                  3.0)))

(tukey.Genotyped.L2 <- with(subset(dados, Environment == "Sinop 01"),
                         HSD.test(Grain_yield, 
                                  Genotype,
                                  45,
                                  3.0)))

(tukey.Genotyped.L3 <- with(subset(dados, Environment == "C. Mourão 01"),
                         HSD.test(Grain_yield, 
                                  Genotype,
                                  45,
                                  3.0)))

(tukey.Genotyped.L4 <- with(subset(dados, Environment == "Londrina"),
                         HSD.test(Grain_yield, 
                                  Genotype,
                                  45,
                                  3.0)))

(tukey.Genotyped.L5 <- with(subset(dados, Environment == "7 Lagoas"),
                         HSD.test(Grain_yield,
                                  Genotype,
                                  45,
                                  3.0)))

(tukey.Genotyped.L6 <- with(subset(dados, Environment == "Goiânia"),
                         HSD.test(Grain_yield, 
                                  Genotype,
                                  45,
                                  3.0)))


(tukey.Genotyped.L7 <- with(subset(dados, Environment == "Dourados"),
                         HSD.test(Grain_yield, 
                                  Genotype,
                                  45,
                                  3.0)))

(tukey.Genotyped.L8 <- with(subset(dados, Environment == "S. Rai. das Mangabeiras"),
                         HSD.test(Grain_yield, 
                                  Genotype,
                                  45,
                                  3.0)))

(tukey.Genotyped.L9 <- with(subset(dados, Environment == "Vilhena"),
                          HSD.test(Grain_yield, 
                                   Genotype,
                                   45,
                                   3.0)))

(tukey.Genotyped.L10 <- with(subset(dados, Environment == "Sinop 02"),
                          HSD.test(Grain_yield, 
                                   Genotype,
                                   45,
                                   3.0)))

(tukey.Genotyped.L11 <- with(subset(dados, Environment == "C. Mourão 02"),
                          HSD.test(Grain_yield, 
                                   Genotype,
                                   45,
                                   3.0)))

(tukey.Genotyped.L12 <- with(subset(dados, Environment == "N. S. das Dores 01"),
                          HSD.test(Grain_yield, 
                                   Genotype,
                                   45,
                                   3.0)))

(tukey.Genotyped.L13 <- with(subset(dados, Environment == "N. S. das Dores 02"),
                          HSD.test(Grain_yield, 
                                   Genotype,
                                   45,
                                   3.0)))



tukey.Genotyped.L1$groups$Genotype <- rownames(tukey.Genotyped.L1$groups)
tukey.Genotyped.L1$groups$Environment <- "Planaltina"

tukey.Genotyped.L2$groups$Genotype <- rownames(tukey.Genotyped.L2$groups)
tukey.Genotyped.L2$groups$Environment <- "Sinop 01"

tukey.Genotyped.L3$groups$Genotype <- rownames(tukey.Genotyped.L3$groups)
tukey.Genotyped.L3$groups$Environment <- "C. Mourão 01"

tukey.Genotyped.L4$groups$Genotype <- rownames(tukey.Genotyped.L4$groups)
tukey.Genotyped.L4$groups$Environment <- "Londrina"

tukey.Genotyped.L5$groups$Genotype <- rownames(tukey.Genotyped.L5$groups)
tukey.Genotyped.L5$groups$Environment <- "7 Lagoas"

tukey.Genotyped.L6$groups$Genotype <- rownames(tukey.Genotyped.L6$groups)
tukey.Genotyped.L6$groups$Environment <- "Goiânia"

tukey.Genotyped.L7$groups$Genotype <- rownames(tukey.Genotyped.L8$groups)
tukey.Genotyped.L7$groups$Environment <- "Dourados"

tukey.Genotyped.L8$groups$Genotype <- rownames(tukey.Genotyped.L9$groups)
tukey.Genotyped.L8$groups$Environment <- "S. Rai. das Mangabeiras"

tukey.Genotyped.L9$groups$Genotype <- rownames(tukey.Genotyped.L10$groups)
tukey.Genotyped.L9$groups$Environment <- "Vilhena"

tukey.Genotyped.L10$groups$Genotype <- rownames(tukey.Genotyped.L11$groups)
tukey.Genotyped.L10$groups$Environment <- "Sinop 02"

tukey.Genotyped.L11$groups$Genotype <- rownames(tukey.Genotyped.L12$groups)
tukey.Genotyped.L11$groups$Environment <- "C. Mourão 02"

tukey.Genotyped.L12$groups$Genotype <- rownames(tukey.Genotyped.L13$groups)
tukey.Genotyped.L12$groups$Environment <- "N. S. das Dores 01"

tukey.Genotyped.L13$groups$Genotype <- rownames(tukey.Genotyped.L14$groups)
tukey.Genotyped.L13$groups$Environment <- "N. S. das Dores 02"



(tukey.Genotype <- data.frame(rbind(
  tukey.Genotyped.L1$groups,
  tukey.Genotyped.L2$groups,
  tukey.Genotyped.L3$groups,
  tukey.Genotyped.L4$groups,
  tukey.Genotyped.L5$groups,
  tukey.Genotyped.L6$groups,
  tukey.Genotyped.L7$groups,
  tukey.Genotyped.L8$groups,
  tukey.Genotyped.L9$groups,
  tukey.Genotyped.L10$groups,
  tukey.Genotyped.L11$groups,
  tukey.Genotyped.L12$groups,
  tukey.Genotyped.L13$groups
)))


#plots individuais
ggplot(tukey.Genotyped.L1$groups,
       aes(x = Genotype,
           y = Grain_yield,
           label = groups,
           fill = Genotype)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(x = Genotype,
                    ymin = Grain_yield - tukey.Genotyped.L1$statistics$MSD/2,
                    ymax = Grain_yield + tukey.Genotyped.L1$statistics$MSD/2)) +
  facet_grid(~ Name_city) +
  geom_text(aes(x = Genotype,
                y = Grain_yield + tukey.Genotyped.L1$statistics$MSD/2 + 1)) +
  xlab("Tratamentos") +
  ylab("Rendimento de Graos")

#plot para todos juntos
install.packages("GGally")
library(GGally)

ggpairs(tukey.Genotype, columns= 1:4,
        aes(x = Genotype,
            y = Grain_yield,
            label = groups,
            fill = Genotype)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(x = Genotype,
                    ymin = Grain_yield - tukey.Genotyped.L1$statistics$MSD/2,
                    ymax = Grain_yield + tukey.Genotyped.L1$statistics$MSD/2)) +
  facet_grid(~ Environment) +
  geom_text(aes(x = Genotype,
                y = Grain_yield + tukey.Genotyped.L1$statistics$MSD/2 + 1)) +
  theme(legend.position = "button")
xlab("Tratamentos") +
  ylab("Rendimento de Graos")

