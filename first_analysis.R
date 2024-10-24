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
          facet_wrap(~Name_city) +
          xlab("Tratamentos") +
          ylab("Rendimento de Grao (t/ha)") +
          theme_bw() +
          theme(legend.position = "bottom",
                legend.direction = "horizontal",
                legend.title = element_blank())

          
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
          
          (max(QMResiduo)/min(QMResiduo))
          
         # ANOVA 
          mod.conj <- aov(Grain_yield ~ Name_city + Name_city:Block+
                            Treat + Name_city:Treat, 
                          data=dados)
          anova(mod.conj)
          
        ## Análise exploratória (descritiva)

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
          
          