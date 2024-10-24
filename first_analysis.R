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
          
