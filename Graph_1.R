###########################
#Autor: João Lucas
#Data: 08/05/2022
###########################
#Mestrado profissional em Economia e Finanças
#Curso: Introdução a Ciencia de dados
#instituição: Fundação Getúlio Vargas
###########################


###########################
library(ggplot2)
library(gganimate)
library(magrittr)
library(gifski)
library(av)


######## Gráfico reclamações vs quantidade de clientes ########

grafico_1 <- ggplot(hist_reclam_2,
                    aes(size= I(4), 
                        shape  = Tipo, 
                        color = Categoria_2, 
                        x = log(Quantidade.total.de.clientes...CCS.e.SCR), 
                        y = log(Quantidade.de.reclamações.reguladas.procedentes)))+
  geom_point(alpha = 0.6)+
  theme_classic()+
  theme(text = element_text(size = 17))+
  theme(legend.text = element_text(size = 13))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  guides(shape = guide_legend(override.aes = list(size=5)))+
  scale_y_continuous(breaks = seq(-2, 20, 2))+
  #theme(legend.position = "bottom", legend.text = element_text(size = 10))+
  labs(
    x = "Log(Quantidade de clientes)",
    y = "Log(Quantidade de reclamações)",
    shape = "Categoria",
    color = "Tamanho Instituição",
    title = "Reclamação contra bancos e outras instituições financeiras ao Bacen",
    #subtitle = "Reclamações vs Quantidade de clientes (2021)",
    caption = "Fonte: BCB\nElaboração própria")

grafico_1 <- grafico_1 + transition_time(Ano)+
  labs(subtitle = "Reclamações vs Quantidade de clientes ({as.integer(frame_time)})")


p <- animate(grafico_1, fps = 6, height = 500, width = 900, end_pause = 50, renderer = av_renderer())
anim_save(filename = "grafico_1.MP4", p)


p <- animate(grafico_1, fps = 6, height = 500, width = 900, end_pause = 50)
anim_save(filename = "grafico_1.gif", p)

