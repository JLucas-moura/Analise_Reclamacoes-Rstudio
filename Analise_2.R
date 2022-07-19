###########################
#Autor: João Lucas
#Data: 11/05/2022
###########################
#Mestrado profissional em Economia e Finanças
#Curso: Introdução a Ciencia de dados
#instituição: Fundação Getúlio Vargas
###########################

library(scales)
library(tidyverse)
library(viridis)
library(zoo)
library(ggplot2)
library(patchwork)

# DATA ANALYSIS ----

# Total de clientes ----

## Correlação entre tamanho da Empresa e Quantidade de clientes
# ponto interessante para mostrar no inicio da analise



## Tratando dados para o gráfico
hist_reclam_2 %>% 
  mutate(Trimestre = as.yearqtr(as.Date(paste0(Ano,'-', ifelse(Trimestre == 2, 4,
                                                               ifelse(Trimestre == 3, 7,
                                                                      ifelse(Trimestre == 4, 10, 1))),
                                               '-', 01)))) %>% 
  group_by(Trimestre) %>% 
  dplyr::select(c(Trimestre, `Instituição.financeira`, `Quantidade.total.de.clientes...CCS.e.SCR`)) %>% 
  group_by(Trimestre) %>% 
  top_n(5, `Quantidade.total.de.clientes...CCS.e.SCR`) -> data_plot_hist_2_total_clientes

colnames(data_plot_hist_2_total_clientes) <-  c('date', 'instituicao', 'quantidadeclientes')
## Executando o Gráfico
data_plot_hist_2_total_clientes %>% 
  ggplot(aes(x = date, y = quantidadeclientes, color = instituicao)) +
  geom_line(size = 1) +
  geom_point(size = 5, alpha = .5) +
  labs(x = 'Periodo', y = 'Quantidade Total de Clientes',
       title = 'Quantidade total de Clientes',
       subtitle = 'Referente às 5 maiores instituições financeiras (por Trimestre)') +
  scale_y_continuous(labels = comma) +
  scale_color_viridis_d() +
  scale_x_yearqtr(format = "%YT%q",
                  breaks = seq(from = min(data_plot_hist_2_total_clientes$date), 
                               to = max(data_plot_hist_2_total_clientes$date), by = 0.25)) +
  theme_minimal() +
  theme(legend.position="bottom",
        legend.title = element_blank(),
        plot.title = element_text(size = 24, color = 'gray35'),
        plot.subtitle = element_text(size = 16, color = 'gray45'),
        axis.text.x = element_text(angle = 45, hjust = 1))

#ggsave("quantidadetotal.png", width = 1737, height = 738, units = "px", dpi = 90)


# Total reclamações ----
## Como os 5 maiores bancos são BB, Bradesco, Caixa, Itau e santander,
## vamos trabalhar com esses bancos
## Tratando dados para o gráfico total de reclamações
top5Bancos <- c('BRADESCO (conglomerado)', 'BB (conglomerado)',
                'CAIXA ECONÔMICA FEDERAL (conglomerado)',
                'ITAU (conglomerado)', 'SANTANDER (conglomerado)')

filter(hist_reclam_2,
       Instituição.financeira %in% top5Bancos) %>% 
  mutate(Trimestre = as.yearqtr(as.Date(paste0(Ano,'-', ifelse(Trimestre == 2, 4,
                                                               ifelse(Trimestre == 3, 7,
                                                                      ifelse(Trimestre == 4, 10, 1))),
                                               '-', 01)))) %>% 
  group_by(Trimestre) %>% 
  dplyr::select(c(Trimestre, `Instituição.financeira`, `Quantidade.total.de.reclamações`, `Quantidade.total.de.reclamações`,`Quantidade.total.de.clientes...CCS.e.SCR`)) %>% 
  group_by(Trimestre) %>% 
  top_n(5, `Quantidade.total.de.reclamações`) -> data_plot_hist_2_reclamacoes

colnames(data_plot_hist_2_reclamacoes) <-  c('date', 'instituicao', 'reclamacoes', 'quanidadeclientes')
## Executando o gráfico total de reclamações
data_plot_hist_2_reclamacoes %>% 
  ggplot(aes(x = date, y = reclamacoes, color = instituicao)) +
  geom_line(size = 1) +
  geom_point(size = 5, alpha = .5) +
  labs(x = 'Periodo', y = 'Quantidade Total de Reclamações',
       title = 'Quantidade Total de Reclamações',
       subtitle = 'Referente às 5 maiores instituições financeiras (por Trimestre)') +
  scale_y_continuous(labels = comma) +
  scale_color_viridis_d() +
  scale_x_yearqtr(format = "%YT%q",
                  breaks = seq(from = min(data_plot_hist_2_total_clientes$date), 
                               to = max(data_plot_hist_2_total_clientes$date), by = 0.25)) +
  theme_minimal() +
  theme(legend.position="bottom",
        legend.title = element_blank(),
        plot.title = element_text(size = 24, color = 'gray35'),
        plot.subtitle = element_text(size = 16, color = 'gray45'),
        axis.text.x = element_text(angle = 45, hjust = 1))

#ggsave("reclamacoes.png", width = 1737, height = 738, units = "px", dpi = 90)


# Reclamações/numero de clientes ----
## Tratando dados para o gráfico recl/num de clientes
filter(hist_reclam_2,
       Instituição.financeira %in% top5Bancos) %>%
  mutate(Trimestre = as.yearqtr(as.Date(paste0(Ano,'-', ifelse(Trimestre == 2, 4,
                                                               ifelse(Trimestre == 3, 7,
                                                                      ifelse(Trimestre == 4, 10, 1))),
                                               '-', 01))),
         indice = Quantidade.total.de.reclamações/Quantidade.total.de.clientes...CCS.e.SCR) %>% 
  group_by(Trimestre) %>% 
  dplyr::select(c(`Trimestre`, `Instituição.financeira`, `Quantidade.total.de.reclamações`,`Quantidade.total.de.clientes...CCS.e.SCR`, `indice`)) %>%
  group_by(Trimestre) %>% 
  top_n(5, Quantidade.total.de.clientes...CCS.e.SCR) -> data_plot_hist_2_indice

colnames(data_plot_hist_2_indice) <-  c('date', 'instituicao', 'reclamacoes', 'quantidadeclientes', 'indice')
## Executando o gráfico reclam/num de clientes
data_plot_hist_2_indice %>% 
  ggplot(aes(x = date, y = indice, color = instituicao)) +
  geom_line(size = 1) +
  geom_point( size = 5, alpha = .5) +
  labs(x = 'Periodo', y = 'Reclamações por clientes',
       title = 'Reclamações por número de clientes',
       subtitle = 'Referente às 8 maiores instituições financeiras (por Trimestre)') +
  scale_y_continuous(labels = percent) +
  scale_color_viridis_d() +
  scale_x_yearqtr(format = "%YT%q",
                  breaks = seq(from = min(data_plot_hist_2_total_clientes$date), 
                               to = max(data_plot_hist_2_total_clientes$date), by = 0.25)) +
  theme_minimal() +
  theme(legend.position="bottom",
        legend.title = element_blank(),
        plot.title = element_text(size = 24, color = 'gray35'),
        plot.subtitle = element_text(size = 16, color = 'gray45'),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Boxplot do numero de reclamações por cliente ----
## Bradesco
filter(hist_reclam_2,
       Instituição.financeira == 'BRADESCO (conglomerado)') %>%
  ggplot(aes(x = Ano,
             y = Quantidade.total.de.reclamações/Quantidade.total.de.clientes...CCS.e.SCR,
             group = Ano))+
             #fill = Tipo))+
  geom_boxplot() +
  labs(x = '', y = '',
       title = 'Bradesco')+
       #subtitle = '')+
  scale_y_continuous(labels = percent)+
  theme_bw()+
  theme(axis.text.x = element_text(hjust = 1, angle = 50)) -> box_1

# Banco do Brasil
filter(hist_reclam_2,
       Instituição.financeira == 'BB (conglomerado)') %>%
  ggplot(aes(x = Ano,
             y = Quantidade.total.de.reclamações/Quantidade.total.de.clientes...CCS.e.SCR,
             group = Ano))+
  #fill = Tipo))+
  geom_boxplot() +
  labs(x = '', y = '',
       title = 'Banco do Brasil')+
       #subtitle = '')+
  scale_y_continuous(labels = percent)+
  theme_bw()+
  theme(axis.text.x = element_text(hjust = 1, angle = 50)) -> box_2

## Caixa economica federal
filter(hist_reclam_2,
                         Instituição.financeira == 'CAIXA ECONÔMICA FEDERAL (conglomerado)') %>%
  ggplot(aes(x = Ano,
             y = Quantidade.total.de.reclamações/Quantidade.total.de.clientes...CCS.e.SCR,
             group = Ano))+
  #fill = Tipo))+
  geom_boxplot() +
  labs(x = '', y = '',
       title = 'Caixa Econômica Federal')+
  #subtitle = '')+
  scale_y_continuous(labels = percent)+
  theme_bw()+
  theme(axis.text.x = element_text(hjust = 1, angle = 50)) -> box_3

## ITAU
filter(hist_reclam_2,
                Instituição.financeira == 'ITAU (conglomerado)') %>%
  ggplot(aes(x = Ano,
             y = Quantidade.total.de.reclamações/Quantidade.total.de.clientes...CCS.e.SCR,
             group = Ano))+
  geom_boxplot() +
  labs(x = '', y = '',
       title = 'Itau')+
  scale_y_continuous(labels = percent)+
  theme_bw()+
  theme(axis.text.x = element_text(hjust = 1, angle = 50)) -> box_4

## Santander
filter(hist_reclam_2,
       Instituição.financeira == 'SANTANDER (conglomerado)') %>%
  ggplot(aes(x = Ano,
             y = Quantidade.total.de.reclamações/Quantidade.total.de.clientes...CCS.e.SCR,
             group = Ano))+
  geom_boxplot() +
  labs(x = '', y = '',
       title = 'Santander')+
  scale_y_continuous(labels = percent)+
  theme_bw()+
  theme(axis.text.x = element_text(hjust = 1, angle = 50)) -> box_5

## Geral
filter(hist_reclam_2,
       Instituição.financeira %in% top5Bancos) %>%
  ggplot(aes(x = Ano,
             y = Quantidade.total.de.reclamações/Quantidade.total.de.clientes...CCS.e.SCR,
             group = Ano))+
  geom_boxplot() +
  labs(x = '', y = '',
       title = 'Top 5 bancos')+
  scale_y_continuous(labels = percent)+
  theme_bw()+
  theme(axis.text.x = element_text(hjust = 1, angle = 50)) -> box_6

## Juntando todos os box plot
((box_1 + box_2) /
    (box_3 + box_4) /
    (box_5 + box_6))+
  plot_annotation(
  title = "Box Plot - Referente às 5 maiores instituições financeiras",
  subtitle = "Evolução anual do número de reclamações por cliente médio",
  caption = "Fonte: BCB\nElaboração própria") & theme_minimal()


# Maiores Tipos de Reclamações ----
## Tratando dados para o gráfico

## Criando filtro para saber quais os maiores tipos de reclamação por trimestre
## filtrando os maiores bancos
Top5_reclam <- hist_reclam %>%
  filter(Instituição.financeira %in% top5Bancos)
##datas analisadas
datas <- c("2017 Q1", "2017 Q2","2017 Q3","2017 Q4",
           "2018 Q1", "2018 Q2","2018 Q3","2018 Q4",
           "2019 Q1", "2019 Q2","2019 Q3","2019 Q4",
           "2020 Q1", "2020 Q2","2020 Q3","2020 Q4",
           "2021 Q1", "2021 Q2","2021 Q3","2021 Q4")

## somando todos os tipos de reclamações por trimestre
plot_top_reclam <- data.frame()
plot_top_reclam1 <- data.frame()

for (i in datas) {
  plot_top_reclam1 <- filter(Top5_reclam, date_2 == i)
  plot_top_reclam1 <- aggregate(Quantidade.total.de.reclamações ~ Irregularidade, plot_top_reclam1, sum)
  plot_top_reclam1 %>%
    mutate(date = i) -> plot_top_reclam1
  plot_top_reclam <- rbind(plot_top_reclam, plot_top_reclam1)
}

##mudando nome das colunas
colnames(plot_top_reclam) <-  c('Irregularidade', 'quantreclam', 'date')

##agrupando por trimestre  maior tipo dede reclamação
plot_top_reclam %>%
  group_by(date) %>% 
  top_n(1, quantreclam) -> data_plot_top_reclam

## Executando o gráfico do maior tipo de reclamação por período
data_plot_top_reclam %>% 
  ggplot()+
  geom_bar(aes(x = date, y = quantreclam,
               fill = Irregularidade),
           stat="identity",
           width=.2,
           position = "dodge" )+
  labs(x = 'Período', y = 'Quantidade Total de Reclamações',
       title = 'Quantidade Total do Maior Tipo de Reclamação no Período',
       subtitle = 'Referente às 5 maiores instituições financeiras (por Trimestre)') +
  scale_y_continuous(labels = comma) +
  guides(fill = guide_legend(nrow = 7))+
  scale_fill_viridis_d()+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.margin = margin())

## Maiores Reclamações por banco
## BRADESCO
filter(hist_reclam,Instituição.financeira == 'BRADESCO (conglomerado)') %>%
  mutate(Trimestre = as.yearqtr(as.Date(paste0(Ano,'-', ifelse(Trimestre == 2, 4,
                                                               ifelse(Trimestre == 3, 7,
                                                                      ifelse(Trimestre == 4, 10, 1))),
                                               '-', 01)))) %>% 
  group_by(Trimestre) %>% 
  dplyr::select(c(`Trimestre`, `Instituição.financeira`, `Irregularidade`,`Quantidade.total.de.reclamações`)) %>%
  group_by(Trimestre) %>% 
  top_n(1, Quantidade.total.de.reclamações) -> Top_reclam_bra

colnames(Top_reclam_bra) <-  c('date', 'instituicao', 'Irregularidade', 'quantreclam')

## Executando o gráfico
Top_reclam_bra %>% 
  ggplot()+
  geom_bar(aes(x = date, y = quantreclam,
               fill = Irregularidade),
           stat="identity",
           width=.2,
           position = "dodge" )+
  labs(x = 'Período', y = 'Quantidade Total de Reclamações',
       title = 'Quantidade Total do Maior Tipo de Reclamação no Período',
       subtitle = 'Referente ao Banco Bradesco (por Trimestre)') +
  scale_y_continuous(labels = comma) +
  guides(fill = guide_legend(nrow = 9))+
  scale_fill_viridis_d()+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.margin = margin())

## BANCO DO BRASIL
filter(hist_reclam,Instituição.financeira == 'BB (conglomerado)') %>%
  mutate(Trimestre = as.yearqtr(as.Date(paste0(Ano,'-', ifelse(Trimestre == 2, 4,
                                                               ifelse(Trimestre == 3, 7,
                                                                      ifelse(Trimestre == 4, 10, 1))),
                                               '-', 01)))) %>% 
  group_by(Trimestre) %>% 
  dplyr::select(c(`Trimestre`, `Instituição.financeira`, `Irregularidade`,`Quantidade.total.de.reclamações`)) %>%
  group_by(Trimestre) %>% 
  top_n(1, Quantidade.total.de.reclamações) -> Top_reclam_BB

colnames(Top_reclam_BB) <-  c('date', 'instituicao', 'Irregularidade', 'quantreclam')

## Executando o gráfico
Top_reclam_BB %>% 
  ggplot()+
  geom_bar(aes(x = date, y = quantreclam,
               fill = Irregularidade),
           stat="identity",
           width=.2,
           position = "dodge" )+
  labs(x = 'Período', y = 'Quantidade Total de Reclamações',
       title = 'Quantidade Total do Maior Tipo de Reclamação no Período',
       subtitle = 'Referente ao Banco do Brasil (por Trimestre)') +
  scale_y_continuous(labels = comma) +
  guides(fill = guide_legend(nrow = 4))+
  scale_fill_viridis_d()+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.margin = margin())

## CAIXA ECONOMICA
filter(hist_reclam,Instituição.financeira == 'CAIXA ECONÔMICA FEDERAL (conglomerado)') %>%
  mutate(Trimestre = as.yearqtr(as.Date(paste0(Ano,'-', ifelse(Trimestre == 2, 4,
                                                               ifelse(Trimestre == 3, 7,
                                                                      ifelse(Trimestre == 4, 10, 1))),
                                               '-', 01)))) %>% 
  group_by(Trimestre) %>% 
  dplyr::select(c(`Trimestre`, `Instituição.financeira`, `Irregularidade`,`Quantidade.total.de.reclamações`)) %>%
  group_by(Trimestre) %>% 
  top_n(1, Quantidade.total.de.reclamações) -> Top_reclam_caixa

colnames(Top_reclam_caixa) <-  c('date', 'instituicao', 'Irregularidade', 'quantreclam')

## Executando o gráfico
Top_reclam_caixa %>% 
  ggplot()+
  geom_bar(aes(x = date, y = quantreclam,
               fill = Irregularidade),
           stat="identity",
           width=.2,
           position = "dodge" )+
  labs(x = 'Período', y = 'Quantidade Total de Reclamações',
       title = 'Quantidade Total do Maior Tipo de Reclamação no Período',
       subtitle = 'Referente à Caixa Econômica Federal (por Trimestre)') +
  scale_y_continuous(labels = comma) +
  guides(fill = guide_legend(nrow = 5))+
  scale_fill_viridis_d()+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.margin = margin())

## ITAU
filter(hist_reclam,Instituição.financeira == 'ITAU (conglomerado)') %>%
  mutate(Trimestre = as.yearqtr(as.Date(paste0(Ano,'-', ifelse(Trimestre == 2, 4,
                                                               ifelse(Trimestre == 3, 7,
                                                                      ifelse(Trimestre == 4, 10, 1))),
                                               '-', 01)))) %>% 
  group_by(Trimestre) %>% 
  dplyr::select(c(`Trimestre`, `Instituição.financeira`, `Irregularidade`,`Quantidade.total.de.reclamações`)) %>%
  group_by(Trimestre) %>% 
  top_n(1, Quantidade.total.de.reclamações) -> Top_reclam_itau

colnames(Top_reclam_itau) <-  c('date', 'instituicao', 'Irregularidade', 'quantreclam')

## Executando o gráfico
Top_reclam_itau %>% 
  ggplot()+
  geom_bar(aes(x = date, y = quantreclam,
               fill = Irregularidade),
           stat="identity",
           width=.2,
           position = "dodge" )+
  labs(x = 'Período', y = 'Quantidade Total de Reclamações',
       title = 'Quantidade Total do Maior Tipo de Reclamação no Período',
       subtitle = 'Referente ao Banco Itau (por Trimestre)') +
  scale_y_continuous(labels = comma) +
  guides(fill = guide_legend(nrow = 4))+
  scale_fill_viridis_d()+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.margin = margin())

#'SANTANDER (conglomerado)'
filter(hist_reclam,Instituição.financeira == 'SANTANDER (conglomerado)') %>%
  mutate(Trimestre = as.yearqtr(as.Date(paste0(Ano,'-', ifelse(Trimestre == 2, 4,
                                                               ifelse(Trimestre == 3, 7,
                                                                      ifelse(Trimestre == 4, 10, 1))),
                                               '-', 01)))) %>% 
  group_by(Trimestre) %>% 
  dplyr::select(c(`Trimestre`, `Instituição.financeira`, `Irregularidade`,`Quantidade.total.de.reclamações`)) %>%
  group_by(Trimestre) %>% 
  top_n(1, Quantidade.total.de.reclamações) -> Top_reclam_sant

colnames(Top_reclam_sant) <-  c('date', 'instituicao', 'Irregularidade', 'quantreclam')

## Executando o gráfico
Top_reclam_sant %>% 
  ggplot()+
  geom_bar(aes(x = date, y = quantreclam,
               fill = Irregularidade),
           stat="identity",
           width=.2,
           position = "dodge" )+
  labs(x = 'Período', y = 'Quantidade Total de Reclamações',
       title = 'Quantidade Total do Maior Tipo de Reclamação no Período',
       subtitle = 'Referente à Caixa Econômica Federal (por Trimestre)') +
  scale_y_continuous(labels = comma) +
  guides(fill = guide_legend(nrow = 10))+
  scale_fill_viridis_d()+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.margin = margin())

# Resultados dos bancos vs Reclamações ----
## caixa economica e banco do brasil são bancos publicos 
cnpj_3bancos <- c('60.746.948/0001-12', '03.847.461/0001-92', '60.872.504/0001-23', '90.400.888/0001-42', '00.000.000/0001-91')

hist_dre %>%
  filter(CNPJ_CIA %in% cnpj_3bancos) -> top3_DRE

## introduzindo data por trimestre
top3_DRE %>%
  mutate(date_2 = as.yearqtr(top3_DRE$DT_FIM_EXERC, format = "%Y-%m-%d")) %>%
  dplyr::select(c(DENOM_CIA, DS_CONTA, VL_CONTA, DT_REFER, date_2)) %>%
  filter(DS_CONTA == 'Operações de Crédito')  -> top3_DRE #%>%
  subset(top3_DRE, date_2 %in% datas)
## 


