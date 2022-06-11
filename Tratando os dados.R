###########################
#Autor: João Lucas
#Data: 16/02/2022
###########################
#Mestrado profissional em Economia e Finanças
#Curso: Introdução a Ciencia de dados
#instituição: Fundação Getúlio Vargas
###########################


############ Tratamento dos dados #############
install.packages("plyr")
install.packages("magrittr")
install.packages("readr")
install.packages("zoo")
library(magrittr)
library(plyr)
library(dplyr)
library(readr)
library(zoo)

#variaveis que queremos: dataframe p/ cada dre
# Site fonte dos dados CVM
url_1 <- "http://dados.cvm.gov.br/dados/CIA_ABERTA/DOC/ITR/DADOS/"

#Lista com os nomes dos arquivos que desejamos baixar
arquivos_zip <- c("itr_cia_aberta_2011.zip", "itr_cia_aberta_2012.zip", "itr_cia_aberta_2013.zip", "itr_cia_aberta_2014.zip", "itr_cia_aberta_2015.zip", "itr_cia_aberta_2016.zip", "itr_cia_aberta_2017.zip", "itr_cia_aberta_2018.zip", "itr_cia_aberta_2019.zip", "itr_cia_aberta_2020.zip", "itr_cia_aberta_2021.zip")

#Download dos arquivos em um determinado diretório
destfile <- "dados_CVM_zip"

for (i in arquivos_zip) {
  download.file(paste(url_1, i, sep = ""), paste(destfile, "/", i,sep = "" ))
}

#extraindo Zips baixados para diretório escolhido
for (i in arquivos_zip) {
  unzip(paste(destfile, "/", i,sep = "" ), exdir ="dados_CVM_csv")
}

#Criando histórico concatenando dataframe

# Se for para concatenar todos os tipos de dre disponibilizados pela CVM usar codigo abaixo
#nomes <- c("DVA_ind", "DVA_con", "DRE_ind", "DRE_con", "DRA_ind", "DMPL_ind", "DMPL_con", "DFC_MI_ind", "DFC_MI_con", "DFC_MD_ind", "DFC_MD_con", "BPP_ind", "BPP_con", "BPA_ind", "BPA_con")
nome <- "DRE_con"
hist_dre <- data.frame()
for (ano in 2011:2021) {
  #arquivo <- data.frame()
  #  for (nome in nomes) { 
  hist_dre <- rbind(hist_dre, read.csv2(paste("dados_CVM_csv/itr_cia_aberta_", nome,"_", ano, ".csv", sep = "")))
}

# Criando histórico de reclamações do BACEN 
hist_reclam <- data.frame()
for (n in 1:20) {
  hist_reclam <- rbind(hist_reclam,
                       read_delim(paste("dados_raclam/Bancos+e+financeiras+-+Irregularidades+por+instituicao+financeira (", n, ")", ".csv", sep = ""), 
                                  delim = ";", escape_double = FALSE, locale = locale(encoding = "LATIN1"), 
                                  trim_ws = TRUE))
}


hist_reclam_2 <- data.frame()
for (n in 6:25) {
  hist_reclam_2 <- rbind.fill(hist_reclam_2,
                              read_delim(paste("dados_reclam_2/Bancos+e+financeiras+-+Reclamacoes+e+quantidades+de+clientes+por+instituicao+financeira (", n, ")", ".csv", sep = ""), 
                                         delim = ";", escape_double = FALSE, locale = locale(encoding = "LATIN1"), 
                                         trim_ws = TRUE))
}

# Renomeando o nome das colunas 
colnames(hist_reclam_2) <- c("Ano", "Trimestre", "Categoria", "Tipo", "CNPJ.IF", "Instituição.financeira", "Índice", "Quantidade.de.reclamações.reguladas.procedentes", "Quantidade.de.reclamações.reguladas...outras", "Quantidade.de.reclamações.não.reguladas", "Quantidade.total.de.reclamações", "Quantidade.total.de.clientes...CCS.e.SCR", "Quantidade.de.clientes...CCS", "Quantidade.de.clientes...SCR", "Quantidade.de.clientes...FGC", "X")

colnames(hist_reclam) <- c("Ano","Trimestre","Categoria","Tipo","CNPJ.IF","Instituição.financeira","Irregularidade","Quantidade.de.reclamações.reguladas.procedentes","Quantidade.de.reclamações.reguladas...outras","Quantidade.de.reclamações.não.reguladas","Quantidade.total.de.reclamações","X")

# Adicionando a variavel categoria_2, se empresa maior que 4 milhões
hist_reclam_2$Categoria_2 <- hist_reclam_2$Quantidade.total.de.clientes...CCS.e.SCR>4000000
hist_reclam_2$Categoria_2[is.na(hist_reclam_2$Categoria_2)] <- FALSE
hist_reclam_2$Categoria_2 <- replace(hist_reclam_2$Categoria_2, hist_reclam_2$Categoria_2 == "FALSE", "Menos de 4 milhões de clientes")
hist_reclam_2$Categoria_2 <- replace(hist_reclam_2$Categoria_2, hist_reclam_2$Categoria_2 == "TRUE", "Mais de 4 milhões de clientes")


# Tirando o simbolo de numero do 1 e 2  data frame
hist_reclam_2$Trimestre = gsub("\\º","",hist_reclam_2$Trimestre)

hist_reclam$Trimestre = gsub("\\º","",hist_reclam$Trimestre)

#Tentando inserir uma coluna de data no dataframe 1 e 2
hist_reclam_2 <- hist_reclam_2 %>% 
  mutate(tri = ifelse(Trimestre == 2, 4, 
                      ifelse(Trimestre == 3, 7,
                             ifelse(Trimestre == 4, 10, 1)))) %>% 
  mutate(date = as.Date(paste0(tri, '-', Trimestre, '-', Ano), format = "%m-%d-%Y" ))

hist_reclam <- hist_reclam %>% 
  mutate(tri = ifelse(Trimestre == 2, 4, 
                      ifelse(Trimestre == 3, 7,
                             ifelse(Trimestre == 4, 10, 1)))) %>% 
  mutate(date = as.Date(paste0(tri, '-', Trimestre, '-', Ano), format = "%m-%d-%Y" ))

# Inserindo formato de data com Trimestre Quarter/year

hist_reclam_2$date_2 <- as.yearqtr(hist_reclam_2$date, format = "%m-%d-%Y")

hist_reclam$date_2 <- as.yearqtr(hist_reclam$date, format = "%m-%d-%Y")

write.csv2(hist_reclam, "hist_reclam", row.names = FALSE)
write.csv2(hist_reclam_2, "hist_reclam_2", row.names = FALSE)
write.csv(hist_dre, "hist_dre", row.names = FALSE)


