###########################
#Autor: João Lucas
#Data: 15/07/2022
###########################
#Mestrado profissional em Economia e Finanças
#Curso: Introdução a Ciencia de dados
#instituição: Fundação Getúlio Vargas
###########################



#Bibliotecas
install.packages("tm")
install.packages("NLP")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("wordcloud2")
install.packages("ggwordcloud")
library(wordcloud2)
library(RColorBrewer)
library(SnowballC)
library(wordcloud)
library(tm)
library(NLP)
library(ggwordcloud)


# Construindo NUVEM DE PALAVRAS ----
## selecionando a listagem de reclamações e suas respectivas frequências
top5Bancos <- c('BRADESCO (conglomerado)', 'BB (conglomerado)',
                'CAIXA ECONÔMICA FEDERAL (conglomerado)',
                'ITAU (conglomerado)', 'SANTANDER (conglomerado)')

hist_reclam %>%
  filter(Instituição.financeira %in% top5Bancos)%>%
  aggregate(Quantidade.total.de.reclamações ~ Irregularidade, sum) -> data_nuvem
  ##mudando nome das colunas
colnames(data_nuvem) <-  c('Irregularidade', 'quantreclam')

top5_corpus <- str_dup(c(paste(data_nuvem$Irregularidade, "")), data_nuvem$quantreclam)

# Criando corpus dos maiores bancos----
top5_corpus <- Corpus(VectorSource(top5_corpus))

word_list <- c('relacionada', 'relacionadas', 'exceto', 'relativas',
               'adequado', 'não', 'irregularidades', 'irregularidade', 'operações', 'operaçõ')
#Tirando letras maiúsculas
top5_corpus <- tm_map(top5_corpus, PlainTextDocument)
top5_corpus <- tm_map(top5_corpus, tolower)
#Removendo pontuação
top5_corpus <- tm_map(top5_corpus, removePunctuation)
#Retirando as palavras que não agregam
top5_corpus <- tm_map(top5_corpus, removeWords, stopwords("pt"))
top5_corpus <- tm_map(top5_corpus, removeWords, word_list)
# Removendo sinônimos
top5_corpus <- tm_map(top5_corpus, stemDocument)
# Retirando espaços em branco
top5_corpus <- tm_map(top5_corpus, stripWhitespace)
#write(top5_corpus, "top5_corpus")

#fazendo dataframe com base nas frequências das palavras
DTM <- TermDocumentMatrix(top5_corpus)
mat <- as.matrix(DTM)
f <- sort(rowSums(mat), decreasing=TRUE)

dataframe_nuvem <- data.frame(word = names(f), freq = f)

write.csv(dataframe_nuvem, 'dataframe_nuvem')

#Executando núvem de palavras
set.seed(100)
wordcloud(words = dataframe_nuvem$word, freq = dataframe_nuvem$freq, min.freq = 3,
          max.words=250, random.order=FALSE, rot.per=0.30, colors = brewer.pal(10, "BrBG"))



