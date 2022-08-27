### Trabalho Final GGPLOT2 ###

### Curso DSN - Coppead 

### Grupo - Alunos

## Caian Martins
## Carolina Fernandes
## Davi Rocha
## Juliana Mesquita
## Leonardo Mantuano
## Tarciso Gouveia


##limpar as variaveis
rm(list=ls())

setwd("C:/Users/jujum/Desktop")


#install.packages('countrycode')

library(tidyverse)
library(countrycode)
library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)

'-------------BASE DE DADOS: CARREGAR E ANALISAR EXP----------------------------'

## Escolher uma Base de dados e fazer analise exploratoria

#FONTE DO DATASET: https://www.kaggle.com/unsdsn/world-happiness?select=2019.csv#

#SLIDE 3

my_data <- read.csv('2019.csv', sep = "," )

glimpse(my_data)
class(my_data)
dim(my_data)
summary(my_data)
str(my_data)
colnames(my_data)
sapply(my_data, function(x) sum(is.na(x)))


'--------------RANKING PAISES--------------------------'


## Ranking paises mais felizes - SLIDE 4

my_data %>%
  arrange(desc(Score)) %>%
  slice(1:10) %>%
  ggplot(aes(x=reorder(Country.or.region, Score),y=Score))+
  geom_bar(fill=rgb(0.9,0.4,0.9,0.7),stat="identity") +
  coord_flip() +
  geom_text(aes(label=c(1:10)), position=position_dodge(width=0.5), vjust=0.25, hjust=-0.25) +
  theme_light() +
  labs(title='Top 10 paises mais felizes') +
  theme(plot.title = element_text(hjust = 0.4)) +
  xlab("Paises")+ylab("Score Global")


## Histograma Score

ggplot(my_data,aes(Score))+
  geom_histogram(bins=10,fill=rgb(0.9,0.4,0.9,0.7),
                 color="black") +
  labs(title='Histograma Score de Felicidade') +
  xlab("Score Global")

## Ranking paises mais ricos - SLIDE 5

my_data %>%
  arrange(desc(GDP.per.capita)) %>%
  slice(1:10) %>%
  ggplot(aes(x=reorder(Country.or.region, GDP.per.capita),y=GDP.per.capita)) +
  geom_bar(fill=rgb(0.1,0.9,0.1,0.7),stat="identity") +
  coord_flip() +
  geom_text(aes(label=c(1:10)), position=position_dodge(width=0.5), vjust=0.25, hjust=-0.25) +
  theme_light() +
  labs(title='Top 10 paises com melhor PIB per capita') +
  theme(plot.title = element_text(hjust = 0.4)) +
  xlab("Paises")+ylab("GDP per capita")

## Histograma PIB

ggplot(my_data,aes(GDP.per.capita))+
  geom_histogram(bins=10,fill=rgb(0.1,0.9,0.1,0.7),
                 color="black") +
  labs(title='Histograma PIB') +
  xlab("GDP per capita")

## Ranking paises com mais assistencia social - SLIDE 6

my_data %>%
  arrange(desc(Social.support)) %>%
  slice(1:10) %>%
  ggplot(aes(x=reorder(Country.or.region, Social.support),y=Social.support)) +
  geom_bar(fill=rgb(0.7,0.2,0.1,0.7),stat="identity") +
  coord_flip() +
  geom_text(aes(label=c(1:10)), position=position_dodge(width=0.5), vjust=0.25, hjust=-0.25) +
  theme_light() +
  labs(title='Top 10 pa?ses com melhor Assistencia Social') +
  theme(plot.title = element_text(hjust = 0.4)) +
  xlab("Paises")+ylab("Assistencia Social")

## Histograma Assistencia Social

ggplot(my_data,aes(Social.support))+
  geom_histogram(bins=10,fill=rgb(0.7,0.2,0.1,0.7),
                 color="black") +
  labs(title='Histograma Assistencia Social') +
  xlab("Assistencia Social")

## Ranking paises com melhor expectativa de vida - SLIDE 7

my_data %>%
  arrange(desc(Healthy.life.expectancy)) %>%
  slice(1:10) %>%
  ggplot(aes(x=reorder(Country.or.region, Healthy.life.expectancy),y=Healthy.life.expectancy)) +
  geom_bar(fill=rgb(0.2,0.2,0.9,0.3),stat="identity") +
  coord_flip() +
  geom_text(aes(label=c(1:10)), position=position_dodge(width=0.5), vjust=0.25, hjust=-0.25) +
  theme_light() +
  labs(title='Top 10 paises com melhor Expectativa de Vida') +
  theme(plot.title = element_text(hjust = 0.4)) +
  xlab("Paises")+ylab("Expectativa de Vida")

## Histograma Expectativa de Vida

ggplot(my_data,aes(Healthy.life.expectancy))+
  geom_histogram(bins=10,fill=rgb(0.2,0.2,0.9,0.3),
                 color="black") +
  labs(title='Histograma Expectativa de Vida') +
  xlab("Expectativa de Vida")

## Ranking paises com mais liberdade - SLIDE 8

my_data %>%
  arrange(desc(Freedom.to.make.life.choices)) %>%
  slice(1:10) %>%
  ggplot(aes(x=reorder(Country.or.region, Freedom.to.make.life.choices),y=Freedom.to.make.life.choices)) +
  geom_bar(fill=rgb(0.99,0.8,0.1,0.4),stat="identity") +
  coord_flip() +
  geom_text(aes(label=c(1:10)), position=position_dodge(width=0.5), vjust=0.25, hjust=-0.25) +
  theme_light() +
  labs(title='Top 10 paises com mais Liberdade Individual') +
  theme(plot.title = element_text(hjust = 0.4)) +
  xlab("Paises")+ylab("Liberdade Individual")

## Histograma Liberdade Individual

ggplot(my_data,aes(Freedom.to.make.life.choices))+
  geom_histogram(bins=10,fill=rgb(0.99,0.8,0.1,0.4),
                 color="black") +
  labs(title='Histograma Liberdade Individual') +
  xlab("Liberdade Individual")

## Ranking paises com mais generosidade - SLIDE 9

my_data %>%
  arrange(desc(Generosity)) %>%
  slice(1:10) %>%
  ggplot(aes(x=reorder(Country.or.region, Generosity),y=Generosity)) +
  geom_bar(fill=rgb(0.2,0.7,0.1,0.4),stat="identity") +
  coord_flip() +
  geom_text(aes(label=c(1:10)), position=position_dodge(width=0.5), vjust=0.25, hjust=-0.25) +
  theme_light() +
  labs(title='Top 10 paises com mais Generosidade') +
  theme(plot.title = element_text(hjust = 0.4)) +
  xlab("Paises")+ylab("Generosidade")

## Histograma Generosidade

ggplot(my_data,aes(Generosity))+
  geom_histogram(bins=10,fill=rgb(0.2,0.7,0.1,0.4),
                 color="black") +
  labs(title='Histograma Generosidade') +
  xlab("Generosidade")

## Ranking paises com melhor Percepcao sobre Corrupcao - SLIDE 10

my_data %>%
  arrange(desc(Perceptions.of.corruption)) %>%
  slice(1:10) %>%
  ggplot(aes(x=reorder(Country.or.region, Perceptions.of.corruption),y=Perceptions.of.corruption)) +
  geom_bar(fill=rgb(0.1,0.1,0.1,0.4),stat="identity") +
  coord_flip() +
  geom_text(aes(label=c(1:10)), position=position_dodge(width=0.5), vjust=0.25, hjust=-0.25) +
  theme_light() +
  labs(title='Top 10 paises com melhor Percepcao Sobre Corrupcao') +
  theme(plot.title = element_text(hjust = 0.4)) +
  xlab("Paises")+ylab("Percepcao Sobre Corrupcao")

## Histograma Corrupcao

ggplot(my_data,aes(Perceptions.of.corruption))+
  geom_histogram(bins=10,fill=rgb(0.1,0.1,0.1,0.4),
                 color="black") +
  labs(title='Histograma Percepao Sobre Corrupcao') +
  xlab("Percepcao Sobre Corrupcao")



'---------------------SCORE GLOBAL-----------------------'
#SLIDE 11

#Incluindo os codigos das regioes
my_data_region<- my_data %>% 
  mutate(region = countrycode(sourcevar = Country.or.region, 
                              origin = "country.name",
                              destination = "region"))
View(my_data_region)



## Score Global - individual

my_data_region %>%
  arrange(my_data_region,desc(Score)) %>%
  mutate(Country.or.region=factor(Country.or.region, levels=Country.or.region)) %>% 
  ggplot( aes(x=Country.or.region, y=Score))+
  geom_point( size=4, color="red") +
  theme_light() +
  labs(title='Score por pais') +
  xlab("Paises")+ylab("Score Global") +
  theme(axis.text.x = element_text(angle = 90))

#SLIDE 12

my_data_region %>%
  mutate(region = fct_reorder(region, Score)) %>%
  ggplot(aes(x = region , y = Score,fill=region)) + 
  geom_boxplot(show.legend=TRUE) +
  labs(title='Score per Region',
       caption='Source: https://www.kaggle.com/unsdsn/world-happiness?select=2019.csv') + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Regiões")+ylab("Score Global") +
  theme_dark()

## Score Global - com marcacao de regiao - SLIDE 13

my_data_region %>%
  arrange(my_data_region,desc(Score)) %>%
  mutate(Country.or.region=factor(Country.or.region, levels=Country.or.region)) %>% 
  ggplot( aes(x=Country.or.region, y=Score,color=region))+
  geom_point( size=4,) +
  theme_test() +
  xlab("Países")+ylab("Score Global")+
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title='Score por pais segmentado por regiao - desc') +
  theme(legend.background = element_rect(fill="lightblue", 
                                         size=0.5, linetype="solid"))


'-----------GDP PER CAPITA-------------------'


#Agrupando o GDP per capita em 3 categorias - SLIDE 14
hist(my_data_region$Score)
pairs(my_data_region)

hist(my_data_region$GDP.per.capita)
my_data_region$GDP_group = cut(my_data_region$GDP.per.capita,c(0,0.5,1,1.7))
my_data_region
levels(my_data_region$GDP_group) = c("0-0.5","0.6-1",">1")
my_data_region


#Verificando o nivel de GDP por regiao
ggplot(my_data_region)+
  geom_bar(aes(x=region, fill=GDP_group),position="dodge", show.legend = TRUE)+
  coord_flip()+ theme_classic()+
  labs(title='PIB per capita - Regioes') + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Quantidade de países")+ylab("Regioes")




#Vendo a relacao entre GDP per capita - SLIDE 15
ggplot(data=my_data_region) +
  geom_point(aes(x = GDP.per.capita,
                 y = Score,
                 color = region),
             size = 3) +
  labs(title='PIB per capita por Região',
       caption='Source: https://www.kaggle.com/unsdsn/world-happiness?select=2019.csv') + 
  theme(plot.title = element_text(hjust = 0.5)) + theme_grey() + 
  xlab("PIB per Capital")+ylab("Score Global")


# Facet - PIB per capital - SLIDE 16
ggplot(data = my_data_region) + 
  geom_point(aes(x = GDP.per.capita, y = Score, color=region))+
  facet_wrap(~ region,nrow=2)+   theme_linedraw()+
  labs(title='PIB per capita - Regiao',
       caption='Source: https://www.kaggle.com/unsdsn/world-happiness?select=2019.csv') + 
  xlab("PIB per Capital")+ylab("Score Global")+
  theme(plot.title = element_text(hjust = 0.5))





'--------------Assistencia Social--------------------------'


# Boxplot - Assistencia Social SLIDE 17

my_data_region %>%
  mutate(region = fct_reorder(region, Social.support)) %>%
  ggplot(aes(x = region , y = Score,fill=region)) + 
  geom_boxplot(show.legend=TRUE) + theme_bw()+
  labs(title='Assistencia Social',
       caption='Source: https://www.kaggle.com/unsdsn/world-happiness?select=2019.csv') + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Assistencia Social")+ylab("Regioes") + coord_flip()


'---------------Expectativa de Vida------------'

# Bubble - Expectativa de Vida - SLIDE 18
my_data_region %>%
  ggplot(aes(x=GDP.per.capita, y=Healthy.life.expectancy, size = Score,color = region, text=paste(region,Score))) +
  geom_point(alpha=0.7) + theme_bw()+
  scale_size(range = c(0.5, 10), name="Score")+
  labs(title='PIB per capital x Expectativa de vida x Score')+
  xlab("PIB per capital")+ylab("Expectativa de Vida")



'---------------Liberdade individual------------'

# Stacked density plot - Liberdade Individual: SLIDE 19
my_data_region %>%
  mutate(region = fct_reorder(region, Freedom.to.make.life.choices)) %>%
  ggplot(aes(x=Freedom.to.make.life.choices, group=region, fill=region)) +
  geom_density(adjust=1.5, position="fill")+ theme_light() +
  labs(title='Densidade da Liberdade individual por Regiao')+
  xlab("Liberdade Individual")+ylab("Densidade")
       


# Boxplot - Liberdade individual

my_data_region %>%
  mutate(region = fct_reorder(region, Freedom.to.make.life.choices)) %>%
  ggplot(aes(x = region , y = Score,fill=region)) + 
  geom_boxplot(show.legend=TRUE) + theme_bw()+
  labs(title='Liberdade Individual',
       caption='Source: https://www.kaggle.com/unsdsn/world-happiness?select=2019.csv') + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Liberdade Individual")+ylab("Regioes") + coord_flip()


'---------------Generosidade-----------------------'

#Facet wrap- Generosidade - SLIDE 20
ggplot(data = my_data_region) + 
  geom_point(aes(x = Generosity, y = Score, color=region))+
  facet_wrap(~ region,nrow=2) +
  labs(title='Generosidade',
       caption='Source: https://www.kaggle.com/unsdsn/world-happiness?select=2019.csv') + 
  theme(plot.title = element_text(hjust = 0.5)) + theme_bw() +
  xlab("Generosidade")+ylab("Score")



'---------------Corrupcao Percebida-----------------------'

# Boxplot - Corrupcao SLIDE 21

my_data_region %>%
  mutate(region = fct_reorder(region, desc(Perceptions.of.corruption))) %>%
  ggplot(aes(x = region , y = Perceptions.of.corruption,fill=region)) + 
  geom_boxplot(show.legend=TRUE) +
  labs(title='Corrupcao Percebida',
       caption='Source: https://www.kaggle.com/unsdsn/world-happiness?select=2019.csv') + 
  scale_fill_brewer(palette="Reds")+ theme_bw()+
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Regiao")+ylab("Corrupcao Percebida") +
  coord_flip()


'----------------------Score Final---------------'

# Densidade - Score por Grupo  SLIDE 22
ggplot(data=my_data_region, aes(x=Score, group=region, fill=region)) +
  geom_density(adjust=1.5, alpha=.7)+
  labs(title='Score por Regiao')+ theme_bw()+
  theme(plot.title = element_text(hjust = 0.4))+
  xlab("Densidade")+ylab("Score Global")

# Bar - Grupo GDP
ggplot(my_data_region)+
  geom_bar(aes(x=region,fill=GDP_group),position="fill")+
  coord_flip()+theme_classic()+
  labs(title='PIB per capita - Região',
       caption='Source: https://www.kaggle.com/unsdsn/world-happiness?select=2019.csv') + 
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Regioes")+ylab("PIB per capita")



## Evolucao - Resultado Final SLIDE 23

pairs(my_data_region[,c(3:9)], upper.panel=NULL)


# Boxplot - Score por Grupo SLIDE 23

ggplot(data=my_data_region, aes(x=Social.support, group=region, fill=region)) +
  geom_density(adjust=1.5) +
  facet_wrap(~region)+theme_classic() +
  labs(title='Distribuicao da Densidade do Social entre os grupos - Separadamente',
       caption='Source: https://www.kaggle.com/unsdsn/world-happiness?select=2019.csv') + 
  theme(legend.position="none", panel.spacing = unit(0.1, "lines"), axis.ticks.x=element_blank())+
  xlab("Assistencia Social")+ylab("Densidade")



#SLIDES 24 a 26

#Encontrando os top ten de cada categoria e fazendo o gráfico correspondente
ls_sequencia = c('01','02','03','04','05','06','07','08','09','10')

ls_tt_Freedon <- my_data[order(my_data$Freedom.to.make.life.choices, decreasing=TRUE), ][1:10,]
ls_tt_Freedon <- ls_tt_Freedon %>% select(Overall.rank, Freedom.to.make.life.choices, Country.or.region, Score) %>%
  mutate(Description = paste(ls_sequencia, Country.or.region, Freedom.to.make.life.choices, sep=' - '))

ls_tt_GDP <- my_data[order(my_data$GDP.per.capita, decreasing=TRUE), ][1:10,]
ls_tt_GDP <- ls_tt_GDP %>% select(Overall.rank, GDP.per.capita, Country.or.region, Score) %>%
  mutate(Description = paste(ls_sequencia, Country.or.region, GDP.per.capita, sep=' - '))

ls_tt_Social <- my_data[order(my_data$Social.support, decreasing=TRUE), ][1:10,]
ls_tt_Social <- ls_tt_Social %>% select(Overall.rank, Social.support, Country.or.region, Score) %>%
  mutate(Description = paste(ls_sequencia, Country.or.region, Social.support, sep=' - '))

ls_tt_Healthy <- my_data[order(my_data$Healthy.life.expectancy, decreasing=TRUE), ][1:10,]
ls_tt_Healthy <- ls_tt_Healthy %>% select(Overall.rank, Healthy.life.expectancy, Country.or.region, Score) %>%
  mutate(Description = paste(ls_sequencia, Country.or.region, Healthy.life.expectancy, sep=' - '))

ls_tt_Generosity <- my_data[order(my_data$Generosity, decreasing=TRUE), ][1:10,]
ls_tt_Generosity <- ls_tt_Generosity %>% select(Overall.rank, Generosity, Country.or.region, Score) %>%
  mutate(Description = paste(ls_sequencia, Country.or.region, Generosity, sep=' - '))

ls_tt_Corruption <- my_data[order(my_data$Perceptions.of.corruption, decreasing=TRUE), ][1:10,]
ls_tt_Corruption <- ls_tt_Corruption %>% select(Overall.rank, Perceptions.of.corruption, Country.or.region, Score) %>%
  mutate(Description = paste(ls_sequencia, Country.or.region, Perceptions.of.corruption, sep=' - '))

ls_tt_Overall <- my_data[order(my_data$Overall.rank, decreasing=FALSE), ][1:10,]
ls_tt_Overall <- ls_tt_Overall %>% select(Overall.rank, Country.or.region, Score) %>%
  mutate(Description = paste(ls_sequencia, Country.or.region, Overall.rank, sep=' - '))

#Gráficos de Barras TopTen por Categoria x Ranking

ggplot(ls_tt_Freedon)+
  geom_col(aes(y=Description, x=Overall.rank, fill=Description))+
  geom_text(aes(y=Description, x=Overall.rank, label = Overall.rank), vjust=-0.5, size=4)+ 
  coord_flip()+ theme_classic()+
  labs(title='Top 10 - Liberdade Individual') + 
  theme(axis.text.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Rank")+ylab("Top Ten")

ggplot(ls_tt_GDP)+
  geom_col(aes(y=Description, x=Overall.rank, fill=Description))+
  geom_text(aes(y=Description, x=Overall.rank, label = Overall.rank), vjust=-0.5, size=4)+
  coord_flip()+ theme_classic()+
  labs(title='Top 10 - PIB per Capita') + 
  theme(axis.text.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Rank")+ylab("Top Ten")

ggplot(ls_tt_Social)+
  geom_col(aes(y=Description, x=Overall.rank, fill=Description))+
  geom_text(aes(y=Description, x=Overall.rank, label = Overall.rank), vjust=-0.5, size=4)+
  coord_flip()+ theme_classic()+
  labs(title='Top 10 - Assistência Social') + 
  theme(axis.text.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Rank")+ylab("Top Ten")

ggplot(ls_tt_Healthy)+
  geom_col(aes(y=Description, x=Overall.rank, fill=Description))+
  geom_text(aes(y=Description, x=Overall.rank, label = Overall.rank), vjust=-0.5, size=4)+  
  coord_flip()+ theme_classic()+
  labs(title='Top 10 - Expectativa de Vida') + 
  theme(axis.text.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Rank")+ylab("Top Ten")

ggplot(ls_tt_Generosity)+
  geom_col(aes(y=Description, x=Overall.rank, fill=Description))+
  geom_text(aes(y=Description, x=Overall.rank, label = Overall.rank), vjust=-0.5, size=4)+ 
  coord_flip()+ theme_classic()+
  labs(title='Top 10 - Generosidade') + 
  theme(axis.text.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Rank")+ylab("Top Ten")

ggplot(ls_tt_Corruption)+
  geom_col(aes(y=Description, x=Overall.rank, fill=Description))+
  geom_text(aes(y=Description, x=Overall.rank, label = Overall.rank), vjust=-0.5, size=4)+
  coord_flip()+ theme_classic()+
  labs(title='Top 10 - Percepção de Corrupção') + 
  theme(axis.text.x = element_blank()) +
  theme(legend.position = "right") +  
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Rank")+ylab("Top Ten")


#SLIDES 27 A 31


ggplot(data=my_data_region) +
  geom_point(aes(x = GDP.per.capita,
                 y = Score,
                 color = region),
             size = 3) +
  labs(title='PIB per capita por Região',
       caption='Source: https://www.kaggle.com/unsdsn/world-happiness?select=2019.csv') + 
  theme(plot.title = element_text(hjust = 0.5)) + theme_grey() + 
  xlab("PIB per Capita")+ylab("Score Global")


ggplot(data=my_data_region) +
  geom_point(aes(x = Social.support,
                 y = Score,
                 color = region),
             size = 3) +
  labs(title='Assistência Social por Região',
       caption='Source: https://www.kaggle.com/unsdsn/world-happiness?select=2019.csv') + 
  theme(plot.title = element_text(hjust = 0.5)) + theme_grey() + 
  xlab("Assistência Social")+ylab("Score Global")


ggplot(data=my_data_region) +
  geom_point(aes(x = Healthy.life.expectancy,
                 y = Score,
                 color = region),
             size = 3) +
  labs(title='Expectativa de Vida por Região',
       caption='Source: https://www.kaggle.com/unsdsn/world-happiness?select=2019.csv') + 
  theme(plot.title = element_text(hjust = 0.5)) + theme_grey() + 
  xlab("Expectativa de Vida")+ylab("Score Global")



ggplot(data=my_data_region) +
  geom_point(aes(x = Freedom.to.make.life.choices,
                 y = Score,
                 color = region),
             size = 3) +
  labs(title='Liberdade Individual por Região',
       caption='Source: https://www.kaggle.com/unsdsn/world-happiness?select=2019.csv') + 
  theme(plot.title = element_text(hjust = 0.5)) + theme_grey() + 
  xlab("Liberdade Individual")+ylab("Score Global")


ggplot(data=my_data_region) +
  geom_point(aes(x = Generosity,
                 y = Score,
                 color = region),
             size = 3) +
  labs(title='Generosidade por Região',
       caption='Source: https://www.kaggle.com/unsdsn/world-happiness?select=2019.csv') + 
  theme(plot.title = element_text(hjust = 0.5)) + theme_grey() + 
  xlab("Generosidade")+ylab("Score Global")



ggplot(data=my_data_region) +
  geom_point(aes(x = Perceptions.of.corruption,
                 y = Score,
                 color = region),
             size = 3) +
  labs(title='Percepção de Corrupção por Região',
       caption='Source: https://www.kaggle.com/unsdsn/world-happiness?select=2019.csv') + 
  theme(plot.title = element_text(hjust = 0.5)) + theme_grey() + 
  xlab("Percepção de Corrupção")+ylab("Score Global")





