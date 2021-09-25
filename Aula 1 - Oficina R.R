#Oficina R#####

#Sumário####
#Título1####

##Título2####

###Títutlo3####

####Títutlo4####

#####Título5####

######Título6####
#Por enquanto só vai até aqui a divisão.
######Título7####

#Textos grandes sem que seja necessário 
#Colocar o # antes de todas as anotações
#*
#*
#*

#Análise da documentação do R e outras dúvidas
help.start()

#Definir o Diretório de trabalho
Ctrl + shift + H
#Ou então
setwd()
#setwd("~/MEGAsync/2- EVENTOS -UFPR -BOLSA/EVENTOS 2021/1- CURSO DE EXTENSÃO")
setwd("~/MEGAsync/2- EVENTOS -UFPR -BOLSA/EVENTOS 2021/1- CURSO DE EXTENSÃO/1. OFICINA R")
#mostrar outra forma de setar como diretório de trabalho

#Para descrobrir em qual diretório de trabalho está:
getwd()

#Para finalizar a sessão de trabalho
q()

#Lembre de sempre salvar tudo que está sendo feito
ctrl + s

#funções básicas do R

#Objeto de atribuição <-

a <- 1
b <- 3

a
b

c = a + b
c

d = 1.6
e = 1.4
f = d + e

f

#Por ser um software de língua inglesa, 
#os algarismos decimais são separados por ponto.

#Pacotes
install.packages("nome do pacote")
library(nomedopacote)

#Importação de dados

#Consulta:
# http://electionsbr.com/livro/importacao.html#exportando-dados

# Carrega o pacote readr
library(readr) #CSV, TXT
library(readxl) #Excel
library(memisc) #Uma das funções é a recodificação


# Criar um data.frame e abrir em diferentes formatos
#Será um banco sobre Estudo

Nomes <- c("Francisco", "Maria", "Pedro")
Idade <- c(15, 14, 11)
Segunda <- c("Matemática", "Português", "História")
Terça <- c("Inglês", "Informática", "Espanhol")
Notas <- c(5.5, 3.2, 6.3)

Estudo <- data.frame(Nomes, Idade, Segunda, Terça,
                     Notas)
Estudo

#Salvando banco de dados em CSV

write.table(Estudo, file = "Estudo.csv", 
            sep = ",", quote = T)
#Limpar o environment e abrir a de novo

#Abrindo o banco em csv
Estudo <- read.csv("Estudo.csv")

#Salvando no formato do R (Rdata)
save(Estudo, file = "Estudo.Rdata")

#Organizando banco de dados 

# deleta a terceira linha
Estudo2 <- Estudo[-3, ] 
#Essa é uma maneira de fazer a exclusão

#Ver o banco de dados
head(Estudo2)

Estudo2

#excluir uma coluna
Estudo3$Idade <- NULL

Estudo

#Transformar uma variável numérica em categórica
#Primeiro vamos verificar como essa variável está organizada

summary() #Função do R base para tirar um sumarizar a variável

summary(Estudo$Notas)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#3.20    4.35    5.50    5.00    5.90    6.30 

table(Estudo$Segunda)
#História Matemática  Português 
#1          1          1 

#Transformar Notas em uma variável categoria, 
#contendo 2 categorias: nota vermelha e nota azul
#As notas menores que 6 serão vermelhas e maiores que 6 serão azul

Estudo$NotasC <-  recode(Estudo$Notas, "Azul" <- 6.3,
                        "Vermelha" <- c(3.2, 5.5) )  

table(Estudo$NotasC)
# Azul Vermelha 
# 1        2

#Se eu quiser excluir uma categoria? 
Estudo$NotasC <-  recode(Estudo$Notas, "Azul" <- 6.3,
                         "Vermelha" <- c(3.2) ) 

table(Estudo$NotasC)
#Essa categoria exclui vira NA

#Criando uma nova variável
Estudo$Comidas <- c("Lanche", "Pizza", "Sorvete")
Estudo$Altura <- c(1.55, 1.60, 1.43)

#Vamos testar em um banco real?
#Banco do LAPOP 2019
Bra2019 <- read_delim("~/MEGAsync/2- EVENTOS -UFPR -BOLSA/EVENTOS 2021/1- CURSO DE EXTENSÃO/Bra2019.csv", 
                      ";", escape_double = FALSE, trim_ws = TRUE)


#Organizar os dados que queremos utilizar
library(dplyr)

#Queremos trabalhar com as seguintes variáveis
Bra2019$idnum
Bra2019$q1
Bra2019$q2
Bra2019$q10new
Bra2019$colori
Bra2019$ur
Bra2019$jc10
Bra2019$arm2
Bra2019$b2

BraMenor <- select(Bra2019, idnum, q1, q2, q10new, colori, ur,
       jc10, arm2, b2)
#Mas eu não quero trabalhar com a população rural
#Então verificamos qual é a categoria de rural
#UR [PT]. (1) Urbano (2) Rural
#Filtramos apenas para urbano
BraMenor$ur

BraMenor <- BraMenor %>%
  filter(ur == 1)             

#Quero trabalhar apenas com indivíduos maiores de 
#40 anos, então filtramos pela idade 
BraMenor$q2

BraMenor <- BraMenor %>%
  filter(q2 > 40)             

#Se quisessemos comparar as pessoas mais velhas e mais jovens
#Criaríamos 2 bancos novos filtrando pra mais velhos e 
#mais jovens

#Agora vamos organizar os nomes das variáveis 

BraMenor$Sexo <-  BraMenor$q1
BraMenor$Idade <- BraMenor$q2 
BraMenor$Cor <- BraMenor$colori 
BraMenor$Armas <- BraMenor$arm2 
BraMenor$Renda <- BraMenor$q10new 
BraMenor$Golpe <- BraMenor$jc10 

#Salvar esse banco
save(BraMenor, file= "BraMenor.RData")

#Vamos olhar essas variáveis?
#podemos usar comandos para observar a estrutura das mesmas
#ou então observar de maneira gráfica

table(BraMenor$Sexo)
#1   2 
#284 283 

summary(BraMenor$Idade)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 41.00   45.00   52.00   54.26   61.00   92.00 

table(BraMenor$Renda)
# 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 
# 18 18 33 37 62 18 20 32 33 23 51 23 23 25 33 43 30 

#Pacote ggplot2
library(ggplot2)

BraMenor$idnum

#Versão 1 
ggplot(BraMenor, aes(x=Idade)) + 
 geom_histogram()

table(BraMenor$Sexo)
BraMenor$Sexo <- as.character(BraMenor$Sexo)

#Versão 2- por sexo
ggplot(BraMenor, aes(x=Idade, fill = Sexo, label=Sexo)) + 
  geom_histogram()
#Recodificar o sexo

#Versão 3 - Organizando o tamanho e formato e salvar em um objeto
Gra1 <- ggplot(BraMenor, aes(x=Idade, fill = Sexo, label=Sexo)) + 
  geom_histogram(alpha=0.9, size= 5) 

#Versão 4 
Gra1 + labs(title = "Idade por sexo", y= "Frequência",
            x = "Idade") + theme_classic() +  
  theme(legend.position = "none")  +
  theme(text = element_text(family = "serif", size = 12),
        rect = element_blank(),
        panel.grid = element_blank(),
        title = element_text(color = "black"),
        axis.line = element_line(color = "black"))

#Salvando as opções da fonte:
Fonte <- theme(text = element_text(family = "serif", size = 12),
               rect = element_blank(),
               panel.grid = element_blank(),
               title = element_text(color = "black"),
               axis.line = element_line(color = "black"))

BraMenor$idnum
#Outro gráfico
#Boxplot

#Versão 1
ggplot(BraMenor, aes(x= Idade, y= idnum))+  #Linha simples
  geom_boxplot()

#Versão 2 - Colocando a média
ggplot(BraMenor, aes(x= Sexo, y= Idade, fill= Sexo))+  #Linha simples
  geom_boxplot() + stat_summary(fun.y = mean, geom =  "point")
  
#Versão 3 melhorando o shape da média  + mudando a cor
ggplot(BraMenor, aes(x= Sexo, y= Idade, fill= Sexo))+  #Linha simples
  geom_boxplot() + stat_summary(fun.y = mean, geom =  "point", 
    shape = 20, size = 5, col = "red") +
  scale_fill_brewer(palette="Dark2") #Aqui mudamos a escala de cor

#Lembrar de falar sobre o site das cores! 

#versão 4
#Salvar em um objeto
Gra2 <- ggplot(BraMenor, aes(x= Sexo, y= Idade, fill= Sexo))+  #Linha simples
  geom_boxplot() + stat_summary(fun.y = mean, geom =  "point", 
                                shape = 20, size = 5, col = "red") +
  scale_fill_brewer(palette="Dark2")

Gra2 + Fonte + labs(title = "Sexo e idade")
