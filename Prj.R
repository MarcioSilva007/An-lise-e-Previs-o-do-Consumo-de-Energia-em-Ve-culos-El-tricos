# Instalçaõ de pacotes 
install.packages("readxl")

# Liberando pacotes para uso
library(readxl)
library(dplyr)
library(tidyverse)
library(corrplot)
library(ggplot2)
library(randomForest)
library(caret)


# Importação de dados
dados <- read_excel("FEV-data-Excel.xlsx")

# Visualizando DF
View(dados)

# Verificando tipo  
class(dados)

# Verificando Tipo Coluna
str(dados)

# Verificando dimensões 
dim(dados)

# Verificando ocorrência de valores NA
colSums(is.na(dados)) 

# Sumário do modelo
summary(dados)

# Nome das variaveis 
colnames(dados)

# Removendo Colunas
dados <- dados[,-c(1,12,13,14,22,24)]

# Renomeando Varavéis

myColumns <- colnames(dados)
myColumns

myColumns[1] <- "NomeCar"
myColumns[2] <- "Marca"
myColumns[3] <- "Modelo"
myColumns[4] <- "PrecoMinimoBruto"
myColumns[5] <- "PotenciaMotor"
myColumns[6] <- "TorkMaximo"
myColumns[7] <- "TipoDeFreio"
myColumns[8] <- "TipoDeUnidae"
myColumns[9] <- "CapacidadeBateria-kwh"
myColumns[10] <- "Alcance"
myColumns[11] <- "PesoVazioMinimo"
myColumns[12] <- "PesoBrutoAdmissivel"
myColumns[13] <- "CapacidadeMaximoCarga"
myColumns[14] <- "NumeroAssentos"
myColumns[15] <- "NumerosPortas"
myColumns[16] <- "TamanhoPneu"
myColumns[17] <- "VelocidadeMaxima"
myColumns[18] <- "Velocidade_0_a_100"
myColumns[19] <- "MediaConsumoEnergia"



colnames(dados) <- myColumns
rm(myColumns)
View(dados)

# Variáveis e tipos de dados
str(dados)
names(dados)

# Como são poucos os valores NA e um conjunto de dados pequenos irei atribuir a mediana para os valores nulos 
dados <- lapply(dados, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
dados <- as.data.frame(dados)

# Separando Informações
df <- dados %>% separate(TorkMaximo, into = c("disc", "DianteiroTraseiro"), sep = "\\(")
df <- df %>% separate(DianteiroTraseiro, into = c("front", "rear"), sep = "\\+")
df <- df %>% separate(TipoDeFreio, into = c("TipoFreio", "ModeloFreio"), sep = "\\(")

# Removendo Coluna em Modelo Freio Pois tem muitos valores NA
df$ModeloFreio <- NULL

View(df)

# Transformando Váriaveis em fator
df$TipoFreio <- as.factor(df$TipoFreio)
as.factor(df$TipoFreio)

# Removendo Caracter de uma coluna
df$rear <- gsub(")", "", df$rear)
df$front <- gsub(")", "", df$front)

# Colando todos os valores de disco em disco
df$disc <- c("disco")
df$front <- c("dianteiro")
df$rear <- c("traseiro")

colSums(is.na(df)) 

# Verificando Tipo Coluna
str(df)
rm(dados)

###### Analise Exploratória ######
# Obtendo apenas as colunas numéricas
colunas_numericas <- sapply(df, is.numeric)
colunas_numericas

# Filtrando as colunas numéricas para correlação
data_cor <- cor(df[,colunas_numericas])
data_cor
head(data_cor)

# Criando um corrplot
corrplot(data_cor, method = 'color')

# Construção do boxplot
sleepboxplot = boxplot(data = df, MediaConsumoEnergia ~ Velocidade_0_a_100,
                       main = "Mediando consumo media com a Aceleração", horizontal = T,
                       col.main = "red", ylab = "Acelaração", xlab = "Media")
# Histogramas da Velocidade
hist(df$MediaConsumoEnergia, labels = T, breaks = 5, main = "Media de Consumo")
hist(df$Velocidade_0_a_100, labels = T, breaks = 5, main = "Media de Consumo")

# O aumento da velocidade afeta positivamente o consumo
plot(df$MediaConsumoEnergia, df$VelocidadeMaxima)
cor.test(df$MediaConsumoEnergia, df$VelocidadeMaxima, method = "pearson")

# O aumento de pso afeta positivamente o consumo de energia
plot(df$MediaConsumoEnergia, df$PesoBrutoAdmissivel)
cor.test(df$MediaConsumoEnergia, df$PesoBrutoAdmissivel, method = "pearson")

# A potencia do moter afeta positivamente o consumo de energia
plot(df$MediaConsumoEnergia, df$PotenciaMotor)
cor.test(df$MediaConsumoEnergia, df$PotenciaMotor, method = "pearson")

#########
Model1 <- lm(MediaConsumoEnergia ~ PotenciaMotor, data = df)
summary(Model1)

ggplot(df, aes(x = MediaConsumoEnergia, y = PotenciaMotor)) +    
  geom_point(shape = 1) +  
  geom_smooth(method = lm , color = "red", se = FALSE) 

ggplot(df, aes(x = MediaConsumoEnergia, y = PotenciaMotor)) +    
  geom_bar() +  
  geom_smooth(method = lm , color = "red", se = FALSE)  

Importancia <- randomForest(MediaConsumoEnergia ~ ., data = df, importance =TRUE)
varImpPlot(Importancia)
importance(Importancia)

# Separando Váriaveis que vou usar para o treino
df1 <- subset(df, select = -c(NomeCar, disc, Marca, front, rear, TipoFreio, TipoDeUnidae, PesoVazioMinimo, NumeroAssentos, NumerosPortas, TamanhoPneu))
View(df1)


# Criando Modelo
splitData <- function(dataframe, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)*0.8))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset = trainset, testset = testset)
}

# Gerando dados de treino e de teste
dataframe <- splitData(df1 )

# Separando os dados
dados_treino <- dataframe$trainset
dim(dados_treino)
dados_teste <- dataframe$testset
dim(dados_teste)


modelo1 <- randomForest( MediaConsumoEnergia ~ ., data =dados_treino, ntree = 100, nodesize = 10)
print(modelo1)

# Previsões
previsoes <- data.frame(observado = dados_teste$MediaConsumoEnergia,
                        previsto = predict(modelo1, newdata = dados_teste))

# Visualizando o resultado
View(previsoes)
View(dados_teste)

ggplot(previsoes, aes(x = observado, y = previsto)) +    
  geom_point(shape =3) +  
  geom_smooth(method = lm , color = "red")  

# Porcentagem de acerto
totalobs <- sum(previsoes$observado) 
totalobs
totalpervis <- sum(previsoes$previsto)

divi <- (totalpervis / totalobs) *100

print(paste('Acurácia do Modelo', divi))