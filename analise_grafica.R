### Exploração e análise de dados

if(!require(ggplot2)) install.packages("ggplot2")
if(!require(plotly)) install.packages("plotly")
if(!require(forcats)) intall.packages("forcats")
library(forcats)
library(dplyr)
library(ggplot2)
library(plotly)

setwd("~/rdata/projeto-2/")

# Sindrome respiratória aguda grave
srag_sp <- read.csv('SRAG_2020.csv', sep=";")
srag_sp <- subset(srag_sp, select=-c(51:133))
srag_sp <- subset(srag_sp, select=-c(5:8))
srag_sp <- subset(srag_sp, select=-c(6,8))

glimpse(srag_sp)

srag_sp$DT_NOTIFIC <- as.Date(srag_sp$DT_NOTIFIC, format="%m/%d/%Y")
srag_sp <- rename(srag_sp, sexo=CS_SEXO, idade=NU_IDADE_N)      

# Sapply -> aplicando para todos
sapply(srag_sp, function(x) sum(is.na(x))) 
sapply(srag_sp, function(x) sum(is.nan(x)))


# Charts
grafico_barras <- table(srag_sp$sexo)
barplot(grafico_barras, col="blue")

# Com o ggplot
ggplot(srag_sp, aes(x = sexo)) +
  geom_bar(fill = 'red') +
  labs(title= "Quantidade por sexo",
       subtitle= "SRAG",
       x="Sexo", y="Contagem")

## GRÁFICO POR RAÇA
# LIMPEZA
srag_sp <- rename(srag_sp, raca=CS_RACA)
# Verificando valores faltantes
sapply(srag_sp, function(x) sum(is.na(x)))
# Modificando dados raça faltantes
# Filtrando dentro de raça toso que forem na
srag_sp$raca[which(is.na(srag_sp$raca))] <- 6
srag_sp$raca[srag_sp$raca == 9] <- 6

racas <- c("Branca", "Preta", "Amarela", "Parda", "Indigena", "Ignorada")
for (i in 1:6) {
  srag_sp$raca[srag_sp$raca == i] <- racas[i]
}

# Contagem
srag_sp$raca <- fct_infreq(srag_sp$raca) # Ordena pela frequência
srag_sp$raca <- fct_rev(srag_sp$raca)   # Reverte a ordem para decresce

# Calcular a média das contagens por raça
mean_value <- mean(as.numeric(table(srag_sp$raca)))

# Criar o gráfico
ggplot(srag_sp, aes(x = raca)) +
  geom_bar(fill = 'gray', width = 0.6) +
  coord_flip() + 
  geom_hline(yintercept = mean_value, color = "gray", linetype = "dashed") +
  annotate("text", x = length(levels(srag_sp$raca)) + 0.5, y = mean_value *1.05, 
           label = "Média de quantia", color = "gray", hjust = 0, size = 2.5) +
  labs(title = "Quantidade por raça", x = "Raça", y = "Contagem") +
  theme_minimal()

# Gráfico por sexo, raça e região
# Limpando dados de regiao
srag_sp <- rename(srag_sp, regiao=CS_ZONA)
srag_sp$regiao[which(is.na(srag_sp$regiao))] <- 4
regioes <- c("Urbana", "Rural", "Periurbana", "Ignorada")
for (i in 1:4) { srag_sp$regiao[srag_sp$regiao == i] <- regioes[i] }

ggplot(srag_sp, aes(x=raca, y=sexo, fill= factor(regiao))) +
  geom_col(position = "dodge") +
  labs(title="Região por sexo e raça", x= "Raça", y="Sexo", fill="Região")

## Boxplot por idade
srag_sp$idade[srag_sp$TP_IDADE == 2] <- 0
srag_sp$idade[srag_sp$TP_IDADE == 1] <- 0

summary(srag_sp$idade)
boxplot(srag_sp$idade)

# Melhorando
srag_sp_idades <- srag_sp %>% filter(TP_IDADE != 0)
quartis <- quantile(srag_sp_idades$idade)
iqr <- IQR(srag_sp_idades$idade)
lim_inf <- quartis[2] - (1.5 * iqr)
lim_sup <- quartis[4] + (1.5 * iqr)
srag_sp_idades <- srag_sp_idades %>% filter(srag_sp_idades$idade >= lim_inf & 
                                srag_sp_idades$idade <= lim_sup)

boxplot(srag_sp_idades$idade)

ggplot(srag_sp_idades, aes(x= " ", y=idade)) +
  geom_boxplot(width = .3, outlier.color = "purple")

# Colocando gráficos na mesma visualização
par(mfrow=c(1,2))
boxplot(srag_sp$idade, ylab="Idade com outliers")
boxplot(srag_sp_idades$idade, ylab="Idade sem outliers")

# Mosaico gráfico
par(mfrow=c(2,2))
boxplot(srag_sp$idade, ylab="Idade com outliers")
boxplot(srag_sp_idades$idade, ylab="Idade sem outliers")
barplot(grafico_barras, col="blue")
barplot(table(srag_sp$raca), col="red")

### Histograma, estatística descritiva e normalidade
hist(srag_sp$idade, probality=T, col="lightblue")
lines(density(srag_sp$idade), col="blue")

qqnorm(srag_sp_idades$idade, col="lightblue")
qqline(srag_sp_idades$idade, col="blue")

# Com ggplot
ggplot(data=srag_sp, aes(x=idade)) +
  geom_histogram(fill = 'lightblue', bins=25)