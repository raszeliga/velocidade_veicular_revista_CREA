require(RColorBrewer)
library(ggplot2)
library(dplyr)
library(sciplot)
require (nortest)
require(agricolae)


DadosRaio <- read.delim("https://raw.githubusercontent.com/raszeliga/velocidade_veicular_revista_CREA/main/Raios.txt")
head(DadosRaio)
summary(DadosRaio)

azuis <- brewer.pal(4, "Blues")
laranjas <- brewer.pal(4, "Oranges")

# Adicionar coluna Faixa3 com os intervalos dos raios
# Será usado para construir os Boxplot
DadosRaio <- mutate(DadosRaio, Faixa3 = case_when(
  Raio < 5 ~ "1 - Menor que 5",
  Raio >= 5 & Raio <= 7 ~ "2 - Entre 5 e 7",
  Raio > 7 & Raio <= 10 ~ "3 - Entre 7 e 10",
  Raio > 10 ~ "4 - Maior que 10"
))

# Filtrando os dados para extrair as estatísticas descritivas
Dados_Menor_que_5 <- DadosRaio %>% filter(Raio <= 5)
Dados_Entre_5_e_7 <- DadosRaio %>% filter(Raio > 5 & Raio <= 7)
Dados_Entre_7_e_10 <- DadosRaio %>% filter(Raio > 7 & Raio <= 10)
Dados_Maior_que_10 <- DadosRaio %>% filter(Raio > 10)

# Estatísticas para Velocidades na Curva e DeltaV
EstDesc <- function(dados, coluna) {
  dim_dados <- dim(dados)
  summary_deltav <- summary(dados[[coluna]])
  desv_pad <- round(sd(dados[[coluna]]), 2)
  
  resultado <- list(
    dim_dados = dim_dados,
    summary_deltav = summary_deltav,
    desv_pad = desv_pad
  )
  
  return(resultado)
}

EstDesc(Dados_Menor_que_5, "Velc")
EstDesc(Dados_Entre_5_e_7, "Velc")
EstDesc(Dados_Entre_7_e_10, "Velc")
EstDesc(Dados_Maior_que_10, "Velc")

EstDesc(Dados_Menor_que_5, "DeltaV")
EstDesc(Dados_Entre_5_e_7, "DeltaV")
EstDesc(Dados_Entre_7_e_10, "DeltaV")
EstDesc(Dados_Maior_que_10, "DeltaV")

## ANÁLISES VELOCIDADE NA CURVA, GRÁFICOS E TESTES ##

boxplot2 <- ggplot(DadosRaio, aes(x=Faixa3, y = Velc)) +
  geom_boxplot(outlier.colour="red",
               outlier.shape=1,
               outlier.size=3,
               fill=azuis, color="black") +
  labs(title="Velocidades na curva (km/h)",x="Raios (m)", y = "Velocidade (km/h)")

boxplot2 + stat_summary(fun=mean, geom="point", shape=1, size=2) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

## ANÁLISES DELTA-V, GRÁFICOS E TESTES ##

boxplot3 <- ggplot(DadosRaio, aes(x=Faixa3, y = DeltaV)) +
  geom_boxplot(outlier.colour="red",
               outlier.shape=1,
               outlier.size=3,
               fill=laranjas, color="black") +
  labs(title="Variação das Velocidades (km/h)",x="Raios (m)", y = "Variação das Velocidades (km/h)")

boxplot3 + stat_summary(fun=mean, geom="point", shape=1, size=2)

## TESTES ANOVA PARA DELTA-V E VELC ##

ad.test(DadosRaio$DeltaV) ## teste de Normalidade Anderson-Darling ##
lillie.test(DadosRaio$DeltaV)   ## teste de Normalidade Kolmogorov-Smirnov ##
modelRaio <- aov(DeltaV ~ Faixa3, data = DadosRaio)
modelRaio
summary(modelRaio)
tukeyRaio <- HSD.test (modelRaio, "Faixa3", alpha = 0.05, group = TRUE, main = "Agrupamentos", unbalanced=FALSE, console=FALSE)
plot(tukeyRaio)

shapiro.test(modelRaio$residuals) #teste Shapiro-Wilk normalidade dos resíduos
ad.test(modelRaio$residuals) #teste Anderson-Darling normalidade dos resíduos
lillie.test(modelRaio$residuals)  ## teste Kolmogorov-Smirnov normalidade dos residuos
hist(modelRaio$res, main="", xlab = "Resíduos Variação da Velocidade",ylab = "Frequência") #Graficamente

# Teste de Homocedasticidade os dados atendem o pressuposto de normalidade da ANOVA se p-valor > 0,05.
bartlett.test(DeltaV~Faixa3, DadosRaio)

#Ainda e possivel avaliar algum tipo de dependencia atraves da ordenacao dos residuos, 
#caso exista uma ordem de obtencao dos dados conhecida:
#plot(model$fit, order(model$res), xlab="Valores ajustados", ylab="Resíduos")
#title("Resíduos vs Preditos")

## VERIFICAÇÃO PARA A VELOCIDADE DURANTE A CURVA ##

ad.test(DadosRaio$Velc) ## teste de Normalidade Anderson-Darling ##
lillie.test(DadosRaio$Velc)   ## teste de Normalidade Kolmogorov-Smirnov ##
modelRaio2 <- aov(Velc ~ Faixa3, data = DadosRaio)
modelRaio2
summary(modelRaio2)
tukeyRaio2 <- HSD.test (modelRaio2, "Faixa3", alpha = 0.05, group = TRUE, main = "Agrupamentos", unbalanced=FALSE, console=FALSE)
plot(tukeyRaio2)

shapiro.test(modelRaio2$residuals) #teste Shapiro-Wilk normalidade dos resíduos
ad.test(modelRaio2$residuals) #teste Anderson-Darling normalidade dos resíduos
lillie.test(modelRaio2$residuals)  ## teste Kolmogorov-Smirnov normalidade dos residuos
hist(modelRaio$res, main="", xlab = "Resíduos Velocidade na Curva", ylab = "Frequência") #Graficamente

# Teste de Homocedasticidade os dados atendem o pressuposto de normalidade da ANOVA se p-valor > 0,05.
bartlett.test(Velc~Faixa3, DadosRaio)

t.test(Dados_Maior_que_10$Velc)
t.test(Dados_Entre_7_e_10$Velc)
t.test(Dados_Entre_5_e_7$Velc)
t.test(Dados_Menor_que_5$Velc)

lineplot.CI(DadosRaio$Faixa3, DadosRaio$Velc,
            data = DadosRaio,
            ylab = "Velocidade (km/h)",
            xlab ="Raio (m)",
            col = "darkblue",
            lwd = 1)
