#carregar as bibliotecas 
library(readr)
library(readxl)
library(dplyr)
library(tidyverse)
library(writexl)
library(tidyr)
library(ggplot2)
library(survival)
library(ggfortify)
library(ranger)
library(knitr)
library(tibble)
library(lubridate)
library(ggsurvfit)
library(gtsummary)
library(tidycmprsk)
library(webshot2)

#definindo a pasta de trabalho e carregando o banco de dados
setwd("C:\\Users\\buril\\OneDrive\\�rea de Trabalho\\Trabalho de Disserta��o")
dados_consorcio <- read_xlsx("base_final_teste.xlsx")

class(dados_consorcio$POPULA��O)

dados_consorcio$POPULA��O <- as.numeric(dados_consorcio$POPULA��O)


dados_consorcio <- dados_consorcio %>%
mutate(POPULA��O = case_when(POPULA��O< 5000 ~ "At� 5.000 habitantes",
                             POPULA��O>= 5000 & POPULA��O < 10000 ~ "5.001 a 10.000 habitantes",
                             POPULA��O >= 10000 & POPULA��O < 50000 ~ "10.001 a 50.000 habitantes",
                             POPULA��O>= 50000 & POPULA��O < 100000 ~ "50.001 a 100.000 habitantes",
                             POPULA��O >= 100000 & POPULA��O < 500000 ~ "100.001 a 500.000 habitantes",
                             POPULA��O >= 500000 ~ "Mais de 500.000 habitantes"))


## Municipios Consorciados ##

ggplot(dados_consorcio, aes(CONS�RCIO)) +
geom_bar(fill = "cornflowerblue") +
ggtitle("Quantitativo de Munic?pios consorciados") +
xlab("Munic?pios") + ylab("Contagem") +
theme(plot.title = element_text(color="black", size=12, face="bold"),
      axis.title.x = element_text(color="black", size=10, face="bold"),
      axis.title.y = element_text(color="black", size=10, face="bold"))

## frequ?ncia por ano 

ggplot(dados_consorcio, aes(ANO_FORMA��O)) +
geom_histogram(aes(y=..count..),      
               binwidth=1, fill = "cornflowerblue") +
geom_density() +
ggtitle("Quantitativo de Cons?rcios formados ao longo do tempo") +
xlab("Ano") + ylab("Quantitativo") +
theme(plot.title = element_text(color="black", size=12, face="bold"),
      axis.title.x = element_text(color="black", size=10, face="bold"),
      axis.title.y = element_text(color="black", size=10, face="bold"))


## atua??o por ano 
#ggplot(dados_consorcio, aes(x = ATUACAO, y = ANO_FORMA??O)) + 
#geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
# ggtitle("?rea de atua??o por Ano de Forma??o") +
#xlab("?rea de Atua??o") + ylab("Ano") +
# theme(plot.title = element_text(color="black", size=12, face="bold"),
      #axis.title.x = element_text(color="black", size=10, face="bold"),
    #  axis.title.y = element_text(color="black", size=10, face="bold"))


## municipios consorciados por regi?o

dados_teste <- dados_consorcio %>%
filter(CONS�RCIO_A_PARTIR_DA_LEI == 1)

## Capacidade fiscal por Regi?o

ggplot(dados_teste, aes(CAPACIDADEFISCAL_NO_ANOADO��O, y = "")) +
geom_jitter(size = 1, color = "cornflowerblue") +
facet_wrap(vars(REGI�O))+
ggtitle("Capacidade fiscal dos munic?pios consorciados no Ano de Ado??o por Regi?o") +
xlab("Capacidade Fiscal") + ylab("Munic?pios Consorciados") +
theme(plot.title = element_text(color="black", size=10, face="bold"),
      axis.title.x = element_text(color="black", size=10, face="bold"),
      axis.title.y = element_text(color="black", size=10, face="bold"))


## Munic?pios consorciados por Regi?o
ggplot(dados_teste, aes(REGI�O, ..count..)) +
geom_bar(alpha = 1, fill = "cornflowerblue") +
ggtitle("Munic�pios Consorciados Por Regi�o") +
xlab("Região") + ylab("Munic?pios Consorciados") +
theme(plot.title = element_text(color="black", size=12, face="bold"),
      axis.title.x = element_text(color="black", size=10, face="bold"),
      axis.title.y = element_text(color="black", size=10, face="bold"))


dados_teste %>%
count(REGI�O, sort = T)

## Cons?rcio por ?rea de atua??o
ggplot(dados_consorcio, aes(AREA_TEMATICA)) +
geom_bar(alpha = 1, fill = "cornflowerblue") +
ggtitle("Cons�rcios por �rea Tem�tica") +
xlab("Atua��o") + ylab("Contagem") +
theme(plot.title = element_text(color="black", size=12, face="bold"),
      axis.title.x = element_text(color="black", size=10, face="bold"),
      axis.title.y = element_text(color="black", size=10, face="bold")) +
scale_x_discrete(limits = c("Seguran�a", "Cultura", "Turismo", "Infraestrutura", "Meio Ambiente", "Desenvolvimento", "Sa�de"))


frequencia <- table(dados_consorcio$AREA_TEMATICA)

print(frequencia)
## tamanho da popula??o dos munic?pios consorciados


ggplot(dados_teste, aes(POPULA��O)) +
geom_bar(alpha = 1, fill = "cornflowerblue") +
ggtitle("Tamanho Populacional dos munic�pios consorciados") +
xlab("Popula��o") + ylab("Contagem") +
theme(plot.title = element_text(color="black", size=12, face="bold"),
      axis.title.x = element_text(color="black", size=10, face="bold"),
      axis.title.y = element_text(color="black", size=10, face="bold")) +
scale_x_discrete(limits = c("Mais de 500.000 habitantes", "100.001 a 500.000 habitantes", "50.001 a 100.000 habitantes", "5.001 a 10.000 habitantes", "Até 5.000 habitantes", "10.001 a 50.000 habitantes"))


frequencia <- table(dados_consorcio$POPULA��O)

print(frequencia)

## tamanho da população por capacidade fiscal 


ggplot(dados_teste, aes(x=POPULA��O, y=CAPACIDADEFISCAL_NO_ANOADO��O)) + 
geom_boxplot(outlier.colour="red", outlier.shape=8,
             outlier.size=2, fill = "cornflowerblue")+
ggtitle("Popula��o municipal por capacidade fiscal dos munic�pios consorciados") +
xlab("Popula��o") + ylab("Capacidade Fiscal") +
theme(plot.title = element_text(color="black", size=12, face="bold"),
      axis.title.x = element_text(color="black", size=10, face="bold"),
      axis.title.y = element_text(color="black", size=10, face="bold")) +
scale_x_discrete(limits = c("At� 5.000 habitantes", "5.001 a 10.000 habitantes", "10.001 a 50.000 habitantes", "50.001 a 100.000 habitantes", "100.001 a 500.000 habitantes", "Mais de 500.000 habitantes"))



###### tutorial 1 da an�lise de sobreviv�ncia #########
#construindo o objeto de sobrevivência padrão
teste <- with(dados_consorcio, Surv(TEMPO_PARA_FORMA��O, CONS�RCIO))
head(teste, 80)

#Para iniciar nossa análise, usamos a fórmula Surv(futime, status) ~ 1 e a função survfit() para produzir as estimativas de Kaplan-Meier da probabilidade de sobrevivência ao longo do tempo.
km_fit <- survfit(Surv(TEMPO_PARA_FORMA��O, CONS�RCIO) ~ 1, data=dados_consorcio)
summary(km_fit)

survfit(Surv(TEMPO_PARA_FORMA��O, CONS�RCIO) ~ 1, data = dados_consorcio) %>% 
  tbl_survfit(
    times = 1,
    label_header = "**1-year survival (95% CI)**"
  )

str(km_fit)

autoplot(km_fit, xlab = "Anos", ylab = "Probabilidade de Sobreviv�ncia", main = 'Kaplan Meier Plot')

#Em seguida, analisamos as curvas de sobrevivência por tratamento.
km_trt_fit <- survfit(Surv(TEMPO_PARA_FORMA��O, CONS�RCIO) ~ REG_SUDESTE, data=dados_consorcio)
autoplot(km_trt_fit, xlab = "Anos", ylab = "Probabilidade de Sobreviv�ncia", main = 'Kaplan Meier Plot')

km_trt_fit <- survfit(Surv(TEMPO_PARA_FORMA��O, CONS�RCIO) ~ REG_SUL, data=dados_consorcio)
autoplot(km_trt_fit, xlab = "Anos", ylab = "Probabilidade de Sobreviv�ncia", main = 'Kaplan Meier Plot')


#Em seguida, ajustarei um modelo de riscos proporcionais de Cox que faz uso de todas as covariáveis no conjunto de dados.
cox <- coxph(Surv(TEMPO_PARA_FORMA��O, CONS�RCIO) ~ CAPACIDADEFISCAL_NO_ANOADO��O + POPULA��O + N�meroConsorciadosPorRegi�o_por_Anodeado��o + AREA_TEMATICA, data = dados_consorcio)
summary(cox)
cox_fit <- survfit(cox)
autoplot(cox_fit, xlab = "Anos", ylab = "Probabilidade de Sobreviv�ncia", main = 'Kaplan Meier Plot')

t1 <- coxph(Surv(TEMPO_PARA_FORMA��O, CONS�RCIO) ~ POPULA��O +  AREA_TEMATICA + REGI�O, data = dados_consorcio) %>% 
 tbl_regression(exp = TRUE, 
               label = list(AREA_TEMATICA ~ "�rea Tem�tica")) %>%
  modify_table_styling(
    columns = ci,
    rows = reference_row %in% TRUE,
    missing_symbol = "Refer�ncia"
  ) %>%
  bold_labels() %>%
  bold_p() %>%
  italicize_levels()

print(t1)


t2 <- coxph(Surv(TEMPO_PARA_FORMA��O, CONS�RCIO) ~ POPULA��O +  AREA_TEMATICA + REGI�O + N�meroConsorciadosPorRegi�o_por_Anodeado��o + CAPACIDADEFISCAL_NO_ANOADO��O, data = dados_teste) %>% 
  tbl_regression(exp = TRUE, 
                 label = list(AREA_TEMATICA ~ "�rea Tem�tica")) %>%
  modify_table_styling(
    columns = ci,
    rows = reference_row %in% TRUE,
    missing_symbol = "Refer�ncia"
  ) %>%
  bold_labels() %>%
  bold_p() %>%
  italicize_levels()

print(t2)

gt::gtsave(as_gt(t1), file = file.path(tempdir(), "temp.png"))

gt::gtsave(as_gt(t2), file = file.path(tempdir(), "temp.png"))


##A quantidade de interesse de um modelo de regress�o de Cox � uma taxa de risco (HR). A HR representa a propor��o de perigos entre dois grupos em qualquer ponto espec�fico no tempo. 
# A HR � interpretada como a taxa instant�nea de ocorr�ncia do evento de interesse naqueles que ainda est�o em risco para o evento. N�o � um risco, embora seja comumente mal interpretado como tal. Se voc� tiver um par�metro de regress�o ??
#ent�o HR = exp(??). Um HR < 1 indica risco reduzido de morte, enquanto um HR > 1 indica um risco aumentado de morte.
#Assim, o HR = 0,59 implica que 0,59 vezes mais mulheres est�o morrendo do que homens, a qualquer momento. Dito de outra forma, as mulheres t�m um risco de morte significativamente menor do que os homens nesses dados

#Os gráficos mostram como os efeitos das covariáveis mudam ao longo do tempo. Observe a inclinação íngreme e a mudança abrupta na inclinação de Karno.

aa_fit <- aareg(Surv(TEMPO_PARA_FORMA��O, CONS�RCIO) ~ CAPACIDADEFISCAL_NO_ANOADO��O + POPULA��O + N�meroConsorciadosPorRegi�o_por_Anodeado��o + AREA_TEMATICA, 
             data = dados_consorcio)
 
aa_fit

autoplot(aa_fit)


aa_fit_cat <- aareg(survConcordance(TEMPO_PARA_FORMA��O, CONS�RCIO) ~ CAPACIDADEFISCAL_NO_ANOADO��O + POPULA��O + N�meroConsorciadosPorRegi�o_por_Anodeado��o + AREA_TEMATICA,  data = dados_consorcio)


#ranger() constrói um modelo para cada observação no conjunto de dados. O próximo bloco de código constrói o modelo usando as mesmas variáveis usadas no modelo de Cox acima e plota vinte curvas aleatórias, juntamente com uma curva que representa a média global para todos os pacientes. 

dados_na <- dados_consorcio[!is.na(dados_consorcio$N�meroconsorsiadoranterioresporUF_por_anodeado��o),]
dados_na <- dados_consorcio[!is.na(dados_consorcio$CAPACIDADEFISCAL_NO_ANOADO��O),]


r_fit <- ranger(Surv(TEMPO_PARA_FORMA��O, CONS�RCIO) ~ LogPopula��o_no_anoado��o + CAPACIDADEFISCAL_NO_ANOADO��O + 
                N�meroconsorsiadoranterioresporUF_por_anodeado��o, data = dados_na,
              mtry = 3,
              importance = "permutation",
              splitrule = "extratrees",
              verbose = TRUE)

adocao <- r_fit$unique.death.times 
surv_prob <- data.frame(r_fit$survival)
avg_prob <- sapply(surv_prob,mean)

plot(r_fit$unique.death.times,r_fit$survival[1,], 
   type = "l", 
   ylim = c(0,1),
   col = "red",
   xlab = "Anos",
   ylab = "Adoção",
   main = "Curva de Sobrevivência Municipal")

cols <- colors()
for (n in sample(c(2:dim(dados_na)[1]), 20)){
lines(r_fit$unique.death.times, r_fit$survival[n,], type = "l", col = cols[n])
}
lines(death_times, avg_prob, lwd = 2)
legend(500, 0.7, legend = c('Average = black'))

vi <- data.frame(sort(round(r_fit$variable.importance, 4), decreasing = TRUE))
names(vi) <- "importance"
head(vi)

cat("Prediction Error = 1 - Harrell's c-index = ", r_fit$prediction.error)

###### tutorial 2 da an�lise de sobreviv�ncia #########

survival::survfit()

sobrevivencia <- survfit2(Surv(TEMPO_PARA_FORMA��O, CONS�RCIO) ~ 1, data = dados_consorcio) %>% 
ggsurvfit() +
labs(
  x = "Anos",
  y = "Overall survival probability"
) +
add_confidence_interval() +
add_risktable()

plot(sobrevivencia)

## probabilidade de sobreviv�ncia com 1, 5 e -5 anos do advento da lei dos cons�rcios p�blicos intermunicipais

summary(survfit(Surv(TEMPO_PARA_FORMA��O, CONS�RCIO) ~ 1, data = dados_consorcio), times = -5) 

summary(survfit(Surv(TEMPO_PARA_FORMA��O, CONS�RCIO) ~ 1, data = dados_consorcio), times = 1)

summary(survfit(Surv(TEMPO_PARA_FORMA��O, CONS�RCIO) ~ 1, data = dados_consorcio), times = 5)

## n�s descobrimos que 1 ano de probabilidade de sobrevivencia no estudo � de 37% e em 5 anos � de 0.05% ## 
## caso olhemos para antes da lei, vemos que a probabilidade de sobreviv�ncia aumenta, sugerindo que a lei � um marco 
## relevante para os cons�rcios p�blicos intermunicipais
## n�o podemos ignorar a censura dos cons�rcios p�blicos pois leva a uma superestima��o
## da probabilidade de sobreviv�ncia geral. Imagine dois estudos, cada um com 228 munic�pios. 
## H� 165 cons�rcios em cada estudo. A censura � ignorada em uma (linha azul), a censura � contabilizada na outra (linha amarela). 
## Os sujeitos censurados contribuem com informa��es apenas durante uma parte do tempo de acompanhamento e, em seguida, saem do conjunto de risco, reduzindo assim a probabilidade cumulativa de sobreviv�ncia. 
## Ignorar a censura trata erroneamente os pacientes censurados como parte do risco definido para todo o per�odo de acompanhamento.
## posso fazer esse exemplo rodando apenas os munic�pios consorciados com o n�o consorciado


## tabelas legais para a probabilidade de consorciamento intermunicipal ## 
survfit(Surv(TEMPO_PARA_FORMA��O, CONS�RCIO) ~ 1, data = dados_consorcio) %>% 
tbl_survfit(times = c(-2, 1, 5), label_header = "**Probabilidade de Sobreviv�ncia decorrido {time} anos ap�s o advento da Lei (95% CI)**") 

## estimando o tempo m�dio de sobreviv�ncia - Outra quantidade frequentemente de interesse em uma an�lise de sobreviv�ncia � o tempo m�dio de sobreviv�ncia, que quantificamos usando a mediana. 
## N�o se espera que os tempos de sobreviv�ncia sejam normalmente distribu�dos, portanto, a m�dia n�o � um resumo apropriado.

survfit(Surv(TEMPO_PARA_FORMA��O, CONS�RCIO) ~ 1, data = dados_na)

## Vemos que o tempo m�dio de sobreviv�ncia � de 4 anos. 
# Os limites inferior e superior do intervalo de confian�a de 95% tamb�m s�o exibidos.

## gerando uma tabela do tempo m�dio de sobreviv�ncia pela mediana ## aqui o tempo s�o 3 anos ap�s a forma��o dos cons�rcios p�blicos

survfit(Surv(TEMPO_PARA_FORMA��O, CONS�RCIO) ~ 1, data = dados_na) %>% 
tbl_survfit(
  probs = 0.5,
  label_header = "**Tempo m�dio de Sobreviv�ncia estimado pela Mediana (95% CI)**"
)

## comparando a sobreviv�ncia entre grupos 

survdiff(Surv(TEMPO_PARA_FORMA��O, CONS�RCIO) ~ POPULA��O, data = dados_consorcio)

## encontramos um p-valor estatisticamente significante para o modelo rodado, h� diferen�a entre grupos.
## A primeiro m�trica do teste do survdiff est� relacionada ao teste qui-quadrado. O segundo � o teste log-rank que conhe�o. 
## O problema com a execu��o de um teste qui-quadrado em estat�sticas de sobreviv�ncia � que os testes em v�rios pontos de tempo 
# n�o s�o independentes (ou seja, para ser considerado para um ponto de tempo posterior, 
# algu�m deve sobreviver a um ponto de tempo anterior), ent�o eu n�o recomendaria us�-lo.
# Em geral, com an�lise de sobreviv�ncia, use a segunda variante (na verdade, acho que � a metodologia principal).
## Pode-se mostrar que a segunda variante se aproxima de uma distribui��o z (N(0,1)) e uma distribui��o qui-quadrada com um grau de liberdade ao elevar ao quadrado a estat�stica conforme mostrado aqui.
## o termo (O-E)^2/V � a base para o teste de log-rank. Com exce��o do grupo at� 5.000 habitantes e o grupo entre 10.001 a 50.000,
## todos os outros grupos tem fun��o de risco estatisticamente significante. Essa semelhan�a dos dois grupos
## pode sugerir que o processo de difus�o institucional nesse eixo populacional ocorreu de forma semelhante.
## s�o tamb�m os grupos com maior densidade consorciativa. 

#Na Parte 1, abordamos o uso de testes log-rank e regress�o de Cox para examinar associa��es entre covari�veis de interesse e resultados de sobreviv�ncia. 
# Mas essas an�lises dependem da covari�vel sendo medida na linha de base, ou seja, antes do in�cio do tempo de acompanhamento do evento. 
# O que acontece se voc� estiver interessado em uma covari�vel que � medida ap�s o in�cio do tempo de acompanhamento?

survfit2(Surv(TEMPO_PARA_FORMA��O, CONS�RCIO) ~ AREA_TEMATICA, data = dados_teste) %>% 
  ggsurvfit() +
  labs(
    x = "Dias ap�s a cria��o da lei do cons�rcio p�blico",
    y = "Probabilidade de Sobreviv�ncia Geral"
  ) +
  add_risktable()


coxph(
  Surv(TEMPO_PARA_FORMA��O, CONS�RCIO) ~ AREA_TEMATICA, 
  data = dados_teste
) %>% 
  tbl_regression(exp = TRUE)

##Riscos proporcionais

mv_fit <- coxph(Surv(TEMPO_PARA_FORMA��O, CONS�RCIO) ~ REGI�O + AREA_TEMATICA, data = dados_consorcio)
cz <- cox.zph(mv_fit)
print(cz)

plot(cz)

## Aqui vemos que com valores de p < 0,05, 
#rejeitamos a hip�tese nula e conclu�mos que a suposi��o de riscos proporcionais 
# � satisfeita para cada covari�vel individual e tamb�m para o modelo geral




