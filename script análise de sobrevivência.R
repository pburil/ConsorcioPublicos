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
setwd("C:\\Users\\buril\\OneDrive\\Área de Trabalho\\Trabalho de Dissertação")
dados_consorcio <- read_xlsx("base_final_teste.xlsx")

class(dados_consorcio$POPULAÇÃO)

dados_consorcio$POPULAÇÃO <- as.numeric(dados_consorcio$POPULAÇÃO)


dados_consorcio <- dados_consorcio %>%
mutate(POPULAÇÃO = case_when(POPULAÇÃO< 5000 ~ "Até 5.000 habitantes",
                             POPULAÇÃO>= 5000 & POPULAÇÃO < 10000 ~ "5.001 a 10.000 habitantes",
                             POPULAÇÃO >= 10000 & POPULAÇÃO < 50000 ~ "10.001 a 50.000 habitantes",
                             POPULAÇÃO>= 50000 & POPULAÇÃO < 100000 ~ "50.001 a 100.000 habitantes",
                             POPULAÇÃO >= 100000 & POPULAÇÃO < 500000 ~ "100.001 a 500.000 habitantes",
                             POPULAÇÃO >= 500000 ~ "Mais de 500.000 habitantes"))


## Municipios Consorciados ##

ggplot(dados_consorcio, aes(CONSÓRCIO)) +
geom_bar(fill = "cornflowerblue") +
ggtitle("Quantitativo de Munic?pios consorciados") +
xlab("Munic?pios") + ylab("Contagem") +
theme(plot.title = element_text(color="black", size=12, face="bold"),
      axis.title.x = element_text(color="black", size=10, face="bold"),
      axis.title.y = element_text(color="black", size=10, face="bold"))

## frequ?ncia por ano 

ggplot(dados_consorcio, aes(ANO_FORMAÇÃO)) +
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
filter(CONSÓRCIO_A_PARTIR_DA_LEI == 1)

## Capacidade fiscal por Regi?o

ggplot(dados_teste, aes(CAPACIDADEFISCAL_NO_ANOADOÇÃO, y = "")) +
geom_jitter(size = 1, color = "cornflowerblue") +
facet_wrap(vars(REGIÃO))+
ggtitle("Capacidade fiscal dos munic?pios consorciados no Ano de Ado??o por Regi?o") +
xlab("Capacidade Fiscal") + ylab("Munic?pios Consorciados") +
theme(plot.title = element_text(color="black", size=10, face="bold"),
      axis.title.x = element_text(color="black", size=10, face="bold"),
      axis.title.y = element_text(color="black", size=10, face="bold"))


## Munic?pios consorciados por Regi?o
ggplot(dados_teste, aes(REGIÃO, ..count..)) +
geom_bar(alpha = 1, fill = "cornflowerblue") +
ggtitle("Municípios Consorciados Por Região") +
xlab("RegiÃ£o") + ylab("Munic?pios Consorciados") +
theme(plot.title = element_text(color="black", size=12, face="bold"),
      axis.title.x = element_text(color="black", size=10, face="bold"),
      axis.title.y = element_text(color="black", size=10, face="bold"))


dados_teste %>%
count(REGIÃO, sort = T)

## Cons?rcio por ?rea de atua??o
ggplot(dados_consorcio, aes(AREA_TEMATICA)) +
geom_bar(alpha = 1, fill = "cornflowerblue") +
ggtitle("Consórcios por Área Temática") +
xlab("Atuação") + ylab("Contagem") +
theme(plot.title = element_text(color="black", size=12, face="bold"),
      axis.title.x = element_text(color="black", size=10, face="bold"),
      axis.title.y = element_text(color="black", size=10, face="bold")) +
scale_x_discrete(limits = c("Segurança", "Cultura", "Turismo", "Infraestrutura", "Meio Ambiente", "Desenvolvimento", "Saúde"))


frequencia <- table(dados_consorcio$AREA_TEMATICA)

print(frequencia)
## tamanho da popula??o dos munic?pios consorciados


ggplot(dados_teste, aes(POPULAÇÃO)) +
geom_bar(alpha = 1, fill = "cornflowerblue") +
ggtitle("Tamanho Populacional dos municípios consorciados") +
xlab("População") + ylab("Contagem") +
theme(plot.title = element_text(color="black", size=12, face="bold"),
      axis.title.x = element_text(color="black", size=10, face="bold"),
      axis.title.y = element_text(color="black", size=10, face="bold")) +
scale_x_discrete(limits = c("Mais de 500.000 habitantes", "100.001 a 500.000 habitantes", "50.001 a 100.000 habitantes", "5.001 a 10.000 habitantes", "AtÃ© 5.000 habitantes", "10.001 a 50.000 habitantes"))


frequencia <- table(dados_consorcio$POPULAÇÃO)

print(frequencia)

## tamanho da populaÃ§Ã£o por capacidade fiscal 


ggplot(dados_teste, aes(x=POPULAÇÃO, y=CAPACIDADEFISCAL_NO_ANOADOÇÃO)) + 
geom_boxplot(outlier.colour="red", outlier.shape=8,
             outlier.size=2, fill = "cornflowerblue")+
ggtitle("População municipal por capacidade fiscal dos municípios consorciados") +
xlab("População") + ylab("Capacidade Fiscal") +
theme(plot.title = element_text(color="black", size=12, face="bold"),
      axis.title.x = element_text(color="black", size=10, face="bold"),
      axis.title.y = element_text(color="black", size=10, face="bold")) +
scale_x_discrete(limits = c("Até 5.000 habitantes", "5.001 a 10.000 habitantes", "10.001 a 50.000 habitantes", "50.001 a 100.000 habitantes", "100.001 a 500.000 habitantes", "Mais de 500.000 habitantes"))



###### tutorial 1 da análise de sobrevivência #########
#construindo o objeto de sobrevivÃªncia padrÃ£o
teste <- with(dados_consorcio, Surv(TEMPO_PARA_FORMAÇÃO, CONSÓRCIO))
head(teste, 80)

#Para iniciar nossa anÃ¡lise, usamos a fÃ³rmula Surv(futime, status) ~ 1 e a funÃ§Ã£o survfit() para produzir as estimativas de Kaplan-Meier da probabilidade de sobrevivÃªncia ao longo do tempo.
km_fit <- survfit(Surv(TEMPO_PARA_FORMAÇÃO, CONSÓRCIO) ~ 1, data=dados_consorcio)
summary(km_fit)

survfit(Surv(TEMPO_PARA_FORMAÇÃO, CONSÓRCIO) ~ 1, data = dados_consorcio) %>% 
  tbl_survfit(
    times = 1,
    label_header = "**1-year survival (95% CI)**"
  )

str(km_fit)

autoplot(km_fit, xlab = "Anos", ylab = "Probabilidade de Sobrevivência", main = 'Kaplan Meier Plot')

#Em seguida, analisamos as curvas de sobrevivÃªncia por tratamento.
km_trt_fit <- survfit(Surv(TEMPO_PARA_FORMAÇÃO, CONSÓRCIO) ~ REG_SUDESTE, data=dados_consorcio)
autoplot(km_trt_fit, xlab = "Anos", ylab = "Probabilidade de Sobrevivência", main = 'Kaplan Meier Plot')

km_trt_fit <- survfit(Surv(TEMPO_PARA_FORMAÇÃO, CONSÓRCIO) ~ REG_SUL, data=dados_consorcio)
autoplot(km_trt_fit, xlab = "Anos", ylab = "Probabilidade de Sobrevivência", main = 'Kaplan Meier Plot')


#Em seguida, ajustarei um modelo de riscos proporcionais de Cox que faz uso de todas as covariÃ¡veis no conjunto de dados.
cox <- coxph(Surv(TEMPO_PARA_FORMAÇÃO, CONSÓRCIO) ~ CAPACIDADEFISCAL_NO_ANOADOÇÃO + POPULAÇÃO + NúmeroConsorciadosPorRegião_por_Anodeadoção + AREA_TEMATICA, data = dados_consorcio)
summary(cox)
cox_fit <- survfit(cox)
autoplot(cox_fit, xlab = "Anos", ylab = "Probabilidade de Sobrevivência", main = 'Kaplan Meier Plot')

t1 <- coxph(Surv(TEMPO_PARA_FORMAÇÃO, CONSÓRCIO) ~ POPULAÇÃO +  AREA_TEMATICA + REGIÃO, data = dados_consorcio) %>% 
 tbl_regression(exp = TRUE, 
               label = list(AREA_TEMATICA ~ "Área Temática")) %>%
  modify_table_styling(
    columns = ci,
    rows = reference_row %in% TRUE,
    missing_symbol = "Referência"
  ) %>%
  bold_labels() %>%
  bold_p() %>%
  italicize_levels()

print(t1)


t2 <- coxph(Surv(TEMPO_PARA_FORMAÇÃO, CONSÓRCIO) ~ POPULAÇÃO +  AREA_TEMATICA + REGIÃO + NúmeroConsorciadosPorRegião_por_Anodeadoção + CAPACIDADEFISCAL_NO_ANOADOÇÃO, data = dados_teste) %>% 
  tbl_regression(exp = TRUE, 
                 label = list(AREA_TEMATICA ~ "Área Temática")) %>%
  modify_table_styling(
    columns = ci,
    rows = reference_row %in% TRUE,
    missing_symbol = "Referência"
  ) %>%
  bold_labels() %>%
  bold_p() %>%
  italicize_levels()

print(t2)

gt::gtsave(as_gt(t1), file = file.path(tempdir(), "temp.png"))

gt::gtsave(as_gt(t2), file = file.path(tempdir(), "temp.png"))


##A quantidade de interesse de um modelo de regressão de Cox é uma taxa de risco (HR). A HR representa a proporção de perigos entre dois grupos em qualquer ponto específico no tempo. 
# A HR é interpretada como a taxa instantânea de ocorrência do evento de interesse naqueles que ainda estão em risco para o evento. Não é um risco, embora seja comumente mal interpretado como tal. Se você tiver um parâmetro de regressão ??
#então HR = exp(??). Um HR < 1 indica risco reduzido de morte, enquanto um HR > 1 indica um risco aumentado de morte.
#Assim, o HR = 0,59 implica que 0,59 vezes mais mulheres estão morrendo do que homens, a qualquer momento. Dito de outra forma, as mulheres têm um risco de morte significativamente menor do que os homens nesses dados

#Os grÃ¡ficos mostram como os efeitos das covariÃ¡veis mudam ao longo do tempo. Observe a inclinaÃ§Ã£o Ã­ngreme e a mudanÃ§a abrupta na inclinaÃ§Ã£o de Karno.

aa_fit <- aareg(Surv(TEMPO_PARA_FORMAÇÃO, CONSÓRCIO) ~ CAPACIDADEFISCAL_NO_ANOADOÇÃO + POPULAÇÃO + NúmeroConsorciadosPorRegião_por_Anodeadoção + AREA_TEMATICA, 
             data = dados_consorcio)
 
aa_fit

autoplot(aa_fit)


aa_fit_cat <- aareg(survConcordance(TEMPO_PARA_FORMAÇÃO, CONSÓRCIO) ~ CAPACIDADEFISCAL_NO_ANOADOÇÃO + POPULAÇÃO + NúmeroConsorciadosPorRegião_por_Anodeadoção + AREA_TEMATICA,  data = dados_consorcio)


#ranger() constrÃ³i um modelo para cada observaÃ§Ã£o no conjunto de dados. O prÃ³ximo bloco de cÃ³digo constrÃ³i o modelo usando as mesmas variÃ¡veis usadas no modelo de Cox acima e plota vinte curvas aleatÃ³rias, juntamente com uma curva que representa a mÃ©dia global para todos os pacientes. 

dados_na <- dados_consorcio[!is.na(dados_consorcio$NúmeroconsorsiadoranterioresporUF_por_anodeadoção),]
dados_na <- dados_consorcio[!is.na(dados_consorcio$CAPACIDADEFISCAL_NO_ANOADOÇÃO),]


r_fit <- ranger(Surv(TEMPO_PARA_FORMAÇÃO, CONSÓRCIO) ~ LogPopulação_no_anoadoção + CAPACIDADEFISCAL_NO_ANOADOÇÃO + 
                NúmeroconsorsiadoranterioresporUF_por_anodeadoção, data = dados_na,
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
   ylab = "AdoÃ§Ã£o",
   main = "Curva de SobrevivÃªncia Municipal")

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

###### tutorial 2 da análise de sobrevivência #########

survival::survfit()

sobrevivencia <- survfit2(Surv(TEMPO_PARA_FORMAÇÃO, CONSÓRCIO) ~ 1, data = dados_consorcio) %>% 
ggsurvfit() +
labs(
  x = "Anos",
  y = "Overall survival probability"
) +
add_confidence_interval() +
add_risktable()

plot(sobrevivencia)

## probabilidade de sobrevivência com 1, 5 e -5 anos do advento da lei dos consórcios públicos intermunicipais

summary(survfit(Surv(TEMPO_PARA_FORMAÇÃO, CONSÓRCIO) ~ 1, data = dados_consorcio), times = -5) 

summary(survfit(Surv(TEMPO_PARA_FORMAÇÃO, CONSÓRCIO) ~ 1, data = dados_consorcio), times = 1)

summary(survfit(Surv(TEMPO_PARA_FORMAÇÃO, CONSÓRCIO) ~ 1, data = dados_consorcio), times = 5)

## nós descobrimos que 1 ano de probabilidade de sobrevivencia no estudo é de 37% e em 5 anos é de 0.05% ## 
## caso olhemos para antes da lei, vemos que a probabilidade de sobrevivência aumenta, sugerindo que a lei é um marco 
## relevante para os consórcios públicos intermunicipais
## não podemos ignorar a censura dos consórcios públicos pois leva a uma superestimação
## da probabilidade de sobrevivência geral. Imagine dois estudos, cada um com 228 municípios. 
## Há 165 consórcios em cada estudo. A censura é ignorada em uma (linha azul), a censura é contabilizada na outra (linha amarela). 
## Os sujeitos censurados contribuem com informações apenas durante uma parte do tempo de acompanhamento e, em seguida, saem do conjunto de risco, reduzindo assim a probabilidade cumulativa de sobrevivência. 
## Ignorar a censura trata erroneamente os pacientes censurados como parte do risco definido para todo o período de acompanhamento.
## posso fazer esse exemplo rodando apenas os municípios consorciados com o não consorciado


## tabelas legais para a probabilidade de consorciamento intermunicipal ## 
survfit(Surv(TEMPO_PARA_FORMAÇÃO, CONSÓRCIO) ~ 1, data = dados_consorcio) %>% 
tbl_survfit(times = c(-2, 1, 5), label_header = "**Probabilidade de Sobrevivência decorrido {time} anos após o advento da Lei (95% CI)**") 

## estimando o tempo médio de sobrevivência - Outra quantidade frequentemente de interesse em uma análise de sobrevivência é o tempo médio de sobrevivência, que quantificamos usando a mediana. 
## Não se espera que os tempos de sobrevivência sejam normalmente distribuídos, portanto, a média não é um resumo apropriado.

survfit(Surv(TEMPO_PARA_FORMAÇÃO, CONSÓRCIO) ~ 1, data = dados_na)

## Vemos que o tempo médio de sobrevivência é de 4 anos. 
# Os limites inferior e superior do intervalo de confiança de 95% também são exibidos.

## gerando uma tabela do tempo médio de sobrevivência pela mediana ## aqui o tempo são 3 anos após a formação dos consórcios públicos

survfit(Surv(TEMPO_PARA_FORMAÇÃO, CONSÓRCIO) ~ 1, data = dados_na) %>% 
tbl_survfit(
  probs = 0.5,
  label_header = "**Tempo médio de Sobrevivência estimado pela Mediana (95% CI)**"
)

## comparando a sobrevivência entre grupos 

survdiff(Surv(TEMPO_PARA_FORMAÇÃO, CONSÓRCIO) ~ POPULAÇÃO, data = dados_consorcio)

## encontramos um p-valor estatisticamente significante para o modelo rodado, há diferença entre grupos.
## A primeiro métrica do teste do survdiff está relacionada ao teste qui-quadrado. O segundo é o teste log-rank que conheço. 
## O problema com a execução de um teste qui-quadrado em estatísticas de sobrevivência é que os testes em vários pontos de tempo 
# não são independentes (ou seja, para ser considerado para um ponto de tempo posterior, 
# alguém deve sobreviver a um ponto de tempo anterior), então eu não recomendaria usá-lo.
# Em geral, com análise de sobrevivência, use a segunda variante (na verdade, acho que é a metodologia principal).
## Pode-se mostrar que a segunda variante se aproxima de uma distribuição z (N(0,1)) e uma distribuição qui-quadrada com um grau de liberdade ao elevar ao quadrado a estatística conforme mostrado aqui.
## o termo (O-E)^2/V é a base para o teste de log-rank. Com exceção do grupo até 5.000 habitantes e o grupo entre 10.001 a 50.000,
## todos os outros grupos tem função de risco estatisticamente significante. Essa semelhança dos dois grupos
## pode sugerir que o processo de difusão institucional nesse eixo populacional ocorreu de forma semelhante.
## são também os grupos com maior densidade consorciativa. 

#Na Parte 1, abordamos o uso de testes log-rank e regressão de Cox para examinar associações entre covariáveis de interesse e resultados de sobrevivência. 
# Mas essas análises dependem da covariável sendo medida na linha de base, ou seja, antes do início do tempo de acompanhamento do evento. 
# O que acontece se você estiver interessado em uma covariável que é medida após o início do tempo de acompanhamento?

survfit2(Surv(TEMPO_PARA_FORMAÇÃO, CONSÓRCIO) ~ AREA_TEMATICA, data = dados_teste) %>% 
  ggsurvfit() +
  labs(
    x = "Dias após a criação da lei do consórcio público",
    y = "Probabilidade de Sobrevivência Geral"
  ) +
  add_risktable()


coxph(
  Surv(TEMPO_PARA_FORMAÇÃO, CONSÓRCIO) ~ AREA_TEMATICA, 
  data = dados_teste
) %>% 
  tbl_regression(exp = TRUE)

##Riscos proporcionais

mv_fit <- coxph(Surv(TEMPO_PARA_FORMAÇÃO, CONSÓRCIO) ~ REGIÃO + AREA_TEMATICA, data = dados_consorcio)
cz <- cox.zph(mv_fit)
print(cz)

plot(cz)

## Aqui vemos que com valores de p < 0,05, 
#rejeitamos a hipótese nula e concluímos que a suposição de riscos proporcionais 
# é satisfeita para cada covariável individual e também para o modelo geral




