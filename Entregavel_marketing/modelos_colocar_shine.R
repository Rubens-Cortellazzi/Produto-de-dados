library(pacoteFRT)
pacoteFRT::dadosFRT

############################### INtrodução ################################
#Nossa empresa, a Modelagens FPRTV foi contratada por uma empresa de Marketing para auxiliar estatísticamente
#a empresa. Para isso, eles disseram que já tinham o banco de dados montado e precisavam de ajuda para conseguir
#visualizar, também gostariam de algo interativo e gostariam de ter uma parte estatística muito desenvolvida.
#Para isso, pensamos em desenvolver um pacote que contém funções para auxiliar na modelagem da regressão linear,
#no pacote também, foi inserido a base dedados que a empresa forneceu para nós. Além disso, pensamos em desenvolver
#um shiny, que seja iterativo para usuário e assim facilitar a vida da empresa. Por último, desenvolvemos uma
#API em que é uma forma rápida de conseguir fazer a predição de vendas, alterando os valores para cada fator
#envolvido, sendo que na API foi desenvolvida também para retornar um gráfico dependendo de qual dos três fatores
#que inflenciam as vendas ser chamado.

####################### Análise descritiva #############################

############## Variáveis do modelo ###################
#Fazer um summary geral
summary(pacoteFRT::dadosFRT)

#Fazer um head em que a pessoa pode pedir qunatos ele vai querer que apareça.
head(pacoteFRT::dadosFRT, 10)

#Citar colunas
colnames(pacoteFRT::dadosFRT)

#Histograma face
pacoteFRT::dadosFRT %>% ggplot() +
  geom_histogram(aes(x = facebook), color = "Black", fill = "Blue")+
  theme_bw()

#Boxplot face
pacoteFRT::dadosFRT %>% ggplot() +
  geom_boxplot(aes(x = facebook), color = "Black", fill = "Blue")+
  theme_bw()

#Histograma newspaper
pacoteFRT::dadosFRT %>% ggplot() +
  geom_histogram(aes(x = newspaper), color = "Black", fill = "Orange")+
  theme_bw()

#Boxplot newspaper
pacoteFRT::dadosFRT %>% ggplot() +
  geom_boxplot(aes(x = newspaper), color = "Black", fill = "Orange")+
  theme_bw()

#Histograma youtube
pacoteFRT::dadosFRT %>% ggplot() +
  geom_histogram(aes(x = youtube), color = "Black", fill = "Green")+
  theme_bw()

#Boxplot youtube
pacoteFRT::dadosFRT %>% ggplot() +
  geom_boxplot(aes(x = youtube), color = "Black", fill = "Green")+
  theme_bw()

############## Variável resposta ###################

#Summary
summary(pacoteFRT::dadosFRT$sales)

#Head e consigo alterar
head(pacoteFRT::dadosFRT$sales,10)

#Histograma
pacoteFRT::dadosFRT %>% ggplot() +
  geom_histogram(aes(x = sales), color = "Blue", fill = "Red")+
  theme_bw()3

#Boxplot
pacoteFRT::dadosFRT %>% ggplot() +
  geom_boxplot(aes(x = sales), color = "Blue", fill = "Red")+
  theme_bw()

#Gráfico de linha entre vendas e facebook
pacoteFRT::dadosFRT %>% ggplot() +
  geom_line(aes(x = sales, y = facebook), color = "Blue") +
  theme_bw()

#Gráfico de linha entre vendas e youtube
pacoteFRT::dadosFRT %>% ggplot() +
  geom_line(aes(x = sales, y = youtube), color = "Red") +
  theme_bw()

#Gráfico de linha entre vendas e newspaper
pacoteFRT::dadosFRT %>% ggplot() +
  geom_line(aes(x = sales, y = newspaper), color = "Green") +
  theme_bw()

#GRáfio de vendas e todos os outros fatores
pacoteFRT::dadosFRT %>% ggplot() +
  geom_line(aes(x = sales, y = newspaper), color = "Green") +
  geom_line(aes(x = sales, y = youtube), color = "Red") +
  geom_line(aes(x = sales, y = facebook), color = "Blue") +
  theme_bw()

#Correlações
library(GGally)
ggpairs(pacoteFRT::dadosFRT)
################### Seleção de modelos ######################

#######Todas as variáveis ###############
modelo <- lm(sales ~ ., data = pacoteFRT::dadosFRT)
modelo
anova(modelo)
summary(modelo)
coef(modelo)
r2 = summary(modelo)$r.squared
r2adj = summary(modelo)$adj.r.squared

############ Melhores variáveis #############

#Backward regression
back <- backward_regression(pacoteFRT::dadosFRT, "sales")
summary(back)
anova(back)
coef(back)
r2_b = summary(back)$r.squared
r2adj_b = summary(back)$adj.r.squared

#Forward regression
forw <- forward_regression(pacoteFRT::dadosFRT, "sales")
summary(forw)
anova(forw)
coef(forw)
r2_w = summary(forw)$r.squared
r2adj_w = summary(forw)$adj.r.squared

#Gráfico cp de mallow
Cp_Mallow(pacoteFRT::dadosFRT, "sales")

#Gráfico BIC
bic_grafico(pacoteFRT::dadosFRT, "sales")

######################### Análise de resíduos #########################

#Modelo Completo
plot(modelo, which = 1, las = 1, pch = 19, col = "blue")
plot(modelo, which = 2, las = 1, pch = 19, col = "blue")
plot(modelo, which = 4, las = 1, pch = 19, col = "blue")

#Backward
plot(back, which = 1, las = 1, pch = 19, col = "blue")
plot(back, which = 2, las = 1, pch = 19, col = "blue")
plot(back, which = 4, las = 1, pch = 19, col = "blue")

#Forward
plot(forw, which = 1, las = 1, pch = 19, col = "blue")
plot(forw, which = 2, las = 1, pch = 19, col = "blue")
plot(forw, which = 4, las = 1, pch = 19, col = "blue")


################# Predição #####################

#A pessoa vai poder escolher qunato ela vai querer para cada um

#MOdelo completo
xnew = data.frame(facebook = 100, youtube = 100, newspaper = 100)
predict(modelo, xnew, interval = "confidence")
predict(modelo, xnew, interval = "prediction")

#Back
xnewb = data.frame(facebook = 100, youtube = 100, newspaper = 100)
#xnewb = data.frame(facebook = 100, youtube = 100), da o mesmo que o de cima, então não influencia colocar mais em qual nã tem.
predict(back, xnewb, interval = "confidence")
predict(back, xnewb, interval = "prediction")

#Forward
xnewf = data.frame(facebook = 100, youtube = 100, newspaper = 100)
#xnewb = data.frame(facebook = 100, youtube = 100), da o mesmo que o de cima, então não influencia colocar mais em qual nã tem.
predict(forw, xnewf, interval = "confidence")
predict(forw, xnewf, interval = "prediction")


######################### Conclusão #######################
#Assim, finalizamos a parte do Shiny em que o usuário consegue fazer alterações e analisar qual
#a parte principal para o usuário. Sendo que é realizada uma análise completa do banco de dados.
#Além disso, notamos que as vendas por newspaper não são relevantes para o backward e forward regression
#sendo que a predição e o intervalo de confiança são alterados um pouco dependendo do modelo que está sendo implementado.
