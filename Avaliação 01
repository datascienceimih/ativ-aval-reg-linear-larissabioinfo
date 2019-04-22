# Se os pacotes necessários não estiverem instalados, faça a instalação
if (! "MASS" %in% installed.packages()) install.packages("MASS")
if (! "dplyr" %in% installed.packages()) install.packages("dplyr")
if (! "ggplot2" %in% installed.packages()) install.packages("ggplot2")
if (! "readr" %in% installed.packages()) install.packages("readr")
if (! "texreg" %in% installed.packages()) install.packages("texreg")
if (! "GGally" %in% installed.packages()) install.packages("GGally")
if (! "ISLR" %in% installed.packages()) install.packages("ISLR")

# Carregando o pacote
library(readr)
library(dplyr) #manipulação 
library(texreg) #tabelas de regressão formatadas 
library(ggplot2)
library(GGally)
library(ISLR)

### 8 
## Questão A
#Use a função lm () para executar uma regressão linear simples 
#com mpg como a resposta e a potência como o preditor. Use a função summary () para imprimir os resultados

model1 = lm(mpg ~ horsepower, data = Auto)
summary(model1)

#I. Existe uma relação entre o preditor e a resposta?
#R - Os valores p para os coeficientes de regressão são quase zero. 
#Isso implica em significância estatística, o que, por sua vez, significa que existe um relacionamento.

#II. Quão forte é a relação entre o preditor e a resposta?
#O valor R ^ {2} indica que cerca de 61% da variação na variável de resposta (mpg) é devida à 
#variável preditor (cavalo-vapor).

#III. A relação entre o preditor e a resposta é positiva ou negativa? 
#O coeficiente de regressão para "cavalos-força" é negativo. Portanto, o relacionamento é negativo.

#IV. Qual é o mpg previsto associado a uma potência de 98? Quais são os intervalos de previsão e 
#confiança de 95% associados?
#O intervalo de confiança de 95%

predict(model1, data.frame(horsepower = c(85)), interval ="confidence")

#intervalo de previsão de 95%

predict(model1, data.frame(horsepower = c(85)), interval ="prediction")
#Como esperado, o intervalo de previsão é mais amplo que o intervalo de confiança.


##Questao B
#Plote a resposta e o preditor. 
#Use a função abline () para exibir a linha de regressão de mínimos quadrados.

attach(car)
plot(mpg~horsepower, main =" mpg x Horsepower", xlab = " horsepower", ylab ="mpg")
abline(coef = coef(model1), col ="blue")

detach(car)

#Question-c
#Use the plot() function to produce diagnostic plots of the least squares regression fit. 
#Comment on any problems you see with the fit.

par(mfrow=c(2,2))
plot(model1)

#Gráficos
#primeiro:relacionamento não linear entre as variáveis preditor e resposta.
#segundo:  mostra que os resíduos são normalmente distribuídos. 
#terceiro: gráfico mostra que a variância dos erros é constante. 
#quarto:não há pontos de alavancagem nos dados.

#----------------------------------------------------------------------------------------------

### 9. Esta questão envolve o uso de regressão linear múltipla na
# Conjunto de dados automático.
# (a) Produz uma matriz de dispersão que inclui todas as variáveis
# no conjunto de dados.

library("ISLR")
pairs(car)


# (b) Calcule a matriz de correlações entre as variáveis usando
#a função cor (). Você precisará excluir a variável de nome, cor ()
#que é qualitativo

cor(car[, names(car) !="name"])


# (c) Use a função lm () para executar uma regressão linear múltipla
#with mpg como a resposta e todas as outras variáveis, exceto nome como
# os preditores. Use a função summary () para imprimir os resultados.
#Comment na saída.
model = lm(mpg ~. -name, data = car)
summary(model)

#Por exemplo:
#1. Existe uma relação entre os preditores e a resposta?
#2. Quais preditores parecem ter um significado estatisticamente
# relacionamento com a resposta?
#3 O que sugere o coeficiente da variável ano?

  
# (d) Use a função plot () para produzir plotagens de diagnóstico linear
# ajuste de regressão. Comente sobre quaisquer problemas que você vir com o ajuste.
#Os lotes residuais sugerem quaisquer exceções incomumente grandes?
# O gráfico de alavancagem identifica quaisquer observações com níveis de
# alavancagem?
par (mfrow = c (2,2))
trama (modelo)

par(mfrow = c(2,2))
plot(model)

#O primeiro gráfico mostra que existe uma relação não linear entre o respondente e os preditores;
#O segundo gráfico mostra que os resíduos são normalmente distribuídos e inclinados para a direita;
#O terceiro gráfico mostra que a variância constante da suposição de erro não é verdadeira para esse modelo;
#O terceiro gráfico mostra que não há pontos de alavancagem. 
#No entanto, há uma observação que se destaca como um potencial ponto de alavancagem (rotulado 14 no gráfico)

#---------------------------------------------------------------------------------------------------------
### 10
# QUESTAO A
#Ajuste um modelo de regressão múltipla para prever vendas usando Price, Urban, e US.

?Carseats
head(Carseats)
str(Carseats)
model1 = lm(Sales ~ Price+Urban+US, data= Carseats)
summary(model1)

# QUESTÃO B
#Forneça uma interpretação de cada coeficiente no modelo. 
#Tenha cuidado - algumas das variáveis do modelo são qualitativas!
#Resposta:

# quando o preço aumenta em $ 1000, o número de carros vendidos diminui em 54.459.
#A venda de uma loja não é afetada por estar ou não em uma área urbana.

#--------------------------------------------------------------------------------------

###13
#Questao A 
#Usando a função rnorm (), crie um vetor, x, 
#contendo 100 observações extraídas de uma distribuição N (0, 1). Isso representa um recurso, X.

x = rnorm(100, mean= 0, sd =1)

#Questão b
#Usando a função rnorm (), crie um vetor, eps, contendo 100 observações de uma distribuição N (0, 0,25),
#ou seja, uma distribuição normal com média zero e variância 0,25

eps = rnorm(100, mean =0, sd = 0.25)

#Questao c
#Usando x e eps, gere um vetor y de acordo com o modelo Y = −1 + 0.5X + eps. 
#Qual é o comprimento do vetor y? Quais são os valores de βo e β1 neste modelo linear?
y = -1+0.5*x+eps
length(y)

#Questao d
#Crie um gráfico de dispersão exibindo o relacionamento entre xe y. Comente o que você observa.
plot(y~x)
#Resposta : relacao linear

#Questao E
#Ajuste um modelo linear de mínimos quadrados para prever y usando x. 
#Comente sobre o modelo obtido. Como β ^ o e β ^ 1 se comparam com βo e β1?
model1 = lm(y~x)
summary(lm.map1)
#β ^ o e β ^ 1 são  signficantes 
#R2 = 0,84 -  o modelo se ajusta muito bem.

#Questao-f
#Exibe a linha de mínimos quadrados no gráfico de dispersão obtido em (d).
#Desenhe a linha de regressão de população na plotagem, em uma cor diferente.
#Use o comando legenda () para criar uma legenda apropriada

plot(y~x); abline(model1, col ="red") 
legend("bottomright", c("Regression line"), lwd=1, col="red",bty ="n")

#Questao-g
#Ajuste um modelo de regressão polinomial que prevê y usando x e x ^ 2. 
#Existe evidência de que o termo quadrático melhora o ajuste do modelo? Explique sua resposta.__
model1 = lm(y~poly(x,2))
summary(model1)

#O coeficiente de regressão do termo quadrático não é  significativo
# não há evidências de que o termo quadrático melhore o modelo.

#Questao-h
#Repita (a) - (f) depois de modificar o processo de geração de dados 
#de forma que haja menos ruído nos dados. 
#O modelo (parte c) deve permanecer o mesmo. Você pode fazer isso 
#diminuindo a variação da distribuição normal usada para gerar o termo de erro em (b). 
#Descreva seus resultados.

x = rnorm(100, mean= 0, sd =1)
eps = rnorm(100, mean =0, sd = 0.10)
y = -1+0.5*x+eps
model2 = lm(y~x)
summary(model2)

plot(y~x); abline(model2, col ="blue") 
legend("bottomright", c("regressao linear"), lwd=1, col="blue",bty ="n")

model2 = lm(y~poly(x,2))
summary(model2)

#R-quadrado = 0,95 mostra que este modelo se ajusta melhor aos dados.
#O termo quadrático é insignificante


#QUESTAO-i
#Repita (a) - (f) depois de modificar o processo de geração de dados de forma 
#que haja menos ruído nos dados. O modelo (em parte-c) deve permanecer o mesmo. 
#Você pode fazer isso diminuindo a variação da distribuição normal usada para gerar o termo de erro em (b). 
#Descreva seus resultados.

x = rnorm(100, mean= 0, sd =1)
eps = rnorm(100, mean =0, sd = 0.50)
y = -1+0.5*x+eps
model3 = lm(y~x)
summary(model3)

plot(y~x); abline(model3, col ="blue") 
legend("bottomright", c("Regression line"), lwd=1, col="red",bty ="n")

model3 = lm(y~poly(x,2))
summary(model3)


#R2 = 0,4482 é muito baixa
#a linha de regressão não está ajustando bem os pontos.
# é estatisticamente insignificante.

#Questao-j
#Quais são os intervalos de confiança para βo e β1 com base no conjunto de dados original, 
#no conjunto de dados mais ruidosos e no conjunto de dados menos ruidoso? Comente seus resultados.

model1 = lm(y~x); model2 = lm(y~x); model3 = lm(y~x)
confint(model1);confint(model2); confint(model3)

# mesmos intervalos de confiança para os parâmetros βo e β1 de cada modelo.

#----------------------------------------------------------------------------
###15
#Questao-a
#Usando a função rnorm (), crie um vetor, x, contendo 100 observações extraídas 
#de uma distribuição N (0, 1). Isto representa uma característica, X. 
#A última linha corresponde à criação de um modelo linear no qual y é uma função de x1 e x2. 
#Escreva a forma do modelo linear. 
#Quais são os coeficientes de regressão?

set.seed(1)
x1 = runif (100)
x2 = 0.5*x1+rnorm (100)/10
y = 2+2*x1+0.3*x2+rnorm (100)

#R : Os coeficientes de regressão são βo = 2 + rnorm (100), β1 = 2 e β3 = 0,3

#Questão b
#Qual é a correlação entre x1 e x2? 
#Crie um gráfico de dispersão exibindo o relacionamento entre as variáveis.

cor(x1, x2); plot(x1, x2)

#Question-c
#Usando esses dados, ajuste uma regressão de mínimos quadrados para prever y usando x1 e x2.
#Descreva os resultados obtidos. O que são βˆ0, βˆ1 e βˆ2? Como eles se relacionam com os verdadeiros βo, β1 e β2?
#Pode rejeitar a hipótese nula Ho: β1 = 0?

model1 =lm(y~x1+x2)
summary(model1)

# βo = 2,1305, β1 = 1,4396 e β3 = 1,0097. 
#β1 rejeito a hipótese nula a um nível de confiança de 99% .
#β2  rejeito a hipótese nula.

#Questao-d
#Agora, ajuste uma regressão de mínimos quadrados para prever y usando apenas x1. 
#Comente seus resultados. Você pode rejeitar a hipótese nula Ho: β1 = 0?

model2 = lm(y~x1)
summary(model2)

# a hipótese nula é rejeitada para o coeficiente de regressão β1  
#o preditor x1 pode explicar cerca de 20% das mudanças na variável de resposta y.

#Questao-e
#Agora, ajuste uma regressão de mínimos quadrados para prever y usando apenas x2.
#Comente seus resultados. Você pode rejeitar a hipótese nula Ho: β2 = 0?

model3 = lm(y~x2)
summary(model3)

#rejeitar a hipótese nula para o coeficiente de regressão β2
# o preditor x2 pode explicar cerca de 17% das mudanças na variável de resposta y.

#Questao-g
#Agora, suponha que obtemos uma observação adicional, que infelizmente foi mismeasured.
#Re-ajuste os modelos lineares de (c) para (e) usando esses novos dados. 
#Que efeito essa nova observação tem em cada um dos modelos? Em cada modelo, esta observação é um outlier? 
#Um ponto de alta alavancagem? 
#Ambos? Explique suas respostas.

x1=c(x1, 0.1)
x2=c(x2, 0.8)
y=c(y,6)

model4 =lm(y~x1+x2)
summary(model4);
plot(lm.fit);

model5 =lm(y~x1)
summary(model5);

plot(model5); 

#nos dois casos x1 é estatisticamente significativo para as variações em y. 

model6 =lm(y~x2)
summary(model6);
plot(model6); 

#Em ambos os casos  x2 é estatisticamente significativo para as variações em y. 
#esse modelo é melhor que o primeiro.

#Para este modelo, usando a distância do cozinheiro como referência,
#o ponto de dados adicionado não é um ponto discrepante.





