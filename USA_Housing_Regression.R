# Pacotes necessários
library(ggplot2)       # Pacote para gráficos
library(lmtest)        # Pacote para testes estatísticos em modelos de regressão
library(car)           # Pacote para análise de regressão
library(tseries)       # Pacote para séries temporais
library(hnp)           # Pacote para envelopes de Half-Normal plots

# Importando os dados
dados <- read.csv("USA_Housing.csv", header = TRUE)
str(dados)  # Estrutura dos dados
summary(dados)

# Ajustar as margens
par(mar = c(1, 1, 1, 1))

# Verificar as colunas numéricas
numeric_cols <- sapply(dados, is.numeric)
cor(dados[, numeric_cols])  # Matriz de correlação para colunas numéricas
pairs(dados[, numeric_cols])  # Diagramas de dispersão para colunas numéricas

# Ajustando o modelo de regressão linear
fit_old <- lm(Price ~ Avg..Area.Income + Avg..Area.House.Age + 
                Avg..Area.Number.of.Rooms + Avg..Area.Number.of.Bedrooms +
                Area.Population, data = dados)
summary(fit_old)  # Resumo do modelo antigo

# Ajustando o modelo sem uma das variáveis com base na função step()
fit <- lm(Price ~ Avg..Area.Income + Avg..Area.House.Age + 
            Avg..Area.Number.of.Rooms +
            Area.Population, data = dados)
summary(fit)  # Resumo do modelo


# Seleção de variáveis usando o método stepwise
step(fit)

n <- nrow(dados)
options(max.print = 100)  # Configuração para imprimir muitas linhas

# Medidas de influência
influence.measures(fit)

# Ajustando as margens dos gráficos
par(mar = c(5, 4, 4, 2) + 0.1)

# Alavancagem
hatvalues_plot <- hatvalues(fit)
h_bar <- fit$rank / n
limite <- 2 * h_bar
plot(hatvalues_plot, ylab = "Alavancagem")
abline(h = limite, col = "red", lty = 2)
which(hatvalues_plot > limite)

# DFFIT
dffits_plot <- dffits(fit)
limite <- 2 * sqrt(fit$rank / n)
plot(dffits_plot, ylab = "DFFITS")
abline(h = c(-limite, limite), col = "red", lty = 2)
which(abs(dffits_plot) > limite)

# DFBETA
dfbetas_fit <- dfbetas(fit)
limite <- 2 / sqrt(n)

# Definindo um layout de múltiplos painéis
num_vars <- ncol(dfbetas_fit)
num_cols <- 2  # Número de colunas para os gráficos
num_rows <- ceiling(num_vars / num_cols)  # Número de linhas necessárias

# Ajustando as margens e layout para múltiplos gráficos
par(mfrow = c(num_rows, num_cols), mar = c(4, 4, 2, 1))

# Plotando todos os gráficos de DFBETA
for (i in 1:num_vars) {
  plot(dfbetas_fit[, i], ylab = paste("DFBETA", i), main = paste("DFBETA", colnames(dfbetas_fit)[i]))
  abline(h = c(-limite, 0, limite), col = c("red", "blue", "red"), lty = c(2, 1, 2))
}

# Resetando o layout para um único gráfico
par(mfrow = c(1, 1))

# Distância de Cook
cooks_distance_plot <- cooks.distance(fit)
limite <- 4 / (n - fit$rank)
plot(cooks_distance_plot, ylab = "Distância de Cook")
abline(h = limite, col = "red", lty = 2)
which(cooks_distance_plot > limite)
max_cook <- which.max(cooks_distance_plot) #4717 - observação influente

# Resíduos
residuo <- rstudent(fit) # residuo studentizado

plot(residuo,type='p',pch="+",main="Residuos",xlab="indices") # plota os residuos do modelo
abline(h=c(-2,0,2),lty=3) # inclui linhas horizontais no grafico

which(abs(residuo)>3)

hist(residuo) # histograma dos residuos
hnp(fit, resid.type = "student", halfnormal = FALSE)  # Envelope simulado

# Testando as suposições do modelo
## [S0] O modelo está corretamente especificado
## [S1] A média dos erros é zero
## [S2] Homoscedasticidade dos erros
## [S3] Não autocorrelação 
## [S4] Ausência de Multicolinearidade
## [S5] Normalidade dos erros

# Teste de especificação (RESET test)
# H0: O modelo está corretamente especificado
resettest(fit)

# Teste t para a média dos erros
# H0: média dos erros é igual a zero
t.test(resid(fit), mu = 0, alternative = "two.sided")
t.test(resid(fit_sem_influente), mu = 0, alternative = "two.sided")

# Teste de Breusch-Pagan para Heteroscedasticidade
# H0: erros são homoscedásticos
bptest(fit, studentize = TRUE) #NÃO ATENDEU
bptest(fit_sem_influente, studentize = TRUE) #ATENDEU

# Teste de Durbin-Watson para autocorrelação
# H0: Não há autocorrelação 
dwtest(fit)
acf(rstudent(fit))

dwtest(fit_sem_influente)

# Fatores de Inflação de Variância (VIF)
vif(fit)
vif(fit_sem_influente)

# Teste Jarque-Bera de Normalidade
# H0: Os erros possuem distribuição normal
jarque.bera.test(resid(fit))
jarque.bera.test(resid(fit_sem_influente))

# Removendo observação influente
observacao_influente <- 4717
dados_sem_influente <- dados[-observacao_influente, ]

# Ajustando o modelo sem a observação influente
fit_sem_influente <- lm(Price ~ Avg..Area.Income + Avg..Area.House.Age + 
                          Avg..Area.Number.of.Rooms + Area.Population, data = dados_sem_influente)
summary(fit_sem_influente) #R Ajustado maior

#Refazer testes: TODAS AS SUPOSIÇÕES ATENDIDAS

# Previsões para os dados da amostra usada na estimação
predicted_values <- predict(fit) 

# Exemplo de predição com novos dados usando valores médios do conjunto de dados
novos_dados <- data.frame(
  Avg..Area.Income = mean(dados$Avg..Area.Income, na.rm = TRUE),
  Avg..Area.House.Age = mean(dados$Avg..Area.House.Age, na.rm = TRUE),
  Avg..Area.Number.of.Rooms = mean(dados$Avg..Area.Number.of.Rooms, na.rm = TRUE),
  Avg..Area.Number.of.Bedrooms = mean(dados$Avg..Area.Number.of.Bedrooms, na.rm = TRUE),
  Area.Population = mean(dados$Area.Population, na.rm = TRUE)
)
predicted_new <- predict(fit, newdata = novos_dados)
predicted_new
