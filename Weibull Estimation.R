# ------------------------------
# Estimação do Modelo Weibull
# ------------------------------
install.packages("farver")

library(ggplot2)

# 1. Função Densidade da Distribuição Weibull (PDF)
weibull_pdf <- function(x, k, lambd) {
  if (any(x <= 0)) {
    stop("Os valores de x devem ser positivos.")
  }
  (k / lambd) * (x / lambd)^(k - 1) * exp(-(x / lambd)^k)
}

# 2. Função Quantílica da Weibull (Q(p))
weibull_quantile <- function(p, k, lambd) {
  lambd * (-log(1 - p))^(1 / k)
}

# 3. Função Log-Verossimilhança
loglik_weibull <- function(par, x) {
  k <- par[1]  # Parâmetro de forma (k)
  lambd <- par[2]  # Parâmetro de escala (lambda)
  
  # Calcular a log-verossimilhança somando os logaritmos das densidades
  log_likelihood <- sum(log(weibull_pdf(x, k, lambd)))
  
  # Retorna a log-verossimilhança negativa (porque optim minimiza)
  return(-log_likelihood)
}

# 4. Gerar Dados Simulados da Distribuição Weibull
set.seed(123)
k_true <- 1.5   # Valor real de k (forma)
lambd_true <- 2  # Valor real de lambda (escala)
n <- 10000         # Tamanho da amostra

# Gerar probabilidades uniformes
p <- runif(n)

# Gerar valores simulados da distribuição Weibull
x_simulado <- weibull_quantile(p, k_true, lambd_true)

# Exibir histograma dos dados simulados
hist(x_simulado, main = "Dados Simulados da Distribuição Weibull", xlab = "x", col = "lightblue", border = "black")

# 5. Estimar os Parâmetros via Máxima Verossimilhança
valores_iniciais <- c(1, 1)  # Chutes iniciais para k e lambda

# Estimação via optim
resultado <- optim(par = valores_iniciais, fn = loglik_weibull, x = x_simulado, method = "SANN")

# Extraindo os parâmetros estimados
estimates <- resultado$par

cat("Parâmetro k estimado:", round(estimates[1], 4), "\n")
cat("Parâmetro lambda estimado:", round(estimates[2], 4), "\n")

# Comparação com os valores verdadeiros
cat("Parâmetro k verdadeiro:", k_true, "\n")
cat("Parâmetro lambda verdadeiro:", lambd_true, "\n")

# O resultado da estimativa de máxima verossimilhança deve ser próximo dos valores verdadeiros

# 1. Gráfico: Histograma dos dados simulados
ggplot(data.frame(x_simulado), aes(x = x_simulado)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "blue", alpha = 0.5) +
  labs(title = "Histograma dos Dados Simulados (Weibull)", x = "Valores Simulados", y = "Densidade") +
  theme_minimal()

# 2. Gráfico: Comparação da densidade verdadeira e ajustada
x_vals <- seq(0.001, max(x_simulado), length.out = 100)

# Densidade com os parâmetros estimados
densidade_ajustada <- weibull_pdf(x_vals, estimates[1], estimates[2])

# Densidade com os parâmetros verdadeiros
densidade_verdadeira <- weibull_pdf(x_vals, k_true, lambd_true)

# Data frame para plotagem
df_densidade <- data.frame(x_vals, densidade_verdadeira, densidade_ajustada)

# Gráfico comparando densidades
ggplot(df_densidade, aes(x = x_vals)) +
  geom_line(aes(y = densidade_verdadeira), color = "blue", size = 1, linetype = "dashed", 
            label = "Verdadeira") +
  geom_line(aes(y = densidade_ajustada), color = "red", size = 1, linetype = "solid", 
            label = "Ajustada") +
  labs(title = "Comparação das Densidades Weibull", x = "x", y = "Densidade") +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_color_manual(name = "Densidade", values = c("Verdadeira" = "blue", "Ajustada" = "red"))
