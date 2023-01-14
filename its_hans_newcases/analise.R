# Instalação e Carregamento dos Pacotes Necessários para a Aula -----------

pacotes <- c("tseries",     # Gráficos e estatísticas
             "forecast",    # Forecast, gráficos e testes
             "tidyverse",   # Manipulação de dados
             "ggfortify",   # Visualização de dados
             "FinTS",       # Análises de ST
             "lmtest",      # Teste de modelos
             "broom")       # Resume estatísticas para tibble

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


# Dados de série temporal de casos novos de hanseníase
dados <- read.csv2(file = "dados_hanseniase.csv", header = T, sep = ';')


# Gerar a série temporal anual/mensal
ts_hans_newcases <- ts(dados, start = c(2018,1), frequency = 12)


# Plotar a série temporal
autoplot(ts_hans_newcases[,"CASOS_NOVOS"]) + 
  ggtitle("") + 
  xlab("Meses") + 
  ylab("Casos novos de hanseníase") + 
  ylim(0, 3000)

autoplot(ts_hans_newcases[,"DEGRAU"]) + 
  ggtitle("") + 
  xlab("Meses") + 
  ylab("Casos novos de hanseníase")


# Modelo de erros ARIMA
y <- ts_hans_newcases[,"CASOS_NOVOS"] # Variável dependente
x <- ts_hans_newcases[,"DEGRAU"]      # Variável de intervenção (dummy: 0-1)

# Modelo
fit <- auto.arima(y, xreg = x)

summary(fit)

# Análise dos resíduos
checkresiduals(fit)

# Teste de significância
coeftest(fit, df = Inf, level = 0.95)


# Gráfico final
(ajustados <- fit$fitted)

ts.plot(ts_hans_newcases[,"CASOS_NOVOS"], ajustados, lty = c(1,1), type = "l", 
        lwd = c(1,2), col = c(1, "red"), xlab="Mês/ano de diagnóstico", 
        ylab = "Número de casos novos", ylim = c(0, 3000), main = "ARIMA(0,0,0)(1,0,0)[12] errors") 
legend("bottomleft", col = c("black", "red"), 
       legend = c("Observado", "Ajustado"), lty = c( 1, 1), 
       lwd = c(1,1,1), cex = , bty="n") 
abline(v = 2020.27, col = "grey", lty = 2, lwd = 2) 
text(2020.40,2500,"Pandemia COVID-19", adj = c(0,1), col = "gray")


# Apresnetação de tabela
tidy(fit)
glance(fit)
