# Instalação e Carregamento dos Pacotes Necessários para a Aula -----------

pacotes <- c("tseries",
             "forecast",
             "tidyverse",
             "ggfortify",
             "FinTS",
             "lmtest")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


# Dados 
dados <- read.csv2(file = "dados_hanseniase.csv", header = T, sep = ';')


# Série temporal
ts_hans_newcases <- ts(dados, start = c(2018,1), frequency = 12)

plot(ts_hans_newcases[,c("CASOS_NOVOS", "DEGRAU")], main = "")

autoplot(ts_hans_newcases[,"CASOS_NOVOS"]) + 
  ggtitle("") + 
  xlab("Meses") + 
  ylab("Casos novos de hanseníase") + 
  ylim(0, 3000)


# Modelo ARIMA
y <- ts_hans_newcases[,"CASOS_NOVOS"]
x <- ts_hans_newcases[,"DEGRAU"]

fit <- auto.arima(y, xreg = x)

summary(fit)

checkresiduals(fit)

coeftest(fit, df = Inf, level = 0.95)


# testes estatísticos
d1 <- Box.test(residuos, type = "Ljung-Box", lag = 15) # Ljung-Box 
d2 <- ArchTest(residuos, lags = 15) # ARCH
d3 <- jarque.bera.test(residuos) # Jarque-Bera

#Tabela de resultados
testes_di <- c(d1$method, d2$method, d3$method) 
estt_di <- as.numeric(c(d1$statistic, d2$statistic, d3$statistic)) 
valorp_di <- c(d1$p.value, d2$p.value, d3$p.value) 
resultados_di <- cbind(estt_di, valorp_di) 
rownames(resultados_di) <- testes_di 
colnames(resultados_di) <- c("Estatística", "p") 
print(resultados_di, digits = 4)


# Gráfico
(ajustados <- fit$fitted)

ts.plot(ts_hans_newcases[,"CASOS_NOVOS"], ajustados, lty = c(1,1), type = "l", 
        lwd = c(1,2), col = c(1, "red"), xlab="Mês/ano de diagnóstico", 
        ylab = "Número de casos novos", ylim = c(0, 3000)) 
legend("bottomleft", col = c("black", "red"), 
       legend = c("Observado", "Ajustado"), lty = c( 1, 1), 
       lwd = c(1,1,1), cex = , bty="n") 
abline(v = 2020.27, col = "darkorchid4", lty = 2, lwd = 2) 
text(2020.40,2500,"Pandemia COVID-19", adj = c(0,1), col = "gray")

