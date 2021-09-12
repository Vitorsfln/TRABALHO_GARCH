
#seleciona o diretorio do arquivo como diretorio de trabalho
my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)

#limpa as variaveis (shift+F10 se quiser resetar tudo incluindo plots, pacotes, console, etc)
rm(list=ls()) 
#retira notacao cientifica
options(scipen = 999)



#DADOS

#carrega pacotes para abrir arquivos de excel e manipular os dados
library(readxl)
library(tidyquant)
library(tidyverse)

#carrega os dados do excel
df <- read_excel("DADOS_TRABALHO.xlsx", 
                 col_types = c("text","date", "numeric"))

df$data <- as.Date(df$data, format = "%Y-%m-%d")

#calcula os retornos
price.data <- df %>% 
  na.omit(df) %>% 
  group_by(ticker) %>%
  tq_mutate(select = price.close,
            mutate_fun = periodReturn,
            period = "daily",
            col_rename = "ret")

#tira as observacoes perdidas no calculo dos retornos
price.data <- price.data[-c(1, 1484, 2967), ]

#calcula o retorno e volatilidade (desvio padrão condicional) do portfolio
port.df <- price.data %>% 
  tq_portfolio(assets_col = ticker,
               returns_col = ret,
               weights = c(0.5, 0.3, 0.2),
               geometric = FALSE,
               col_rename = "p.ret") %>% 
  mutate(vol = sqrt((p.ret-mean(p.ret, na.rm = TRUE))^2) )



#GRAFICOS DOS RETORNOS

library(cowplot)

p1 <- ggplot(price.data[1:1482, ], aes(data, ret)) + 
  geom_line() + 
  labs(title = ("BOVA11"),
       x = " ",
       y = "Retornos") + 
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  theme(panel.grid.minor = element_blank()) + 
  geom_hline(yintercept = 0, color = "red")

p2 <- ggplot(price.data[1483:2964, ], aes(data, ret)) + 
  geom_line() + 
  labs(title = ("IVVB11"),
       x = " ",
       y = "Retornos") + 
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  theme(panel.grid.minor = element_blank()) + 
  geom_hline(yintercept = 0, color = "red")

p3 <- ggplot(price.data[2965:4446, ], aes(data, ret)) + 
  geom_line() + 
  labs(title = ("SMAL11"),
       x = " ",
       y = "Retornos") + 
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  theme(panel.grid.minor = element_blank()) + 
  geom_hline(yintercept = 0, color = "red") 

p4 <- ggplot(port.df, aes(data, p.ret)) + 
  geom_line() + 
  labs(title = ("PORTFOLIO"),
       x = " ",
       y = "Retornos") + 
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  theme(panel.grid.minor = element_blank()) + 
  geom_hline(yintercept = 0, color = "red") 

grid1 <- plot_grid(p1, p2, p3, p4, nrow = 2, 
                   labels = " ")
grid1



#ESTATISTICAS DESCRITIVAS

#assimetria e curtose nao vem com o R
library(moments)
library(xtable)

stat.desc = matrix(NA, nrow = 5, ncol = 4)

rownames(stat.desc) = c("Obs.", "Media", "D.P.", "Assim.", "Curt.")
colnames(stat.desc) = c("BOVA11","IVVB11", "SMAL11", "PORTOFOLIO")

calc.stat.desc = function(x){
  out = rep(0, 5)
  out[1] = length(x)
  out[2] = mean(x)
  out[3] = sd(x)
  out[4] = skewness(x)
  out[5] = kurtosis(x) #curtose bruta
  return(out)
}

#multiplica os retornos por 100 para ficar mais facil de interpretar a tabela. Ou seja, mostra-se a porcentagem, nao numeros decimais
stat.desc[,1] = calc.stat.desc(100*price.data$ret[1:1482])
stat.desc[,2] = calc.stat.desc(100*price.data$ret[1483:2964])
stat.desc[,3] = calc.stat.desc(100*price.data$ret[2965:4446])
stat.desc[,4] = calc.stat.desc(100*port.df$p.ret)

#salva a tabela como arquivo
tabela1 <- xtable(stat.desc, digits = 3)
print.xtable(tabela1, type = "html", file = "tabela1.html")



#TESTES DE RAIZ UNITARIA

library(tseries)

#ADF e PP H0 representa estacionariedade, KPPS H0 representa nao estacionariedade
raiz.unitaria.mat1 = matrix(NA, ncol = 2, nrow = 3)
colnames(raiz.unitaria.mat1) = c("Ret (statistic)", "Ret (p-value)")
rownames(raiz.unitaria.mat1) = c("ADF", "PP", "KPSS")

raiz.unitaria.mat1[1,1] = adf.test(port.df$p.ret, alternative = c("stationary"))$statistic
raiz.unitaria.mat1[2,1] = pp.test(port.df$p.ret, alternative = c("stationary"))$statistic
raiz.unitaria.mat1[3,1] = kpss.test(port.df$p.ret, null = c("Level"))$statistic

raiz.unitaria.mat1[1,2] = adf.test(port.df$p.ret, alternative = c("stationary"))$p.value
raiz.unitaria.mat1[2,2] = pp.test(port.df$p.ret, alternative = c("stationary"))$p.value
raiz.unitaria.mat1[3,2] = kpss.test(port.df$p.ret, null = c("Level"))$p.value

tabela2 <- xtable(raiz.unitaria.mat1)
print.xtable(tabela2, type = "html", file = "tabela2.html")

raiz.unitaria.mat2 = matrix(NA, ncol = 2, nrow = 3)
colnames(raiz.unitaria.mat2) = c("Vol (statistic)","Vol (p-value)")
rownames(raiz.unitaria.mat2) = c("ADF", "PP", "KPSS")

raiz.unitaria.mat2[1,1] = adf.test(port.df$vol, alternative = c("stationary"))$statistic
raiz.unitaria.mat2[2,1] = pp.test(port.df$vol, alternative = c("stationary"))$statistic
raiz.unitaria.mat2[3,1] = kpss.test(port.df$vol, null = c("Level"))$statistic

raiz.unitaria.mat2[1,2] = adf.test(port.df$vol, alternative = c("stationary"))$p.value
raiz.unitaria.mat2[2,2] = pp.test(port.df$vol, alternative = c("stationary"))$p.value
raiz.unitaria.mat2[3,2] = kpss.test(port.df$vol, null = c("Level"))$p.value

tabela3 <- xtable(raiz.unitaria.mat2)
print.xtable(tabela3, type = "html", file = "tabela3.html")



#TESTE PARA EFEITOS ARCH

#testa a autocorrelacao dos retornos quadraticos ate quinze defasagens
library(FinTS)

do_arch_test <- function(x, max_lag = 15) {
  require(FinTS)
  require(tidyverse)
  
  do_single_arch <- function(x, used_lag)  {
    test_out <- FinTS::ArchTest(x, lags = used_lag)
    
    res_out <- tibble(Lag = used_lag,
                      `LMStatistic` = test_out$statistic, 
                      `pvalue` = test_out$p.value)
  }
  
  tab_out <- bind_rows(map(1:max_lag,.f = do_single_arch, x = x))
  
  return(tab_out)
}

arch.lm <- do_arch_test(x = port.df$p.ret, max_lag = 15)

tabela4 <- xtable(arch.lm)
print.xtable(tabela4, type = "html", file = "tabela4.html")



#SEPARACAO DA AMOSTRA PARA PREVISAO

port.sample <- filter(port.df, data < as.Date("2018-01-02"))

ggplot(port.df, aes(data, p.ret)) +
  geom_point() +
  geom_vline(xintercept = port.df$data [741],  color = "red") +
  labs(x = " ",
       y = "Retornos") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())



#SELECAO E ESTIMACAO DOS MODELOS

library(rugarch)

# garch

#escolhe a melhor especificacao do modelo atraves de criterios de informacao (AIC/BIC)
escolhe.ordem.garch = function(ret){
  max.order.p = 3
  max.order.q = 3
  ICS = matrix(0, ncol = 2, nrow = max.order.p*max.order.q)
  nomes.linhas = vector(length = max.order.p*max.order.q)
  cnt = 1
  for (p in  1:max.order.p){
    for (q in 1:max.order.q) {
      spec = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                        variance.model = list(model = "sGARCH", garchOrder = c(q,p)),
                        distribution.model = "norm")
      fit = ugarchfit(data = port.sample$p.ret, spec = spec)
      print(p)
      print(q)
      ICS[cnt,] = infocriteria(fit)[1:2]
      nomes.linhas[cnt] = paste("GARCH(",p,",",q,")",sep="")
      cnt = cnt + 1
    }  
  }
  melhor.AIC = (ICS[,1] == min(ICS[,1]))*1
  melhor.BIC = (ICS[,2] == min(ICS[,2]))*1
  out.mat = matrix(NA, ncol = 4, nrow = max.order.p*max.order.q)
  colnames(out.mat) = c("AIC", "min(AIC)", "BIC", "min(BIC)")
  rownames(out.mat) = nomes.linhas
  out.mat[,1] = ICS[,1]
  out.mat[,2] = melhor.AIC
  out.mat[,3] = ICS[,2]
  out.mat[,4] = melhor.BIC
  return(out.mat)
}

garch.BIC.AIC = escolhe.ordem.garch(port.sample$p.ret)
tabela5 <- xtable(garch.BIC.AIC)
print.xtable(tabela5, type = "html", file = "tabela5.html")

#primeiro fazemos a especificacao do modelo, foi selecionado o garch(1,1)
garch.spec = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                        variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                        distribution.model = "norm")

#agora estimamos o modelo
garch.fit = ugarchfit(data = port.sample$p.ret, spec = garch.spec)

#grafico do desvio padrao condicional estimado pelo GARCH dentro da amostra
plot(port.sample$data, port.sample$vol, t = "l", col = "grey", xlab = " ", ylab = "Retornos absolutos")
lines(port.sample$data, garch.fit@fit$sigma, col = "red")
title(main = "GARCH", line = 1, adj = 0)


# ewma

ewma.spec = ugarchspec(variance.model = list(model = "iGARCH", garchOrder = c(1,1)),
                       mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
                       distribution.model = "norm", fixed.pars = list(omega = 0))

ewma.fit = ugarchfit(data = port.sample$p.ret, spec = ewma.spec)

plot(port.sample$data, port.sample$vol, t = "l", col = "grey", xlab = " ", ylab = "Retornos absolutos")
lines(port.sample$data, ewma.fit@fit$sigma, col = "red")
title(main = "EWMA", line = 1, adj = 0)



#VALUE AT RISK DENTRO DA AMOSTRA

# garch

in.sample.fit.garch = garch.fit@fit$sigma

#calcula VaR5
in.sample.VaR5.garch = mean(port.sample$p.ret) + qnorm(0.05)*in.sample.fit.garch

#grafico VaR5
plot(port.sample$data, port.sample$p.ret, col = "grey", xlab = " ", ylab = "Retornos")
lines(port.sample$data, qnorm(0.05)*sqrt(garch.fit@fit$var), col = "red")
title(main = "VaR5 GARCH", line = 1, adj = 0)

#calcula numero e porcentagem de violacoes
viol.in.sample.VaR5.garch = (port.sample$p.ret < in.sample.VaR5.garch)*1
pviol.in.sample.VaR5.garch = sum(viol.in.sample.VaR5.garch)/length(in.sample.VaR5.garch)

#mostra numero e porcentagem de violacoes
sum(viol.in.sample.VaR5.garch)
pviol.in.sample.VaR5.garch


# ewma

in.sample.fit.ewma = ewma.fit@fit$sigma

#calcula VaR5
in.sample.VaR5.ewma = mean(port.sample$p.ret) + qnorm(0.05)*in.sample.fit.ewma

#grafico VaR5
plot(port.sample$data, port.sample$p.ret, col = "grey", xlab = " ", ylab = "Retornos")
lines(port.sample$data, qnorm(0.05)*sqrt(ewma.fit@fit$var), col = "red")
title(main = "VaR5 EWMA", line = 1, adj = 0)

#calcula numero e porcentagem de violacoes
viol.in.sample.VaR5.ewma = (port.sample$p.ret < in.sample.VaR5.ewma)*1
pviol.in.sample.VaR5.ewma = sum(viol.in.sample.VaR5.ewma)/length(in.sample.VaR5.ewma)

#mostra numero e porcentagem de violacoes
sum(viol.in.sample.VaR5.ewma)
pviol.in.sample.VaR5.ewma



#PREVISAO

library(timeSeries)
oos.dates = port.df$data[742:length(port.df$data)]
out.sample.ret = port.df$p.ret[742:length(port.df$p.ret)]

# garch

#faz a previsao fora da amostra
garch.roll = ugarchroll(garch.spec, data = port.df$p.ret, n.start = 741, refit.every = 1, n.ahead = 1)
garch.predicted.sigma = garch.roll@forecast$density %>% select(Sigma)
garch.realized.ret = garch.roll@forecast$density %>% select(Realized)

#grafico da volatilidade media prevista
plot(oos.dates, abs(garch.roll@forecast$density$Realized), xlab = " ", ylab = "Retornos absolutos")
lines(oos.dates, garch.roll@forecast$density$Sigma, col = "blue", lw = 2)
title(main = "PREVISÃO VOLATILIDADE GARCH", line = 1, adj = 0)

#grafico do VaR5 previsto
plot(oos.dates, garch.roll@forecast$density$Realized, xlab = " ", ylab = "Retornos")
lines(oos.dates, garch.roll@forecast$VaR$`alpha(5%)`, col = "red", lw = 2)
title(main = "PREVISÃO VaR5 GARCH", line = 1, adj = 0)

#calcula numero e porcentagem de violacoes
viol.out.sample.VaR5.garch = (out.sample.ret < garch.roll@forecast$VaR$`alpha(5%)`)*1
pviol.out.sample.VaR5.garch = sum(viol.out.sample.VaR5.garch)/length(garch.roll@forecast$VaR$`alpha(5%)`)

#mostra numero e porcentagem de violacoes
sum(viol.out.sample.VaR5.garch)
pviol.out.sample.VaR5.garch


# ewma 

#faz a previsao fora da amostra
ewma.roll = ugarchroll(ewma.spec, data = port.df$p.ret, n.start = 741, refit.every = 1, n.ahead = 1)
ewma.predicted.sigma = ewma.roll@forecast$density %>% select(Sigma)
ewma.realized.ret = ewma.roll@forecast$density %>% select(Realized)

#grafico da volatilidade media prevista
plot(oos.dates, abs(ewma.roll@forecast$density$Realized), xlab = " ", ylab = "Retornos absolutos")
lines(oos.dates, ewma.roll@forecast$density$Sigma, col = "blue", lw = 2)
title(main = "PREVISÃO VOLATILIDADE EWMA", line = 1, adj = 0)

#grafico do VaR5 previsto
plot(oos.dates, ewma.roll@forecast$density$Realized, xlab = " ", ylab = "Retornos")
lines(oos.dates, ewma.roll@forecast$VaR$`alpha(5%)`, col = "red", lw = 2)
title(main = "PREVISÃO VaR5 EWMA", line = 1, adj = 0)

#calcula numero e porcentagem de violacoes
viol.out.sample.VaR5.ewma = (out.sample.ret < ewma.roll@forecast$VaR$`alpha(5%)`)*1
pviol.out.sample.VaR5.ewma = sum(viol.out.sample.VaR5.ewma)/length(ewma.roll@forecast$VaR$`alpha(5%)`)

#mostra numero e porcentagem de violacoes
sum(viol.out.sample.VaR5.ewma)
pviol.out.sample.VaR5.ewma



