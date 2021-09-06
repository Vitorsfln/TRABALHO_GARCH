
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
df <- read_excel('DADOS_TRABALHO.xlsx', 
                 col_types = c('text','date', 'numeric'))

df$data <- as.Date(df$data, format = '%Y-%m-%d')

#calcula os retornos
price.data <- df %>% 
  na.omit(df) %>% 
  group_by(ticker) %>%
  tq_mutate(select = price.close,
            mutate_fun = periodReturn,
            period = 'daily',
            col_rename = 'ret')

price.data <- price.data[-c(1, 1235, 2469), ]

port.df <- price.data %>% 
  tq_portfolio(assets_col = ticker,
               returns_col = ret,
               weights = c(0.5, 0.3, 0.2),
               geometric = FALSE,
               col_rename = 'p.ret') %>% 
  mutate(vol = sqrt((p.ret-mean(p.ret, na.rm = TRUE))^2) )



#GRAFICOS DOS RETORNOS

library(cowplot)

p1 <- ggplot(price.data[1:1233, ], aes(data, ret)) + 
  geom_line() + 
  labs(title = ("BOVA11"),
       x = " ",
       y = "Retornos") + 
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  theme(panel.grid.minor = element_blank()) + 
  geom_hline(yintercept = 0, color = 'red')

p2 <- ggplot(price.data[1234:2466, ], aes(data, ret)) + 
  geom_line() + 
  labs(title = ("IVVB11"),
       x = " ",
       y = "Retornos") + 
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  theme(panel.grid.minor = element_blank()) + 
  geom_hline(yintercept = 0, color = 'red')

p3 <- ggplot(price.data[2467:3699, ], aes(data, ret)) + 
  geom_line() + 
  labs(title = ("SMAL11"),
       x = " ",
       y = "Retornos") + 
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  theme(panel.grid.minor = element_blank()) + 
  geom_hline(yintercept = 0, color = 'red') 

p4 <- ggplot(port.df, aes(data, p.ret)) + 
  geom_line() + 
  labs(title = ("PORTFOLIO"),
       x = " ",
       y = "Retornos") + 
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  theme(panel.grid.minor = element_blank()) + 
  geom_hline(yintercept = 0, color = 'red') 

grid1 <- plot_grid(p1, p2, p3, p4, nrow = 2, 
                   labels = " ")
grid1


#ESTATISTICAS DESCRITIVAS

#assimetria e curtose n?o vem com o R
library(moments)
library(xtable)

stat.desc = matrix(NA, nrow = 8, ncol = 1)

rownames(stat.desc) = c("Obs.", "M?n.", "M?dia", "Mediana", "M?x.", "D.P.", "Assim.", "Curt.")
colnames(stat.desc) = c("Retorno")

calc.stat.desc = function(x){
  out = rep(0, 8)
  out[1] = length(x)
  out[2] = min(x)
  out[3] = mean(x)
  out[4] = median(x)
  out[5] = max(x)
  out[6] = sd(x)
  out[7] = skewness(x)
  out[8] = kurtosis(x) #curtose bruta
  return(out)
}

#multiplica os retornos por 100 para ficar mais facil de interpretar a tabela. Ou seja, mostra-se a porcentagem, nao numeros decimais
stat.desc[,1] = calc.stat.desc(100*port.df$p.ret)

#salva a tabela como arquivo
tabela1 <- xtable(stat.desc, digits = 3)
print.xtable(tabela1, type = "html", file = "tabela1.html")



#TESTES DE RAIZ UNITARIA(?)



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

tabela2 <- xtable(arch.lm)
print.xtable(tabela2, type = "html", file = "tabela2.html")



#SELECAO E ESTIMACAO DOS MODELOS

library(rugarch)

# arch 

#escolhe a melhor defasagem atraves de criterios de informacao
escolhe.ordem.arch = function(ret, modelo="sGARCH"){
  max.order.q = 3
  ICS = matrix(0, ncol = 2, nrow = 1*max.order.q)
  nomes.linhas = vector(length = max.order.q)
  cnt = 1
  for (q in 2:max.order.q) {
    spec = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                      variance.model = list(model = "sGARCH", garchOrder = c(q,0)),
                      distribution.model = "norm")
    fit = ugarchfit(data = port.df$p.ret, spec = spec, solver = 'hybrid')
    print(q)
    ICS[cnt,] = infocriteria(fit)[1:2]
    nomes.linhas[cnt] = paste("ARCH(0,",q,")",sep = "")
    cnt = cnt + 1
  }  
  melhor.AIC = (ICS[,1] == min(ICS[,1]))*1
  melhor.BIC = (ICS[,2] == min(ICS[,2]))*1
  out.mat = matrix(NA, ncol = 4, 1*max.order.q)
  colnames(out.mat) = c("AIC", "min(AIC)", "BIC", "min(BIC)")
  rownames(out.mat) = nomes.linhas
  out.mat[,1] = ICS[,1]
  out.mat[,2] = melhor.AIC
  out.mat[,3] = ICS[,2]
  out.mat[,4] = melhor.BIC
  return(out.mat)
}

arch.BIC.AIC = escolhe.ordem.arch(port.df$p.ret)
tabela3 <- xtable(arch.BIC.AIC)
print.xtable(tabela3, type = "html", file = "tabela3.html")

#primeiro fazemos a especificacao do modelo (no caso, a funcao teve problema para calcular os criterios do arch(1), mas podemos calcula-lo separadamente para comparar os valores), foi selecionado o arch(1)
arch.spec = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                       variance.model = list(model = "sGARCH", garchOrder = c(1,0)),
                       distribution.model = "norm")

#agora estimamos o modelo
arch.fit = ugarchfit(data = port.df$p.ret, spec = arch.spec, solver = 'hybrid')

#grafico do desvio padrao condicional estimado
plot(port.df$data, port.df$vol, t = "l", col = "grey", xlab = " ", ylab = "Retornos absolutos")
lines(port.df$data, arch.fit@fit$sigma, col = "red")
title(main = "ARCH", line = 1, adj = 0)


# garch

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
      fit = ugarchfit(data = port.df$p.ret, spec = spec)
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

garch.BIC.AIC = escolhe.ordem.garch(port.df$p.ret)
tabela4 <- xtable(garch.BIC.AIC)
print.xtable(tabela4, type = "html", file = "tabela4.html")

#foi selecionado o garch(1,1)
garch.spec = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                        variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                        distribution.model = "norm")

garch.fit = ugarchfit(data = port.df$p.ret, spec = garch.spec)

#grafico do desvio padrao condicional estimado
plot(port.df$data, port.df$vol, t = "l", col = "grey", xlab = " ", ylab = "Retornos absolutos")
lines(port.df$data, garch.fit@fit$sigma, col = "red")
title(main = "GARCH", line = 1, adj = 0)



#VALUE AT RISK DENTRO DA AMOSTRA

# arch

#calcula VaR5
in.sample.fit.arch = arch.fit@fit$sigma

in.sample.VaR5.arch = mean(port.df$p.ret) + qnorm(0.05)*in.sample.fit.arch

#grafico VaR5
plot(port.df$data, port.df$p.ret, col = "grey", xlab = " ", ylab = "Retornos")
lines(port.df$data, qnorm(0.05)*sqrt(arch.fit@fit$var), col = "red")
title(main = "VaR ARCH(1)", line = 1, adj = 0)


# garch

#calcula VaR5
in.sample.fit.garch = garch.fit@fit$sigma

in.sample.VaR5.garch = mean(port.df$p.ret) + qnorm(0.05)*in.sample.fit.garch

#grafico VaR5
plot(port.df$data, port.df$p.ret, col = "grey", xlab = " ", ylab = "Retornos")
lines(port.df$data, qnorm(0.05)*sqrt(garch.fit@fit$var), col = "red")
title(main = "VaR GARCH(1,1)", line = 1, adj = 0)


