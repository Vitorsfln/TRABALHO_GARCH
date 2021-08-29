
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

#retira as observacoes perdidas no calculo dos retornos
price.data <- price.data[-c(1, 1235, 2469), ]

port.data <- price.data %>% 
  tq_portfolio(assets_col = ticker,
               returns_col = ret,
               weights = c(0.5, 0.3, 0.2),
               geometric = FALSE,
               col_rename = 'port.ret')



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

p4 <- ggplot(port.data, aes(data, port.ret)) + 
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

#assimetria e curtose nao vem com o R
library(moments)
library(xtable)

stat.desc = matrix(NA, nrow = 8, ncol = 1)

rownames(stat.desc) = c("Obs.", "Min.", "Media", "Mediana", "Max.", "D.P.", "Assim.", "Curt.")
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
stat.desc[,1] = calc.stat.desc(100*port.data$port.ret)

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

arch.lm <- do_arch_test(x = port.data$port.ret, max_lag = 15)

tabela2 <- xtable(arch.lm)
print.xtable(tabela2, type = "html", file = "tabela2.html")



#SELECAO E ESTIMACAO DOS MODELOS



