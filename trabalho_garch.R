
#seleciona o diretório do arquivo como diretório de trabalho
my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)

#limpa as variáveis (shift+F10 se quiser resetar tudo incluindo plots, pacotes, console, etc)
rm(list=ls()) 
#retira notação científica
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

#retira as observações perdidas no cálculo dos retornos
price.data <- price.data[-c(1, 1235, 2469), ]

port.data <- price.data %>% 
  tq_portfolio(assets_col = ticker,
               returns_col = ret,
               weights = c(0.5, 0.3, 0.2),
               geometric = FALSE,
               col_rename = 'port.ret')