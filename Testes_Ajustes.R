
# Carregar Bibliotecas e Funcoes ####
source("Funcoes.R")

# Carregar Dados Brutos ####

raw_data <- read.csv("dados_brutos.csv", sep = ",", dec = ".", header = T)
dados_ht <- read.csv("extra_info.csv", sep = ",", dec = ".", header = T)

# Calc vol total pelos métodos: Cubagem,criterion, e composto para limites de 1 a 6 ####

all_data <- bind_rows(CubagemComp(raw_data,Comp=0,id = "Alternativa 2"),
                      CubagemComp(raw_data, 1.3, id  = "Alternativa 3"),
                      CubagemComp(raw_data, 2.3, id  = "Alternativa 4"),
                      CubagemComp(raw_data, 3.3, id  = "Alternativa 5"),
                      CubagemComp(raw_data, 4.3, id  = "Alternativa 6"),
                      CubagemComp(raw_data, 5.3, id  = "Alternativa 7")) %>%
           left_join(dados_ht, by = "arvore") %>%
           filter(arvore != 14) %>% #Retirando Outlier
           arrange(Alternativa)
             
# demonstracao marcio ####

names(raw_data)[names(raw_data) == "Alternativa" ] <- "metodo"
names(raw_data)[names(raw_data) == "metodo" ] <- "Alternativa"

Comp = 0
          raw_data %>%
            group_by(metodo, arvore) %>%
            mutate( # funcao para adicionar novas variaveis
              AS_CC = (dcc^2 * pi) / 40000, # Calculo da AS com casca
              VCC   = ( (AS_CC + lead(AS_CC) )/2 ) * (lead(secao) - secao) ) %>% # Calculo do volume com casca
            summarise(
              Alternativa = "Alternativa 2",
              VCC = sum(VCC, na.rm = T)    ) %>%
            ungroup %>%
            spread(metodo, VCC) %>%
            rename(
              vol_criterion = Criterion, 
              vol_cubagem   = Cubagem     ) %>%
            mutate(er     = round(((vol_criterion - vol_cubagem)/vol_cubagem)*100, 2) )
            
         
  Comp = 1         
            bind_rows( 
              filter(raw_data, metodo == "Cubagem" & secao <= 1.3),
              filter(raw_data, metodo == "Criterion" & secao > 1.3) ) %>%
            arrange(arvore, secao) %>%
            group_by(arvore) %>%
            mutate( # funcao para adicionar novas variaveis
              AS_CC = (dcc^2 * pi) / 40000, # Calculo da AS com casca
              VCC   = ( (AS_CC + lead(AS_CC) )/2 ) * (lead(secao) - secao) ) %>% # Calculo do volume com casca
            summarise(
              Alternativa   = "Alternativa 3",
              vol_criterion = sum(VCC, na.rm = T)    ) %>%
              bind_cols( 
                raw_data %>%
                filter(metodo == "Cubagem") %>% 
                group_by(arvore) %>%
                mutate( # funcao para adicionar novas variaveis
                AS_CC = (dcc^2 * pi) / 40000, # Calculo da AS com casca
                VCC   = ( (AS_CC + lead(AS_CC) )/2 ) * (lead(secao) - secao) ) %>% # Calculo do volume com casca
                summarise(vol_cubagem = sum(VCC, na.rm = T) ) %>%
                  select(-arvore)  
                                       ) %>%
              mutate(er = round(((vol_criterion - vol_cubagem)/vol_cubagem)*100, 2) )
          

          



# Exportar dados ####

#write.csv(all_data, "D:/Documents/Trabalhos_Mensuracao/Projeto_Criterion/Criterion_R/tab_totaliz.csv")
#write.xlsx(all_data, "D:/Documents/Trabalhos_Mensuracao/Projeto_Criterion/Criterion_R/tab_totaliz.xlsx")

# Importar dados ####

#all_data  <- read.xlsx("critxcub.xlsx", sheetName = "all", header = TRUE)
#all_data <- read.csv("tab_totaliz.csv", header = TRUE, check.names = FALSE)

# Teste F de Graybill com resultado ####

vol_cub_graybill_res <- all_data %>% 
  #filter(Alternativa %in% c("Alternativa 2", "Alternativa 3", "Alternativa 4", "Alternativa 5") ) %>%
  group_by(Alternativa) %>%
  do(Gt(.$vol_cubagem,.$vol_criterion)) %>%
  ungroup(.) %>%
  do(rbind(all_data %>% 
             filter(Alternativa == "Alternativa 2") %>% 
             do(Gt(.$ht_cubagem,.$ht_criterion)) %>% 
             mutate(Alternativa = "Ht est"),.)) %>%
  do(data.frame(row.names=.$Alternativa,.[,-6]))

# Teste F de Graybill sem resultado ####

vol_cub_graybill <- all_data %>% 
  #filter(Alternativa %in% c("Alternativa 2", "Alternativa 3", "Alternativa 4", "Alternativa 5") ) %>%
    group_by(Alternativa) %>%
  do(Gt(.$vol_cubagem,.$vol_criterion, 1)) %>%
  ungroup(.) %>%
  do(rbind(all_data %>% 
             filter(Alternativa == "Alternativa 2") %>% 
             do(Gt(.$ht_cubagem,.$ht_criterion, 1)) %>% 
             mutate(Alternativa = "Ht est"),.)) %>%
  do(data.frame(row.names=.$Alternativa,.[,-5]))

# Ajuste dos Modelos de Schummacher (Volume) e Kozak (Taper) ####

# Dap e Ht Criterion

tab_coef_all_crit <- all_data %>%
  group_by(Alternativa) %>% # Criterion ate cubagem 6.3
  do(tabReg_(lm(formula = log(.$vol_criterion) ~ log(.$dap_criterion) + log(.$ht_criterion)))) %>%
  ungroup(.) %>%
  do(rbind(all_data %>%
             filter(Alternativa == "Alternativa 2") %>% #Cubagem
             do(tabReg_(lm(formula = log(.$vol_cubagem) ~ log(.$dap_cubagem) + log(.$ht_cubagem)))) %>%
            mutate(Alternativa = "Alternativa 1"), ., #atencao aqui
           raw_data %>% #Kozak
             mutate(d_sob_dap_quad = (dcc/dap)^2, h_sob_ht_quad = (secao/ht)^2) %>%
             group_by(Alternativa) %>%
             do(tabReg_(lm(formula = .$d_sob_dap_quad ~ sqrt(.$h_sob_ht_quad) + .$h_sob_ht_quad))) %>%
             ungroup(.) %>%
             mutate(Alternativa = c("Kozak Criterion", "Kozak Cubagem"))
            )) %>%
  do(data.frame(row.names=.$Alternativa,.[,-6]))

            
            
            
            tab_coef_all_crit <- bind_rows(
              
                         all_data %>% # modelo de volume utilizando cubagem padrao
                         filter(Alternativa == "Alternativa 2") %>% #Cubagem
                           do(mod = lm(log(vol_criterion) ~ log(dap_criterion) + log(ht_criterion), data = .  )  ) %>%
                           rowwise %>% # aplicar funcoes na linha
                           transmute(b0=coef(mod)[1], # a variavel mod, criada anteriormente, possui os coeficientes na ordem, por isso os extraimos com []
                                     b1=coef(mod)[2],
                                     b2=coef(mod)[3],
                                     Rsqr=summary(mod)[[9]], # extraimos r quadrado ajustado do summario de mod
                                     Std.Error=summary(mod)[[6]],
                                     Alternativa = "Alternativa 1"), # extraimos o erro do summario de mod
  

                         all_data %>% # cubagem metodos alternativos
                           group_by(Alternativa) %>% # Criterion ate cubagem 6.3
                           do(mod = lm(log(vol_criterion) ~ log(dap_criterion) + log(ht_criterion), data = .  )  ) %>%
                           mutate(b0=coef(mod)[1], # a variavel mod, criada anteriormente, possui os coeficientes na ordem, por isso os extraimos com []
                                  b1=coef(mod)[2],
                                  b2=coef(mod)[3],
                                  Rsqr=summary(mod)[[9]], # extraimos r quadrado ajustado do summario de mod
                                  Std.Error=summary(mod)[[6]]) %>% # extraimos o erro do summario de mod
                           select(-mod), 
                         
                       raw_data %>% #Kozak
                         mutate(d_sob_dap_quad = (dcc/dap)^2, h_sob_ht_quad = (secao/ht)^2) %>%
                         group_by(Alternativa) %>%
                         do(mod = lm(d_sob_dap_quad ~ sqrt(h_sob_ht_quad) + h_sob_ht_quad, data=. ) ) %>%
                         transmute(
                                b0=coef(mod)[1], # a variavel mod, criada anteriormente, possui os coeficientes na ordem, por isso os extraimos com []
                                b1=coef(mod)[2],
                                b2=coef(mod)[3],
                                Rsqr=summary(mod)[[9]], # extraimos r quadrado ajustado do summario de mod
                                Std.Error=summary(mod)[[6]]) %>% # extraimos o erro do summario de mod
                         ungroup %>%
                         mutate(Alternativa = c("Kozak Criterion", "Kozak Cubagem"))
               ) %>%
              do(data.frame(row.names=.$Alternativa,.[,-6]))
            
            
            
            
            
 # Dap e Ht Cubagem



tab_coef_all_crit_cor <- all_data %>%
  group_by(Alternativa) %>%  # Ajuste das equcacoes das alternativas 2 a 7
  do(tabReg_(lm(formula = log(.$vol_criterion) ~ log(.$dap_cubagem) + log(.$ht_cubagem)))) %>%
  ungroup(.) %>%
  do(rbind(all_data %>% # Rbind une os 3 ajustes
             filter(Alternativa == "Alternativa 2") %>% # Ajuste alternativa 1 (Cubagem)
             do(tabReg_(lm(formula = log(.$vol_cubagem) ~ log(.$dap_cubagem) + log(.$ht_cubagem)))) %>%
             mutate(Alternativa = "Alternativa 1"), .,
           raw_data %>% # Ajuste Kozak
             mutate(d_sob_dap_quad = (dcc/dap)^2, h_sob_ht_quad = (secao/ht)^2) %>%
             group_by(Alternativa) %>%
             do(tabReg_(lm(formula = .$d_sob_dap_quad ~ sqrt(.$h_sob_ht_quad) + .$h_sob_ht_quad))) %>%
             ungroup(.) %>%
             mutate(Alternativa = c("Kozak Criterion", "Kozak Cubagem"))
  )) %>%
  do(data.frame(row.names=.$Alternativa,.[,-6])) # Passa a coluna "Alternativa" para os nomes das linhas

# Estimacao do volume utilizando as equacoes em dados de inventario florestal ####

# Importacao dos dados, calculo de variaveis
dados_inv <- read.csv("dados_mc.csv", sep = ",", dec = ".", header = T) %>%
  na.omit %>%
  filter(DESCESPECIE == "CLONE", CODTALHAO %in% c(3619, 3674, 3914, 4536, 4736)) %>%
  mutate(DAP = CAP / pi, ln_HT = log(ALT1), ln_DAP = log(DAP))

#resumir nome da variavel
v <- tab_coef_all_crit_cor

#Calculo de volume para cada alternativa
dados_inv1 <- dados_inv %>%
  mutate(Alternativa_1 = exp(v["Alternativa 1","b0"] + v["Alternativa 1","b1"]*ln_HT + v["Alternativa 1","b2"]*ln_DAP), 
         Alternativa_2 = exp(v["Alternativa 2","b0"] + v["Alternativa 2","b1"]*ln_HT + v["Alternativa 2","b2"]*ln_DAP),
         Alternativa_3 = exp(v["Alternativa 3","b0"] + v["Alternativa 3","b1"]*ln_HT + v["Alternativa 3","b2"]*ln_DAP),
         Alternativa_4 = exp(v["Alternativa 4","b0"] + v["Alternativa 4","b1"]*ln_HT + v["Alternativa 4","b2"]*ln_DAP),
         Alternativa_5 = exp(v["Alternativa 5","b0"] + v["Alternativa 5","b1"]*ln_HT + v["Alternativa 5","b2"]*ln_DAP),
         Alternativa_6 = exp(v["Alternativa 6","b0"] + v["Alternativa 6","b1"]*ln_HT + v["Alternativa 6","b2"]*ln_DAP),
         Alternativa_7 = exp(v["Alternativa 7","b0"] + v["Alternativa 7","b1"]*ln_HT + v["Alternativa 7","b2"]*ln_DAP)
         )
  
#dados_inv1[,c(30,31,32,33,34)]
#gather(dados_inv1[,c(31,32,33,34)], "Alternativa", "VOL_EST")

vol_est <- cbind(gather(dados_inv1[,c(31,32,33,34,35,36)], "Alternativa", "VOL_EST"), VOL_OBS = rep(dados_inv1[,"Alternativa_1"], 6), HT = rep(dados_inv1[,"ALT1"], 6)) %>%
  group_by(Alternativa) %>%
  mutate(er = round(((VOL_EST - VOL_OBS)/VOL_OBS)*100, 2), er_med = mean(er))

levels(vol_est$Alternativa) <- c("Alternativa 2", "Alternativa 3", "Alternativa 4", "Alternativa 5", "Alternativa 6", "Alternativa 7")


vol_est_graybill_res <- vol_est %>%
  group_by(Alternativa) %>%
  do(Gt(.$VOL_OBS, .$VOL_EST)) %>%
  ungroup %>%
do(data.frame(row.names=.$Alternativa,.[,-1]))

vol_est_graybill <- vol_est %>%
  group_by(Alternativa) %>%
  do(Gt(.$VOL_OBS, .$VOL_EST,1)) %>%
  ungroup %>%
  do(data.frame(row.names=.$Alternativa,.[,-1]))


tab_graybill_res <- rbind(vol_cub_graybill_res, vol_est_graybill_res)
tab_graybill <- rbind(vol_cub_graybill, vol_est_graybill)

#

# exportar tabelas para xlsx ####

#  - Tabelas Finais 
write.xlsx(vol_cub_graybill, "D:/Documents/Trabalhos_Mensuracao/Projeto_Criterion/Criterion_R/Tabelas/Tabelas_Finais/vol_cub_graybill.xlsx")
write.xlsx(vol_cub_graybill_res, "D:/Documents/Trabalhos_Mensuracao/Projeto_Criterion/Criterion_R/Tabelas/Tabelas_Finais/vol_cub_graybill_res.xlsx")
write.xlsx(tab_coef_all_crit, "D:/Documents/Trabalhos_Mensuracao/Projeto_Criterion/Criterion_R/Tabelas/Tabelas_Finais/tab_coef_all_crit.xlsx")
write.xlsx(tab_coef_all_crit_cor, "D:/Documents/Trabalhos_Mensuracao/Projeto_Criterion/Criterion_R/Tabelas/Tabelas_Finais/tab_coef_all_crit_cor.xlsx")
write.xlsx(vol_est_graybill, "D:/Documents/Trabalhos_Mensuracao/Projeto_Criterion/Criterion_R/Tabelas/Tabelas_Finais/vol_est_graybill.xlsx")
write.xlsx(vol_est_graybill_res, "D:/Documents/Trabalhos_Mensuracao/Projeto_Criterion/Criterion_R/Tabelas/Tabelas_Finais/vol_est_graybill_res.xlsx")
write.xlsx(tab_graybill, "D:/Documents/Trabalhos_Mensuracao/Projeto_Criterion/Criterion_R/Tabelas/Tabelas_Finais/tab_graybill.xlsx")
write.xlsx(tab_graybill_res, "D:/Documents/Trabalhos_Mensuracao/Projeto_Criterion/Criterion_R/Tabelas/Tabelas_Finais/tab_graybill_res.xlsx")

# Visualizar as tabelas Finais ####

tab_coef_all_crit
tab_coef_all_crit_cor

vol_cub_graybill_res
vol_cub_graybill

vol_est_graybill_res 
vol_est_graybill

tab_graybill_res
tab_graybill

