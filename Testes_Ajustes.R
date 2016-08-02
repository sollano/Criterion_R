# Carregar Bibliotecas e Funcoes ####
source("Funcoes.R")

# Carregar Dados Brutos ####

raw_data <- read.csv("dados_brutos.csv", sep = ";", dec = ",", header = T)
dados_ht <- read.csv("extra_info.csv"  , sep = ";", dec = ",", header = T)

# Calc vol total pelos métodos: Cubagem,criterion, e composto para limites de 1 a 6 ####

all_data <- bind_rows(CubagemComp(raw_data,Comp=0,id = "Alternativa_2"),
                      CubagemComp(raw_data, 1.3, id  = "Alternativa_3"),
                      CubagemComp(raw_data, 2.3, id  = "Alternativa_4"),
                      CubagemComp(raw_data, 3.3, id  = "Alternativa_5")) %>%
           left_join(dados_ht, by = "arvore") %>%
           filter(arvore != 14) %>% #Retirando Outlier
           arrange(Alternativa)

# demonstracao marcio ####

Comp = 0
          raw_data %>%
            group_by(metodo, arvore) %>%
            mutate( # funcao para adicionar novas variaveis
              AS_CC = (dcc^2 * pi) / 40000, # Calculo da AS com casca
              VCC   = ( (AS_CC + lead(AS_CC) )/2 ) * (lead(secao) - secao) ) %>% # Calculo do volume com casca
            summarise(
              Alternativa = "Alternativa_2",
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
              Alternativa   = "Alternativa_3",
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
          

          



# F de Graybill cubagem ####

gt_cub_vol <- all_data %>% 
  group_by(Alternativa) %>%
  do(Gt(.$vol_cubagem,.$vol_criterion, id = "cubagem_vol")) 
            
gt_cub_ht <- all_data %>% 
  filter(Alternativa == "Alternativa_2") %>% 
  do(Gt(.$ht_cubagem,.$ht_criterion, id = "cubagem_ht")) %>% 
  mutate(Alternativa = "Alternativa 2")             

# Ajuste dos Modelos de Schummacher (Volume) e Kozak (Taper) ####

 # Dap e Ht Cubagem

tab_coef_f <- bind_rows(
  
  all_data %>% # modelo de volume utilizando cubagem padrao
    filter(Alternativa == "Alternativa_2") %>% #Cubagem
    do(mod = lm(log(vol_cubagem) ~ log(dap_cubagem) + log(ht_cubagem), data = .  )  ) %>%
    rowwise %>% # aplicar funcoes na linha
    transmute(b0=coef(mod)[1], # a variavel mod, criada anteriormente, possui os coeficientes na ordem, por isso os extraimos com []
              b1=coef(mod)[2],
              b2=coef(mod)[3],
              Rsqr=summary(mod)[[9]], # extraimos r quadrado ajustado do summario de mod
              Std.Error=summary(mod)[[6]],
              Alternativa = "Alternativa_1"), # extraimos o erro do summario de mod
  
  all_data %>% # cubagem metodos alternativos
    group_by(Alternativa) %>% # Criterion ate cubagem 6.3
    do(mod = lm(log(vol_criterion) ~ log(dap_cubagem) + log(ht_cubagem), data = .  )  ) %>%
    mutate(b0=coef(mod)[1], # a variavel mod, criada anteriormente, possui os coeficientes na ordem, por isso os extraimos com []
           b1=coef(mod)[2],
           b2=coef(mod)[3],
           Rsqr=summary(mod)[[9]], # extraimos r quadrado ajustado do summario de mod
           Std.Error=summary(mod)[[6]]) %>% # extraimos o erro do summario de mod
    select(-mod), 
  
  raw_data %>% #Kozak
    mutate(d_sob_dap_quad = (dcc/dap)^2, h_sob_ht_quad = (secao/ht)^2) %>%
    group_by(metodo) %>%
    do(mod = lm(d_sob_dap_quad ~ sqrt(h_sob_ht_quad) + h_sob_ht_quad, data=. ) ) %>%
    transmute(
      b0=coef(mod)[1], # a variavel mod, criada anteriormente, possui os coeficientes na ordem, por isso os extraimos com []
      b1=coef(mod)[2],
      b2=coef(mod)[3],
      Rsqr=summary(mod)[[9]], # extraimos r quadrado ajustado do summario de mod
      Std.Error=summary(mod)[[6]]) %>% # extraimos o erro do summario de mod
    ungroup %>%
    mutate(Alternativa = c("Kozak Criterion", "Kozak Cubagem"))
) 
            
# Estimacao do volume utilizando as equacoes em dados de inventario florestal ####


# Importando e Preparando os dados de inventario ####

dados_inv_raw <- read.csv("dados_mc.csv", sep = ",", dec = ".", header = T) %>% mutate(DAP = CAP / pi, HT = ALT1, ALT1 = NULL,CAP=NULL)

#dados_inv_raw <- read_excel("~/Trabalhos_Mensuracao/Proj V&M/Dados_IA/Dados_JPO.xlsx",                  sheet = "IPC") %>% mutate(DAP = CAP / pi, HT = ALT1, ALT1 = NULL,CAP=NULL)

# Podemos utilizar todos os talhoes contidos neste dado
# ou podemos selecionar talhoes aleatoriamente 
            
# Funcao que seleciona niveis de um fator aleatoriamente
#talhoes <- sample.factor.levels(dados_inv_raw_, "CODTALHAO", 5)

# todos os talhoes
talhoes <- levels(factor(dados_inv_raw$CODTALHAO))

# Removemos arvores quebras, com ponta seca, etc
dados_inv_raw_ <- dados_inv_raw %>%
 filter(DESCCATEGORIA %in% c("Normal", "Dominante") )

# Tab de coeficientes
 tab_coef_inv <-  dados_inv_raw_ %>%
   filter(CODTALHAO %in% talhoes,
          !is.na(DAP)           ,
          !is.na(HT)            ) %>%
   hdjoin(c("CODTALHAO", "CODPARCELA"), "HT",  "DAP", "DESCCATEGORIA", "Dominante") %>%
   mutate(LN_HT = log(HT), INV_DAP = 1/DAP, LN_HD = log(HD) ) %>% # variaveis necessarias para reg
   group_by(CODTALHAO) %>% # chave
   do(mod = lm(LN_HT ~ INV_DAP + LN_HD, data =.)) %>% # Ajuste do Modelo
   mutate(b0=coef(mod)[1], 
          b1=coef(mod)[2],
          b2=coef(mod)[3],
          Rsqr=summary(mod)[[9]],
          Std.Error=summary(mod)[[6]]   ) %>%
   select(-mod) 
 
 # calculo da altura estimada utilziando os coeficientes gerados anteriormente
 dados_inv <- dados_inv_raw_ %>% # Definicao do df
   filter(CODTALHAO %in% talhoes,
          !is.na(DAP)            ) %>%
   hdjoin(grupos=c("CODTALHAO", "CODPARCELA"), HT = "HT", DAP = "DAP", OBS = "DESCCATEGORIA", dom = "Dominante") %>%
   right_join(tab_coef_inv, by = c("CODTALHAO") ) %>% # uniao com coef da eq de altura
   mutate(HT_EST = ifelse(!is.na(HT), HT, exp(b0 + b1*1/DAP + b2*log(HD) ) ) ) %>% # calculo do HT para arvores nm
   select(-b0,-b1,-b2,-Rsqr, -Std.Error) # remocao de variaveis descartaveis
 

# Calculo de volume para cada alternativa ####

 # Calculo do volume utilizando a tabela de coeficientes de volume gerados anteriormente
 # e uma funcao customizada, para facilitar o codigo
dados_inv_arv <- dados_inv %>%
   sch_vol(tab_coef_f[1, ], name ="Alternativa_1") %>% 
   sch_vol(tab_coef_f[2, ], name ="Alternativa_2") %>% 
   sch_vol(tab_coef_f[3, ], name ="Alternativa_3") %>% 
   sch_vol(tab_coef_f[4, ], name ="Alternativa_4") %>% 
   sch_vol(tab_coef_f[5, ], name ="Alternativa_5") # %>% head
 

# Calculo do erro em nivel arvore ####
 
vol_est_arv <- dados_inv_arv %>%
   select(matches("Alternativa"), HT_EST, CODTALHAO, CODPARCELA) %>% 
   gather("Alternativa", "VOL_EST", -CODTALHAO, -CODPARCELA, - Alternativa_1, -HT_EST ) %>% 
   select(CODTALHAO, 
          CODPARCELA,
          Alternativa,  
          VOL_OBS = Alternativa_1, 
          VOL_EST, 
          HT = HT_EST ) %>%
   mutate(er = round(((VOL_EST - VOL_OBS)/VOL_OBS)*100, 2) )

# Calculo do erro em nivel parcela ####
 
 vol_est_parcela <- vol_est_arv                 %>% 
   group_by(CODTALHAO, CODPARCELA, Alternativa) %>% 
   summarise(VOL_OBS = sum(VOL_OBS),
             VOL_EST = sum(VOL_EST),
             HT = mean(HT)         ,
             er = mean(er)                     )%>% 
   arrange(Alternativa)
 
 
 
 
 
# Calculo do erro em nivel talhao ####

vol_est_talhao <- vol_est_arv      %>% 
  group_by(CODTALHAO, Alternativa) %>% 
  summarise(VOL_OBS = sum(VOL_OBS),
            VOL_EST = sum(VOL_EST),
            HT = mean(HT)         ,
            er = mean(er)         )%>% 
  arrange(Alternativa)



# F de graybill inventario  ####

gt_inv_arv <- vol_est_arv %>%
  group_by(Alternativa) %>%
  do(Gt(.$VOL_OBS, .$VOL_EST, "inv_vol_arvore")) 

gt_inv_parc <- vol_est_parcela %>%
  group_by(Alternativa) %>%
  do(Gt(.$VOL_OBS, .$VOL_EST, "inv_vol_parcela")) 
 
gt_inv_talh <- vol_est_talhao %>%
  group_by(Alternativa) %>%
  do(Gt(.$VOL_OBS, .$VOL_EST, "inv_vol_talhao")) 
 

tab_graybill <- bind_rows(gt_cub_vol, 
                      gt_cub_ht, 
                      gt_inv_arv, 
                      gt_inv_parc, 
                      gt_inv_talh) %>% 
  select(F_H0, F_crit, P_valor, Alpha, Teste, id, Alternativa) %>% 
  arrange(id, Alternativa)

tab_graybill

# exportar tabelas para xlsx ####

# utlizamos a funcao write.xlsx2 do pacote xlsx;
# Utilizamos xlsx2 pois a funcao com 2 no final
# converte os separadores para o padrao do computador (brasileiro);
# precisamos converter os dados para data frame, pois,
# o pacote nao lida bem com objetos tbl_df (criados pelo dplyr);
# utilizamos row.names=F para remover row.names da tabela ao exportar.

write.xlsx2(as.data.frame(all_data)       , "Tabelas/cub_vol_arvore_alternativas.xlsx", row.names=F)
write.xlsx2(as.data.frame(tab_coef_f)     , "Tabelas/tab_coef_f.xlsx", row.names=F)
write.xlsx2(as.data.frame(vol_est_arv)    , "Tabelas/vol_est_arv.xlsx", row.names=F)
write.xlsx2(as.data.frame(vol_est_parcela), "Tabelas/vol_est_parcela.xlsx", row.names=F)
write.xlsx2(as.data.frame(vol_est_talhao) , "Tabelas/vol_est_talhao.xlsx", row.names=F)
write.xlsx2(as.data.frame(tab_graybill)   , "Tabelas/tab_graybill.xlsx", row.names=F)

