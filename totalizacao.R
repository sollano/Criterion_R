
# Carregar Bibliotecas e Funcoes ####

source("Funcoes.R")

# Carregar Dados ####

raw_data <- read.csv("dados_brutos.csv", sep = ",", dec = ".", header = T)
dados_ht <- read.csv("extra_info.csv", sep = ",", dec = ".", header = T)

# Calcular o vol total pelos métodos: Cubagem, criterion, e composto para limites de 1 a 6 ####

all_data <- raw_data %>%
  do(CubagemComp(., 1.3, id = "Criterion",0)) %>%
  do(rbind(.,CubagemComp(raw_data, 1.3, id = "Cubagem 1,3"))) %>%
  do(rbind(.,CubagemComp(raw_data, 2.3, id = "Cubagem 2,3"))) %>%
  do(rbind(.,CubagemComp(raw_data, 3.3, id = "Cubagem 3,3"))) %>%
  do(rbind(.,CubagemComp(raw_data, 4.3, id = "Cubagem 4,3"))) %>%
  do(rbind(.,CubagemComp(raw_data, 5.3, id = "Cubagem 5,3"))) %>%
  do(rbind(.,CubagemComp(raw_data, 6.3, id = "Cubagem 6,3"))) %>%
  do(merge(., dados_ht, by = "arvore")) %>%
  .[c("arvore","metodo","ht_cubagem","ht_criterion","dap_cubagem", "dap_criterion","vol_cubagem","vol_criterion","er")] %>%
  arrange(metodo) 

#Visualizar tabelas ####

all_data



# exportar tabelas ####

write.csv(all_data, "D:/Documents/Trabalhos_Mensuracao/Projeto_Criterion/Criterion_R/tab_totaliz.csv")
write.xlsx(all_data, "D:/Documents/Trabalhos_Mensuracao/Projeto_Criterion/Criterion_R/tab_totaliz.xlsx")








