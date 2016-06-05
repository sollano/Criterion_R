
# Pacotes ####

library(xlsx)
library(dplyr)
library(tidyr)
library(grid)
library(ggplot2)
library(ggthemes)
library(extrafont)
library(gridExtra)
loadfonts(device="win", quiet = T)


# Funcoes ####

Smallian <- function(Dados, Arvore, di, L, id = "V_Smallian") {
 
   AS <- (di ^ 2 * pi) / 40000
  
  dataC <- data.frame()
  
  for(i in 1:nrow(Dados))
  {
    if(is.na(Arvore[i+1]))
    {
      aux <- NA
      dataC <- rbind(dataC, aux)
    }
    else
    {
      if(Arvore[i] != Arvore [i+1])
      {
        aux <- NA
        dataC <- rbind(dataC, aux)
      }
      else
      {
        if(Arvore[i] == Arvore[i+1])
        {
          aux <- ((AS[i] + AS[i+1]) / 2) * (L[i+1] - L[i])
          dataC <- rbind(dataC, aux)
        }
      }
    }
  }
  names(dataC) <- c(id)
  dataCC <- cbind(Dados, dataC)
  dataCC[is.na(dataCC)] <- 0
  return(dataCC)
  
}

GraybillTest <- function(Y1, Yj, Tabela_Num = 0, alpha = 0.05) {
  n <- length(Y1)
  
  Sum_Y1 <- sum(Y1)
  Sum_Y1_Quad <- sum(Y1^2)
  Mean_Y1 <- mean(Y1)
  Var_Y1 <- var(Y1)
  Sd_Y1 <- sqrt(Var_Y1)
  
  Sum_Yj <- sum(Yj)
  Sum_Yj_Quad <- sum(Yj^2)
  Mean_Yj <- mean(Yj)
  Var_Yj <- var(Yj)
  Sd_Yj <- sqrt(Var_Yj)
  
  n <- length(Y1)
  Sum_Y1Yj <- sum(Y1 * Yj)
  c <- Sum_Yj^2/n
  
  b1 <- cov(Y1, Yj)/var(Y1)
  b0 <- Mean_Yj-(b1*Mean_Y1)
  
  yJlinha <- t(Yj)
  yJlinha_yJ <- yJlinha %*% Yj
  
  beta_linha <- matrix(c(b0,b1), nrow=1, byrow=TRUE)
  betalinha_y1linha_yJ <- matrix(c(b0,b1), nrow=1, byrow=TRUE) %*% (matrix(c(rep(1, times=n), Y1), nrow=2, byrow=TRUE) %*% Yj)
  
  SST <- yJlinha_yJ - c
  SSR <- betalinha_y1linha_yJ - c
  SSE <- SST - SSR
  MSE <- SSE/(n-2)
  SY1Yj <- sqrt(MSE)
  r_Quad <- SSR/SST
  
  GL1 <- 1
  GL2 <- n -1 -1
  
  beta <- matrix(c(b0,b1), nrow=2, byrow=TRUE)
  theta <- matrix(c(0,1), nrow=2, byrow=TRUE)
  beta_theta <- beta - theta
  beta_theta_linha <- t(beta_theta)
  
  Y1linha_Y1 <- (matrix(c(rep(1, times=n), Y1), nrow=2, byrow=TRUE)) %*% (matrix(c(rep(1, times=n), Y1), nrow=n, byrow=FALSE))
  beta_theta_linha__Y1linha_Y1<- beta_theta_linha %*% Y1linha_Y1
  beta_theta_linha__Y1linha_Y1__beta_theta_linha <- beta_theta_linha__Y1linha_Y1 %*% beta_theta
  FH0 <- round(beta_theta_linha__Y1linha_Y1__beta_theta_linha/(2*MSE),4)
  pvalor <- round(pf(FH0,GL1,GL2,lower=F),6)
  
  Ftab <- round (qf(p=alpha, df1=2, df2=GL2, lower.tail = FALSE),4)
  if(FH0 > Ftab){Resultado <- "*"}else(Resultado <- "ns")
  if(FH0 > Ftab){Conclusao <- "Yj e estatisticamente diferente de Y1, para o alpha estabelecido"}else{Conclusao <- "Yj e estatisticamente igual a Y1, para o alpha estabelecido"}
  
  Tab_Results_Num_graybill <- matrix(c(Mean_Y1, Mean_Yj, Var_Y1, Var_Yj, Sd_Y1, Sd_Yj, n, 2, GL2, Ftab, FH0, alpha, pvalor), byrow=TRUE)
  rownames(Tab_Results_Num_graybill) <- c("Média Y1", "Média Yj", "Variância Y1", "Variância Yj", "Desvio Padrao Y1", "Desvio Padrao Yj", "Observacoes", "g.l 1", "g.l 2", "Ftab", "F(H0)", "alpha",  "p valor")
  colnames(Tab_Results_Num_graybill) <- c("Resultado")
  
  colY1 <- c(round(Mean_Y1,4), round(Var_Y1,4), round(Sd_Y1,4), n, 2, Ftab, FH0, alpha, pvalor, Resultado, Conclusao)
  colYj <- c(round(Mean_Yj,4), round(Var_Yj,4), round(Sd_Yj,4), n, GL2, " ", " ", " ", " ", " ", " ")
  Tab_Results_graybill <- cbind(colY1, colYj)
  rownames(Tab_Results_graybill) <- c("Média", "Variância", "Desvio Padrão", "Observações", "g.l.", "F tabelado", "F(H0)", "Alpha", "P-valor", "Teste", "Conclusão")
  colnames(Tab_Results_graybill) <- c("V. Padrão (Y1)", "V. Proposto (Yj)")
  
  if(Tabela_Num == 0){return(Tab_Results_graybill)}else(return(Tab_Results_Num_graybill))
}

GraybillTest_df <- function(Y1, Yj, Tabela_Num = 0, alpha = 0.05) {
  n <- length(Y1)
  
  Sum_Y1 <- sum(Y1)
  Sum_Y1_Quad <- sum(Y1^2)
  Mean_Y1 <- mean(Y1)
  Var_Y1 <- var(Y1)
  Sd_Y1 <- sqrt(Var_Y1)
  
  Sum_Yj <- sum(Yj)
  Sum_Yj_Quad <- sum(Yj^2)
  Mean_Yj <- mean(Yj)
  Var_Yj <- var(Yj)
  Sd_Yj <- sqrt(Var_Yj)
  
  n <- length(Y1)
  Sum_Y1Yj <- sum(Y1 * Yj)
  c <- Sum_Yj^2/n
  
  b1 <- cov(Y1, Yj)/var(Y1)
  b0 <- Mean_Yj-(b1*Mean_Y1)
  
  yJlinha <- t(Yj)
  yJlinha_yJ <- yJlinha %*% Yj
  
  beta_linha <- matrix(c(b0,b1), nrow=1, byrow=TRUE)
  betalinha_y1linha_yJ <- matrix(c(b0,b1), nrow=1, byrow=TRUE) %*% (matrix(c(rep(1, times=n), Y1), nrow=2, byrow=TRUE) %*% Yj)
  
  SST <- yJlinha_yJ - c
  SSR <- betalinha_y1linha_yJ - c
  SSE <- SST - SSR
  MSE <- SSE/(n-2)
  SY1Yj <- sqrt(MSE)
  r_Quad <- SSR/SST
  
  GL1 <- 1
  GL2 <- n -1 -1
  
  beta <- matrix(c(b0,b1), nrow=2, byrow=TRUE)
  theta <- matrix(c(0,1), nrow=2, byrow=TRUE)
  beta_theta <- beta - theta
  beta_theta_linha <- t(beta_theta)
  
  Y1linha_Y1 <- (matrix(c(rep(1, times=n), Y1), nrow=2, byrow=TRUE)) %*% (matrix(c(rep(1, times=n), Y1), nrow=n, byrow=FALSE))
  beta_theta_linha__Y1linha_Y1<- beta_theta_linha %*% Y1linha_Y1
  beta_theta_linha__Y1linha_Y1__beta_theta_linha <- beta_theta_linha__Y1linha_Y1 %*% beta_theta
  FH0 <- round(beta_theta_linha__Y1linha_Y1__beta_theta_linha/(2*MSE),4)
  pvalor <- round(pf(FH0,GL1,GL2,lower=F),6)
  
  Ftab <- round (qf(p=alpha, df1=2, df2=GL2, lower.tail = FALSE),4)
  if(FH0 > Ftab){Resultado <- "*"}else(Resultado <- "ns")
  if(FH0 > Ftab){Conclusao <- "Yj e estatisticamente diferente de Y1, para o alpha estabelecido"}else{Conclusao <- "Yj e estatisticamente igual a Y1, para o alpha estabelecido"}
  
  Tab_Results_Num_graybill <- matrix(c(Mean_Y1, Mean_Yj, Var_Y1, Var_Yj, Sd_Y1, Sd_Yj, n, 2, GL2, Ftab, FH0, alpha, pvalor), byrow=TRUE)
  rownames(Tab_Results_Num_graybill) <- c("Média Y1", "Média Yj", "Variância Y1", "Variância Yj", "Desvio Padrao Y1", "Desvio Padrao Yj", "Observacoes", "g.l 1", "g.l 2", "Ftab", "F(H0)", "alpha",  "p valor")
  colnames(Tab_Results_Num_graybill) <- c("Resultado")
  
  colY1 <- c(round(Mean_Y1,4), round(Var_Y1,4), round(Sd_Y1,4), n, 2, Ftab, FH0, alpha, pvalor, Resultado, Conclusao)
  colYj <- c(round(Mean_Yj,4), round(Var_Yj,4), round(Sd_Yj,4), n, GL2, " ", " ", " ", " ", " ", " ")
  Tab_Results_graybill <- cbind(colY1, colYj)
  rownames(Tab_Results_graybill) <- c("Média", "Variância", "Desvio Padrão", "Observações", "g.l.", "F tabelado", "F(H0)", "Alpha", "P-valor", "Teste", "Conclusão")
  colnames(Tab_Results_graybill) <- c("V. Padrão (Y1)", "V. Proposto (Yj)")
  
  if(Tabela_Num == 0){return(as.data.frame(Tab_Results_graybill))}else(return(as.data.frame(Tab_Results_Num_graybill)))
}

tabReg <- function(x, Coeficientes = 2) {
  aux <- c(coef(x), summary(x)$r.squared, summary(x)$sigma)
  if(Coeficientes == 1)
  {
    names(aux) <- c("b0", "b1", "Rsqr", "Std.Error")
  }
  if(Coeficientes == 2)
  {
    names(aux) <- c("b0", "b1", "b2", "Rsqr", "Std.Error")
  }
  if(Coeficientes == 3)
  {
    names(aux) <- c("b0", "b1", "b2", "b3", "Rsqr", "Std.Error")
  }
  return(aux)
}

Ajuste_Schummacher <- function(data) {
  
  Schummacher_Model_Cub <- lm(formula = log(data$vol_cubagem) ~ log(data$dap_cubagem) + log(data$ht_cubagem))
  tab1 <- tabReg(Schummacher_Model_Cub)
  
  Schummacher_Model_Crit <- lm(formula = log(data$vol_criterion) ~ log(data$dap_criterion) + log(data$ht_criterion))
  tab2 <- tabReg(Schummacher_Model_Crit)
  
  Schummacher_Model_Crit_Cub <- lm(formula = log(data$vol_criterion) ~ log(data$dap_cubagem) + log(data$ht_cubagem))
  tab3 <- tabReg(Schummacher_Model_Crit_Cub)
  
  aux <- rbind(tab1,tab2,tab3)
  rownames(aux) <- c("Cubagem", "Criterium", "Cub_Crit")
  
  return(aux)
  
  }
  
   
Gt <- function(Y1, Yj, Tabela_Num = 0, alpha = 0.05) {
    n <- length(Y1)
    
    Sum_Y1 <- sum(Y1)
    Sum_Y1_Quad <- sum(Y1^2)
    Mean_Y1 <- mean(Y1)
    Var_Y1 <- var(Y1)
    Sd_Y1 <- sqrt(Var_Y1)
    
    Sum_Yj <- sum(Yj)
    Sum_Yj_Quad <- sum(Yj^2)
    Mean_Yj <- mean(Yj)
    Var_Yj <- var(Yj)
    Sd_Yj <- sqrt(Var_Yj)
    
    n <- length(Y1)
    Sum_Y1Yj <- sum(Y1 * Yj)
    c <- Sum_Yj^2/n
    
    b1 <- cov(Y1, Yj)/var(Y1)
    b0 <- Mean_Yj-(b1*Mean_Y1)
    
    yJlinha <- t(Yj)
    yJlinha_yJ <- yJlinha %*% Yj
    
    beta_linha <- matrix(c(b0,b1), nrow=1, byrow=TRUE)
    betalinha_y1linha_yJ <- matrix(c(b0,b1), nrow=1, byrow=TRUE) %*% (matrix(c(rep(1, times=n), Y1), nrow=2, byrow=TRUE) %*% Yj)
    
    SST <- yJlinha_yJ - c
    SSR <- betalinha_y1linha_yJ - c
    SSE <- SST - SSR
    MSE <- SSE/(n-2)
    SY1Yj <- sqrt(MSE)
    r_Quad <- SSR/SST
    
    GL1 <- 1
    GL2 <- n -1 -1
    
    beta <- matrix(c(b0,b1), nrow=2, byrow=TRUE)
    theta <- matrix(c(0,1), nrow=2, byrow=TRUE)
    beta_theta <- beta - theta
    beta_theta_linha <- t(beta_theta)
    
    Y1linha_Y1 <- (matrix(c(rep(1, times=n), Y1), nrow=2, byrow=TRUE)) %*% (matrix(c(rep(1, times=n), Y1), nrow=n, byrow=FALSE))
    beta_theta_linha__Y1linha_Y1<- beta_theta_linha %*% Y1linha_Y1
    beta_theta_linha__Y1linha_Y1__beta_theta_linha <- beta_theta_linha__Y1linha_Y1 %*% beta_theta
    FH0 <- round(beta_theta_linha__Y1linha_Y1__beta_theta_linha/(2*MSE),4)
    pvalor <- round(pf(FH0,GL1,GL2,lower=F),6)
    
    Ftab <- round (qf(p=alpha, df1=2, df2=GL2, lower.tail = FALSE),4)
    if(FH0 > Ftab){Resultado <- "*"}else(Resultado <- "ns")
    
    Tab_Results_Num_graybill <- t(c(Ftab, FH0, alpha, pvalor))
    colnames(Tab_Results_Num_graybill) <- c("Ftab", "F(H0)", "alpha",  "p valor")
    
    Tab_Results_graybill <- t(c(Ftab, FH0, alpha, pvalor, Resultado))
    colnames(Tab_Results_graybill) <- c("F tabelado", "F(H0)", "Alpha", "P-valor", "Resultado")
    
    if(Tabela_Num == 0){return(as.data.frame(Tab_Results_graybill))}else(return(as.data.frame(Tab_Results_Num_graybill)))
  }
 
CubagemComp <- function(Dados, Limite, id = "Criterion_", Comp = 1) {
  
  require(dplyr)
  
  cubagem <- function(aux2, id = "vol_criterion") {
    AS <- (aux2$dcc ^ 2 * pi) / 40000
    
    dataC <- data.frame()
    
    for(i in 1:nrow(aux2))
    {
      if(is.na(aux2$arvore[i+1]))
      {
        aux3 <- NA
        dataC <- rbind(dataC, aux3)
      }
      else
      {
        if(aux2$arvore[i] != aux2$arvore [i+1])
        {
          aux3 <- NA
          dataC <- rbind(dataC, aux3)
        }
        else
        {
          if(aux2$arvore[i] == aux2$arvore[i+1])
          {
            aux3 <- ((AS[i] + AS[i+1]) / 2) * (aux2$secao[i+1] - aux2$secao[i])
            dataC <- rbind(dataC, aux3)
          }
        }
      }
    }
    names(dataC) <- c(id)
    dataCC <- cbind(aux2, dataC)
    dataCC[is.na(dataCC)] <- 0
    return(dataCC)
  }
  
  cub <- Dados %>%
    filter(Alternativa == "Cubagem") %>%
    do(cubagem(., id = "vol_cubagem")) %>%
    select(arvore, vol_cubagem) %>% 
    group_by(arvore) %>% 
    summarize(vol_cubagem= sum(vol_cubagem))
  
  if(Comp == 1){  
    aux2  <- Dados %>%
      filter(Alternativa == "Cubagem" & secao <= Limite) %>%
      do(rbind(.,filter(Dados, Alternativa == "Criterion" & secao > Limite)))
  }
  else
  {
    aux2 <- filter(Dados, Alternativa == "Criterion")
  }
  
  crit <- aux2 %>%
    arrange(arvore, secao)%>%
    do(cubagem(.)) %>%
    select(arvore, vol_criterion) %>% 
    group_by(arvore) %>% 
    summarize(vol_criterion = sum(vol_criterion)) %>%
    mutate(Alternativa = rep(id,length(arvore)))%>%
    select(-arvore) %>%
    do(cbind(cub,.)) %>%
    mutate(er = round(((vol_criterion - vol_cubagem)/vol_cubagem)*100, 2), er_med = mean(er)) %>%
    .[ ,c("Alternativa", "arvore", "vol_cubagem", "vol_criterion", "er", "er_med")]
  
  return(crit)
  
}


tabReg_ <- function(x, Coeficientes = 2) {
  aux <- c(coef(x), summary(x)$r.squared, summary(x)$sigma)
  if(Coeficientes == 1)
  {
    names(aux) <- c("b0", "b1", "Rsqr", "Std.Error")
  }
  if(Coeficientes == 2)
  {
    names(aux) <- c("b0", "b1", "b2", "Rsqr", "Std.Error")
  }
  if(Coeficientes == 3)
  {
    names(aux) <- c("b0", "b1", "b2", "b3", "Rsqr", "Std.Error")
  }
  return(as.data.frame(t(aux)))
}

