
# Pacotes ####

library(readxl)
suppressPackageStartupMessages(library(xlsx))
library(tidyverse)
library(ggthemes)
library(broom)
library(grid)
suppressPackageStartupMessages(library(extrafont))
suppressPackageStartupMessages(library(gridExtra))
loadfonts(device="win", quiet = T)


# Funcoes ####

Gt <- function(Y1, Yj, id, alpha = 0.05) {
  
  fit <- lm(Yj ~ Y1)
  
  
  QMRes <- sum(residuals(fit)^2)/fit$df.residual
  
  beta_theta <- coef(fit) - c(0,1)

  Y1linha_Y1 <- cbind(c(length(Y1), sum(Y1)), 
                      c(sum(Y1), sum(Y1^2)))
  
  FH0 <- round(
    (t(beta_theta)%*%Y1linha_Y1%*%beta_theta)
    /(2*QMRes),
    4)
  
  Ftab <- round(
    qf(p=alpha, df1=2, df2=fit$df.residual, lower.tail = FALSE),
    4)
  
  pvalor <- signif(
    pf(FH0,2,fit$df.residual,lower=F),
    4)
  
    if(FH0 > Ftab){Resultado <- "*"}else(Resultado <- "ns")
    
    Tab_Results_graybill <- data.frame("F_H0"    = FH0, 
                                       "F_crit"  = Ftab, 
                                       "P_valor" = pvalor, 
                                       "Alpha"   = alpha,
                                       "Teste"   = Resultado,
                                       "id"      = id) 

    return(Tab_Results_graybill)

} 

CubagemComp <- function(Dados, Limite, id = "Criterion_", Comp = 1){
  
  if(!is.null(Dados$Alternativa)){names(Dados)[names(Dados) == "Alternativa" ] <- "metodo"}
  
  if(Comp == 0)
  {
    df <- Dados %>%
      group_by(metodo, arvore) %>%
      mutate( # funcao para adicionar novas variaveis
        AS_CC = (dcc^2 * pi) / 40000, # Calculo da AS com casca
        VCC   = ( (AS_CC + lead(AS_CC) )/2 ) * (lead(secao) - secao) ) %>% # Calculo do volume com casca
      summarise(
        Alternativa = id,
        VCC = sum(VCC, na.rm = T)    ) %>%
      ungroup %>%
      spread(metodo, VCC) %>%
      rename(
        vol_criterion = Criterion, 
        vol_cubagem   = Cubagem     ) %>%
      mutate(er     = round(((vol_criterion - vol_cubagem)/vol_cubagem)*100, 2) )
    
  }  
  
  if(Comp == 1)         
  {
    
    df <- bind_rows( 
      filter(Dados, metodo == "Cubagem" & secao <= Limite),
      filter(Dados, metodo == "Criterion" & secao > Limite) ) %>%
      arrange(arvore, secao) %>%
      group_by(arvore) %>%
      mutate( # funcao para adicionar novas variaveis
        AS_CC = (dcc^2 * pi) / 40000, # Calculo da AS com casca
        VCC   = ( (AS_CC + lead(AS_CC) )/2 ) * (lead(secao) - secao) ) %>% # Calculo do volume com casca
      summarise(
        Alternativa   = id,
        vol_criterion = sum(VCC, na.rm = T)    ) %>%
      bind_cols( 
        Dados %>%
          filter(metodo == "Cubagem") %>% 
          group_by(arvore) %>%
          mutate( # funcao para adicionar novas variaveis
            AS_CC = (dcc^2 * pi) / 40000, # Calculo da AS com casca
            VCC   = ( (AS_CC + lead(AS_CC) )/2 ) * (lead(secao) - secao) ) %>% # Calculo do volume com casca
          summarise(vol_cubagem = sum(VCC, na.rm = T) ) %>%
          select(-arvore)  
      ) %>%
      mutate(er = round(((vol_criterion - vol_cubagem)/vol_cubagem)*100, 2) ) 
    
    
  }  
  return(df)
}

sch_vol <- function(df, modvol, name){
  df %>%
    cbind(modvol)  %>% 
    mutate_(.dots    = 
              setNames(list(~exp(b0 + b1*log(DAP) + b2*log(HT_EST) ) ), nm = name  ) ) %>%
    select(-matches("b"),-Rsqr, -Std.Error, -Alternativa )
}

hdjoin <- function(df, grupos, HT, DAP, OBS, dom, names="HD"){
  suppressPackageStartupMessages(require(dplyr))
  require(lazyeval)
  
  x <- df %>%
    group_by_(.dots = grupos) %>%
    filter_( 
      .dots =
        interp(~ !is.na(HT), HT = as.name(HT), .values =  list( HT = as.name(HT) ) ) ,
      interp(~ !is.na(DAP), DAP = as.name(DAP), .values = list( DAP = as.name(DAP) )  ),
      interp(~ OBS == dom, OBS = as.name(OBS), .values = list(OBS = as.name(OBS) ) )
    ) %>%
    summarise_(
      .dots = 
        setNames(
          list(
            interp( ~mean(HT), HT = as.name(HT) )
          ),
          nm=names
        )
    ) %>%
    ungroup
  
  
  df %>%
    filter_( 
      .dots =
        interp(~ !is.na(DAP), DAP = as.name(DAP), .values = list( DAP = as.name(DAP) )  )
    ) %>%
    right_join(x, by = grupos)
  
}

hd <- function(df, grupos, HT, DAP, OBS, dom, names="HD"){
  suppressPackageStartupMessages(require(dplyr))
  require(lazyeval)
  
  df %>%
    group_by_(.dots = grupos) %>%
    filter_( 
      .dots =
        interp(~ !is.na(HT), HT = as.name(HT), .values =  list( HT = as.name(HT) ) ) ,
      interp(~ !is.na(DAP), DAP = as.name(DAP), .values = list( DAP = as.name(DAP) )  ),
      interp(~ OBS == dom, OBS = as.name(OBS), .values = list(OBS = as.name(OBS) ) )
    ) %>%
    summarise_(
      .dots = 
        setNames(
          list(
            interp( ~mean(HT), HT = as.name(HT) )
          ),
          nm=names
        )
    ) %>%
    ungroup
}

sample.factor.levels <- function(df,factor, n, rep = F){
  df %>%
    group_by_(factor) %>%
    summarise(a = n()) %>%
    sample_n(n, replace = rep) %>%
    select_(factor) %>%
    .[[1]]
  
  }
