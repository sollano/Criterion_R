
# Carregar Bibliotecas ####

source("Funcoes.R")

# Cubagem Boxplots por Árvore ####

box_arvore <- all_data %>%
  filter(Alternativa == "Alternativa_2") %>%
  select(volume = vol_cubagem) %>%
  mutate(Alternativa = rep("Alternativa_1", 1:length(.)) ) %>%
  rbind(select(all_data, Alternativa, volume = vol_criterion)) %>%
  mutate(Alternativa = factor(Alternativa, labels = 1:5)  ) %>% 
  ggplot(aes(x = Alternativa,
             y = volume, 
             colour = Alternativa, 
             fill = Alternativa, 
             alpha = 1)) + 
  geom_boxplot(show.legend = FALSE) + 
  stat_summary(fun.y=mean, geom="point", shape=18, size=3, show.legend = F) + 
  labs(x="Alternativa", 
       y="Volume (m³)",
       title = "Boxplots de Volume por Árvore" ) + 
  theme_igray(base_family = "Arial Unicode MS") + 
  theme(panel.margin = unit(2, "lines"), 
        plot.title   = element_text(size = 16, face="bold", vjust = 0.9), 
        axis.title   = element_text(size = 14), 
        axis.text    = element_text(size = 12),
        strip.text.x = element_text(size = 16))  

# Kozak ####

kzk  <- raw_data %>% 
  mutate(d_sob_dap = dcc/dap, h_sob_ht = secao/ht,
         metodo_ = factor(metodo, labels = c("b", "a") ),
         metodo = relevel(metodo_,  "a")) %>% 
  ggplot(aes(x=d_sob_dap, y=h_sob_ht)) + 
  geom_point(size = 2, alpha = .4) + 
  labs(x="d/dap", y="h/ht", title="Ajuste do Modelo de Kozak") + 
  scale_x_continuous(breaks=c(0,.5,  1, 1.5)) + 
  coord_fixed(ratio=3)  + 
  theme_grey(base_family = "Arial Unicode MS" ) + 
  facet_grid(. ~ metodo) + 
  theme_igray(base_family = "Arial Unicode MS") + 
  theme(panel.margin = unit(2, "lines"), 
        plot.title   = element_text(size = 16, face="bold", vjust = 0.9), 
        axis.title   = element_text(size = 14), 
        axis.text    = element_text(size = 12),
        strip.text.x = element_text(size = 16))

# Cubagem Res Porcentagem Dipersao ####

cub_res_disp <- all_data %>%
  mutate(Alternativa = factor(Alternativa, labels = c("a", "b", "c", "d"))  ) %>% 
  ggplot(aes(vol_cubagem, er))  + 
  geom_hline(yintercept = 0, colour = "gray45") + 
  geom_point(aes(colour=ht_cubagem), size = 3)  + 
  facet_wrap( ~ Alternativa, nrow = 1) + 
  labs(x      = "Volume Alternativa 1 (m³)", 
       y      = "Resíduo (%)"              , 
       colour = "HT (m)"         ,
       title  = "Gráfico de Dispersão  dos residuos em relação à Alternativa 1" ) +
  coord_cartesian(ylim = c(-40,40) ) + 
  scale_colour_gradient(low = "ligh tgrey", high = "black") + 
  theme_igray(base_family = "Arial Unicode MS") + 
  theme(panel.margin = unit(2, "lines"), 
        plot.title   = element_text(size = 16, face="bold", vjust = 0.9), 
        axis.title   = element_text(size = 14), 
        axis.text    = element_text(size = 12),
        strip.text.x = element_text(size = 16))  


# Cubagem Res Porcentagem Histograma ####

cub_res_hist <-  all_data %>%
  mutate(Alternativa = factor(Alternativa, labels = c("a", "b", "c", "d"))  ) %>% 
  ggplot(aes(er, ..density..))  + 
  geom_vline(xintercept = 0, colour = "gray45")  + 
  geom_histogram(binwidth = 4) + 
  facet_wrap(~Alternativa, nrow = 1) + 
  labs(y = "Densidade", 
       x= "Residuo (%)", 
       title = "Histograma para os Resíduos das Diferentes Alternativas") + 
  xlim(-30, 30) +
  theme_igray(base_family = "Arial Unicode MS") + 
  theme(panel.margin = unit(2, "lines"), 
        plot.title   = element_text(size = 16, face="bold", vjust = 0.9), 
        axis.title   = element_text(size = 14), 
        axis.text    = element_text(size = 12),
        strip.text.x = element_text(size = 16))  


# Cubagem Vol X VolEST ####

cub_vol_obs_est <-  all_data %>%
  mutate(Alternativa = factor(Alternativa, labels = c("a", "b", "c", "d"))  ) %>% 
  ggplot(aes(vol_cubagem, vol_criterion))  + 
  geom_point(aes(colour=er), size = 3)  + 
  geom_smooth(method="lm", colour="gray25") + 
  facet_wrap( ~ Alternativa, nrow = 1) + 
  labs(x      = "Volume Alternativa 1 (m³)", 
       y      = "Volume Alternativas  (m³)"     , 
       colour = "Res (%)"        ,
       title  = "Gráfico de Dispersão para o Volume Calculado pelas Alternativas 2 a 5 em relacao a 1") + 
  scale_colour_gradient(low = "ligh tgrey", high = "black") + 
  theme_igray(base_family = "Arial Unicode MS") + 
  theme(panel.margin = unit(2, "lines"), 
        plot.title   = element_text(size = 16, face="bold", vjust = 0.9), 
        axis.title   = element_text(size = 14), 
        axis.text    = element_text(size = 12),
        strip.text.x = element_text(size = 16))  


# Schumacher & Hall Modelo ####


schummacher_crit <-  all_data %>%
  mutate(Alternativa = factor(Alternativa, labels = c("a", "b", "c", "d"))  ) %>% 
  ggplot(aes(x = log(dap_cubagem) + log(ht_criterion),
             y = log(vol_criterion))) + 
  geom_point(size=4) +
  stat_smooth(method ="lm", color="gray45") + 
 facet_wrap( ~ Alternativa, nrow = 1) + 
  labs(x     = "Ln(DAP) + Ln(HT)", 
       y     = "Ln(VCC)", 
       title = "Ajuste do Modelo de Schummacher & Hall") + 
  theme_igray(base_family = "Arial Unicode MS") + 
  theme(panel.margin = unit(2, "lines"), 
        plot.title   = element_text(size = 16, face="bold", vjust = 0.9), 
        axis.title   = element_text(size = 14), 
        axis.text    = element_text(size = 12),
        strip.text.x = element_text(size = 16))  

# GRAFICOS NIVEL DE ARVORE
# Inventario Res Porcentagem Dipersao Arvore ####
#cub_res_vol_est_all 

inv_res_disp_arv  <-  vol_est_arv %>%
  mutate(Alternativa = factor(Alternativa, labels = c("a", "b", "c", "d"))  ) %>% 
  ggplot(aes(VOL_OBS, er))  + 
  geom_hline(yintercept = 0, colour = "gray45") + 
  geom_point(aes(colour=HT), size = 3)  + 
  facet_wrap( ~ Alternativa, nrow = 1)  + 
  labs(x      = "Volume Estimado Alternativa 1 (m³)", 
       y      = "Resíduo (%)", 
       colour = "HT (m)",
       title  = "Dispersão dos Residuos do Volume Estimado das Diferentes Alternativas\n em nivel de Arvore") + 
  coord_cartesian(ylim = c(-40,40)) + 
  scale_colour_gradient(low = "ligh tgrey", high = "black") + 
  theme_igray(base_family = "Arial Unicode MS") + 
  theme(panel.margin = unit(2, "lines"), 
        plot.title   = element_text(size = 16, face="bold", vjust = 0.9), 
        axis.title   = element_text(size = 14), 
        axis.text    = element_text(size = 12),
        strip.text.x = element_text(size = 16))  

# Inventario Res porcentagem Histograma Arvore ####

inv_hist_vol_arv <-  vol_est_arv %>%
  mutate(Alternativa = factor(Alternativa, labels = c("a", "b", "c", "d"))  ) %>% 
  ggplot(aes(er, ..density..)) +
  geom_vline(xintercept = 0, colour = "gray45")  + 
  geom_histogram(binwidth = 1) + 
  facet_wrap(~Alternativa, nrow = 1) + 
  xlim(-30, 30) + 
  labs(y     = "Densidade", 
       x     = "Residuo (%)", 
       title = "Histograma para os Resíduos do Volume Estimado pelo Modelo de Schumacher") + 
  theme_igray(base_family = "Arial Unicode MS") + 
  theme(panel.margin = unit(2, "lines"), 
        plot.title   = element_text(size = 16, face="bold", vjust = 0.9), 
        axis.title   = element_text(size = 14), 
        axis.text    = element_text(size = 12),
        strip.text.x = element_text(size = 16))  




# Inventario Vol X VolEST Arvore ####

inv_vol_obs_est_arv <-  vol_est_arv %>%
  mutate(Alternativa = factor(Alternativa, labels = c("a", "b", "c", "d"))  ) %>% 
  ggplot(aes(VOL_OBS, VOL_EST))  + 
  geom_point(aes(colour=er), size = 3)  + 
  geom_smooth(method="lm", colour="gray25") + 
  facet_wrap( ~ Alternativa, nrow = 1) + 
  labs(x      = "Volume Estimado Alternativa 1 (m³)", 
       y      = "Volume Estimado (m³)", 
       colour = "Res (%)", 
       title  = "Gráfico de Dispersão para o Volume Estimado pelas Alternativas 2 a 5 em relacao a 1 \n em nivel de Arvore") + 
  scale_colour_gradient(low = "ligh tgrey", high = "black") + 
  theme_igray(base_family = "Arial Unicode MS") + 
  theme(panel.margin = unit(2, "lines"), 
        plot.title   = element_text(size = 16, face="bold", vjust = 0.9), 
        axis.title   = element_text(size = 14), 
        axis.text    = element_text(size = 12),
        strip.text.x = element_text(size = 16))  


# GRAFICOS NIVEL DE PARCELA
# Inventario Res Porcentagem Dipersao parcela ####

inv_res_disp_parc  <-  vol_est_parcela %>%
  mutate(Alternativa = factor(Alternativa, labels = c("a", "b", "c", "d"))  ) %>% 
  ggplot(aes(VOL_OBS, er))  + 
  geom_hline(yintercept = 0, colour = "gray45") + 
  geom_point(aes(colour=HT), size = 3)  + 
  facet_wrap( ~ Alternativa, nrow = 1)  + 
  labs(x      = "Volume Estimado Alternativa 1 (m³)", 
       y      = "Resíduo (%)", 
       colour = "HT (m)",
       title  = "Dispersão dos Residuos do Volume Estimado das Diferentes Alternativas\n em nivel de Parcela") + 
  coord_cartesian(ylim = c(-40,40)) + 
  scale_colour_gradient(low = "ligh tgrey", high = "black") + 
  theme_igray(base_family = "Arial Unicode MS") + 
  theme(panel.margin = unit(2, "lines"), 
        plot.title   = element_text(size = 16, face="bold", vjust = 0.9), 
        axis.title   = element_text(size = 14), 
        axis.text    = element_text(size = 12),
        strip.text.x = element_text(size = 16))  

# Inventario Res porcentagem Histograma parcela ####

inv_hist_vol_parc <-  vol_est_parcela %>%
  mutate(Alternativa = factor(Alternativa, labels = c("a", "b", "c", "d"))  ) %>% 
  ggplot(aes(er, ..density..)) +
  geom_vline(xintercept = 0, colour = "gray45")  + 
  geom_histogram(binwidth = 1) + 
  facet_wrap(~Alternativa, nrow = 1) + 
  xlim(-30, 30) + 
  labs(y     = "Densidade", 
       x     = "Residuo (%)", 
       title = "Histograma para os Resíduos do Volume Estimado pelo Modelo de Schumacher") + 
  theme_igray(base_family = "Arial Unicode MS") + 
  theme(panel.margin = unit(2, "lines"), 
        plot.title   = element_text(size = 16, face="bold", vjust = 0.9), 
        axis.title   = element_text(size = 14), 
        axis.text    = element_text(size = 12),
        strip.text.x = element_text(size = 16))  




# inventario Vol X VolEST parcela ####

inv_vol_obs_est_parc <-  vol_est_parcela %>%
  mutate(Alternativa = factor(Alternativa, labels = c("a", "b", "c", "d"))  ) %>% 
  ggplot(aes(VOL_OBS, VOL_EST))  + 
  geom_point(aes(colour=er), size = 3)  + 
  geom_smooth(method="lm", colour="gray25") + 
  facet_wrap( ~ Alternativa, nrow = 1) + 
  labs(x      = "Volume Estimado Alternativa 1 (m³)", 
       y      = "Volume Estimado (m³)", 
       colour = "Res (%)", 
       title  = "Gráfico de Dispersão para o Volume Estimado pelas Alternativas 2 a 5 em relacao a 1 \n em nivel de Arvore") + 
  scale_colour_gradient(low = "ligh tgrey", high = "black") + 
  theme_igray(base_family = "Arial Unicode MS") + 
  theme(panel.margin = unit(2, "lines"), 
        plot.title   = element_text(size = 16, face="bold", vjust = 0.9), 
        axis.title   = element_text(size = 14), 
        axis.text    = element_text(size = 12),
        strip.text.x = element_text(size = 16))  


# GRAFICOS NIVEL DE TALHAO ####
# Inventario Res Porcentagem Dipersao TALHAO ####

inv_res_disp_talh  <-  vol_est_talhao %>%
  mutate(Alternativa = factor(Alternativa, labels = c("a", "b", "c", "d"))  ) %>% 
  ggplot(aes(VOL_OBS, er))  + 
  geom_hline(yintercept = 0, colour = "gray45") + 
  geom_point(aes(colour=HT), size = 3)  + 
  facet_wrap( ~ Alternativa, nrow = 1)  + 
  labs(x      = "Volume Estimado Alternativa 1 (m³)", 
       y      = "Resíduo (%)", 
       colour = "HT (m)",
       title  = "Dispersão dos Residuos do Volume Estimado das Diferentes Alternativas\n em nivel de Talhao") + 
  coord_cartesian(ylim = c(-40,40)) + 
  scale_colour_gradient(low = "ligh tgrey", high = "black") + 
  theme_igray(base_family = "Arial Unicode MS") + 
  theme(panel.margin = unit(2, "lines"), 
        plot.title   = element_text(size = 16, face="bold", vjust = 0.9), 
        axis.title   = element_text(size = 14), 
        axis.text    = element_text(size = 12),
        strip.text.x = element_text(size = 16))  

# Inventario Res porcentagem Histograma TALHAO ####

inv_hist_vol_talh <-  vol_est_talhao %>%
  mutate(Alternativa = factor(Alternativa, labels = c("a", "b", "c", "d"))  ) %>% 
  ggplot(aes(er, ..density..)) +
  geom_vline(xintercept = 0, colour = "gray45")  + 
  geom_histogram(binwidth = 1) + 
  facet_wrap(~Alternativa, nrow = 1) + 
  xlim(-30, 30) + 
  labs(y     = "Densidade", 
       x     = "Residuo (%)", 
       title = "Histograma para os Resíduos do Volume Estimado pelo Modelo de Schumacher") + 
  theme_igray(base_family = "Arial Unicode MS") + 
  theme(panel.margin = unit(2, "lines"), 
        plot.title   = element_text(size = 16, face="bold", vjust = 0.9), 
        axis.title   = element_text(size = 14), 
        axis.text    = element_text(size = 12),
        strip.text.x = element_text(size = 16))  




# Inventario Vol X VolEST TALHAO ####

inv_vol_obs_est_talh <-  vol_est_talhao %>%
  mutate(Alternativa = factor(Alternativa, labels = c("a", "b", "c", "d"))  ) %>% 
  ggplot(aes(VOL_OBS, VOL_EST))  + 
  geom_point(aes(colour=er), size = 3)  + 
  geom_smooth(method="lm", colour="gray25") + 
  facet_wrap( ~ Alternativa, nrow = 1) + 
  labs(x      = "Volume Estimado Alternativa 1 (m³)", 
       y      = "Volume Estimado (m³)", 
       colour = "Res (%)", 
       title  = "Gráfico de Dispersão para o Volume Estimado pelas Alternativas 2 a 5 em relacao a 1 \n em nivel de Arvore") + 
  scale_colour_gradient(low = "ligh tgrey", high = "black") + 
  theme_igray(base_family = "Arial Unicode MS") + 
  theme(panel.margin = unit(2, "lines"), 
        plot.title   = element_text(size = 16, face="bold", vjust = 0.9), 
        axis.title   = element_text(size = 14), 
        axis.text    = element_text(size = 12),
        strip.text.x = element_text(size = 16))  





# Graficos inventario grid.arrange ####

# Grid arrange Res Porcentagem Dispersao ####

inv_res_disp_grid <- grid.arrange(
  vol_est_arv %>%
    mutate(Alternativa = factor(Alternativa, labels = c("a", "b", "c", "d"))  ) %>% 
    ggplot(aes(VOL_OBS, er))  + 
    geom_hline(yintercept = 0, colour = "gray45") + 
    geom_point(aes(color=HT), size = 3, show.legend = F)  + 
    facet_wrap( ~ Alternativa, nrow = 1)  + 
    labs(x = NULL,
         y = NULL,
         title = "A") +
    coord_cartesian(ylim = c(-40,40)) + 
    scale_colour_gradient(low = "ligh tgrey", high = "black") + 
    theme_igray(base_family = "Arial Unicode MS") + 
    theme(panel.margin = unit(2, "lines"), 
          plot.title   = element_text(size = 16, face="bold", vjust = 0.9), 
          axis.title   = element_text(size = 14), 
          axis.text    = element_text(size = 12),
          strip.text.x = element_text(size = 16)) +
    guides(color = guide_colorbar(title= NULL, label = F, barheight = 0, barwidth = 2.3))
  ,
  vol_est_parcela %>%
    mutate(Alternativa = factor(Alternativa, labels = c("a", "b", "c", "d"))  ) %>% 
    ggplot(aes(VOL_OBS, er))  + 
    geom_hline(yintercept = 0, colour = "gray45") + 
    geom_point(aes(color=HT), size = 3, show.legend = F)  + 
    facet_wrap( ~ Alternativa, nrow = 1)  + 
    labs(x = NULL,
         y = NULL,
         color = "HT (m)",
         title = "B") +
    coord_cartesian(ylim = c(-40,40)) + 
    scale_colour_gradient(low = "ligh tgrey", high = "black") + 
    theme_igray(base_family = "Arial Unicode MS") + 
    theme(panel.margin = unit(2, "lines"), 
          plot.title   = element_text(size = 16, face="bold", vjust = 0.9), 
          axis.title   = element_text(size = 14), 
          axis.text    = element_text(size = 12),
          strip.text.x = element_text(size = 16)) ,  
  
  vol_est_talhao %>%
    mutate(Alternativa = factor(Alternativa, labels = c("a", "b", "c", "d"))  ) %>% 
    ggplot(aes(VOL_OBS, er))  + 
    geom_hline(yintercept = 0, colour = "gray45") + 
    geom_point(aes(color=HT), size = 3, show.legend = F)  + 
    facet_wrap( ~ Alternativa, nrow = 1)  + 
    labs(x = NULL,
         y = NULL,
         title = "C") +
    coord_cartesian(ylim = c(-40,40)) + 
    scale_colour_gradient(low = "ligh tgrey", high = "black") + 
    theme_igray(base_family = "Arial Unicode MS") + 
    theme(panel.margin = unit(2, "lines"), 
          plot.title   = element_text(size = 16, face="bold", vjust = 0.9), 
          axis.title   = element_text(size = 14), 
          axis.text    = element_text(size = 12),
          strip.text.x = element_text(size = 16)) +
    guides(color = guide_colorbar(title= NULL, label = F, barheight = 0, barwidth = 2.3))   ,
  left     = grid.text("Resíduo (%)", gp=gpar(fontsize=16 ), rot = 90) ,
  bottom   = grid.text("Volume Estimado Alternativa 1 (m³)", gp=gpar(fontsize=16 ) )  )


# Grid arrange Res porcentagem histograma ####

inv_hist_vol_grid <- grid.arrange( 
  vol_est_arv %>%
    mutate(Alternativa = factor(Alternativa, labels = c("a", "b", "c", "d"))  ) %>% 
    ggplot(aes(er, ..density..)) +
    geom_vline(xintercept = 0, colour = "gray45")  + 
    geom_histogram(binwidth = 1) + 
    facet_wrap(~Alternativa, nrow = 1) + 
    xlim(-30, 30) + 
    labs(x = NULL,
         y = NULL,
         title = "A") + 
    theme_igray(base_family = "Arial Unicode MS") + 
    theme(panel.margin = unit(2, "lines"), 
          plot.title   = element_text(size = 16, face="bold", vjust = 0.9), 
          axis.title   = element_text(size = 14), 
          axis.text    = element_text(size = 12),
          strip.text.x = element_text(size = 16)) ,
  vol_est_parcela %>%
    mutate(Alternativa = factor(Alternativa, labels = c("a", "b", "c", "d"))  ) %>% 
    ggplot(aes(er, ..density..)) +
    geom_vline(xintercept = 0, colour = "gray45")  + 
    geom_histogram(binwidth = 1) + 
    facet_wrap(~Alternativa, nrow = 1) + 
    xlim(-30, 30) + 
    labs(x = NULL,
         y = NULL,
         title = "B") + 
    theme_igray(base_family = "Arial Unicode MS") + 
    theme(panel.margin = unit(2, "lines"), 
          plot.title   = element_text(size = 16, face="bold", vjust = 0.9), 
          axis.title   = element_text(size = 14), 
          axis.text    = element_text(size = 12),
          strip.text.x = element_text(size = 16))  ,
  
  vol_est_talhao %>%
    mutate(Alternativa = factor(Alternativa, labels = c("a", "b", "c", "d"))  ) %>% 
    ggplot(aes(er, ..density..)) +
    geom_vline(xintercept = 0, colour = "gray45")  + 
    geom_histogram(binwidth = 1) + 
    facet_wrap(~Alternativa, nrow = 1) + 
    xlim(-30, 30) + 
    labs(x = NULL,
         y = NULL,
         title = "C") +
    theme_igray(base_family = "Arial Unicode MS") + 
    theme(panel.margin = unit(2, "lines"), 
          plot.title   = element_text(size = 16, face="bold", vjust = 0.9), 
          axis.title   = element_text(size = 14), 
          axis.text    = element_text(size = 12),
          strip.text.x = element_text(size = 16)) ,
  left     = grid.text("Densidade", gp=gpar(fontsize=16 ), rot = 90) ,
  bottom   = grid.text("Resíduo (%)" , gp=gpar(fontsize=16 ) )  )
 

# grid arrange vol_obs x vol_est ####


inv_vol_obs_est_grid <- grid.arrange(
  vol_est_arv %>%
    mutate(Alternativa = factor(Alternativa, labels = c("a", "b", "c", "d"))  ) %>% 
    ggplot(aes(VOL_OBS, VOL_EST))  + 
    geom_point(aes(colour=er), size = 3)  + 
    geom_smooth(method="lm", colour="gray25") + 
    facet_wrap( ~ Alternativa, nrow = 1) + 
    labs(x = NULL,
         y = NULL,
         colour = "Res (%)", 
         title = "A") + 
    scale_colour_gradient(low = "ligh tgrey", high = "black") + 
    theme_igray(base_family = "Arial Unicode MS") + 
    theme(panel.margin = unit(2, "lines"), 
          plot.title   = element_text(size = 16, face="bold", vjust = 0.9), 
          axis.title   = element_text(size = 14), 
          axis.text    = element_text(size = 12),
          strip.text.x = element_text(size = 16)) , 
  
  vol_est_parcela %>%
    mutate(Alternativa = factor(Alternativa, labels = c("a", "b", "c", "d"))  ) %>% 
    ggplot(aes(VOL_OBS, VOL_EST))  + 
    geom_point(aes(colour=er), size = 3)  + 
    geom_smooth(method="lm", colour="gray25") + 
    facet_wrap( ~ Alternativa, nrow = 1) + 
    labs(x = NULL,
         y = NULL,
         colour = "Res (%)",
         title = "B") + 
    scale_colour_gradient(low = "ligh tgrey", high = "black") + 
    theme_igray(base_family = "Arial Unicode MS") + 
    theme(panel.margin = unit(2, "lines"), 
          plot.title   = element_text(size = 16, face="bold", vjust = 0.9), 
          axis.title   = element_text(size = 14), 
          axis.text    = element_text(size = 12),
          strip.text.x = element_text(size = 16)) ,
  
  vol_est_talhao %>%
    mutate(Alternativa = factor(Alternativa, labels = c("a", "b", "c", "d"))  ) %>% 
    ggplot(aes(VOL_OBS, VOL_EST))  + 
    geom_point(aes(colour=er), size = 3)  + 
    geom_smooth(method="lm", colour="gray25") + 
    facet_wrap( ~ Alternativa, nrow = 1) + 
    labs(x = NULL,
         y = NULL,
         colour = "Res (%)",
         title = "C") + 
    scale_colour_gradient(low = "ligh tgrey", high = "black") + 
    theme_igray(base_family = "Arial Unicode MS") + 
    theme(panel.margin = unit(2, "lines"), 
          plot.title   = element_text(size = 16, face="bold", vjust = 0.9), 
          axis.title   = element_text(size = 14), 
          axis.text    = element_text(size = 12),
          strip.text.x = element_text(size = 16)) ,
  bottom   = grid.text("Volume Obervado (m³)", gp=gpar(fontsize=16 ) ),
  left     = grid.text("Volume Estimado (m³)", gp=gpar(fontsize=16 ), rot = 90) )


# Visualizar os graficos Finais ####

box_arvore
kzk
cub_res_disp
cub_res_hist
schummacher_crit
inv_res_disp_grid
inv_hist_vol_grid
inv_vol_obs_est_grid




# Exportar os gráficos em .png ####

w <- 12
h <- 6

w1 <- 12
h1 <- 10

ggsave("graficos/kzk.png", kzk, width = 8, height = 8)
ggsave("graficos/box_arvore.png", box_arvore, width = w, height = h)
ggsave("graficos/cub_res_disp.png", cub_res_disp, width = w, height = h)
ggsave("graficos/cub_res_hist.png", cub_res_hist, width = w, height = h)
ggsave("graficos/cub_vol_obs_est.png", cub_vol_obs_est, width = w, height = h)
ggsave("graficos/schummacher_crit.png", schummacher_crit, width = w, height = h)


ggsave("graficos/inv_res_disp_arv.png", inv_res_disp_arv, width = w, height = h)
ggsave("graficos/inv_hist_vol_arv.png", inv_hist_vol_arv, width = w, height = h)
ggsave("graficos/inv_vol_obs_est_arv.png", inv_vol_obs_est_arv, width = w, height = h)

ggsave("graficos/inv_res_disp_parc.png", inv_res_disp_parc, width = w, height = h)
ggsave("graficos/inv_hist_vol_parc.png", inv_hist_vol_parc, width = w, height = h)
ggsave("graficos/inv_vol_obs_est_parc.png", inv_vol_obs_est_parc, width = w, height = h)

ggsave("graficos/inv_res_disp_talh.png", inv_res_disp_talh, width = w, height = h)
ggsave("graficos/inv_hist_vol_talh.png", inv_hist_vol_talh, width = w, height = h)
ggsave("graficos/inv_vol_obs_est_talh.png", inv_vol_obs_est_talh, width = w, height = h)


ggsave("graficos/inv_res_disp_grid.png", inv_res_disp_grid, width = w1, height = h1)
ggsave("graficos/inv_hist_vol_grid.png", inv_hist_vol_grid, width = w1, height = h1)
ggsave("graficos/inv_vol_obs_est_grid.png", inv_vol_obs_est_grid, width = w1, height = h1)


