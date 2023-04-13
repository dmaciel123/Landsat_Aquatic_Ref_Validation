#Plots



plot_rrs2 = function(estimado, medido, METODO, campanha, size_axis, size_txt, size_title) {
  
  
  
  
  df3 = data.frame(est = estimado, 
                   measured = medido, 
                   campanha = campanha) %>% na.omit() %>% filter(measured > 0 & est > 0 & est< 10)
  
  
  
  MAPE_Sumarrise__ <- mean(abs((df3$measured-df3$est)/df3$measured))*100 %>% round(1)
  MAE_Sumarrise_ <- 10^median((log10(df3$est)-log10(df3$measured))) %>% round(2)
  BIAS <- 10^mean(abs(log10(df3$est)-log10(df3$measured))) %>% round(2)
  N_Sumarrise_ =    nrow(df3)
  
  
  cor_summarise = cor(df3$est, df3$measured)
  MAPE_Sumarrise__ = round(MAPE_Sumarrise__, 2)
  cor_summarise = round(cor_summarise, 2)
  
  df3 %>% ggplot() + 
    geom_point(aes(x = measured, y = est),size = 4, color = 'black') + 
    #geom_point(aes(x = measured, y = secchi_FQ_021), col = 'red') + 
    
    geom_text(x = (0.05), y = 0.02, aes(label = paste("MAPE = ", MAPE_Sumarrise__)),
              size = size_txt) + 
    
    geom_text(x = (0.05), y = 0.015, 
              aes(label = paste("R = ", cor_summarise)), 
              size = size_txt) + 
    
    geom_text(x = (0.05), y = 0.01, 
              aes(label = paste("MAE = ", MAE_Sumarrise_)),
              size = size_txt) + 
    
    geom_text(x = 0.05, y = 0.005, aes(label = paste("BIAS = ", BIAS)), size = size_txt) + 
    
    
    geom_text(x = 0.05, y = 0, 
              aes(label = paste("N = ", N_Sumarrise_)), size = size_txt) + 
    
    
    geom_abline(slope = 1, intercept = 0) + 
    
    
    scale_x_continuous(limits = c(0,0.06), name = expression(R[rs]~insitu~(sr^-1))) + 
    scale_y_continuous(limits = c(0,0.06), name = expression(R[rs]~satellite~(sr^-1))) + 
    
    
    labs(title=METODO) +
    theme_bw() + 
    theme(panel.grid.major = element_line(colour = "#d3d3d3"),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          text=element_text(family = "Tahoma"),
          plot.title=element_text(face = 'bold', size = size_title),
          axis.title = element_text(face="bold", size = size_axis),
          axis.text.x = element_text(colour="black", size = size_axis),
          axis.text.y = element_text(colour="black", size = size_axis),
          axis.line = element_line(size=2, colour = "black"),
          strip.text = element_text(size=size_axis)) +
    theme(plot.margin = unit(c(4,4,4,4), "lines")) +
    geom_abline(linetype = "dashed") 
  
  
  
}

#Plots

plot_rrs = function(insitu, sat, colors, separador, METODO, campanha, max, size_axis, size_txt) {
  
  
  
  df3 = data.frame(est = sat, 
                   measured = insitu, 
                   separador = separador,
                   colors = colors) %>% na.omit() %>% filter(measured > 0 & est > 0)
  
  
  df3$error_ <- abs((df3$measured-df3$est)/df3$measured)*100
  df3$BIAS <- log10(df3$est)-log10(df3$measured)
  df3$MAE <- abs(log10(df3$est)-log10(df3$measured))
  df3$Y = log10(df3$est/df3$measured)
  
  
  MAPE_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(MAPES = mean(error_))
  MAE_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(MAE = 10^median(MAE))
  BIAS_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(BIAS = 10^mean(BIAS))
  N_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(N = length( separador ))
  E_summarise = df3 %>% group_by(separador) %>% summarise(E = 100*(10^(median(abs(Y)))-1))
  bias2_summarise = df3 %>% group_by(separador) %>% summarise(BIAS2 = 100*sign(median(Y))*(10^(abs(median(Y)))-1))
  cor_summarise = df3 %>% group_by(separador) %>% summarise(R2 = cor(est,measured))
  
  df = data.frame(MAPE_Sumarrise_$separador,
                  round(MAPE_Sumarrise_$MAPES,2),
                  round(cor_summarise$R2,2), 
                  round(E_summarise$E,2), 
                  round(bias2_summarise$BIAS2,2), 
                  round(N_Sumarrise_$N,2))
  
  
  names(df) = c('separador', 'MAPE', 'COR', 'E', 'BIAS', 'N')
  
  df3 %>% ggplot() + 
    geom_point(aes(x = measured, y = est, color = colors),size = 1) + 
    
    facet_wrap(~separador) + 
    
    
    
    geom_text(x = 0, y = max, hjust = 0, vjust = 1,
              aes(label = paste0(" R = ", COR, "\n", 
                                 "MAPE = ", MAPE, "\n", 
                                 "E = ", E, "\n",
                                 "BIAS = ", BIAS, "\n", 
                                 "N = ", N, '\n')), data = df, size = size_txt) +
    
    
    
    geom_abline(slope = 1, intercept = 0) + 
    
    
    scale_x_continuous(limits = c(0,max), name = expression(R[rs]~In-Situ (sr^-1))) + 
    scale_y_continuous(limits = c(0,max), name = expression(R[rs]~Satellite (sr^-1))) + 
    
    
    labs(title=METODO) +
    theme_bw() + 
    theme(panel.grid.major = element_line(colour = "#d3d3d3"),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.title = element_text(face="bold", size = size_axis),
          axis.text.x = element_text(colour="black", size = size_axis),
          axis.text.y = element_text(colour="black", size = size_axis),
          axis.line = element_line(size=2, colour = "black"),
          strip.text = element_text(size=size_axis), 
          legend.key.size = unit(2, 'cm'), 
          legend.key.height= unit(3, 'cm'), 
          legend.title = element_text(size=30), 
          legend.text = element_text(size=30)) +
    guides(color = guide_legend(override.aes = list(size = 10))) +
    labs(fill = 'Bands') +
    theme(plot.margin = unit(c(4,4,4,4), "lines")) +
    geom_abline(linetype = "dashed") 
  
  
  
}



plot_surface_reflectance = function(insitu, sat, colors, separador, METODO, campanha, max, size_axis, size_txt) {
  
  
  
  df3 = data.frame(est = sat, 
                   measured = insitu, 
                   separador = separador,
                   colors = colors) %>% na.omit() %>% filter(measured > 0 & est > 0)
  
  
  df3$error_ <- abs((df3$measured-df3$est)/df3$measured)*100
  df3$BIAS <- log10(df3$est)-log10(df3$measured)
  df3$MAE <- abs(log10(df3$est)-log10(df3$measured))
  df3$Y = log10(df3$est/df3$measured)
  
  
  MAPE_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(MAPES = mean(error_))
  MAE_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(MAE = 10^median(MAE))
  BIAS_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(BIAS = 10^mean(BIAS))
  N_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(N = length( separador ))
  E_summarise = df3 %>% group_by(separador) %>% summarise(E = 100*(10^(median(abs(Y)))-1))
  bias2_summarise = df3 %>% group_by(separador) %>% summarise(BIAS2 = 100*sign(median(Y))*(10^(abs(median(Y)))-1))
  cor_summarise = df3 %>% group_by(separador) %>% summarise(R2 = cor(est,measured))
  
  df = data.frame(MAPE_Sumarrise_$separador,
                  round(MAPE_Sumarrise_$MAPES,2),
                  round(cor_summarise$R2,2), 
                  round(E_summarise$E,2), 
                  round(bias2_summarise$BIAS2,2), 
                  round(N_Sumarrise_$N,2))
  
  
  names(df) = c('separador', 'MAPE', 'COR', 'E', 'BIAS', 'N')
  
  df3 %>% ggplot() + 
    geom_point(aes(x = measured, y = est),color = colors, size = 2) + 
    
    facet_wrap(~separador, nrow = length(unique(df3$separador))) + 
    
    
    
    geom_text(x = 0, y = max, hjust = 0, vjust = 1,
              aes(label = paste0("E = ", E, "\n",
                                 "BIAS = ", BIAS, "\n", 
                                 "MAPE = ", MAPE, "\n", 
                                 "N = ", N, '\n')), data = df, size = size_txt) +
    
    
    
    geom_abline(slope = 1, intercept = 0) + 
    
    
    scale_x_continuous(limits = c(0,max), name = expression(paste(rho, ~(In-Situ)))) + 
    scale_y_continuous(limits = c(0,max), name = expression(paste(rho, ~ (Satellite)))) + 
    
    
    labs(title=METODO) +
    theme_bw() + 
    theme(panel.grid.major = element_line(colour = "#d3d3d3"),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.title = element_text(face="bold", size = size_axis),
          axis.text.x = element_text(colour="black", size = size_axis),
          axis.text.y = element_text(colour="black", size = size_axis),
          axis.line = element_line(size=2, colour = "black"),
          strip.text = element_text(size=size_axis), 
          legend.key.size = unit(2, 'cm'), 
          legend.key.height= unit(3, 'cm'), 
          legend.title = element_text(size=30), 
          legend.text = element_text(size=30)) +
    guides(color = guide_legend(override.aes = list(size = 10))) +
    labs(fill = 'Bands') +
    theme(legend.position = "none")+
  
    theme(plot.margin = unit(c(2,2,2,2), "lines")) +
    geom_abline(linetype = "dashed") 
  
  
  
}




plot_surface_reflectance_log = function(insitu, sat, colors, separador, METODO, campanha, max, size_axis, size_txt) {
  
  
  
  df3 = data.frame(est = sat, 
                   measured = insitu, 
                   separador = separador,
                   colors = colors) %>% na.omit() %>% filter(measured > 0 & est > 0)
  
  
  df3$error_ <- abs((df3$measured-df3$est)/df3$measured)*100
  df3$BIAS <- log10(df3$est)-log10(df3$measured)
  df3$MAE <- abs(log10(df3$est)-log10(df3$measured))
  df3$Y = log10(df3$est/df3$measured)
  
  
  MAPE_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(MAPES = mean(error_))
  MAE_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(MAE = 10^median(MAE))
  BIAS_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(BIAS = 10^mean(BIAS))
  N_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(N = length( separador ))
  E_summarise = df3 %>% group_by(separador) %>% summarise(E = 100*(10^(median(abs(Y)))-1))
  bias2_summarise = df3 %>% group_by(separador) %>% summarise(BIAS2 = 100*sign(median(Y))*(10^(abs(median(Y)))-1))
  cor_summarise = df3 %>% group_by(separador) %>% summarise(R2 = cor(est,measured))
  SLOPE = df3 %>% group_by(separador) %>% summarise(SLOPE = lm(log10(est)~log10(measured))$coefficients[2])
  RMSLE = df3 %>% group_by(separador) %>% summarise(RMSLE = rmsle(actual = measured, predicted = est))
  RMSLE2 = df3 %>% group_by(separador) %>% summarise(RMSLE = sqrt(mean((log10(est)-log10(measured))^2)))
  
  
  
  df = data.frame(MAPE_Sumarrise_$separador,
                  round(MAPE_Sumarrise_$MAPES,1),
                  round(RMSLE2$RMSLE, 2),
                  round(SLOPE$SLOPE,2), 
                  round(E_summarise$E,1), 
                  round(bias2_summarise$BIAS2,1), 
                  round(N_Sumarrise_$N,2))
  
  BETA = '\U03B2'
  epsilon = '\U03B5'
  
  
  names(df) = c('separador', 'MAPE','RMSLE', 'SLOPE', 'E', 'BIAS', 'N')
  
  df3 %>% ggplot() + 
    geom_point(aes(x = measured, y = est),color = colors, size = 2) + 
    
    facet_wrap(~separador, ncol = length(unique(df3$separador))) + 
    
    
    
    geom_label(x = -3, y = -0.8, hjust = 0,  label.padding = unit(0.3, "lines"),
              aes(label = paste0(epsilon, " = ", E,'%', "\n",
                                 BETA, " = ", BIAS, '%', "\n", 
                                 "S = ", SLOPE)), data = df, size = size_txt) +
    
    
    
    geom_abline(slope = 1, intercept = 0) + 
    
    scale_x_log10(#breaks = trans_breaks("log10", function(x) 10^x), 
                  labels = trans_format("log10", math_format(10^.x)), 
                  limits = c(0.001,0.6), breaks = c(0.0001, 0.001, 0.01, 0.1), 
                  name = expression(paste(rho[s], ~'(in situ)'~'[ ]'))) + 
    
    
    scale_y_log10(#breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)),
                  limits = c(0.001,0.6), breaks = c(0.0001, 0.001, 0.01, 0.1), 
                  name = expression(paste(rho[s], ~ (Satellite)~'[ ]'))) +
    
    
    labs(title=METODO) +
    theme_bw() + 
    theme(panel.grid.major = element_line(colour = "#d3d3d3"),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.title = element_text(face="bold", size = size_axis),
          axis.text.x = element_text(colour="black", size = size_axis),
          axis.text.y = element_text(colour="black", size = size_axis),
          axis.line = element_line(size=2, colour = "black"),
          strip.text = element_text(size=size_axis), 
          legend.key.size = unit(2, 'cm'), 
          legend.key.height= unit(3, 'cm'), 
          legend.title = element_text(size=30), 
          legend.text = element_text(size=30)) +
    guides(color = guide_legend(override.aes = list(size = 10))) +
    labs(fill = 'Bands') +
    theme(legend.position = "none")+
    
    theme(plot.margin = unit(c(2,2,2,2), "lines")) +
    geom_abline(linetype = "dashed") 
  
  
  
}

plot_surface_reflectance_log_oli_etm = function(insitu, sat, colors, separador, METODO, campanha, max, size_axis, size_txt) {
  
  
  
  df3 = data.frame(est = sat, 
                   measured = insitu, 
                   separador = separador,
                   colors = colors) %>% na.omit() %>% filter(measured > 0 & est > 0)
  
  
  df3$error_ <- abs((df3$measured-df3$est)/df3$measured)*100
  df3$BIAS <- log10(df3$est)-log10(df3$measured)
  df3$MAE <- abs(log10(df3$est)-log10(df3$measured))
  df3$Y = log10(df3$est/df3$measured)
  
  
  MAPE_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(MAPES = mean(error_))
  MAE_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(MAE = 10^median(MAE))
  BIAS_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(BIAS = 10^mean(BIAS))
  N_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(N = length( separador ))
  E_summarise = df3 %>% group_by(separador) %>% summarise(E = 100*(10^(median(abs(Y)))-1))
  bias2_summarise = df3 %>% group_by(separador) %>% summarise(BIAS2 = 100*sign(median(Y))*(10^(abs(median(Y)))-1))
  cor_summarise = df3 %>% group_by(separador) %>% summarise(R2 = cor(est,measured))
  SLOPE = df3 %>% group_by(separador) %>% summarise(SLOPE = lm(log10(est)~log10(measured))$coefficients[2])
  RMSLE = df3 %>% group_by(separador) %>% summarise(RMSLE = rmsle(actual = measured, predicted = est))
  RMSLE2 = df3 %>% group_by(separador) %>% summarise(RMSLE = sqrt(mean((log10(est)-log10(measured))^2)))
  
  
  
  df = data.frame(MAPE_Sumarrise_$separador,
                  round(MAPE_Sumarrise_$MAPES,1),
                  round(RMSLE2$RMSLE, 2),
                  round(SLOPE$SLOPE,2), 
                  round(E_summarise$E,1), 
                  round(bias2_summarise$BIAS2,1), 
                  round(N_Sumarrise_$N,2))
  
  BETA = '\U03B2'
  epsilon = '\U03B5'
  
  
  names(df) = c('separador', 'MAPE','RMSLE', 'SLOPE', 'E', 'BIAS', 'N')
  
  df3 %>% ggplot() + 
    geom_point(aes(x = measured, y = est),color = colors, size = 2) + 
    
    facet_wrap(~separador, ncol = length(unique(df3$separador))) + 
    
    
    
    geom_text(x = -4, y = -0.3, hjust = 0, vjust = 1,
              aes(label = paste0(epsilon, " = ", E,'%', "\n",
                                 BETA, " = ", BIAS, '%', "\n", 
                                 "S = ", SLOPE, "\n")), data = df, size = size_txt) +
    
    
    
    geom_abline(slope = 1, intercept = 0) + 
    
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                  labels = trans_format("log10", math_format(10^.x)), 
                  limits = c(0.0001,0.5),
                  name = expression(paste(OLI~rho[s],'[ ]'))) + 
    
    
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)),
                  limits = c(0.0001,0.5), 
                  name = expression(paste(ETM+~rho[s],'[ ]'))) +
    
    
    labs(title=METODO) +
    theme_bw() + 
    theme(panel.grid.major = element_line(colour = "#d3d3d3"),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.title = element_text(face="bold", size = size_axis),
          axis.text.x = element_text(colour="black", size = size_axis),
          axis.text.y = element_text(colour="black", size = size_axis),
          axis.line = element_line(size=2, colour = "black"),
          strip.text = element_text(size=size_axis), 
          legend.key.size = unit(2, 'cm'), 
          legend.key.height= unit(3, 'cm'), 
          legend.title = element_text(size=30), 
          legend.text = element_text(size=30)) +
    guides(color = guide_legend(override.aes = list(size = 10))) +
    labs(fill = 'Bands') +
    theme(legend.position = "none")+
    
    theme(plot.margin = unit(c(2,2,2,2), "lines")) +
    geom_abline(linetype = "dashed") 
  
  
  
}

plot_surface_reflectance_log_etm_tm = function(insitu, sat, colors, separador, METODO, campanha, max, size_axis, size_txt) {
  
  
  
  df3 = data.frame(est = sat, 
                   measured = insitu, 
                   separador = separador,
                   colors = colors) %>% na.omit() %>% filter(measured > 0 & est > 0)
  
  
  df3$error_ <- abs((df3$measured-df3$est)/df3$measured)*100
  df3$BIAS <- log10(df3$est)-log10(df3$measured)
  df3$MAE <- abs(log10(df3$est)-log10(df3$measured))
  df3$Y = log10(df3$est/df3$measured)
  
  
  MAPE_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(MAPES = mean(error_))
  MAE_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(MAE = 10^median(MAE))
  BIAS_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(BIAS = 10^mean(BIAS))
  N_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(N = length( separador ))
  E_summarise = df3 %>% group_by(separador) %>% summarise(E = 100*(10^(median(abs(Y)))-1))
  bias2_summarise = df3 %>% group_by(separador) %>% summarise(BIAS2 = 100*sign(median(Y))*(10^(abs(median(Y)))-1))
  cor_summarise = df3 %>% group_by(separador) %>% summarise(R2 = cor(est,measured))
  SLOPE = df3 %>% group_by(separador) %>% summarise(SLOPE = lm(log10(est)~log10(measured))$coefficients[2])
  RMSLE = df3 %>% group_by(separador) %>% summarise(RMSLE = rmsle(actual = measured, predicted = est))
  RMSLE2 = df3 %>% group_by(separador) %>% summarise(RMSLE = sqrt(mean((log10(est)-log10(measured))^2)))
  
  
  
  df = data.frame(MAPE_Sumarrise_$separador,
                  round(MAPE_Sumarrise_$MAPES,1),
                  round(RMSLE2$RMSLE, 2),
                  round(SLOPE$SLOPE,2), 
                  round(E_summarise$E,1), 
                  round(bias2_summarise$BIAS2,1), 
                  round(N_Sumarrise_$N,2))
  
  BETA = '\U03B2'
  epsilon = '\U03B5'
  
  
  names(df) = c('separador', 'MAPE','RMSLE', 'SLOPE', 'E', 'BIAS', 'N')
  
  df3 %>% ggplot() + 
    geom_point(aes(x = measured, y = est),color = colors, size = 2) + 
    
    facet_wrap(~separador, ncol = length(unique(df3$separador))) + 
    
    
    
    geom_text(x = -4, y = -0.3, hjust = 0, vjust = 1,
              aes(label = paste0(epsilon, " = ", E,'%', "\n",
                                 BETA, " = ", BIAS, '%', "\n", 
                                 "S = ", SLOPE, "\n")), data = df, size = size_txt) +
    
    
    
    geom_abline(slope = 1, intercept = 0) + 
    
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                  labels = trans_format("log10", math_format(10^.x)), 
                  limits = c(0.0001,0.5),
                  name = expression(paste(ETM+~rho[s],'[ ]'))) + 
    
    
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)),
                  limits = c(0.0001,0.5), 
                  name = expression(paste(TM+~rho[s],'[ ]'))) +
    
    
    labs(title=METODO) +
    theme_bw() + 
    theme(panel.grid.major = element_line(colour = "#d3d3d3"),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.title = element_text(face="bold", size = size_axis),
          axis.text.x = element_text(colour="black", size = size_axis),
          axis.text.y = element_text(colour="black", size = size_axis),
          axis.line = element_line(size=2, colour = "black"),
          strip.text = element_text(size=size_axis), 
          legend.key.size = unit(2, 'cm'), 
          legend.key.height= unit(3, 'cm'), 
          legend.title = element_text(size=30), 
          legend.text = element_text(size=30)) +
    guides(color = guide_legend(override.aes = list(size = 10))) +
    labs(fill = 'Bands') +
    theme(legend.position = "none")+
    
    theme(plot.margin = unit(c(2,2,2,2), "lines")) +
    geom_abline(linetype = "dashed") 
  
  
  
}


plot_surface_reflectance_RASTER_log_oli_etm = function(insitu, sat, colors, separador, METODO, campanha, max, size_axis, size_txt) {
  
  
  
  df3 = data.frame(est = sat, 
                   measured = insitu, 
                   separador = separador,
                   colors = colors) %>% na.omit() %>% filter(measured > 0 & est > 0)
  
  
  df3$error_ <- abs((df3$measured-df3$est)/df3$measured)*100
  df3$BIAS <- log10(df3$est)-log10(df3$measured)
  df3$MAE <- abs(log10(df3$est)-log10(df3$measured))
  df3$Y = log10(df3$est/df3$measured)
  
  
  MAPE_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(MAPES = mean(error_))
  MAE_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(MAE = 10^median(MAE))
  BIAS_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(BIAS = 10^mean(BIAS))
  N_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(N = length( separador ))
  E_summarise = df3 %>% group_by(separador) %>% summarise(E = 100*(10^(median(abs(Y)))-1))
  bias2_summarise = df3 %>% group_by(separador) %>% summarise(BIAS2 = 100*sign(median(Y))*(10^(abs(median(Y)))-1))
  cor_summarise = df3 %>% group_by(separador) %>% summarise(R2 = cor(est,measured))
  SLOPE = df3 %>% group_by(separador) %>% summarise(SLOPE = lm(log10(est)~log10(measured))$coefficients[2])
  RMSLE = df3 %>% group_by(separador) %>% summarise(RMSLE = rmsle(actual = measured, predicted = est))
  RMSLE2 = df3 %>% group_by(separador) %>% summarise(RMSLE = sqrt(mean((log10(est)-log10(measured))^2)))
  
  
  
  df = data.frame(MAPE_Sumarrise_$separador,
                  round(MAPE_Sumarrise_$MAPES,1),
                  round(RMSLE2$RMSLE, 2),
                  round(SLOPE$SLOPE,2), 
                  round(E_summarise$E,1), 
                  round(bias2_summarise$BIAS2,1), 
                  round(N_Sumarrise_$N,2))
  
  BETA = '\U03B2'
  epsilon = '\U03B5'
  
  
  names(df) = c('separador', 'MAPE','RMSLE', 'SLOPE', 'E', 'BIAS', 'N')
  
  df3 %>% ggplot(aes(x = measured, y = est)) + 
   # geom_point(,color = colors, size = 2) + 
    geom_pointdensity(adjust = 4, size = 3) +
    scale_color_viridis() +
    
    facet_wrap(~separador, ncol = 2) + 
    
    
    
    geom_text(x = 0, y = 0.5, hjust = 0, vjust = 1,
              aes(label = paste0(epsilon, " = ", E,'%', "\n",
                                 BETA, " = ", BIAS, '%', "\n", 
                                 "S = ", SLOPE, "\n",
                                 "N = ", N, "\n")), data = df, size = size_txt) +
    
    
    
    geom_abline(slope = 1, intercept = 0) + 
    
    
    scale_x_continuous(limits = c(0,0.5), name = expression(paste(rho, ~(OLI)))) + 
    scale_y_continuous(limits = c(0,0.5), name = expression(paste(rho, ~ (ETM)))) + 
    
    
    labs(title=METODO) +
    theme_bw() + 
    theme(panel.grid.major = element_line(colour = "#d3d3d3"),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.title = element_text(face="bold", size = size_axis),
          axis.text.x = element_text(colour="black", size = size_axis),
          axis.text.y = element_text(colour="black", size = size_axis),
          axis.line = element_line(size=2, colour = "black"),
          strip.text = element_text(size=size_axis), 
          legend.key.size = unit(2, 'cm'), 
          legend.key.height= unit(3, 'cm'), 
          legend.title = element_text(size=30), 
          legend.text = element_text(size=30)) +
    guides(color = guide_legend(override.aes = list(size = 10))) +
    labs(fill = 'Bands') +
    theme(legend.position = "none")+
    
    theme(plot.margin = unit(c(2,2,2,2), "lines")) +
    geom_abline(linetype = "dashed") 
  
  
  
}

plot_ndci = function(estimado, medido, METODO, campanha, size_axis, size_txt, size_title) {
  
  
  
  
  df3 = data.frame(est = estimado, 
                   measured = medido, 
                   campanha = campanha) %>% na.omit() 
  
  
  
  MAPE_Sumarrise__ <- mean(abs((df3$measured-df3$est)/df3$measured))*100 %>% round(1)
  MAE_Sumarrise_ <- 10^median((log10(df3$est)-log10(df3$measured))) %>% round(2)
  BIAS <- 10^mean(abs(log10(df3$est)-log10(df3$measured))) %>% round(2)
  N_Sumarrise_ =    nrow(df3)
  
  
  cor_summarise = cor(df3$est, df3$measured)
  MAPE_Sumarrise__ = round(MAPE_Sumarrise__, 2)
  cor_summarise = round(cor_summarise, 2)
  
  df3 %>% ggplot() + 
    geom_point(aes(x = measured, y = est),size = 4, color = 'black') + 
    #geom_point(aes(x = measured, y = secchi_FQ_021), col = 'red') + 
    
    geom_text(x = (0.5), y = 0.5, aes(label = paste("MAPE = ", MAPE_Sumarrise__)),
              size = size_txt) + 
    
    geom_text(x = (0.5), y = 0.4, 
              aes(label = paste("R = ", cor_summarise)), 
              size = size_txt) + 
    
    geom_text(x = (0.5), y = 0.3, 
              aes(label = paste("MAE = ", MAE_Sumarrise_)),
              size = size_txt) + 
    
    geom_text(x = 0.5, y = 0.2, aes(label = paste("BIAS = ", BIAS)), size = size_txt) + 
    
    
    geom_text(x = 0.5, y = 0.1, 
              aes(label = paste("N = ", N_Sumarrise_)), size = size_txt) + 
    
    
    geom_abline(slope = 1, intercept = 0) + 
    
    
    scale_x_continuous(limits = c(-0.5,1), name = expression(NDCI~insitu)) + 
    scale_y_continuous(limits = c(-0.5,1), name = expression(NDCI~satellite)) + 
    
    
    labs(title=METODO) +
    theme_bw() + 
    theme(panel.grid.major = element_line(colour = "#d3d3d3"),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          text=element_text(family = "Tahoma"),
          plot.title=element_text(face = 'bold', size = size_title),
          axis.title = element_text(face="bold", size = size_axis),
          axis.text.x = element_text(colour="black", size = size_axis),
          axis.text.y = element_text(colour="black", size = size_axis),
          axis.line = element_line(size=2, colour = "black"),
          strip.text = element_text(size=size_axis)) +
    theme(plot.margin = unit(c(4,4,4,4), "lines")) +
    geom_abline(linetype = "dashed") 
  
  
  
}

plot_com_facet_wrap = function(X, Y,bandas,legend_size, separador, METODO, size_axis, size_txt) {
  
  require(RColorBrewer)
  
  
  df3 = data.frame(est = Y, 
                   measured = X, 
                   separador = separador, 
                   cores = bandas) %>% na.omit() %>% filter(measured > 0 & est > 0 & est< 10)
  
  
  df3$error_ <- abs((df3$measured-df3$est)/df3$measured)*100
  df3$BIAS <- log10(df3$est)-log10(df3$measured)
  df3$MAE <- abs(log10(df3$est)-log10(df3$measured))
  
  
  
  MAPE_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(MAPES = mean(error_))
  MAE_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(MAE = 10^median(MAE))
  BIAS_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(BIAS = 10^mean(BIAS))
  N_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(N = length( separador ))
  
  
  cor_summarise = df3 %>% group_by(separador) %>% summarise(R2 = cor(est,measured))
  
  
  df3 %>% ggplot() + 
    geom_point(aes(x = measured, y = est, color = cores), size = 4) + 

    facet_wrap(~separador) + 
    
    geom_text(x = (0.05), y = 0.01,
              aes(label = paste0("MAPE: ", round(MAPES,2))), 
              data = MAPE_Sumarrise_, size = size_txt, hjust = 0) + 
    
    geom_text(x = (0.05), y = 0.02, 
              aes(label = paste0("R: ", round(R2,2))), 
              data = cor_summarise, size = size_txt, hjust= 0) + 
    
    #geom_text(x = (0.11), y = 0.02, 
    #          aes(label = paste0("MAE: ", round(MAE,2))), 
    #          data = MAE_Sumarrise_, size = size_txt) + 
    #
    #geom_text(x = (0.11), y = 0.03, 
    #          aes(label = paste0("BIAS: ", round(BIAS,2))), 
    #          data = BIAS_Sumarrise_, size = size_txt) + 
    
    
    geom_text(x = (0.05), y = 0.03, 
              aes(label = paste0("N: ", round(N,2))), 
              data = N_Sumarrise_, size = size_txt, hjust =0) + 
    
    geom_abline(slope = 1, intercept = 0) + 
    
    
    scale_x_continuous(limits = c(0,0.12), breaks = c(0,0.03,0.06,0.09), name = expression(Measured~Surface~Reflectance)) + 
    scale_y_continuous(limits = c(0,0.12), breaks = c(0,0.03,0.06,0.09), name = expression('Sentinel-2'~Surface~Reflectance)) + 
    
    #scale_shape_manual(name = "Bands",values = c(15,16,17,18,15,16,17,18,15,16,17,18,15,16,17,18,15,16,17,18,17,18))+
    scale_fill_manual(name =  "Bands", values=c(rep('black',9)))+
    scale_color_manual(name = "Bands", values=c(rep('black',9)))+
    
    #scale_fill_manual(values=rep('green', 8))+
    
    #scale_color_manual(values=c('blue','green', 'red', 'chocolate1','coral', 'darkviolet', 'coral4', 'brown4'))+
    theme_bw() + 
    theme(panel.grid.major = element_line(colour = "#d3d3d3"),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          text=element_text(family = "Tahoma"),
          axis.title = element_text(face="bold", size = size_axis),
          axis.text.x = element_text(colour="black", size = size_axis),
          axis.text.y = element_text(colour="black", size = size_axis),
          axis.line = element_line(size=2, colour = "black"),
          strip.text = element_text(size=size_axis),
          strip.text.x = element_text(margin = margin(7, 7, 7, 7)),
          legend.title = element_text(face = 'bold', size = legend_size),
          legend.text = element_text(face = 'bold', size = legend_size)) +
    #theme(plot.margin = unit(c(7,7,7,7), "lines")) +
    geom_abline(linetype = "dashed") 
  
  
  
}

plot_com_facet_wrapRRS = function(X, Y, bandas,legend_size, separador, METODO, size_axis, size_txt) {
  
  require(RColorBrewer)
  require(Metrics)
  
  df3 = data.frame(est = Y, 
                   measured = X, 
                   separador = separador, 
                   cores = bandas) %>% na.omit() %>% filter(measured > 0 & est > 0 & est< 10)
  
  
  df3$error_ <- abs((df3$measured-df3$est)/df3$measured)*100
  df3$BIAS <- log10(df3$est)-log10(df3$measured)
  df3$MAE <- abs(log10(df3$est)-log10(df3$measured))
  
  
  
  MAPE_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(MAPES = mean(error_))
  MAE_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(MAE = 10^median(MAE))
  BIAS_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(BIAS = 10^mean(BIAS))
  N_Sumarrise_ =   df3 %>% group_by(separador) %>% summarise(N = length( separador ))
  RMSE_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(RMSE = rmse(actual = measured,predicted = est))
  
  
  cor_summarise = df3 %>% group_by(separador) %>% summarise(R2 = cor(est,measured))
  
  
  df3 %>% ggplot() + 
    geom_point(aes(x = measured, y = est, shape = cores, color = cores), size = 4) + 
    
    facet_wrap(~separador) + 
    
    geom_text(x = (0.05), y = 0.025,
              aes(label = paste0("MAPE: ", round(MAPES,2))), 
              data = MAPE_Sumarrise_, size = size_txt) + 
    
    geom_text(x = (0.05), y = 0.02, 
              aes(label = paste0("R: ", round(R2,2))), 
              data = cor_summarise, size = size_txt) + 
    
    geom_text(x = (0.05), y = 0.015, 
              aes(label = paste0("MAE: ", round(MAE,2))), 
              data = MAE_Sumarrise_, size = size_txt) + 
    
    geom_text(x = (0.05), y = 0.01, 
              aes(label = paste0("BIAS: ", round(BIAS,2))), 
              data = BIAS_Sumarrise_, size = size_txt) + 
    
    
    geom_text(x = (0.05), y = 0.005, 
              aes(label = paste0("RMSE: ", round(RMSE,3))), 
              data = RMSE_Sumarrise_, size = size_txt) + 
    
  
    geom_text(x = (0.05), y = 0, 
              aes(label = paste0("N: ", round(N,2))), 
              data = N_Sumarrise_, size = size_txt) + 
    
    geom_abline(slope = 1, intercept = 0) + 
    
    scale_x_continuous(limits = c(0,0.06), name = expression(R[rs]~insitu~(sr^-1))) + 
    scale_y_continuous(limits = c(0,0.06), name = expression(R[rs]~satellite~(sr^-1))) + 
    
    scale_shape_manual(values = rep(20,20))+
    #scale_fill_manual( values = rep('black',10))+
    #scale_color_manual(values = rep('black',10))+
    
    #scale_fill_manual(values=rep('green', 8))+
    
    
    #scale_color_manual(values=c('blue','green', 'red', 'chocolate1','coral', 'darkviolet', 'coral4', 'brown4'))+
    theme_bw() + 
    theme(panel.grid.major = element_line(colour = "#d3d3d3"),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          panel.spacing = unit(2,'lines'),
          text=element_text(family = "Tahoma"),
          axis.title = element_text(face="bold", size = size_axis),
          axis.text.x = element_text(colour="black", size = size_axis),
          axis.text.y = element_text(colour="black", size = size_axis),
          axis.line = element_line(size=2, colour = "black"),
          strip.text = element_text(size=size_axis),
          legend.title = element_text(face = 'bold', size = legend_size),
          legend.text = element_text(face = 'bold', size = legend_size)) +
    theme(plot.margin = unit(c(4,4,4,4), "lines")) +
    geom_abline(linetype = "dashed") 
  
  
  
}


plot_com_facet_wrapGLORIA = function(X, Y, bandas,legend_size, separador, METODO, size_axis, size_txt, cores) {
  
  require(RColorBrewer)
  require(Metrics)
  
  df3 = data.frame(est = Y, 
                   measured = X, 
                   separador = separador, 
                   cores = bandas) %>% na.omit() %>% filter(measured > 0 & est > 0 & est< 10)
  
  
  df3$error_ <- abs((df3$measured-df3$est)/df3$measured)*100
  df3$BIAS <- log10(df3$est)-log10(df3$measured)
  df3$MAE <- abs(log10(df3$est)-log10(df3$measured))
  
  
  
  MAPE_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(MAPES = mean(error_))
  MAE_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(MAE = 10^median(MAE))
  BIAS_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(BIAS = 10^mean(BIAS))
  N_Sumarrise_ =   df3 %>% group_by(separador) %>% summarise(N = length( separador ))
  RMSE_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(RMSE = rmse(actual = measured,predicted = est))
  
  
  cor_summarise = df3 %>% group_by(separador) %>% summarise(R2 = cor(est,measured))
  
  
  df3 %>% ggplot() + 
    geom_point(aes(x = measured, y = est), col = cores, size = 4) + 
    
    facet_wrap(~separador) + 
    
    geom_text(x = (0.05), y = 0.025,
              aes(label = paste0("MAPE: ", round(MAPES,2))), 
              data = MAPE_Sumarrise_, size = size_txt) + 
    
    geom_text(x = (0.05), y = 0.02, 
              aes(label = paste0("R: ", round(R2,2))), 
              data = cor_summarise, size = size_txt) + 
    
    geom_text(x = (0.05), y = 0.015, 
              aes(label = paste0("MAE: ", round(MAE,2))), 
              data = MAE_Sumarrise_, size = size_txt) + 
    
    geom_text(x = (0.05), y = 0.01, 
              aes(label = paste0("BIAS: ", round(BIAS,2))), 
              data = BIAS_Sumarrise_, size = size_txt) + 
    
    
    geom_text(x = (0.05), y = 0.005, 
              aes(label = paste0("RMSE: ", round(RMSE,3))), 
              data = RMSE_Sumarrise_, size = size_txt) + 
    
    
    geom_text(x = (0.05), y = 0, 
              aes(label = paste0("N: ", round(N,2))), 
              data = N_Sumarrise_, size = size_txt) + 
    
    geom_abline(slope = 1, intercept = 0) + 
    
    scale_x_continuous(limits = c(0,0.06), name = expression(R[rs]~Measured)) + 
    scale_y_continuous(limits = c(0,0.06), name = expression(R[rs]~Predicted)) + 
    
    scale_shape_manual(values = rep(20,20))+

    theme_bw() + 
    theme(panel.grid.major = element_line(colour = "#d3d3d3"),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          panel.spacing = unit(2,'lines'),
          text=element_text(family = "Tahoma"),
          axis.title = element_text(face="bold", size = size_axis),
          axis.text.x = element_text(colour="black", size = size_axis),
          axis.text.y = element_text(colour="black", size = size_axis),
          axis.line = element_line(size=2, colour = "black"),
          strip.text = element_text(size=size_axis),
          legend.title = element_text(face = 'bold', size = legend_size),
          legend.text = element_text(face = 'bold', size = legend_size)) +
    theme(plot.margin = unit(c(4,4,4,4), "lines")) +
    geom_abline(linetype = "dashed") 
  
  
  
}

ploting_error_bars_lakes = function(X, Y, bandas,legend_size, separador, METODO, size_axis, size_txt, cores, size_title) {
  
  df3 = data.frame(est = Y, 
                   measured = X, 
                   separador = separador) %>% na.omit() %>% filter(measured > 0 & est > 0 & est< 10)
  
  
  df3$error_ <- abs((df3$measured-df3$est)/df3$measured)*100
  df3$BIAS <- log10(df3$est)-log10(df3$measured)
  df3$MAE <- abs(log10(df3$est)-log10(df3$measured))
  
  
  
  MAPE_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(MAPES = mean(error_))
  MAE_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(MAE = 10^median(MAE))
  BIAS_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(BIAS = 10^mean(BIAS))
  N_Sumarrise_ =   df3 %>% group_by(separador) %>% summarise(N = length( separador ))
  RMSE_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(RMSE = rmse(actual = measured,predicted = est))
  cor_summarise = df3 %>% group_by(separador) %>% summarise(R2 = cor(est,measured))
  
  df.res = data.frame(MAPE = MAPE_Sumarrise_$MAPES, 
                      mAe = MAE_Sumarrise_$MAE,
                      BIAS = BIAS_Sumarrise_$BIAS, 
                      N = N_Sumarrise_$N, 
                      RMSE = RMSE_Sumarrise_$RMSE, 
                      cor = cor_summarise$R2,
                      local = MAPE_Sumarrise_$separador)
  a = melt(df.res)
  
  a %>% ggplot() + 
    
    geom_bar(aes(x = (local), y = value),stat='identity', size = 4) + 
    
    facet_wrap(~variable, scales = 'free') + 
    
  
    
    scale_x_discrete(name = expression(R[rs]~Measured)) + 
    scale_y_continuous(name = expression(R[rs]~Predicted)) + 
    
    theme_bw() + 
    theme(panel.grid.major = element_line(colour = "#d3d3d3"),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          panel.spacing = unit(2,'lines'),
          text=element_text(family = "Tahoma"),
          axis.title = element_text(face="bold", size = size_title),
          axis.text.x = element_text(colour="black", size = size_axis),
          axis.text.y = element_text(colour="black", size = size_axis),
          axis.line = element_line(size=2, colour = "black"),
          strip.text = element_text(size=size_title),
          legend.title = element_text(face = 'bold', size = legend_size),
          legend.text = element_text(face = 'bold', size = legend_size)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    theme(plot.margin = unit(c(4,4,4,4), "lines")) 
  
  
  
}

plot_com_facet_wrap_comapracao_metodos = function(X, Y, bandas,legend_size, separador, METODO, size_axis, size_txt) {
  
  require(RColorBrewer)
  require(Metrics)
  
  df3 = data.frame(est = Y, 
                   measured = X, 
                   separador = separador, 
                   cores = bandas) %>% na.omit() %>% filter(measured > 0 & est > 0 & est< 10)
  
  
  df3$error_ <- abs((df3$measured-df3$est)/df3$measured)*100
  df3$BIAS <- log10(df3$est)-log10(df3$measured)
  df3$MAE <- abs(log10(df3$est)-log10(df3$measured))
  df3$ratio = df3$est/df3$measured
  
  
  MAPE_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(MAPES = mean(error_))
  MAE_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(MAE = 10^median(MAE))
  BIAS_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(BIAS = 10^mean(BIAS))
  N_Sumarrise_ =   df3 %>% group_by(separador) %>% summarise(N = length( separador))
  MEAN_RATIO =   df3 %>% group_by(separador) %>% summarise(MEAN_RATIO = mean(ratio, na.rm = T))
  RMSE_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(RMSE = rmse(actual = measured,predicted = est))
  
  
  cor_summarise = df3 %>% group_by(separador) %>% summarise(R2 = cor(est,measured))
  
  
  df3 %>% ggplot() + 
    geom_point(aes(x = measured, y = est, shape = cores),  color = cores, size = 4) + 
    
    facet_wrap(~separador, ncol =3) + 
    
    geom_text(x = (0.05), y = 0.02,
              aes(label = paste0("Mean Ratio: ", round(MEAN_RATIO,2))), 
              data = MEAN_RATIO, size = size_txt) + 
    
    geom_text(x = (0.05), y = 0.015, 
              aes(label = paste0("R: ", round(R2,2))), 
              data = cor_summarise, size = size_txt) + 
    
    geom_text(x = (0.05), y = 0.010, 
              aes(label = paste0("MAE: ", round(MAE,2))), 
              data = MAE_Sumarrise_, size = size_txt) + 
    
    
    geom_text(x = (0.05), y = 0.005, 
              aes(label = paste0("RMSE: ", round(RMSE,3))), 
              data = RMSE_Sumarrise_, size = size_txt) + 
    
    geom_text(x = (0.05), y = 0, 
              aes(label = paste0("N: ", round(N,2))), 
              data = N_Sumarrise_, size = size_txt) + 
    
    geom_abline(slope = 1, intercept = 0) + 
    
    scale_x_continuous(limits = c(0,0.06), name = expression(R[rs]~Sen2Cor~(sr^-1))) + 
    scale_y_continuous(limits = c(0,0.06), name = expression(R[rs]~SIAC~(sr^-1))) + 
    
    scale_shape_manual(values = rep(20,20))+
    #scale_fill_manual( values = rep('black',10))+
    #scale_color_manual(values = rep('black',10))+
    
    #scale_fill_manual(values=rep('green', 8))+
    
    
    #scale_color_manual(values=c('blue','green', 'red', 'chocolate1','coral', 'darkviolet', 'coral4', 'brown4'))+
    theme_bw() + 
    theme(panel.grid.major = element_line(colour = "#d3d3d3"),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          panel.spacing = unit(2,'lines'),
          text=element_text(family = "Tahoma"),
          axis.title = element_text(face="bold", size = size_axis),
          axis.text.x = element_text(colour="black", size = size_axis),
          axis.text.y = element_text(colour="black", size = size_axis),
          axis.line = element_line(size=2, colour = "black"),
          strip.text = element_text(size=size_axis),
          legend.title = element_text(face = 'bold', size = legend_size),
          legend.text = element_text(face = 'bold', size = legend_size)) +
    theme(plot.margin = unit(c(4,4,4,4), "lines")) +
    geom_abline(linetype = "dashed") 
  
  
  
}

plot_com_facet_wrap_comapracao_metodos_secchi = function(X, Y, bandas,legend_size, separador, METODO, size_axis, size_txt) {
  
  require(RColorBrewer)
  require(Metrics)
  
  df3 = data.frame(est = Y, 
                   measured = X, 
                   separador = separador, 
                   cores = bandas) %>% na.omit() %>% filter(measured > 0 & est > 0 & est< 10)
  
  
  df3$error_ <- abs((df3$measured-df3$est)/df3$measured)*100
  df3$BIAS <- log10(df3$est)-log10(df3$measured)
  df3$MAE <- abs(log10(df3$est)-log10(df3$measured))
  df3$ratio = df3$est/df3$measured
  
  
  MAPE_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(MAPES = mean(error_))
  MAE_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(MAE = 10^median(MAE))
  BIAS_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(BIAS = 10^mean(BIAS))
  N_Sumarrise_ =   df3 %>% group_by(separador) %>% summarise(N = length( separador))
  MEAN_RATIO =   df3 %>% group_by(separador) %>% summarise(MEAN_RATIO = mean(ratio, na.rm = T))
  RMSE_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(RMSE = rmse(actual = measured,predicted = est))
  
  
  cor_summarise = df3 %>% group_by(separador) %>% summarise(R2 = cor(est,measured))
  
  
  df3 %>% ggplot() + 
    geom_point(aes(x = measured, y = est, shape = cores, color = cores), size = 4) + 
    
    facet_wrap(~separador, ncol =3) + 
    
    geom_text(x = (5), y = 5,
              aes(label = paste0("Mean Ratio: ", round(MEAN_RATIO,2))), 
              data = MEAN_RATIO, size = size_txt) + 
    
    geom_text(x = (5), y = 4, 
              aes(label = paste0("R: ", round(R2,2))), 
              data = cor_summarise, size = size_txt) + 
    
    geom_text(x = (5), y = 3, 
              aes(label = paste0("MAE: ", round(MAE,2))), 
              data = MAE_Sumarrise_, size = size_txt) + 
    
    
    geom_text(x = (5), y = 2, 
              aes(label = paste0("RMSE: ", round(RMSE,3))), 
              data = RMSE_Sumarrise_, size = size_txt) + 
    
    geom_text(x = (5), y = 1, 
              aes(label = paste0("N: ", round(N,2))), 
              data = N_Sumarrise_, size = size_txt) + 
    
    geom_abline(slope = 1, intercept = 0) + 
    
    scale_x_continuous(limits = c(0,7), name = expression(Z[sd]~Sen2Cor~(sr^-1))) + 
    scale_y_continuous(limits = c(0,7), name = expression(Z[sd]~SIAC~(sr^-1))) + 
    
    scale_shape_manual(values = rep(20,20))+
    #scale_fill_manual( values = rep('black',10))+
    #scale_color_manual(values = rep('black',10))+
    
    #scale_fill_manual(values=rep('green', 8))+
    
    
    #scale_color_manual(values=c('blue','green', 'red', 'chocolate1','coral', 'darkviolet', 'coral4', 'brown4'))+
    theme_bw() + 
    theme(panel.grid.major = element_line(colour = "#d3d3d3"),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          panel.spacing = unit(2,'lines'),
          text=element_text(family = "Tahoma"),
          axis.title = element_text(face="bold", size = size_axis),
          axis.text.x = element_text(colour="black", size = size_axis),
          axis.text.y = element_text(colour="black", size = size_axis),
          axis.line = element_line(size=2, colour = "black"),
          strip.text = element_text(size=size_axis),
          legend.title = element_text(face = 'bold', size = legend_size),
          legend.text = element_text(face = 'bold', size = legend_size)) +
    theme(plot.margin = unit(c(4,4,4,4), "lines")) +
    geom_abline(linetype = "dashed") 
  
  
  
}

plot_com_facet_wrap_NDCI = function(X, Y, bandas,legend_size, separador, METODO, size_axis, size_txt) {
  
  require(RColorBrewer)
  
  
  df3 = data.frame(est = Y, 
                   measured = X, 
                   separador = separador, 
                   cores = bandas) %>% na.omit() %>% filter(measured > 0 & est > 0 & est< 10)
  
  
  df3$error_ <- abs((df3$measured-df3$est)/df3$measured)*100
  df3$BIAS <- log10(df3$est)-log10(df3$measured)
  df3$MAE <- abs(log10(df3$est)-log10(df3$measured))
  
  
  
  MAPE_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(MAPES = mean(error_))
  MAE_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(MAE = 10^median(MAE))
  BIAS_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(BIAS = 10^mean(BIAS))
  N_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(N = length( separador ))
  
  
  cor_summarise = df3 %>% group_by(separador) %>% summarise(R2 = cor(est,measured))
  
  
  df3 %>% ggplot() + 
    geom_point(aes(x = measured, y = est), size = 4) + 

    facet_wrap(~separador) + 
    
    geom_text(x = (0.6), y = 0,
              aes(label = paste0("MAPE: ", round(MAPES,2))), 
              data = MAPE_Sumarrise_, size = size_txt, hjust = 0) + 
    
    geom_text(x = (0.6), y = 0.1, 
              aes(label = paste0("R: ", round(R2,2))), 
              data = cor_summarise, size = size_txt, hjust = 0) + 
    
   # geom_text(x = (0.8), y = 0.2, 
   #           aes(label = paste0("MAE: ", round(MAE,2))), 
   #           data = MAE_Sumarrise_, size = size_txt) + 
   # 
   # geom_text(x = (0.8), y = 0.3, 
   #           aes(label = paste0("BIAS: ", round(BIAS,2))), 
   #           data = BIAS_Sumarrise_, size = size_txt) + 
    
    
    geom_text(x = (0.6), y = 0.2, 
              aes(label = paste0("N: ", round(N,2))), 
              data = N_Sumarrise_, size = size_txt, hjust = 0) + 
    
    geom_abline(slope = 1, intercept = 0) + 
    
    
    scale_x_continuous(limits = c(0,1), name = expression(Measured~NDCI)) + 
    scale_y_continuous(limits = c(0,1), name = expression('Sentinel-2 NDCI')) + 
    
    scale_fill_manual(name = "NDCI", values='black')+
    scale_color_manual(name = "NDCI", values='black')+
    
    #scale_fill_manual(values=rep('green', 8))+
    
    
    #scale_color_manual(values=c('blue','green', 'red', 'chocolate1','coral', 'darkviolet', 'coral4', 'brown4'))+
    theme_bw() + 
    theme(panel.grid.major = element_line(colour = "#d3d3d3"),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          text=element_text(family = "Tahoma"),
          axis.title = element_text(face="bold", size = size_axis),
          axis.text.x = element_text(colour="black", size = size_axis),
          axis.text.y = element_text(colour="black", size = size_axis),
          axis.line = element_line(size=2, colour = "black"),
          strip.text = element_text(size=size_axis),
          legend.title = element_text(face = 'bold', size = legend_size),
          legend.text = element_text(face = 'bold', size = legend_size)) +
    theme(plot.margin = unit(c(4,4,4,4), "lines")) +
    geom_abline(linetype = "dashed") 
  
  
  
}



plot_rrs_glint_sem_glint = function(estimado, medido, METODO, campanha, size_axis, size_txt, size_title) {
  

  
  df3 = data.frame(est = estimado, 
                   measured = medido, 
                   campanha = campanha) %>% na.omit() %>% filter(measured > 0 & est > 0 & est< 10)
  
  
  
  MAPE_Sumarrise__ <- mean(abs((df3$measured-df3$est)/df3$measured))*100 %>% round(1)
  MAE_Sumarrise_ <- 10^median((log10(df3$est)-log10(df3$measured))) %>% round(2)
  BIAS <- 10^mean(abs(log10(df3$est)-log10(df3$measured))) %>% round(2)
  N_Sumarrise_ =    nrow(df3)
  
  
  cor_summarise = cor(df3$est, df3$measured)
  MAPE_Sumarrise__ = round(MAPE_Sumarrise__, 2)
  cor_summarise = round(cor_summarise, 2)
  
  df3 %>% ggplot() + 
    geom_point(aes(x = measured, y = est),size = 4, color = 'black') + 
    #geom_point(aes(x = measured, y = secchi_FQ_021), col = 'red') + 
    
    geom_text(x = (0.05), y = 0.02, aes(label = paste("MAPE = ", MAPE_Sumarrise__)),
              size = size_txt) + 
    
    geom_text(x = (0.05), y = 0.015, 
              aes(label = paste("R = ", cor_summarise)), 
              size = size_txt) + 
    
    geom_text(x = (0.05), y = 0.01, 
              aes(label = paste("MAE = ", MAE_Sumarrise_)),
              size = size_txt) + 
    
    geom_text(x = 0.05, y = 0.005, aes(label = paste("BIAS = ", BIAS)), size = size_txt) + 
    
    
    geom_text(x = 0.05, y = 0, 
              aes(label = paste("N = ", N_Sumarrise_)), size = size_txt) + 
    
    
    geom_abline(slope = 1, intercept = 0) + 
    
    
    scale_x_continuous(limits = c(0,0.06), name = expression(R[rs]~insitu~(sr^-1))) + 
    scale_y_continuous(limits = c(0,0.06), name = expression(R[rs]~satellite~(sr^-1))) + 
    
    
    labs(title=METODO) +
    theme_bw() + 
    theme(panel.grid.major = element_line(colour = "#d3d3d3"),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          text=element_text(family = "Tahoma"),
          plot.title=element_text(face = 'bold', size = size_title),
          axis.title = element_text(face="bold", size = size_axis),
          axis.text.x = element_text(colour="black", size = size_axis),
          axis.text.y = element_text(colour="black", size = size_axis),
          axis.line = element_line(size=2, colour = "black"),
          strip.text = element_text(size=size_axis)) +
    theme(plot.margin = unit(c(4,4,4,4), "lines")) +
    geom_abline(linetype = "dashed") 
  
  
  
}

#Plots