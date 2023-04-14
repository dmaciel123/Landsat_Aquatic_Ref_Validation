### This script will compare Rrs values in situ with LaSRC or LEDAPS algorithsm

require(data.table)
require(dplyr)
require(ggplot2)
source("Scripts/figuras.R")
require(Metrics)
require(scales)
require(ggpubr)


statistics_separator = function(estimado, medido, separador) {
  
  df3 = data.frame(est = estimado, 
                   measured = medido, 
                   separador = separador) %>% na.omit() %>% filter(measured > 0 & est > 0 & est< 10)
  
  
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
  SLOPE = df3 %>% group_by(separador) %>% summarise(SLOPE = lm(log(est)~log(measured))$coefficients[2])
  RMSLE = df3 %>% group_by(separador) %>% summarise(RMSLE = sqrt(mean((log(est)-log(measured))^2)))
  
  cor_summarise = df3 %>% group_by(separador) %>% summarise(R2 = cor(est,measured))
  
  
  resultados = data.frame(Separador = MAPE_Sumarrise_$separador, 
                          MAPE = MAPE_Sumarrise_$MAPES,
                          BIAS = BIAS_Sumarrise_$BIAS, 
                          N = N_Sumarrise_$N, 
                          R = cor_summarise$R2,
                          bias2 = bias2_summarise$BIAS2,
                          E = E_summarise$E, 
                          SLOPE = SLOPE$SLOPE,
                          RMSLE = RMSLE$RMSLE)
  
  return(resultados)
  
  
  
  
  
}

all = fread('Data/Matchups.csv')[,-1]

all$Class = as.character(all$Class)

# Select id vars
id_vars = select(all, !((contains('insitu') | contains('satellite')))) %>% colnames()

insitu = select(all, contains("insitu")) %>% melt()
sat =  select(all, !contains("insitu")) %>% melt(id = id_vars) 

## Remove the "satellite word from the collumn variable

sat$variable = gsub(x = sat$variable, pattern = "_satellite", replacement = '')
sat$sensor_band = paste(sat$sensor, sat$variable)


final = sat
final$insitu = insitu$value


final[final$sensor == 'TM' & final$variable == 'CA', 'insitu'] = 0.1
final[final$sensor == 'ETM+' & final$variable == 'CA', 'insitu'] = 0.1

selection = final


selection = transform(final,
                      sensor_band=factor(sensor_band,levels=c("TM Blue","TM Green","TM Red", 'TM NIR', 
                                                              'ETM+ Blue', 'ETM+ Green', 'ETM+ Red', 'ETM+ NIR', 
                                                              'OLI CA', 'OLI Blue', 'OLI Green', 'OLI Red', 'OLI NIR', 
                                                              'OLI2 CA', 'OLI2 Blue', 'OLI2 Green', 'OLI2 Red', 'OLI2 NIR')))




### Data filtered for only TM, ETM+ and OLI


selection2 = transform(final,
                       sensor_band=factor(sensor_band,levels=c('TM CA', "TM Blue","TM Green","TM Red", 'TM NIR', 
                                                               'ETM+ CA',  'ETM+ Blue', 'ETM+ Green', 'ETM+ Red', 'ETM+ NIR', 
                                                               'OLI CA', 'OLI Blue', 'OLI Green', 'OLI Red', 'OLI NIR', 
                                                               'OLI2 CA', 'OLI2 Blue', 'OLI2 Green', 'OLI2 Red', 'OLI2 NIR')))




#CA
selection2$sensor_band = gsub(x = selection2$sensor_band, pattern = 'TM CA', replacement = 'TM CA (443nm)')
selection2$sensor_band = gsub(x = selection2$sensor_band, pattern = 'ETM\\+\\ CA', replacement = 'ETM+ CA (443nm)')
selection2$sensor_band = gsub(x = selection2$sensor_band, pattern = 'OLI CA', replacement = 'OLI CA (443nm)')
selection2$sensor_band = gsub(x = selection2$sensor_band, pattern = 'OLI2 CA', replacement = 'OLI2 CA (443nm)')


#Blue
selection2$sensor_band = gsub(x = selection2$sensor_band, pattern = 'TM Blue', replacement = 'TM Blue (485nm)')
selection2$sensor_band = gsub(x = selection2$sensor_band, pattern = 'ETM\\+\\ Blue', replacement = 'ETM+ Blue (485nm)')
selection2$sensor_band = gsub(x = selection2$sensor_band, pattern = 'OLI Blue', replacement = 'OLI Blue (482nm)')
selection2$sensor_band = gsub(x = selection2$sensor_band, pattern = 'OLI2 Blue', replacement = 'OLI2 Blue (482nm)')

#Green
selection2$sensor_band = gsub(x = selection2$sensor_band, pattern = 'TM Green', replacement = 'TM Green (560nm)')
selection2$sensor_band = gsub(x = selection2$sensor_band, pattern = 'ETM\\+\\ Green', replacement = 'ETM+ Green (560nm)')
selection2$sensor_band = gsub(x = selection2$sensor_band, pattern = 'OLI Green', replacement = 'OLI Green (561nm)')
selection2$sensor_band = gsub(x = selection2$sensor_band, pattern = 'OLI2 Green', replacement = 'OLI2 Green (561nm)')

#Red
selection2$sensor_band = gsub(x = selection2$sensor_band, pattern = 'TM Red', replacement = 'TM Red (660nm)')
selection2$sensor_band = gsub(x = selection2$sensor_band, pattern = 'ETM\\+\\ Red', replacement = 'ETM+ Red (660nm)')
selection2$sensor_band = gsub(x = selection2$sensor_band, pattern = 'OLI Red', replacement = 'OLI Red (655nm)')
selection2$sensor_band = gsub(x = selection2$sensor_band, pattern = 'OLI2 Red', replacement = 'OLI2 Red (655nm)')

#Nir
selection2$sensor_band = gsub(x = selection2$sensor_band, pattern = 'TM NIR', replacement = 'TM NIR (830nm)')
selection2$sensor_band = gsub(x = selection2$sensor_band, pattern = 'ETM\\+\\ NIR', replacement = 'ETM+ NIR (830nm)')
selection2$sensor_band = gsub(x = selection2$sensor_band, pattern = 'OLI NIR', replacement = 'OLI NIR (865nm)')
selection2$sensor_band = gsub(x = selection2$sensor_band, pattern = 'OLI2 NIR', replacement = 'OLI2 NIR (865nm)')




selection2 = transform(selection2,
                       sensor_band=factor(sensor_band,levels=c('TM CA (443nm)', 
                                                               'TM Blue (485nm)',
                                                               'TM Green (560nm)',
                                                               'TM Red (660nm)',
                                                               'TM NIR (830nm)',
                                                               'ETM+ CA (443nm)', 
                                                               'ETM+ Blue (485nm)',
                                                               'ETM+ Green (560nm)',
                                                               'ETM+ Red (660nm)',
                                                               'ETM+ NIR (830nm)',
                                                               'OLI CA (443nm)', 
                                                               'OLI Blue (482nm)',
                                                               'OLI Green (561nm)',
                                                               'OLI Red (655nm)',
                                                               'OLI NIR (865nm)',
                                                               'OLI2 CA (443nm)', 
                                                               'OLI2 Blue (482nm)',
                                                               'OLI2 Green (561nm)',
                                                               'OLI2 Red (655nm)',
                                                               'OLI2 NIR (865nm)')))








###P Plots with log wihtout OLI2 and NIR


selection3 = filter(selection2, variable == 'CA' & sensor == 'OLI')

coastal = plot_surface_reflectance_log(insitu = selection3$insitu*pi,
                                       sat = selection3$value*pi, 
                                       colors = 'blue', 
                                       separador = selection3$sensor_band,
                                       METODO = '', campanha = selection3$variable, 
                                       max = 0.2, size_axis = 35, size_txt = 10)


selection3 = filter(selection2, variable == 'Blue' & sensor != 'OLI2')
BLUE = plot_surface_reflectance_log(insitu = selection3$insitu*pi, sat = selection3$value*pi, colors = 'darkblue', separador = selection3$sensor_band, METODO = '', campanha = selection3$variable, max = 0.2, size_axis = 35, size_txt = 10)


selection3 = filter(selection2, variable == 'Green' & sensor != 'OLI2')
GREEN = plot_surface_reflectance_log(insitu = selection3$insitu*pi, sat = selection3$value*pi, colors = 'green', separador = selection3$sensor_band, METODO = '', campanha = selection3$variable, max = 0.2, size_axis = 35, size_txt = 10)


selection3 = filter(selection2, variable == 'Red' & sensor != 'OLI2')
RED = plot_surface_reflectance_log(insitu = selection3$insitu*pi, sat = selection3$value*pi, colors = 'red', separador = selection3$sensor_band, METODO = '', campanha = selection3$variable, max = 0.2, size_axis = 35, size_txt =10)


selection3 = filter(selection2, variable == 'NIR' & sensor != 'OLI2')
NIR = plot_surface_reflectance_log(insitu = selection3$insitu*pi, sat = selection3$value*pi, colors = 'magenta', separador = selection3$sensor_band, METODO = '', campanha = selection3$variable, max = 0.2, size_axis = 35, size_txt = 10)


coastal2 = ggarrange(NULL, coastal, NULL, ncol = 3, widths = c(1,1.5,1))

result = ggarrange(BLUE,GREEN,RED, nrow = 3) + bgcolor('white')
result2 = ggarrange(coastal, BLUE,GREEN,RED, NIR, nrow = 5) + bgcolor('white')


ggsave(result, filename = 'Outputs/Figures/Figure_02.jpeg',
       width = 18, height = 22, dpi = 300, units = 'in')



## Heatmaps by Optical Water types


selection$class_band = paste(selection$Class, selection$sensor, selection$variable)

selection_new = filter(selection, !is.na(Class))

est = statistics_separator(estimado = selection_new$value, medido = selection_new$insitu, separador = selection_new$class_band) %>% filter(N > 50)

est.melted = melt(est)

est.melted$Class = 0
est.melted$Sensor = 0
est.melted$Band = 0

splited = strsplit(est.melted$Separador, split = ' ')


for(i in 1:nrow(est.melted)) {
  
  
  est.melted$Class[i] = splited[[i]][1]
  est.melted$Sensor[i] = splited[[i]][2]
  est.melted$Band[i] = splited[[i]][3]
  
  
}


est.melted = transform(est.melted,
                       Sensor=factor(Sensor,levels=c("TM", 'ETM+', 'OLI', 'OLI2')))

est.melted = transform(est.melted,
                       Band=factor(Band,levels=c('CA', 'Blue', 'Green', 'Red', 'NIR')))


est.melted = filter(est.melted, !(Sensor == 'TM' & Band == 'CA'))
est.melted = filter(est.melted, !(Sensor == 'ETM+' & Band == 'CA'))

est.melted$value = round(est.melted$value, 2)


size_axis = 20
size_title  = 20


Error2 = est.melted  %>% filter(variable == 'E') %>% 
  mutate(scaled_rank = scales::rescale(rank(value), range(value))) %>% 
  ggplot(aes(x = Class, y = Band, fill = scaled_rank)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  geom_text(aes(label = value), color = "black", size = 7) +
  scale_fill_distiller(palette = "Spectral", direction=-1) +
  scale_y_discrete(name = '') +
  
  coord_fixed() + 
  ggtitle('e(%)') + facet_wrap(~Sensor, ncol = 4) + 
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
        legend.position = 'none',
        strip.text = element_text(size=size_axis)) +
  theme(plot.title = element_text(hjust = 0.5, size = 25))



BIAS = est.melted  %>% filter(variable == 'bias2') %>% 
  mutate(scaled_rank = scales::rescale(rank(abs(value)), range(abs(value)))) %>% 
  ggplot(aes(x = Class, y = Band, fill = scaled_rank)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  geom_text(aes(label = value), color = "black", size = 7) +
  scale_fill_distiller(palette = "Spectral", direction=-1) +
  scale_y_discrete(name = '') +
  
  coord_fixed() + 
  ggtitle('BIAS(%)') + facet_wrap(~Sensor, ncol = 4) + 
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
        legend.position = 'none',
        strip.text = element_text(size=size_axis)) +
  theme(plot.title = element_text(hjust = 0.5, size = 25))



MAPE = est.melted  %>% filter(variable == 'MAPE') %>%  
  mutate(scaled_rank = scales::rescale(rank(value), range(value))) %>% 
  ggplot(aes(x = Class, y = Band, fill = scaled_rank)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  geom_text(aes(label = value), color = "black", size = 7) +
  scale_y_discrete(name = '') +
  
  scale_fill_distiller(palette = "Spectral", direction=-1) +
  coord_fixed() + 
  ggtitle('MAPE (%)') + facet_wrap(~Sensor, ncol = 4) + 
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
        legend.position = 'none',
        
        strip.text = element_text(size=size_axis)) +
  theme(plot.title = element_text(hjust = 0.5, size = 25))



R = est.melted  %>% filter(variable == 'R') %>%  
  mutate(scaled_rank = scales::rescale(rank(value), range(value))) %>% 
  ggplot(aes(x = Class, y = Band, fill = scaled_rank)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  geom_text(aes(label = value), color = "black", size = 7) +
  scale_fill_distiller(palette = "Spectral", direction=1) +
  scale_y_discrete(name = '') +
  
  coord_fixed() + 
  ggtitle('Pearson Correlation Coefficient (R)') + facet_wrap(~Sensor, ncol = 4) + 
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
        legend.position = 'none',
        
        strip.text = element_text(size=size_axis)) +
  theme(plot.title = element_text(hjust = 0.5, size = 25))


RMSLE = est.melted  %>% filter(variable == 'RMSLE') %>%  
  mutate(scaled_rank = scales::rescale(rank(value), range(value))) %>% 
  ggplot(aes(x = Class, y = Band, fill = scaled_rank)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  geom_text(aes(label = value), color = "black", size = 7) +
  scale_fill_distiller(palette = "Spectral", direction=-1) +
  scale_y_discrete(name = '') +
  
  coord_fixed() + 
  ggtitle('RMSLE') + facet_wrap(~Sensor, ncol = 4) + 
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
        legend.position = 'none',
        
        strip.text = element_text(size=size_axis)) +
  theme(plot.title = element_text(hjust = 0.5, size = 25))


SLOPE = est.melted  %>% filter(variable == 'SLOPE') %>%  
  mutate(scaled_rank = scales::rescale(rank(value), range(value))) %>% 
  ggplot(aes(x = Class, y = Band, fill = scaled_rank)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  geom_text(aes(label = value), color = "black", size = 7) +
  scale_fill_distiller(palette = "Spectral", direction=1) +
  coord_fixed() + 
  ggtitle('SLOPE') + facet_wrap(~Sensor, ncol = 4) + 
  scale_y_discrete(name = '') +
  
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
        legend.position = 'none',
        
        strip.text = element_text(size=size_axis)) +
  theme(plot.title = element_text(hjust = 0.5, size = 25))


N = est.melted  %>% filter(variable == 'N' & Band == 'Blue') %>%  
  mutate(Band = 'Sample Size') %>% 
  mutate(scaled_rank = scales::rescale(rank(value), range(value))) %>% 
  ggplot(aes(x = Class, y = Sensor, fill = scaled_rank)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  geom_text(aes(label = value), color = "black", size = 7) +
  scale_fill_distiller(palette = "Spectral", direction=1) +
  scale_y_discrete(name = '') +
  coord_fixed() + 
  ggtitle('Size') + #facet_wrap(~Sensor) + 
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
        legend.position = 'none',
        strip.text = element_text(size=size_axis)) +
  theme(plot.title = element_text(hjust = 0.5, size = 25))



result_full = ggarrange(Error2, BIAS, RMSLE,MAPE, SLOPE, N, ncol = 2, nrow = 3) + bgcolor('white')

ggsave(plot = result_full, filename = 'Outputs/Figures/Figure_03_Sup.jpeg', width = 25, height = 20)

