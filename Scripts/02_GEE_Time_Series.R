require(data.table)
require(dplyr)
require(tidyr)
require(ggplot2)
require(openxlsx)
require(tidyr)

source('Scripts/GeeFunctions.R')


# Load data. It was extracted from the GEE graph using this link: https://code.earthengine.google.com/1fd5b6993ac5bada5486ba6b7f22c9ea
# To reproduce, user should download the graph data in GEE by opening the graph and exporting to .CSV
# It is important to save both graphs (Time-series AND counts)



# Load reflectance data

Crater = fread('Data/Lakes/Crater_lake.csv') %>% gee_read_data()
Erie = fread('Data/Lakes/Lake_erie.csv') %>% gee_read_data()
Okeechobee = fread('Data/Lakes/Lake_Okeechobee.csv') %>% gee_read_data()

# Load count

Crater.count = fread('Data/Lakes/Crater_lake_Count.csv') %>% gee_read_data() 
Erie.count = fread('Data/Lakes/Lake_erie_Count.csv') %>% gee_read_data()
Okeechobee.count = fread('Data/Lakes/Lake_Okeechobee_COUNT.csv') %>% gee_read_data()


Crater.count$green = gsub(x = Crater.count$green, pattern = ',', replacement = '')
Erie.count$green = gsub(x = Erie.count$green, pattern = ',', replacement = '')
Okeechobee.count$green = gsub(x = Okeechobee.count$green, pattern = ',', replacement = '')

Crater$COUNT = as.numeric(Crater.count$green)
Erie$COUNT =  as.numeric(Erie.count$green)
Okeechobee$COUNT =  as.numeric(Okeechobee.count$green)

Crater = filter(Crater, COUNT > 0.2 * max(Crater$COUNT))
Erie = filter(Erie, COUNT > 0.2 * (Erie$COUNT))
Okeechobee = filter(Okeechobee, COUNT > 0.2 * max(Okeechobee$COUNT))


Crater$Local = 'Crater'
Erie$Local = 'Erie'
Okeechobee$Local = 'Okeechobee'

#Merge lakes data

dataset = rbind(Crater, Erie, Okeechobee)

## Timonth Series Plots

# To separate time-series for each sensor in GEE a number is atribuited to each sensor
# e..g, 1 = TM, 2 = ETM+. 
dataset$sensor = gsub(x = dataset$sensor, pattern = '1', replacement = 'TM')
dataset$sensor = gsub(x = dataset$sensor, pattern = '2', replacement = 'ETM')
dataset$sensor = gsub(x = dataset$sensor, pattern = '3', replacement = 'OLI')
dataset$sensor = gsub(x = dataset$sensor, pattern = '4', replacement = 'OLI2')

dataset$date_cor = as.character(dataset$date_cor)

dataset = rename(dataset, Blue = blue)
dataset = rename(dataset, Green = green)
dataset = rename(dataset, Red = red)

dataset$Blue = as.numeric(dataset$Blue)
dataset$Green = as.numeric(dataset$Green)
dataset$Red = as.numeric(dataset$Red)
dataset$nir = as.numeric(dataset$nir)

# Removing OLI-2 from time series analysis

dataset = filter(dataset, sensor != 'OLI2')


# Select only necessary data

dataset.melted = select(dataset, c('Blue', 'Green', 'Red', 'nir', 'date_cor', 'Local', 'sensor')) %>% melt()

dataset.melted$date_cor = as.Date(dataset.melted$date_cor)

data2 = dataset.melted %>% separate(date_cor, into = c('year', 'month', 'day'))
data2$month = as.numeric(data2$month)



plot_ts_bands_month_median = function(data2, nrows, ymax, band, add_coords) {
  
  if(is.na(ymax) == T) {filter_max = 0.2}
  if(is.na(ymax) != T) {filter_max = ymax}
  
  data2_year = data2 %>% filter(value > 0 & value < filter_max & variable == band) %>% mutate(year_local = paste(month, year, Local, variable, sensor)) %>% na.omit() %>% 
    group_by(year_local) %>% summarise(media = median(value, na.rm = T)) 
  
  
  
  data2_year = data2_year %>% separate(year_local, into = c('month', 'year', 'local', 'band','sensor'))
  
  data2_year$local_sensor = paste(data2_year$local, data2_year$sensor)
  
  data2_year$sensor_local_year_band = paste(data2_year$sensor, data2_year$local, data2_year$year, data2_year$band)
  
  size_axis = 25
  size_title = 25
  
  data2_year$local_band = paste(data2_year$local, data2_year$band)
  
  test2 <- data2_year %>% 
    group_by(local_sensor) %>%
    arrange(sensor_local_year_band) %>%
    mutate(media_5years = slider::slide_dbl(media, median, .before = 1, .after = 1)) %>%
    ungroup()
  
  test2$Band_Sensor = paste(test2$band, test2$sensor)
  
  test2$local = paste('Lake', test2$local)
  
  test2$local_band = paste(test2$local, test2$band)
  
  test2$Sensor = test2$sensor
  
  
  test2$Sensor = gsub(x = test2$Sensor, pattern = 'TM', replacement  = 'TM')
  test2$Sensor = gsub(x = test2$Sensor, pattern = 'ETM', replacement  = 'ETM+')
  test2$Sensor = gsub(x = test2$Sensor, pattern = 'OLI', replacement  = 'OLI')
  test2$Sensor = gsub(x = test2$Sensor, pattern = 'OLI2', replacement  = 'OLI-2')
  
  ## Plot
  
  test2 = transform(test2,
                    Sensor=factor(Sensor,levels=c('TM', 'ETM+', 'OLI')))
  
  test2$Date = as.Date(paste(test2$year, test2$month, 01, sep = '-'))
  
  Median_TM = test2 %>% group_by(local_sensor) %>% summarise(MEDIAN = median(media))
  
  locais = unique(test2$local)
  
  est = data.frame(local = locais, 
                   TM =   test2 %>% separate(local, c('lake', 'local')) %>% group_by(local) %>% filter(sensor == 'TM') %>%   summarise(MEDIAN =  round(median(media),3)),
                   ETM =   test2 %>% separate(local, c('lake', 'local')) %>% group_by(local) %>% filter(sensor == 'ETM') %>%  summarise(MEDIAN = round(median(media),3)),
                   OLI =   test2 %>% separate(local, c('lake', 'local')) %>% group_by(local) %>% filter(sensor == 'OLI') %>%  summarise(MEDIAN = round(median(media),3)))
  
  if(add_coords == T) {
    
    est$Lat = c(42.940,41.82,26.94)
    est$Long = c(-122.10,-83.18,-80.79)
  }
  
  size_txt = 8
  
  POS = test2 %>% group_by(local) %>% summarise(max.pos = max(media_5years))
  est$POS = POS$max.pos
  
  
  resultados_median_3Years = test2 %>%  
    
    ggplot() + 
    
    geom_line(aes(x = Date,
                  y = media_5years,
                  color = Sensor), size = 0.8) + 
    
    geom_point(aes(x = Date,
                   y = media_5years,
                   color = Sensor, shape = Sensor), size = 1.5) + 
    
    facet_wrap(~local, scale = 'free', nrow = nrows, labeller = labeller(local = 
                                                                           c("Lake Crater" = "Crater Lake",
                                                                             "Lake Erie" = "Western Lake Erie",
                                                                             "Lake Okeechobee" = "Lake Okeechobee"))) +
    
    geom_label(x = as.Date(c('1983-01-01')), hjust = 0, vjust = 1,
               aes(y = POS, label = paste0("Median TM = ", TM.MEDIAN, "\n", 
                                           "Median ETM+ = ", ETM.MEDIAN, "\n", 
                                           "Median OLI = ", OLI.MEDIAN)), data = est, size = 6) +
    
    geom_label(x = as.Date(c('2017-01-01')), hjust = 0, vjust = 1,
               aes(y = POS, label = paste0("Lat = ", Lat, "  Long = ", Long)), data = est, size = 6) +
    
    
    scale_y_continuous(limits = c(0, NA), name = expression(rho[s]~"[]")) + 
    scale_x_date(breaks = '5 year',date_minor_breaks = "1 year",  date_labels = '%Y') +
    #breaks = c(as.Date('1980-01-01'), as.Date('1990-01-01'),  as.Date('2000-01-01'), as.Date('2010-01-01'), as.Date('2020-01-01')), ) +
    
    scale_color_manual(values = c('black', 'red', 'blue')) +
    
    
    labs(title = paste(band, "Band", sep = ' '),
         y = "Reflectance",
         x = "Year", size = 50) +
    
    
    theme_bw() + 
    theme(panel.grid.major = element_line(colour = "#d3d3d3"),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.title = element_text(size = 20), 
          legend.text = element_text(size = 20),
          text=element_text(family = "Tahoma"),
          plot.title=element_text(face = 'bold', size = size_title),
          axis.title = element_text(face="bold", size = size_axis),
          axis.text.x = element_text(colour="black", size = size_axis),
          axis.text.y = element_text(colour="black", size = size_axis),
          axis.line = element_line(size=2, colour = "black"),
          strip.text = element_text(size=size_axis)) +
    guides(fill=guide_legend(title="Sensor")) +
    theme(plot.margin = unit(c(1,1,1,1), "lines"))
  
  resultados_median_3Years
  
  return(resultados_median_3Years)
}

plot_band = plot_ts_bands_month_median(data2 = data2, nrows = 3, ymax = 0.2, add_coords =T, 
                                        band = 'Red')

# Change the filename to the expected band (e..g, reg, NIR, blue..)

ggsave(plot = plot_band, filename = paste('Outputs/TimeSeries/Red.jpeg', sep = ''),
       width = 20, height = 15, dpi = 200, units = 'in')
