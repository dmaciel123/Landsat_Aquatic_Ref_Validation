gee_read_data = function(landsat) {
  
  
  landsat$`system:time_start`<-gsub(pattern = "Jan", replacement = "01", landsat$`system:time_start`)
  landsat$`system:time_start`<-gsub(pattern = "Feb", replacement = "02", landsat$`system:time_start`)
  landsat$`system:time_start`<-gsub(pattern = "Mar", replacement = "03", landsat$`system:time_start`)
  landsat$`system:time_start`<-gsub(pattern = "Apr", replacement = "04", landsat$`system:time_start`)
  landsat$`system:time_start`<-gsub(pattern = "May", replacement = "05", landsat$`system:time_start`)
  landsat$`system:time_start`<-gsub(pattern = "Jun", replacement = "06", landsat$`system:time_start`)
  landsat$`system:time_start`<-gsub(pattern = "Jul", replacement = "07", landsat$`system:time_start`)
  landsat$`system:time_start`<-gsub(pattern = "Aug", replacement = "08", landsat$`system:time_start`)
  landsat$`system:time_start`<-gsub(pattern = "Sep", replacement = "09", landsat$`system:time_start`)
  landsat$`system:time_start`<-gsub(pattern = "Oct", replacement = "10", landsat$`system:time_start`)
  landsat$`system:time_start`<-gsub(pattern = "Nov", replacement = "11", landsat$`system:time_start`)
  landsat$`system:time_start`<-gsub(pattern = "Dec", replacement = "12", landsat$`system:time_start`)
  
  landsat$date_cor = as.Date(landsat$`system:time_start`, format = '%m %d, %Y')
  
  dados = landsat
  
  dados$julian.day = yday(dados$date_cor)
  
  
  
  mensais = dados %>% separate(date_cor, into = c("ano", "mes", "dia"))
  
  mensais$mes <- as.factor(mensais$mes)
  
  dados$ano <- as.numeric(mensais$ano)
  dados$mes <- as.numeric(mensais$mes)
  
  return(dados)
  
  
}



model_application = function(landsat_data, GC, model_to_predict, model_name, sensor) {
  
  
  if(sensor == 'ETM' | sensor == 'TM') {
    
    landsat_data = rename(landsat_data, blue  = SR_B1)
    landsat_data = rename(landsat_data, green  = SR_B2)
    landsat_data = rename(landsat_data, red  = SR_B3)
    landsat_data = rename(landsat_data, nir  = SR_B4)
    landsat_data = rename(landsat_data, swir22  = SR_B7)
    
    landsat_data$green_blue = landsat_data$green/landsat_data$blue
    landsat_data$green_red = landsat_data$green/landsat_data$red
    
    #Convert to Rrs
    
    landsat_data$blue = landsat_data$blue /pi
    landsat_data$green = landsat_data$green /pi
    landsat_data$red = landsat_data$red/pi
    landsat_data$nir = landsat_data$nir /pi
    landsat_data$swir22 = landsat_data$swir22 /pi
    
    
    
    if(GC == T) {
      
      landsat_data$blue = landsat_data$blue - landsat_data$swir22
      landsat_data$green = landsat_data$green - landsat_data$swir22
      landsat_data$red = landsat_data$red - landsat_data$swir22
      landsat_data$nir = landsat_data$nir - landsat_data$swir22
      
    }
    
    
    landsat_data$PREDICTED = predict(model_to_predict, landsat_data)
    
    names(landsat_data)[names(landsat_data) == 'PREDICTED'] <- model_name
    
    return(landsat_data)
  }
  
  
  if(sensor == 'OLI') {
    
    landsat_data = rename(landsat_data, coastal  = SR_B1)
    landsat_data = rename(landsat_data, blue  = SR_B2)
    landsat_data = rename(landsat_data, green  = SR_B3)
    landsat_data = rename(landsat_data, red  = SR_B4)
    #landsat_data = rename(landsat_data, nir  = SR_B5)
    landsat_data = rename(landsat_data, swir22  = SR_B7)
    
    landsat_data$green_blue = landsat_data$green/landsat_data$blue
    landsat_data$green_red = landsat_data$green/landsat_data$red
    
    #Convert to Rrs
    
    landsat_data$coastal = landsat_data$coastal /pi
    landsat_data$blue = landsat_data$blue /pi
    landsat_data$green = landsat_data$green /pi
    landsat_data$red = landsat_data$red/pi
    #landsat_data$nir = landsat_data$nir /pi
    landsat_data$swir22 = landsat_data$swir22 /pi
    
    
    if(GC == T) {
      
      landsat_data$blue = landsat_data$blue - landsat_data$swir22
      landsat_data$green = landsat_data$green - landsat_data$swir22
      landsat_data$red = landsat_data$red - landsat_data$swir22
      #landsat_data$nir = landsat_data$nir - landsat_data$swir22
      
    }
    
    
    landsat_data = filter(landsat_data, coastal >0)
    
    landsat_data$PREDICTED = predict(model_to_predict, landsat_data)
    
    names(landsat_data)[names(landsat_data) == 'PREDICTED'] <- model_name
    
    return(landsat_data)
  }
  
}


process_gee = function(data,entropy, max_secchi) {
  
  
  data_read = data
  
  data_read = rename(data_read, Secchi = secchi_predicted)
  
  data_read = filter(data_read, Secchi < 5)
  
  data_read$date_cor = as.Date(data_read$date_cor)
  
  data_read$entropy = (data_read$Secchi-mean(data_read$Secchi, na.rm =))/sd(data_read$Secchi)
  
  if(entropy == T) {data_read$Secchi = data_read$entropy}
  
  
  mensal = plot_time_series(data = data_read$ano,Secchi = data_read$Secchi, color = 'black',
                            MAX = max(data_read$Secchi),MIN = 0, 
                            separador = data_read$mes, size_axis = 10, titulo = 'a', size_title = 10, size_txt = 5)
  
  
  completa = plot_serie_completa(ano = data_read$ano, mes = data_read$mes, NDCI = data_read$Secchi, 
                                 size_axis = 10, MIN = 0, MAX =2,
                                 titulo = 'a',size_title = 10, size_txt = 10, DATE = data_read$date_cor)
  
  
  a = data_read %>% group_by(ano) %>% summarise(media = mean(Secchi))
  
  
  ano = plot_serie_completa(ano = a$ano, mes = a$ano, NDCI = a$media, 
                            size_axis = 10, MIN = 0, MAX = max_secchi,
                            titulo = 'a',size_title = 10, size_txt = 10,
                            DATE = as.Date(paste(a$ano,'/01/01', sep = ''), format = c('%Y/%m/%d')))
  
  full.results = list(completos = completa, mensal = mensal, anual =  ano)
  
  return(full.results)
  
}


save_results = function(gee_data, local) { 
  
  #Index calcualtion
  gee_data$green_blue = gee_data$green/gee_data$blue
  gee_data$green_red = gee_data$green/gee_data$red
  
  #Adding sensor names
  
  tm = filter(gee_data, sensor ==1)
  etm = filter(gee_data, sensor ==2)
  oli = filter(gee_data, sensor ==3)
  oli2 = filter(gee_data, sensor ==4)
  
  write.csv(tm, paste('Outputs/LocationsRrs/TM/tm_', local, '.csv', sep = ''))
  write.csv(etm, paste('Outputs/LocationsRrs/etm/etm_', local, '.csv', sep = ''))
  write.csv(oli, paste('Outputs/LocationsRrs/oli/oli_', local, '.csv', sep = ''))
  write.csv(oli2, paste('Outputs/LocationsRrs/oli2/oli2_', local, '.csv', sep = ''))
  
  
  
  
}

