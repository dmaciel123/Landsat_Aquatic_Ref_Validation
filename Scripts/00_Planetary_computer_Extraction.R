library(dplyr, quietly = T)
library(tidyr, quietly = T)
library(sf, quietly = T)
library(sp, quietly = T)
library(rstac, quietly = T)
library(terra, quietly = T)
library(rgdal, quietly = T)
library(data.table, quietly = T)

#Loading stac collection
s_obj <- stac("https://planetarycomputer.microsoft.com/api/stac/v1/")

#Shapefile that will be used to extract 

data = fread('Data/Matchups.csv')
shp = vect('Data/Shapefile/matchups_2_days_filtered2.shp')

shp$Date = as.Date(data$date.x)

# Filte by date (Landsat-5/TM)

shp.new = shp[as.Date(shp$Date) < as.Date('2011-01-01'),]


for(i in 1:nrow(shp.new)) {
  
  meta_limno.shp.subset = shp.new[i,]
  
  
  BBOX = c(ext(meta_limno.shp.subset)[2],
           ext(meta_limno.shp.subset)[3],
           ext(meta_limno.shp.subset)[1]+0.01,
           ext(meta_limno.shp.subset)[4])
  
  if(90 > BBOX[2]) {
    DATA_start = as.Date(meta_limno.shp.subset$Date)-5
    DATA_end = as.Date(meta_limno.shp.subset$Date)+5
    
    datas = paste(DATA_start, DATA_end, sep = '/')
    
    it_obj <- s_obj %>%
      stac_search(collections = "landsat-c2-l2",
                  bbox = BBOX,
                  datetime = datas) %>%
      get_request() %>%
      items_filter(`platform` == 'landsat-5') %>% # Change to landsat-7, landsat-8 or landsat-9
      items_sign(sign_fn = sign_planetary_computer())
    
    if(items_length(it_obj) > 0) {
      
      for(K in 1:items_length(it_obj)) {
        
        blue <- paste0("/vsicurl/", it_obj$features[[K]]$assets$blue$href) %>% rast()
        green <- paste0("/vsicurl/", it_obj$features[[K]]$assets$green$href) %>% rast()
        red <- paste0("/vsicurl/", it_obj$features[[K]]$assets$red$href) %>% rast()
        nir <- paste0("/vsicurl/", it_obj$features[[K]]$assets$nir$href) %>% rast()
        swir16 <- paste0("/vsicurl/", it_obj$features[[K]]$assets$swir16$href) %>% rast()
        swir22 <- paste0("/vsicurl/", it_obj$features[[K]]$assets$swir22$href) %>% rast()
        QA <- paste0("/vsicurl/", it_obj$features[[K]]$assets$qa_pixel$href) %>% rast()
        
        stack = c(blue, green, red, nir,swir16, swir22, QA)
        
        meta_limno.shp.subset.proj = terra::project(meta_limno.shp.subset, stack)
        meta_limno.shp.subset.proj.buffer = terra::buffer(meta_limno.shp.subset.proj, width = 300)
        
        stack.crop = crop(stack, meta_limno.shp.subset.proj.buffer)
        
        stack.focal = terra::focal(stack.crop, w = 5, fun = median, na.rm = T)
        
        res = extract(stack.focal, meta_limno.shp.subset.proj)
        #resQA = extract(stack, meta_limno.shp.subset.proj.buffer, fun = median, na.rm = T)
        
        meta_limno.shp.subset.proj$blue = res[,2]
        meta_limno.shp.subset.proj$green = res[,3]
        meta_limno.shp.subset.proj$red = res[,4]
        meta_limno.shp.subset.proj$nir = res[,5]
        meta_limno.shp.subset.proj$swir16 = res[,6]
        meta_limno.shp.subset.proj$swir22 = res[,7]
        meta_limno.shp.subset.proj$QA = res[,8]
        meta_limno.shp.subset.proj$Image_Name = it_obj$features[[K]]$id
        
        
        
        
        if(exists('rrs_extraction') == T) {
          
          
          rrs_extraction = rbind(rrs_extraction,data.frame(meta_limno.shp.subset.proj))
          
          write.csv(rrs_extraction, 'Outputs/Extraction/reflectance_landsat5.csv')
        }
        
        if(exists('rrs_extraction') == F) {
          
          rrs_extraction = data.frame(meta_limno.shp.subset.proj)
          
          write.csv(rrs_extraction, 'Outputs/Extraction/reflectance_landsat5.csv')
        } 
        
      }
      
    }
    
    #Sys.sleep(5)   
    
    print(i)
    
  }
  
}
