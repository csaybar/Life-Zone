#'Calculate Life Zone in R
#'@author Cesar Aybar 
rm(list = ls())
library(dplyr)
library(data.table)
library(raster)

lifezone<-function(HLZ_f95="~/.",PP,BioT,Etp,output="~/."){
  if(missing(Etp)){
    Etp = 58.93*BioT
    writeRaster(Etp,sprintf("%s/Etp.tif",output),overwrite=T)
    Etp=raster(sprintf("%s/Etp.tif",output))}
  if(inMemory(BioT)||inMemory(PP)||inMemory(Etp)) 
     stop("Error the values of one of the rasters is in memory")
  if(!all.equal(crs(BioT),crs(PP),crs(Etp)))
    stop("Error the Raster CRSs are not the same")
  if(!all.equal(res(Etp),res(PP),res(BioT)) %>% is.logical)
    stop("Error the raster's resolution is different")
  if(!all.equal(extent(Etp) %>% as.matrix(),
                extent(BioT)%>% as.matrix(),
                extent(PP)%>% as.matrix()) %>% is.logical)
    stop("Error the raster's extent is different")

  PP_data<-getValues(PP) 
  PP_data[is.na(PP_data)]=1
  pp_data<-round(PP_data,1)
  BioT_data<-getValues(BioT)
  BioT_data[is.na(BioT_data)]=1
  lat_data <- rasterToPoints(BioT)[,2] %>% as.numeric
  df<-data.table(LAT=lat_data,BioT=BioT_data,Rain=pp_data) 
  write.table(df,sprintf("%s/test_input.txt",output),row.names = F,sep = ",")
  
  system(sprintf("gfortran -o %s/life.exe %s",output,HLZ_f95))
  system(sprintf("cd %s ;./life.exe",output))
  
  r<-fread(sprintf("%s/HLZ_out.txt",output),sep=" ")
  zonasdevida<-PP
  life_zone_cod<-data.frame(zone=sort(unique(r$VI)),code=1:length(unique(r$VI)),stringsAsFactors = F)
  factor_zone<-as.factor(r$VI)
  levels(factor_zone)<-life_zone_cod$zone
  zonasdevida[,]=as.numeric(factor_zone)
  spplot(zonasdevida)
  writeRaster(zonasdevida,sprintf("%s/life_zone.tif",output))
}

#example to run
# lifezone(HLZ_f95 = "/home/aybarpc01/Desktop/LIFE_ZONE/HLZ.f95",
#          PP = PP,BioT = BioT,output = /home/aybarpc01/Desktop/LIFZONEData/try)