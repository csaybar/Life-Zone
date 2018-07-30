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
  # if(inMemory(BioT)||inMemory(PP)||inMemory(Etp)) 
  #    stop("Error the values of one of the rasters is in memory")
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
  lat_data <- coordinates(BioT) %>% tbl_df%>%  dplyr::select(y) %>% 
                 as.matrix %>% as.numeric
  df<-data.table(LAT=lat_data,BioT=BioT_data,Rain=pp_data) 
  write.table(df,sprintf("%s/test_input.txt",output),row.names = F,sep = ",")
  latin = readLines(HLZ_f95)
  latin[223]<-sprintf("  integer,parameter::n=%d,nx=3",nrow(df))
  writeLines(latin,con=HLZ_f95)
  system(sprintf("gfortran -o %s/life.exe %s",output,HLZ_f95))
  system(sprintf("cd %s ;./life.exe", output))
  
  r<-fread(sprintf("%s/HLZ_out.txt",output),sep=" ")
  zonasdevida<-PP
  altitude<-PP
  life_zone_cod<-data.frame(zone=sort(unique(r$VI)),code=1:length(unique(r$VI)),stringsAsFactors = F)
  factor_zone<-as.factor(r$VI)
  levels(factor_zone)<-life_zone_cod$zone
  zonasdevida[,]=as.numeric(factor_zone)
  writeRaster(zonasdevida,sprintf("%s/life_zone.tif",output),overwrite=T)
  
  altitude_zone_cod<-data.frame(zone=sort(unique(r$Zone)),code=1:length(unique(r$Zone)),stringsAsFactors = F)
  factor_zone<-as.factor(r$Zone)
  levels(factor_zone)<-altitude_zone_cod$code
  altitude[,]<-as.numeric(factor_zone)
  writeRaster(altitude,sprintf("%s/altitude_zone.tif",output),overwrite=T)

  return(list(lifezone=list(raster=zonasdevida,code=life_zone_cod),
         altitude=list(raster=altitude,code=altitude_zone_cod)))
}

#example to run
# PP<-raster("/home/senamhi-cesar/Github/Life-Zone/13-PPyr.tif")
# BioT<-raster("/home/senamhi-cesar/Github/Life-Zone/25-BioTyr.tif")
# lf<-lifezone(HLZ_f95 = "/home/senamhi-cesar/Github/Life-Zone/HLZ.f95",
#          PP = PP,BioT = BioT*1,output = "/home/senamhi-cesar/Escritorio/TRY")
