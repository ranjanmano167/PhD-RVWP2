setwd('..')
source("./2_Scripts/weightRain.R")

eventData=read.csv(("./1_Data/eventData.csv"),header=TRUE)
rainGauPos=as.matrix(read.csv("./1_Data/loc.csv"),header=TRUE)
bdryData=readOGR(dsn="./1_Data/ArcGIS/bfcatch2.shp",layer="bfcatch2")
bdryCoords=bdryData@polygons[[1]]@Polygons[[1]]@coords
bdryCoordsRev=data.frame(x=rev(bdryCoords[,1]), y=rev(bdryCoords[,2]))
bdryThiess=as.data.frame(matrix(c(415300, 432715,415735,432715,415735,
                                  432890, 415300,432890),ncol=2,byrow=TRUE))
names(bdryThiess)=c("x","y")

k=c(14)  #selEvents
j=c("15 min") #timeScale
#raingauges
gauMean=TRUE #if the mean value of pair raingauge should be used or not
rgNo=seq(1,15,2) #raingauge numbers  (doesn't make any dofferent if TRUE is selected above) 

#reading from csv files and extracting data for the event date and time
fileName=paste(eventData[k,2])
eventStart=as.POSIXct(eventData[k,3],tz="",format = "%d/%m/%Y %H:%M")
eventEnd=as.POSIXct(eventData[k,4],tz="",format = "%d/%m/%Y %H:%M")
eventDur=as.numeric(difftime(eventEnd, eventStart, units="hours"))
tsInt=csvToInten("./1_Data/",fileName,eventStart,eventEnd,timeScale=j)
avrIntAll=get_avr(tsInt,gauMean,rgNo)

#calculating thiessen weights fordifferent combinations of raingauges
wtsThi=combn(1:8,4,function (x) thiWeights(rgCoord=rainGauPos,rgId=x,
                                           bdryBas=bdryThiess,
                                           bdryThi=bdryThiess),simplify=TRUE)

plot(X=NULL,y=NULL)
#estimating weighted rainfall using weights derived from above
for (i in 1:ncol(wtsThi)){
  wgtRain=weightRain(rainData=avrIntAll, weights=wtsThi[3,i][[1]],
                     rgId=c(wtsThi[1,i][[1]]))
  
  write.csv(wgtRain,paste0("./3_Results/Wgt_rain_",i,".csv"))
}

#Thiessen weights
aaa=thiWeights(rgCoord=rainGauPos,rgId=c(1:8),bdryBas=bdryThiess,
               bdryThi=bdryThiess)

























# ##Bin
# #average rainfall
# avrRain=rowMeans(avrIntAll)
# 
# #exporting data to csv files
# write.csv(avrIntAll,paste0(directoryData,"event_all_",k,".csv"))
# write.csv(weightedRain,paste0(directoryData,"event_Weighted_",k,".csv"))
# write.csv(avrRain,paste0(directoryData,"event_avr_",k,".csv"))

# ##otherway
# b=voronoipolygons(a)
# plot(b)







