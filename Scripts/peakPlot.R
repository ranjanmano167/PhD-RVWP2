###runoff peaks vs number of raingauges analyses###

library(calibrate)
library(quantreg)
setwd('..')
source("./2_Scripts/weightRain.R")
theme_set(theme_grey(base_size = 18))

# © WaPUG 2002 WaPUG Code of Practice for the Hydraulic Modelling of Sewer Systems Version 3.001
# Page 38 of 69
# i)    cond1: The total depth should be greater than 5 mm (thres_cond1).
# ii)   cond2: The range of total durations of the rainfall should vary. Ideally one storm should
#       have a duration (thres_cond2 in min) equal to ½ of the time of concentration (Tc) of the system, one
#       storm equal to Tc, and the third more than twice the time Tc.
# iii)  cond3: The rainfall intensity should be greater than 6 mm/hour (thres_cond3) for more than 4 minutes.
#       (Have to check with timestep of the timeseries selected. The default is set to 6, assuming the timestep is 4 min)
# iv)   cond4: The period between events should be sufficient for the flow to return to dry weather
#       conditions. Not conidered here, check it manually from filtered events

#reading rainfall time series and extracting the events using timeSerToEvents
rainfall=read.csv(paste0("./1_Data/InfoWorks/Subcatchment_",8,"_Wgt_rain_",1,
                         " Rainfall intensity_rainfall.csv"),header=TRUE)
allEvents=timeSerToEvents(timeSer=rainfall[,c(1,3)],thres_cond1=10,thres_cond2=20,
                    thres_cond3=5,plot=TRUE)


###not neeeded for this analyses: Just to get all weighted rainfall#####
#start
numComb=c(8,28,56,70,56,28,8,1)
rainfallAllComb=NULL
for(j in 1:8) {
  rainfallAll=NULL
  for (i in 1:numComb[j]){
    rainfall_s=read.csv(paste0("./1_Data/InfoWorks/Subcatchment_",j,"_Wgt_rain_",i," Rainfall intensity_rainfall.csv"),header=TRUE)
    rainfallAll=cbind(rainfallAll,rainfall_s[,3])
  }
  
  rainfallAllComb=abind(rainfallAllComb,rainfallAll,along=2)
}

#converting it to a zoo object
rainfallTS=data.frame(rainfall_s[,1],rainfallAllComb)
rainfallZoo=read.zoo(rainfallTS,header = TRUE, index = 1, tz = "", 
                     format = "%d/%m/%Y %H:%M")
#end


#reading all discharge timeseries and combining them along the columnes to produce one matrix 
numComb=c(8,28,56,70,56,28,8,1)
runoffAllComb=NULL

for(j in 1:8) {
  runoffAll=NULL

  for (i in 1:numComb[j]){
    runoff=read.csv(paste0("./1_Data/InfoWorks/Subcatchment_",j,"_Wgt_rain_",i," Rainfall intensity_qsurf01.csv"),header=TRUE)
    runoffAll=cbind(runoffAll,runoff[,3])
  }
  
  runoffAllComb=abind(runoffAllComb,runoffAll,along=2)
}

#converting it to a zoo object
runoffTS=data.frame(runoff[,1],runoffAllComb)
runoffZoo=read.zoo(runoffTS,header = TRUE, index = 1, tz = "", 
                   format = "%d/%m/%Y %H:%M")


#peak plot
runoffEventAll=NULL
rainfallEventAll=NULL
maxPeaksAll=NULL
qqq=c(1:length(allEvents[[3]]))
qqq=c(10)
for (i in qqq) {
  xxx=i
  rainfallEvent1=allEvents[[3]][[xxx]][,1]
  #Extracing the runoff event using date and time of the corrosponding rainfall event chosen before (rainfallEvent1)
  #Note: 15 min is added to the end time since time of concentration is around 15 min
  rainfallEvent_c=window(rainfallZoo,start=allEvents[[2]][1,xxx],end=(allEvents[[2]][2,xxx]))
  runoffEvent1=window(runoffZoo,start=allEvents[[2]][1,xxx],end=(allEvents[[2]][2,xxx]+15*60))
  #   runoff=coredata(runoffEvent1)
  #   EventRan=c(max(colmaxrunoff),min(runoff[runoff>0.001]))
  maxAll=peakPlot(runoffEvents=runoffEvent1,rainfallEvent=rainfallEvent1,numComb=c(8,28,56,70,56,28,8,1))[[1]]
  maxPeaks=maxAll[[1]]
  maxInt=maxAll[[2]]
  runoffEventAll=rbind(runoffEventAll,runoffEvent1)
  rainfallEventAll=rbind(rainfallEventAll,rainfallEvent_c)
  maxPeaksAll=rbind(maxPeaksAll,maxPeaks)
}

#all discharge &  rainfall
autoplot(runoffEventAll*1000,facet=NULL)+
  labs(title="Variability in runoff",x=paste0("Time (",as.Date(index(runoffEventAll[1,])),")"),y="Runoff [L/s]")+
  theme(legend.position = "none")
autoplot(rainfallEventAll,facet=NULL, xlab=)+
  labs(title="Variability in rainfall intensity",x=paste0("Time (",as.Date(index(rainfallEventAll[1,])),")"),y="Intensity [mm/h]")+
  theme(legend.position = "none")

#min and max of rainfall peak
colMax=apply(rainfallEventAll,2,max)
which.min(colMax)
which.max(colMax)
autoplot(rainfallEventAll[,c(1,6)],facet=NULL, xlab=)+
  geom_line(size=1)+
  labs(x=paste0("Time (",as.Date(index(rainfallEventAll[1,])),")"),y="Intensity [mm/h]")+
  scale_colour_discrete(breaks=c("X1", "X6"),labels=c("min","max"))+
  theme(legend.position="none")


#min and max of runoff peak
colMax=apply(runoffEventAll,2,max)
which.min(colMax)
which.max(colMax)
autoplot(runoffEventAll[,c(1,4)]*1000,facet=NULL, xlab=)+
  geom_line(size=1)+
  labs(x=paste0("Time (",as.Date(index(rainfallEventAll[1,])),")"),y="Runoff [L/s]")+
  scale_colour_discrete(breaks=c("X1", "X4"),labels=c("min","max"))+
  theme(legend.position="none")

  theme(legend.position="bottom",
        legend.title=element_blank())



#not used

maxPeak=apply(maxPeaksAll,1,max)
minPeak=apply(maxPeaksAll,1,min)


runoff_sel=runoffEventAll[,1:8]

runoffAllComb=runoffAllComb[runoffAllComb[,255]<0.2,]
runoffAllComb=runoffAllComb[runoffAllComb[,255]>0.02,]
yyy=as.vector(runoffAllComb[,c(93:162)])
zzz=rep(as.vector(runoffAllComb[,255]),70)
clr=rainbow(5)
ltyp=c(3,2,1,2,3)
clr=c("lightblue1","green","blue","green","lightblue1")
plot(zzz,yyy,xlab="Runoff- 8 raingauges [cums]",ylab="Runoff - 4 raingauges [cums]")
quaReg=rq(yyy~zzz,tau=c(0.95,0.75,0.5,0.25,0.05))
for (i in 1:5){
  abline(a=quaReg[[1]][1,i],b=quaReg[[1]][2,i],col=clr[3],lty=ltyp[i])
}
legend("bottomright",bty="n", 
       legend=c("Median","50% CI","90% CI"),
       col ="blue", lty=c(1,2,3),lw=1)


#bin
#theme calssic
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.border = element_blank(),
#         panel.background = element_blank(),
#         axis.line = element_line(color = 'black')) 
#         lineheight=.8, 


#plotting all cumulative rainfalls

# aaa=read.zoo("complete_min_fil.csv",sep = ",", header = TRUE, 
#              index = 1, tz = "", format = "%d/%m/%Y %H:%M")
# 
# rgNo=seq(1,15,2)
# avrIntAll=NULL
# for (i in rgNo) {
#   avrInt=apply(aaa[,i:(i+1)],1,mean)
#   avrIntAll=cbind(avrIntAll,avrInt)
# } 
# 
# avrIntAll=as.data.frame(avrIntAll)
# colnames(avrIntAll)=1:8
# write.csv(avrIntAll,"temp1.csv")
# bbb=read.zoo("temp1.csv",sep = ",", header = TRUE, 
#              index = 1)
# plot(bbb,plot.type="single",col=rainbow(ncol(bbb)),
#      xlab="Month (2012)",ylab="Cumulative rainfall [mm/hr]")
# grid(nx=NA,ny=NULL)