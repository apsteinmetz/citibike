#get citibike data
library(data.table)
library(dplyr)
library(ggplot2)
library(grid)
library(ggmap)
library(stringr)

setInternet2(use=TRUE)  #needed to allow HTTPS in Windows
startDate=as.Date("2013-7-1")
endDate=as.Date("2015-2-1")
dl<-format(seq(startDate,endDate,by="month"),format="%Y%m")
fileList<-paste("https://s3.amazonaws.com/tripdata/",dl,"-citibike-tripdata.zip",sep="")

dat <-NULL
for (zname in fileList) {
  print(zname)
  temp <- tempfile()
  download.file(zname,temp)
  con <- unzip(temp,junkpaths=TRUE)
  print("appending data to table")
  dat <- rbind(dat,as.data.table(read.csv(con)))
  unlink(temp)

}


print("writing file")
write.csv(dat,"citibike.csv")

print("converting date strings to dates")
datBig$starttime<-as.POSIXct(dat$starttime)
datBig$stoptime<-as.POSIXct(dat$stoptime)

#the median trip is less than 10 minutes
median(dat$tripduration)/60
hist(filter(dat,tripduration<3600)$tripduration/60,xlab="minutes",main="Citibike Ride Duration")

#the evening commute is more popular than the morning commute.  This probably creates imbalances.
hist(hour(dat$starttime),breaks=24,xlab="Start Time",main="Citibike Ride Times")
#or, in ggplot
qplot(hour(starttime),data=dat,geom="histogram",binwidth=1)

#how does start time relate to trip duration?
byStart<-group_by(dat,start=hour(starttime))
d<-summarise(byStart,count=n(),duration=mean(tripduration,na.rm=TRUE)/60)
qplot(y=count,x=avgDuration,data=d)+ geom_text(aes(label=start),vjust=-1)

#what stations have the biggest imbalances
byStartStation<-group_by(dat,start.station.name)
byEndStation<-group_by(dat,end.station.name)
startStation<-summarise(byStartStation,startCount=n())
endStation<-summarise(byEndStation,endCount=n())
names(endStation)<-c("stationName","endCount")
names(startStation)<-c("stationName","startCount")
inner_join(startStation,endStation)
stationTrips <- mutate(inner_join(startStation,endStation),net=startCount-endCount)
qplot(net,data=stationTrips,geom="histogram")


names(endStation)<-c("stationName","endCount")

#find instances where bike was moved without a rider
tripEnds<-select(dat,bikeid,starttime,start.station.name,end.station.name)%>%arrange(bikeid,starttime)
moves<-tripEnds[tripEnds$start.station.name[-1]!=tripEnds$end.station.name,]
group_by(moves,start.station.name)%>%summarise(count=n())%>%arrange(desc(count))


#create a table of stations this assumes every station in the system is used at least once
stations<-distinct(select(dat0215,name=end.station.name,id=end.station.id,lat=end.station.latitude,lon=end.station.longitude))
##########################################################################################
#look at my rides
artbike<-read.csv("art bike trips.csv")

#put duration in minutes
dtn<-array(as.numeric(unlist(str_split(artbike$duration," min | s"))),c(3,length(artbike$duration)))
artbike$duration<-dtn[1,]+dtn[2,]/60

#add station lat/lon
artbike<-inner_join(artbike,stations,by=c("startStation"="name"))
artbike<-inner_join(artbike,stations,by=c("endStation"="name"))
setnames(artbike,names(artbike),sub(".x",".start",names(artbike)))
setnames(artbike,names(artbike),sub(".y",".end",names(artbike)))


#plot the vectors of rides
qplot(y=lat.start,x=lon.start,data=artbike)+geom_segment(aes(x=lon.start,y=lat.start,xend=lon.end,yend=lat.end),arrow=arrow())

#get a map
centermap=c(mean(c(artbike$lon.start,artbike$lon.end)),mean(c(artbike$lat.start,artbike$lat.end)))
manhattan<-get_map(location=centermap,zoom=12,source="osm")
ggmap(manhattan,extent="normal")+geom_segment(data=artbike,aes(x=lon.start,y=lat.start,xend=lon.end,yend=lat.end),arrow=arrow())

attach(artbike)
myRoutes<-NULL
for (n in 1:nrow(artbike)) {
  myRoute<-route(c(lonstart[n],latstart[n]),c(lonend[n],latend[n]),mode="bicycling")
  # myRoute<-cbind(cbind(lonstart[n],latstart[n]),cbind(lonend[n],latedtn
  myRoutes<-rbind(myRoutes,cbind(tripNum=n,startTime=startTime[n],myRoute))
}
