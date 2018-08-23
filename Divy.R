library(readr)
setwd("~/Desktop/DevUp")
Data <- read_csv("Data.csv")
#to refer to data and types without waiting for whole df to load 
subdata=head(Data)

colnames(Data)
# [1] "TRIP ID"                "START TIME"             "STOP TIME"              "BIKE ID"               
#[5] "TRIP DURATION"          "FROM STATION ID"        "FROM STATION NAME"      "TO STATION ID"         
#[9] "TO STATION NAME"        "USER TYPE"              "GENDER"                 "BIRTH YEAR"            
#[13] "FROM LATITUDE"          "FROM LONGITUDE"         "FROM LOCATION"          "TO LATITUDE"           
#[17] "TO LONGITUDE"           "TO LOCATION"            "Boundaries - ZIP Codes" "Zip Codes"             
#[21] "Community Areas"        "Wards"  

#unique stations
library(plyr)
library(dplyr)
FromStations=unique(Data[,c("FROM STATION ID","FROM LATITUDE","FROM LONGITUDE","FROM STATION NAME")])
ToStations=unique(Data[,c("TO STATION ID","TO LATITUDE","TO LONGITUDE","TO STATION NAME")])
colnames(FromStations)=c("ID","Latitude","Longitude","StationName")
colnames(ToStations)=c("ID","Latitude","Longitude","StationName")
Stations=as.data.frame(unique(rbind.data.frame(FromStations,ToStations)))
Stations= Stations %>% dplyr::group_by(ID) %>% dplyr::mutate(Latitude=median(Latitude,na.rm = T), Longitude=median(Longitude,na.rm = T))
Stations=Stations[!duplicated(Stations$ID), ]

#Tasks
#A: Modeling: “Enter a destination and we’ll tell you how long the trip will take” 
#We need you to build a model that can predict how long a trip will take given a starting point and destination. 
#You will need to get creative about the factors that will predict travel time.
#For example, weather and traffic patterns may have an impact on Divvy Bike travel time. 
#There is certainly data out there – you just should find it.

#B: Descriptive Statistics
#1)	Top 5 stations with the most starts (showing # of starts)

starts = Data %>% group_by(`FROM STATION ID`,`FROM STATION NAME`) %>% summarise(NoOfStarts=n())
#the station name and ID data is unclean, let's use only ID
starts = Data %>% group_by(`FROM STATION ID`) %>% summarise(NoOfStarts=n()) %>% arrange(desc(NoOfStarts))# %>% top_n(40)
starts=merge(starts,Stations,by.x = "FROM STATION ID",by.y = "ID")

DivyIcon <- makeIcon(
  iconUrl = "https://cdn.icon-icons.com/icons2/277/PNG/512/Divvy_30184.png",
  iconWidth = 20, iconHeight = 20,
  iconAnchorX = 20, iconAnchorY = 20,
  shadowUrl = "https://cdn.icon-icons.com/icons2/277/PNG/512/Divvy_30184.png",
  shadowWidth = 20, shadowHeight = 20,
  shadowAnchorX = 20, shadowAnchorY = 20
)

# Create a palette that maps factor levels to colors
#RColorBrewer::display.brewer.all()
pal <- colorFactor(
  palette = 'Blues',
  #palette = c('red', 'blue', 'green', 'purple', 'orange'),
  domain = starts$NoOfStarts
)

pal <- colorNumeric(
  palette = colorRampPalette(c('blue', 'red'))(length(starts$NoOfStarts)), 
  domain = starts$NoOfStarts)

starts$popup=paste(starts$StationName,": ",starts$NoOfStarts)
leaflet(data = starts) %>% addTiles() %>%
  #addMarkers(~Longitude, ~Latitude,icon=DivyIcon,label = ~StationName,popup = ~NoOfStarts) %>%
  addCircles(~Longitude, ~Latitude,
    radius = ~NoOfStarts/2000,fillOpacity = 0.7,weight = 200,col=~pal(NoOfStarts),popup = ~popup,label = ~NoOfStarts,
    stroke = FALSE
  )

#2)	Trip duration by user type
UserTypeDuration = Data %>% group_by(`USER TYPE`) %>% summarise(MinDuration=min(`TRIP DURATION`),
                                                                MaxDuration=max(`TRIP DURATION`),
                                                                MeanDuration=mean(`TRIP DURATION`),
                                                                MedianDuration=median(`TRIP DURATION`))
library(ggplot2)
ggplot(Data[Data$`TRIP DURATION`<10000,c("USER TYPE","TRIP DURATION")], aes(x=`USER TYPE`, y=`TRIP DURATION`)) + 
  geom_boxplot()

#3)	Most popular trips based on start station and stop station
library(dplyr)

library(rgdal)
#Zip
#https://www.cityofchicago.org/city/en/depts/doit/provdrs/gis.html
setwd("/Users/disha.gupta1@ibm.com/Desktop/DevUp/Boundaries - ZIP Codes/")
mydata <- readOGR(dsn = ".", layer = "geo_export_febefe7e-98ef-4b0a-bd4b-8445ec9092af")
#Neighborhoods_2012
#setwd("/Users/disha.gupta1@ibm.com/Desktop/DevUp/Neighborhoods_2012/")
#mydata <- readOGR(dsn = ".", layer = "Neighborhoods_2012b")

library(maptools)
if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()
mypoly.union<- unionSpatialPolygons(mydata,mydata$zip)

#region=c(60601) #schoh
#region=as.character(region)
#mypoly.union = mypoly.union[names(mypoly.union) %in% region,]

mymap <- fortify(mypoly.union)
mymap$fill=1:nrow(mymap)

StationsTemp=Stations
coordinates(Stations) <- ~ Longitude + Latitude
proj4string(Stations) <- CRS("+proj=longlat")
Stations<- spTransform(Stations, proj4string(mydata))
proj4string(Stations) <- proj4string(mydata)
Stations=over(Stations, mydata)
Stations=cbind(Stations,StationsTemp)
Stations=Stations[Stations$zip %in% unique(mymap$id),] 

#PopularMAPTemp=PopularMAP
#coordinates(PopularMAP) <- ~ Longitude + Latitude
#proj4string(PopularMAP) <- CRS("+proj=longlat")
#PopularMAP<- spTransform(PopularMAP, proj4string(mydata))
#proj4string(PopularMAP) <- proj4string(mydata)
#PopularMAP=over(PopularMAP, mydata)
#PopularMAP=cbind(PopularMAP,PopularMAPTemp)
##PopularMAP$zip=as.character(PopularMAP$zip)
#PopularMAP=PopularMAP[PopularMAP$zip %in% unique(mymap$id),] 

popular = Data %>% dplyr::group_by(`FROM STATION ID`,`TO STATION ID`) %>% dplyr::summarise(Popularity=n()) %>% dplyr::arrange(desc(Popularity))
#popular=popular[1:100,]
popular$INDEX=1:nrow(popular)
popularFROM = merge(popular[,c("FROM STATION ID","INDEX","Popularity")],Stations,by.x="FROM STATION ID",by.y="ID")
colnames(popularFROM)[colnames(popularFROM)=="FROM STATION ID"]="ID"
popularTO = merge(popular[,c("TO STATION ID","INDEX","Popularity")],Stations,by.x="TO STATION ID",by.y="ID")
colnames(popularTO)[colnames(popularTO)=="TO STATION ID"]="ID"
PopularMAP=rbind.data.frame(popularFROM,popularTO)

library(RColorBrewer)
library(ggthemes)
ggplot() +
  geom_cartogram(data = mymap, aes(x = long, y = lat, map_id = id,fill=fill),map = mymap) +
  scale_fill_gradientn(colours = rev(brewer.pal(10, "Spectral"))) +
  coord_map() +
  geom_point(data = Stations, aes(x = Longitude, y = Latitude), color="black",size=1)+
  theme_map() +theme(legend.position="bottom") + geom_path(data = PopularMAP,
                                                         aes(x=Longitude, y=Latitude, group=INDEX,alpha=Popularity))

library(leaflet)
DivyIcon <- makeIcon(
  iconUrl = "https://cdn.icon-icons.com/icons2/277/PNG/512/Divvy_30184.png",
  iconWidth = 20, iconHeight = 20,
  iconAnchorX = 20, iconAnchorY = 20,
  shadowUrl = "https://cdn.icon-icons.com/icons2/277/PNG/512/Divvy_30184.png",
  shadowWidth = 20, shadowHeight = 20,
  shadowAnchorX = 20, shadowAnchorY = 20
)
leaflet(data = Stations) %>% addTiles() %>%
  addMarkers(~Longitude, ~Latitude,icon=DivyIcon) #%>% 
  #addPolylines(data = PopularMAP, lng = ~Longitude, lat = ~Latitude, group = ~INDEX)

#4)	Rider performance by Gender and Age based on avg trip distance (station to station), median speed (distance traveled / trip duration)
#5)	What is the busiest bike in Chicago in 2017? How many times was it used? How many minutes was it in use?

#C: My Ideas
#Add demographics/weather data
#Try shiny app
#Do to and from GIS visualizations in R