library(ggmap)
library(stringi)
gps <- read.csv("C:\\Program Files\\R\\R-3.3.1\\bin\\GPS.csv",header = FALSE)
gps <- as.data.frame(gps)
gps_ne <- data.frame(gps[,c(1,5,7)])

gps_ne$V7 <- as.numeric(gps_ne$V7)
gps_ne$V5 <- as.numeric(gps_ne$V5)
gps_ne$V1 <- stri_encode(gps_ne$V1,"","UTF-8")

gps_ne

from <- gps_ne[1,1]

to <- gps_ne[2,1]

route_dfs <- route(from, to , structure = 'route', mode = 'driving')
len <-nrow(gps_ne)-1
len
for(x in c(2:len)){
    route_df <- route(gps_ne[x,1], gps_ne[x+1,1] , structure = 'route', mode = 'driving')
    route_dfs <- rbind(route_dfs,route_df)
}
route_dfs
TaipeiMap <- get_map(location = c(121.54,24.98,121.56,25.01), zoom = 14, maptype = 'roadmap')

TaipeiMapO<- ggmap(TaipeiMap)+ geom_point(data=gps, aes(x=gps_ne$V7, y=gps_ne$V5,size=1), color = 'red') + geom_path(aes(x = lon, y = lat),  color = 'blue', size = 1.5,data = route_dfs, lineend = 'round')
TaipeiMapO

#TaipeiMapO<- ggmap(TaipeiMap)+ geom_point(data=gps, aes(x=gps_ne$V7, y=gps_ne$V5,size=1), color = 'blue')



