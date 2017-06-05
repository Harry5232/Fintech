library("rjson")
library("jsonlite")
library(httr)
library(RCurl)
library(XML)
library(bitops)
library(ggmap)
#json_data <- fromJSON(file="weekdata.json")
w <- fromJSON("weekdata.json", simplifyDataFrame = TRUE) #讀取學校json data
w2 <- as.data.frame(w[2]) #取data  正式data
meta <- read.csv("台北智慧城市空氣盒子2.csv",stringsAsFactors =FALSE)
go <- read.csv("school.csv",stringsAsFactors =FALSE)
w_s <- data.frame() #為國小標記名稱和刪減欄位
w_s2 <- data.frame()

#w_s$namnames(w2)
#w_s <- w_s[,c(1:5,7:9)]
w_s <- w2[,c(1:5,7:11)] #刪減欄位


entries.time <- c()
schooln <- c()
polution <- c()
lng <- c()
lat <- c()
for(i in 1:length(schools)){
  temp <- w_s2[w_s2$school == schools[i], ]
  s <- 1
  for(j in 1:length(t)){
    total <- 0
    num <- 0
    for(k in s:length(temp$entries.time)){
      dif <- difftime(temp$entries.time[k],t[j],units = "secs")
      if(dif < 14400){
        num <- num + 1
        total <- temp$polution[k] +total
      }else{
        s <- k
        break
      }
    }
    m <- total / num 
    entries.time <- c(entries.time,as.character(t[j]))
    schooln <- c(schooln,schools[i])
    polution <- c(polution,m)
    lng<- c(lng,temp$lng[k])
    lat <- c(lat,temp$lat[k])
    print(i)
  }
}
w_s3 <- data.frame(entries.time,schooln,polution,lng,lat)

#difftime(b,a,units = "secs")
#write.csv(w_s2,file="w_s2.csv", sep = ",",row.names = FALSE,quote=F)












schools <- unique(w_s$school)
for(i in 1:length(schools)){
  temp <- w_s[w_s$school == schools[i],c(2,11:15)]
  w_s2 <- rbind(w_s2,temp)
}



#字串轉成數值
w_s$lat <- as.numeric(w_s$lat)
w_s$lng <- as.numeric(w_s$lng)
w_s$level <- as.numeric(w_s$level)

#製作地圖
TaipeiMap <- get_map(location = c(121.43,24.93,121.62,25.19), 
                     zoom = 12, maptype = 'roadmap')
TaipeiMapO<- ggmap(TaipeiMap)+ 
geom_point(data=w_s2, 
           aes(x=lng, y=lat,size=1,color=polution))+ 
  scale_color_continuous(low = "yellow",high = "red")+ 
  guides(size=FALSE)

#放置經緯度
for(i in 1:nrow(w_s)){
  for(j in 1:nrow(go)){
    if(w_s$school[i] %in% go$school[j]){
      w_s$lat[i] <- go$lat[j]
      w_s$lng[i] <- go$lng[j]
      print(i)
    }
  }
}

#轉換成汙染程度
for(i in 1:nrow(w_s)){
  x <- w_s$polution[i]
  if(x < 180){
    w_s$level[i] <- 0.5
  }else if(180 <= x && x < 220){
    w_s$level[i] <- 1
  }else if(220 <= x && x < 300){
    w_s$level[i] <- 1.5
  }else if(300 <= x){
    w_s$level[i] <- 2
  }
}

#google map搜尋地址的經緯度
geoPoint = function(address, verbose=FALSE) {
  #若未輸入地址, return錯誤
  if(verbose) cat(address,"\n")
  root = "http://maps.google.com/maps/api/geocode/"
  #Google編碼為UTF8, 中文要轉碼後帶入URL才會正確
  address = iconv(address,"big5","utf8")
  #POI API型態(XML or JSON)
  return.call = "xml"
  sensor = "false"
  #產生URL
  url_gen = paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  #擷取網頁原始碼
  html_code = xmlParse(url_gen)
  #若status為OK抓取資料, 若不為OK return status
  if(xpathSApply(html_code,"//GeocodeResponse//status",xmlValue)=="OK") {
    lat = xpathSApply(html_code,"//result//geometry//location//lat",xmlValue)
    lng = xpathSApply(html_code,"//result//geometry//location//lng",xmlValue)
    loc_type = xpathSApply(html_code,"//result//geometry//location_type",xmlValue)
    return(cbind(lat, lng, loc_type, address))
  } else {
    return(paste("Status:", xpathSApply(html_code,"//GeocodeResponse//status",xmlValue), sep = " "))
  }
}

for(i in 1:nrow(w_s)){
  place <- geoPoint(w_s$school[i])  
  w_s$lat[i] <- place[1]
  w_s$lng[i] <- place[2]
}


#各項指標來計算汙染程度polution = pm*3 + t*2 + h*1 
#分成四級
for(i in 1:nrow(w_s)){
  w_s$polution[i] <- w_s$entries.s_d0[i] *3 + w_s$entries.s_t0[i] *2 + w_s$entries.s_h0[i]
}


#透過device_ID來找到school名稱
for(i in 1:nrow(w_s) ){
  for(j in 1:nrow(meta)){
    if(w_s$entries.device_id[i] == meta$Device_ID[j]){
      w_s$school[i] <- meta$clas[j] 
    }
    
  }
  
}

w_s <- w_s[w_s$entries.device_id != "28C2DDDD457A",]


