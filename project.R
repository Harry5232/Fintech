library("rjson")
library("jsonlite")
#json_data <- fromJSON(file="weekdata.json")
w <- fromJSON("weekdata.json", simplifyDataFrame = TRUE) #讀取json
w2 <- as.data.frame(w[2]) #取data
w_s <- data.frame() #挑出國小data
#w_s$namnames(w2)


for(i in 1:nrow(w2) ){  #28C2DDDD43F5
  if(w2$entries.device_id[i] == '28C2DDDD47A4'){
    w2$school[i] <- "私立再興小學"
    w_s <- rbind(w_s, w2[i,])
  }

  else if(w2$entries.device_id[i] == '28C2DDDD43F5'){
    w2$school[i] <- "實踐國小"
    w_s <- rbind(w_s, w2[i,])
  }
  
  else if(w2$entries.device_id[i] == '28C2DDDD4505'){
    w2$school[i] <- "景興國小"
    w_s <- rbind(w_s, w2[i,])
  }
  else if(w2$entries.device_id[i] == '28C2DDDD435A'){
    w2$school[i] <- "興華國小"
    w_s <- rbind(w_s, w2[i,])
  }
  else if(w2$entries.device_id[i] == '28C2DDDD4354'){
    w2$school[i] <- "木柵國小"
    w_s <- rbind(w_s, w2[i,])
  }
  else if(w2$entries.device_id[i] == '28C2DDDD4357'){
    w2$school[i] <- "永建國小"
    w_s <- rbind(w_s, w2[i,])
  }
  else if(w2$entries.device_id[i] == '28C2DDDD4231'){
    w2$school[i] <- "溪口國小"
    w_s <- rbind(w_s, w2[i,])
  }
  else if(w2$entries.device_id[i] == '28C2DDDD4234'){
    w2$school[i] <- "興隆國小"
    w_s <- rbind(w_s, w2[i,])
  }
  else if(w2$entries.device_id[i] == '28C2DDDD423C'){
    w2$school[i] <- "萬興國小"
    w_s <- rbind(w_s, w2[i,])
  }
  else if(w2$entries.device_id[i] == '28C2DDDD4571'){
    w2$school[i] <- "萬福國小"
    w_s <- rbind(w_s, w2[i,])
  }
  else if(w2$entries.device_id[i] == '28C2DDDD4578'){
    w2$school[i] <- "武功國小"
    w_s <- rbind(w_s, w2[i,])
  }
  else if(w2$entries.device_id[i] == '28C2DDDD479E'){
    w2$school[i] <- "私立中山小學"
    w_s <- rbind(w_s, w2[i,])
  }
  else if(w2$entries.device_id[i] == '28C2DDDD459D'){
    w2$school[i] <- "力行國小"
    w_s <- rbind(w_s, w2[i,])
  }
  else if(w2$entries.device_id[i] == '28C2DDDD4591'){
    w2$school[i] <- "博嘉國小"
    w_s <- rbind(w_s, w2[i,])
  }
  else if(w2$entries.device_id[i] == '28C2DDDD47BB'){
    w2$school[i] <- "萬芳國小"
    w_s <- rbind(w_s, w2[i,])
  }
  else if(w2$entries.device_id[i] == '28C2DDDD4526'){
    w2$school[i] <- "指南國小"
    w_s <- rbind(w_s, w2[i,])
  }
  else if(w2$entries.device_id[i] == '28C2DDDD42F7'){
    w2$school[i] <- "志清國小"
    w_s <- rbind(w_s, w2[i,])
  }
  else if(w2$entries.device_id[i] == '28C2DDDD435B'){
    w2$school[i] <- "明道國小"
    w_s <- rbind(w_s, w2[i,])
  }
  else if(w2$entries.device_id[i] == '28C2DDDD42E5'){
    w2$school[i] <- "辛亥國小"
    w_s <- rbind(w_s, w2[i,])
  }
  else{
    w2$school[i] <- "h"
  }
}

