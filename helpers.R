library(ISOweek)
library(tidyverse)
library(anomalize)

#### Anomaly Function
anomaly_fluview<-function(alpha=0.05, max_anoms=0.2){
  url<-"https://www.cdc.gov/flu/weekly/weeklyarchives2021-2022/data/NCHSData05.csv"
  data<-read.csv(url,stringsAsFactors = F)
  
  #data<-read.csv("/Users/timothywiemken/Desktop/flu.csv", stringsAsFactors=F)
  data[,"wkyr"]<-paste0(data[,"Year"], "-", ifelse(nchar(data[,"Week"])==1, paste0("0", data[,"Week"]), data[,"Week"]))
  data[,"wkyr"] <- sub("(\\d{4}-)(\\d{2})", "\\1W\\2-1", data[,"wkyr"])
  data[,"wkyr"] <- ISOweek2date(data[,"wkyr"])
  data[,"mort"]<-data[,"Percent.of.Deaths.Due.to.Pneumonia.and.Influenza..P.I."]
  data<-as_tibble(data)

    p4 <- data %>%
    time_decompose(mort, frequency="auto", trend="auto", method="twitter") %>%
    anomalize(remainder, alpha = alpha, max_anoms = max_anoms, method = "gesd") %>%
    time_recompose() %>%
    plot_anomalies(time_recomposed = TRUE) +
    ggtitle("")
  
  data<-as.data.frame(data)
  par(mfrow=c(2,1))
  ### extract and re-plot
  low.ylim<-(floor(min(data[,"mort"], na.rm=T))-1) - ((floor(min(data[,"mort"], na.rm=T))-1) %% 2) 
  hi.ylim<-(ceiling(max(data[,"mort"], na.rm=T))+1) + ((ceiling(min(data[,"mort"], na.rm=T))+1) %% 2) 
  xlab<-data[,"Week"][data[,"Week"]%%10==0]
  x.ind<-as.numeric(as.character(row.names(data)[data[,"Week"]%in%as.numeric(as.character(xlab))]))
  min(data[,"mort"])
  par(mar=c(4,5,4,1))
  plot(data[,"mort"], type="l", ylim=c(low.ylim,hi.ylim),
       xaxt="n", yaxt="n", ylab="", xlab="MMWR Week Number")
  title("Anomaly Detection Method", adj  = 0)        
  polygon(c(seq(1,nrow(data)), rev(seq(1,nrow(data)))), c(p4$data$recomposed_l2, rev(p4$data$recomposed_l1)), col="grey94", border=NA)
  lines(data[,"mort"])
  points(data[,"mort"], cex=ifelse(p4$data$anomaly=="Yes", 1, 0), col=ifelse(p4$data$anomaly=="Yes", "red", "white"), pch=ifelse(p4$data$anomaly=="Yes", 18, 0))
  axis(1, at=x.ind, labels=xlab)
  axis(2, at=seq(low.ylim,hi.ylim, by=2), labels=paste0(seq(low.ylim,hi.ylim, by=2),"%"), las=2)
  mtext(side=2, text="Percent of All Deaths \nDue to Pneumonia and Influenza", line = 3)
  text(x=2, y=2, "2013", cex=0.8)
  text(x=43, y=2, "2014", cex=0.8)
  yearz<-names(table(data[,"Year"]))
  for(i in 3:length(yearz)){
    text(x=43+(52*(i-2)), y=low.ylim, yearz[i], cex=0.8)
  }
  mtext(side=1, line=4, "Red Points = Anomalies")
  
  
  ##### regular plot
  low.ylim<-(floor(min(data[,"mort"], na.rm=T))-1) - ((floor(min(data[,"mort"], na.rm=T))-1) %% 2) 
  hi.ylim<-(ceiling(max(data[,"mort"], na.rm=T))+1) + ((ceiling(min(data[,"mort"], na.rm=T))+1) %% 2) 
  xlab<-data[,"Week"][data[,"Week"]%%10==0]
  x.ind<-as.numeric(as.character(row.names(data)[data[,"Week"]%in%as.numeric(as.character(xlab))]))
  par(mar=c(6,5,3,1))
  plot(data[,"mort"], type="l", ylim=c(low.ylim,hi.ylim),
       xaxt="n", yaxt="n", ylab="", xlab="MMWR Week Number", col="white")
  title("Threshold Method", adj  = 0) 
  polygon(c(seq(1,nrow(data)), rev(seq(1,nrow(data)))), c(data[,"mort"], rev(data[,"Threshold"])), col="red", border=NA)
  polygon(c(seq(1,nrow(data)), rev(seq(1,nrow(data)))), c(data[,"Threshold"], rev(data[,"Expected"])), col="lightgray", border=NA)
  polygon(c(seq(1,nrow(data)), rev(seq(1,nrow(data)))), c(data[,"Expected"], rep(0, nrow(data))), col="white", border=NA)
  lines(data[,"mort"])
  axis(1, at=x.ind, labels=xlab)
  axis(2, at=seq(low.ylim,hi.ylim, by=2), labels=paste0(seq(low.ylim,hi.ylim, by=2),"%"), las=2)
  mtext(side=2, text="Percent of All Deaths \nDue to Pneumonia and Influenza", line = 3)
  text(x=2, y=low.ylim, "2013", cex=0.8)
  text(x=43, y=low.ylim, "2014", cex=0.8)
  yearz<-names(table(data[,"Year"]))
  for(i in 3:length(yearz)){
    text(x=43+(52*(i-2)), y=low.ylim, yearz[i], cex=0.8)
  }
  mtext(side=1, line=4, "Red Shaded Areas = Above Epidemic Threshold")
}
#### end function




#-----------------------------------------------------------------------#

#### Anomaly Function for state
anomaly_fluview_state<-function(dataz, alpha=0.05, max_anoms=0.2, state="Missouri"){
  #dataz<-read.csv("~/Desktop/State_Custom_Data.csv")
  dataz<-subset(dataz, dataz[,"SUB.AREA"]==state)
  dataz[,"year"]<-ifelse(dataz[,"WEEK"]<40, paste0(substr(dataz[,"SEASON"], start=1, stop=2), substr(dataz[,"SEASON"], start=6,stop=7)),
                   substr(dataz[,"SEASON"], start=1, stop=4))
  
  dataz[,"wkyr"]<-paste0(dataz[,"year"], "-", ifelse(nchar(dataz[,"WEEK"])==1, paste0("0", dataz[,"WEEK"]), dataz[,"WEEK"]))
  dataz[,"wkyr"] <- sub("(\\d{4}-)(\\d{2})", "\\1W\\2-1", dataz[,"wkyr"])
  dataz[,"wkyr"] <- ISOweek::ISOweek2date(dataz[,"wkyr"])
  dataz[,"wkyr"]<-as.Date(dataz[,"wkyr"])
  dataz[,"mort"]<-dataz[,"PERCENT.P.I"]
  dataz<-dataz[order(dataz[,"wkyr"]),]
  dataz<-as_tibble(dataz)
  
  p4 <- dataz %>%
    time_decompose(mort, frequency="auto", trend="auto", method="twitter") %>%
    anomalize(remainder, alpha = alpha, max_anoms = max_anoms, method = "gesd") %>%
    time_recompose() %>%
    plot_anomalies(time_recomposed = TRUE) +
    ggtitle("")
  
  
  dataz<-as.data.frame(dataz)
  ### extract and re-plot
  low.ylim<-(floor(min(dataz[,"mort"], na.rm=T))-1) - ((floor(min(dataz[,"mort"], na.rm=T))-1) %% 2) 
  hi.ylim<-(ceiling(max(dataz[,"mort"], na.rm=T))+1) + ((ceiling(min(dataz[,"mort"], na.rm=T))+1) %% 2) 
  xlab<-dataz[,"WEEK"][dataz[,"WEEK"]%%10==0]
  x.ind<-as.numeric(as.character(row.names(dataz)[dataz[,"WEEK"]%in%as.numeric(as.character(xlab))]))
  par(mar=c(4,5,4,1))
  plot(dataz[,"mort"], type="l", ylim=c(low.ylim,hi.ylim),
       xaxt="n", yaxt="n", ylab="", xlab="MMWR Week Number")
  title("Anomaly Detection Method", adj  = 0)        
  polygon(c(seq(1,nrow(dataz)), rev(seq(1,nrow(dataz)))), c(p4$data$recomposed_l2, rev(p4$data$recomposed_l1)), col="grey94", border=NA)
  lines(dataz[,"mort"])
  points(dataz[,"mort"], cex=ifelse(p4$data$anomaly=="Yes", 1, 0), col=ifelse(p4$data$anomaly=="Yes", "red", "white"), pch=ifelse(p4$data$anomaly=="Yes", 18, 0))
  axis(1, at=x.ind, labels=xlab)
  axis(2, at=seq(low.ylim,hi.ylim, by=2), labels=paste0(seq(low.ylim,hi.ylim, by=2),"%"), las=2)
  mtext(side=2, text="Percent of All Deaths \nDue to Pneumonia and Influenza", line = 3)
  yearz<-names(table(dataz[,"year"]))
  
  text(x=2, y=2, as.character(min(yearz)), cex=0.8)
  text(x=43, y=2, as.character(as.numeric(min(yearz))+1), cex=0.8)
  for(i in 3:length(yearz)){
    text(x=43+(52*(i-2)), y=low.ylim, yearz[i], cex=0.8)
  }
  mtext(side=1, line=4, "Red Points = Anomalies")
  
  
}

