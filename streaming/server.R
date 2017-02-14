library(dplyr)
library(data.table)
library(ggplot2)

filenames <- list.files(pattern="*.csv", full.names=TRUE)

data_at_start <- rbindlist(lapply(filenames, fread))

ids_at_start <- unique(data_at_start$sensorID)



IsThereNewFile <- function(){  #  cheap function whose values over time will be tested for equality;
  #  inequality indicates that the underlying value has changed and needs to be 
  #  invalidated and re-read using ReadAllData
  
  filenames <- list.files(pattern="*.csv", full.names=TRUE)
  length(filenames)
}

ReadAllData=function(){ # A function that calculates the underlying value
  
  filenames <- list.files(pattern="*.csv", full.names=TRUE)
  temp= rbindlist(lapply(filenames, fread))
  temp$timestamp =as.POSIXct(as.numeric(as.character(temp$timestamp)),origin="1970-01-01",tz="GMT")
  temp
}



shinyServer(function(input, output,session) {
  
  
  alldata <- reactivePoll(100, session,IsThereNewFile, ReadAllData)   
  
  output$myleaflet <- renderLeaflet({
    latslons=distinct(data_at_start,longitude,latitude,.keep_all = TRUE)
    latslons$latlon=paste(latslons$longitude,latslons$latitude,sep=" , ")
    top =max(latslons$latitude)+ .2
    left =min(latslons$longitude) - .2
    right = max(latslons$longitude)+ .2
    bottom = min(latslons$latitude)- .2
    
    leaflet(latslons)%>%fitBounds(right,bottom,left,top)%>%
      addTiles()%>%
      addMarkers(
        data=latslons,
        label=~as.character(sensorID),
        labelOptions = labelOptions(noHide = T,textOnly = T,textsize = "10px",offset = c(12, -10))
      )%>%
      addMarkers(
        data=latslons,
        label=~as.character(latlon),
        labelOptions = labelOptions(noHide = F,textOnly = T,textsize = "10px",offset = c(12, 10))
      )
  })
  
  output$timeseries_all = renderPlot({
    dat=alldata()
    end=nrow(dat)
    start=1#end-100
    
    if(nrow(dat)>=1){
      dat[start:end,]%>%ggplot(aes(x=timestamp,y=temperature))+
        geom_line(aes(color=factor(sensorID)))+ylim(26, 34)+
        geom_hline(yintercept = 33,linetype="dotted",color="darkblue")+
        labs(x="",y="Temperature",color="Sensor IDs")+
        theme(axis.title.x = element_blank(),
              axis.title.y = element_text(colour="blue",size=14),
              axis.text = element_text(colour="darkred",size=12),
              plot.title = element_blank())
    }
  })
  
  
  ids_too_high_reading=reactive({
    dat=alldata()
    temp=filter(dat,timestamp==max(dat$timestamp))
    ids=temp$sensorID[temp$temperature>34]
    ids
    
  }) 
  
  ids_failed_sensors=reactive({
    dat=alldata()
    temp=filter(dat,timestamp==max(dat$timestamp))
    ids=unique(temp$sensorID)
    previous_ids=new_ids$ids
    previous_ids[!(previous_ids %in% ids)]
    
  }) 
  
  new_ids=reactiveValues(ids=ids_at_start)
  
  output$Too_High <-  renderText({
    
    paste("Sensors with too high Readings: ",length(ids_too_high_reading()))
  })
  
  
  output$Failed_Sensors <-  renderText({
    
    paste("Number of Failed Sensors: ",length(ids_failed_sensors()))
  })
  
  
})
