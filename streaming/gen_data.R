longitude=rnorm(10, mean = 11.4, sd = 0.3)
latitude=rnorm(10, mean = 48.2, sd = 0.1)

sensorID=sample(7777:9999, 10)


i=0
while(TRUE){
  dat=data.frame(timestamp=as.numeric(Sys.time()),sensorID=sensorID,longitude=longitude,latitude=latitude,
                 temperature=rnorm(n=10,mean=30,sd=1))
  write.csv(dat,paste0("sensorData",gsub("[^0-9]","",Sys.time()),".csv"),
            row.names = FALSE)
  Sys.sleep(2)
}

