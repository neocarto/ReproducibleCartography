library("rgdal")
library("rgeos")
library("cartography")

setwd("/home/nlambert/Documents/ReproducibleCartography/brexit")

# import
eu <- readOGR(dsn ="input",layer = "eu")

eu@data <- eu@data[1:2]
colnames(eu@data) <- c("id","name")

# barycentre function 
barycentre <- function(df, spdf,year, area){
  countries <- spdf[spdf@data$id %in% area,]    
  buff <- gBuffer(countries,byid=FALSE, width=1)
  center <- gCentroid(buff)
  centres.df <- rbind(centres.df, data.frame(id=year,x=center$x,y=center$y,nb=length(area)))
  file <- paste(year,".pdf",sep="")
   pdf(paste("output",file,sep="/"))
   layoutLayer(title = year, col = "black", coltitle = "white", bg = NULL,
               scale = 250, frame = TRUE, north = FALSE, south = FALSE,
               extent = eu)
 
   plot(buff,add=T)
   plot(center,add=T)
   dev.off()
  
  return(centres.df)
}



# data building

centres.df <- data.frame(id=character(),x=integer(),y=integer(),nb=integer())
area1957 <- c("R1","FR","BE","IT","LU","NL")
centres.df <- barycentre(centres.df, eu,1957,area1957)
area1973 <- c(area1957,"UK","IE","DK")
centres.df <- barycentre(centres.df, eu,1973,area1973)
area1981 <- c(area1973,"EL")
centres.df <- barycentre(centres.df, eu,1981,area1981)
area1986 <- c(area1981,"ES","PT")
area1990 <- c(area1986[-which(area1986=="R1")],"DE") 
centres.df <- barycentre(centres.df, eu,1986,area1986)
area1995 <- c(area1990,"AT","SE","FI")
centres.df <- barycentre(centres.df, eu,1990,area1990)
area2004 <- c(area1995,"CY", "MT", "EE", "HU", "LT", "LV", "PL", "CZ", "SK", "SI")
centres.df <- barycentre(centres.df, eu,2004,area2004)
area2007 <-c(area2004,"BG","RO")
centres.df <- barycentre(centres.df, eu,2007,area2007)
area2013 <-c(area2007,"HR")
centres.df <- barycentre(centres.df, eu,2013,area2013)
area2016 <-area2013[-which(area2013=="UK")]
centres.df <- barycentre(centres.df, eu,2016,area2016)


# Plot 
pdf("output/trajectoire.pdf")

# plot dots
layoutLayer(title = "titre", col = "black", coltitle = "white", bg = NULL,
            scale = 250, frame = TRUE, north = FALSE, south = FALSE,
            extent = eu[eu@data$id %in% c("FR","DE"),])
barycentres.spdf <- SpatialPointsDataFrame(coords = centres.df[,c("x","y")], data = centres.df,proj4string = CRS(proj4string(eu)))
plot(barycentres.spdf,add=T)
plot(eu, add=T)


# plot segments

for (i in 1:length(barycentres.spdf)){
  j <- i+1
  segments(centres.df$x[i],centres.df$y[i],centres.df$x[j],centres.df$y[j],col='red',lwd=3)
 }

dev.off()


