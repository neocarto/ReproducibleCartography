library("cartography")
library("rgeos")
library("reshape2")

data("nuts2006")

# input
spdf <- nuts0.spdf
spdfid <- "id"
threshold <- 0
sizes <- c(10000,20000,30000,40000) #epaisseur des disc en unités de la carte
df <- nuts0.df
var <- "unemp2008"
dfid <- "id"


# Get units borders
contig.spdf <- getBorders(spdf,spdfid = spdfid)

# CAlcul de la discontinuité (val1 / val2)
df <- df[,c(dfid,var)]
tmp <- data.frame(contig.spdf@data, df[match(contig.spdf@data[,"id1"], df[,dfid]),])[5]
contig.spdf@data$val1 <- as.numeric(tmp[,1])
tmp <- data.frame(contig.spdf@data, df[match(contig.spdf@data[,"id2"], df[,dfid]),])[6]
contig.spdf@data$val2 <- as.numeric(tmp[,1])
contig.spdf@data$disc <- contig.spdf@data$val1/contig.spdf@data$val2

# Détermination du sens de la discontinuité
contig.spdf@data$test <- 0
for (i in 1:length(contig.spdf)){
value <- contig.spdf@data[i,"disc"]
  code <- row.names(contig.spdf@data[i,])
  code2 <- paste(unlist(strsplit(code,"_"))[2],unlist(strsplit(code,"_"))[1],sep="_")
  value1 <- as.numeric(contig.spdf@data[contig.spdf@data$id==code,"disc"])
  value2 <- as.numeric(contig.spdf@data[contig.spdf@data$id==code2,"disc"])   
  if(value1 >= value2){contig.spdf@data$test[i] <- 1}
  }
contig.spdf <- contig.spdf[contig.spdf$test==1,]

max(contig.spdf@data$disc)

# Selection des discontinuités les plus fortes
contig.spdf <- contig.spdf[contig.spdf@data$disc >= threshold,]

# Determination des épaisseurs
breaks <- getBreaks(v = contig.spdf@data$disc, nclass = 4, method = "quantile")
contig.spdf@data$buff <- sizes[findInterval(contig.spdf@data$disc,breaks,all.inside=TRUE)]
contig.spdf@data$buff <- contig.spdf@data$buff*2

# Buffer (todo)
row.names(spdf) <- spdf@data[,spdfid]
contig.buffer.spdf <- rgeos::gBuffer(spgeom = contig.spdf,width = contig.spdf@data$buff,byid = TRUE)
bands <- gIntersection(spdf,contig.buffer.spdf,byid = TRUE)
ids <- sapply(slot(bands, "polygons"), function(x) slot(x, "ID")) 
df <- colsplit(ids, " ", c("idpoly", "idcontig"))
df2 <- colsplit(df$idcontig, "_", c("idcontig","idcontig2"))[1]
df <- cbind(df[1],df2)
row.names(df) <- ids
bands <- SpatialPolygonsDataFrame(bands, df)
bands <- bands[bands@data$idpoly==bands@data$idcontig,]
plot(bands,col="red",border=NA)
plot(contig.spdf,add=T)


