### ------------------------------------------------------
## CARTOGRAPHIE
### ------------------------------------------------------

library("rgeos")
library ("rgdal")
library("cartography")
library("MTA")

listdep <- c("75","77","78","91","92","93","94","95")

# ***********************************
# IMPORT ET MISE EN FORME DES DONNES
# ***********************************

prj <- "+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

communes.spdf <- spTransform(x =  readOGR(dsn = "basemaps/idf.geojson", layer = "OGRGeoJSON"), CRSobj = prj)
communes.spdf@data <- communes.spdf@data[,c("INSEE_COM","NOM_COM")]
colnames(communes.spdf@data) <- c("id","name")
communes.spdf@data$id <- as.character(communes.spdf@data$id)

departements.spdf <- spTransform(x =  readOGR(dsn = "basemaps/departements.geojson", layer = "OGRGeoJSON"), CRSobj = prj)
departements.spdf@data <- departements.spdf@data[,c("CODE_DEPT","NOM_DEPT")]
colnames(departements.spdf@data) <- c("id","name")

results2012 <-read.csv( "data/results_comidf_2012.csv",header=TRUE,sep=",",dec=".",encoding="utf-8")
ids <- gsub(".html+$", "", results2012$link)
ids <- gsub("056AR", "1", ids)
ids <- substr(ids, 6, 100)
results2012$id <- ids
head(results2012)
results2012 <- results2012[c("id","name","nb_jlm2012","tx_jlm2012","abstention", "exprimés")]
head(results2012)

results2017 <-read.csv( "data/results_comidf_2017.csv",header=TRUE,sep=",",dec=".",encoding="utf-8")
ids <- gsub(".html+$", "", results2017$link)
ids <- gsub("056AR", "1", ids)
ids <- substr(ids, 6, 100)
results2017$id <- ids
results2017 <- results2017[c("id","name","nb_jlm2017","tx_jlm2017","abstention", "exprimés")]

moy <- 19.58

# *******
# CARTO *
# *******


results2017$nb <- results2017$nb_jlm2017*100
breaks <-  getBreaks(results2017$tx_jlm2017,6)
breaks <- c(0,2,5,10,15,18,19.58,22,24,29.5,47.09)
cols <-  carto.pal(pal1 = "green.pal",n1=6,pal2 = "orange.pal",n2 = 4)


sizes <- getFigDim(spdf = communes.spdf, width = 600, mar = c(0, 0, 1.2, 0))
sizes[1] <- sizes[1] /100
sizes[2] <- sizes[2] /100

pdf(file = "outputs/mainmap.pdf", width = sizes[1], height = sizes[2],pointsize=1)
opar <- par(mar = c(0, 0, 1.2, 0), mfrow = c(1, 1))
sizes[1] <- sizes[1] * 2


plot(communes.spdf, border = NA, col = NA, bg = "#dbd6ce")
plot(departements.spdf, col = "#dbd6ce", border = "white", lwd = 1,add=T)
smoothLayer(spdf = communes.spdf, df = results2017, 
            var = 'nb', var2 = 'exprimés',
            span = 4000, beta = 2, 
            breaks = breaks,
            col = cols,
            legend.title.txt = "vote JLM\n(potentiel 4km)",
            mask=communes.spdf,
            legend.pos = "bottomleft", legend.values.rnd = 0,add=T) 
plot(departements.spdf, col = NA, border = "white", lwd = 1,add=T)
head(departements.spdf@data)
layoutLayer(title = "Géographie du vote Mélenchon en 2017", author = "", 
            sources = "", frame = TRUE, 
            north = FALSE, scale = 20, col = "black")

# Fin de la planche
dev.off()



