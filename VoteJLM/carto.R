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

# Quelques chiffres pour 2012
sum(results2012$nb_jlm2012)
max(results2012$tx_jlm2012)


# Quelques chiffres pour 2017
sum(results2017$nb_jlm2017)
max(results2017$tx_jlm2017)

# Quelques chiffres pour 2017

# **************************
# PLANCHE CARTOGRAPHIQUE 1 *
# **************************

sizes <- getFigDim(spdf = communes.spdf, width = 600, mar = c(0, 0, 1.2, 0))
sizes[1] <- sizes[1] * 2
sizes[2] <- sizes[2] * 2

# discretisation & colors
serie <- c(results2012$tx_jlm2012,results2017$tx_jlm2017)
hist(serie, probability = TRUE, nclass = 30)
rug(serie)
moy <- mean(serie)
med <- median(serie)
abline(v = moy, col = "red", lwd = 3)
abline(v = med, col = "blue", lwd = 3)
nb <- 8
breaks <- getBreaks(v = serie, nclass = 8, method = "quantile")
breaks2012 <- c(min(results2012$tx_jlm2012),breaks[2:8],max(results2012$tx_jlm2012))
breaks2017 <- c(min(results2017$tx_jlm2017),breaks[2:8],max(results2017$tx_jlm2017))
cols <- carto.pal(pal1 = "wine.pal" ,n1 = nb)

png(filename = "outputs/planche-carto-1.png", width = sizes[1], height = sizes[2], res = 150)
opar <- par(mar = c(0, 0, 1.2, 0), mfrow = c(2, 2))

# MAP 1



plot(communes.spdf, border = NA, col = NA, bg = "#dbd6ce")
plot(departements.spdf, col = "#dbd6ce", border = "white", lwd = 1,add=T)
plot(communes.spdf, col = "white", border = "#487096", lwd = 0.5, add = TRUE)
choroLayer(spdf = communes.spdf,
           df = results2012,
           var = "tx_jlm2012",
           breaks = breaks2012,
           nclass=nb,
           col = cols,
           border = "white",
           lwd=0.2,
           add = T,
           colNA = "white",
           legend.pos = "bottomleft",
           legend.title.txt = "Score en %\ndes suffrages\nexprimés",
           legend.values.rnd = 2)
plot(departements.spdf, col = NA, border = "white", lwd =1.5, add = TRUE)
layoutLayer(title = "Score de Jean-Luc Mélenchon en 2012", author = "", 
            sources = "", frame = TRUE, 
            north = FALSE, scale = NULL, col = "black")

# MAP 2

plot(communes.spdf, border = NA, col = NA, bg = "#dbd6ce")
plot(departements.spdf, col = "#DDDDDD", border = "white", lwd = 1,add=T)
plot(communes.spdf, col = "#dbd6ce", border = "#EEEEEE", lwd = 0.5, add = TRUE)
plot(departements.spdf, col = NA, border = "white", lwd =1.5, add = TRUE)
propSymbolsChoroLayer(spdf = communes.spdf,
           df = results2012,
           var = "exprimés",
           var2 = "tx_jlm2012",
           breaks = breaks2012,
           nclass=nb,
           col = cols,
           border = "#DDDDDD",
           lwd=0.4,
           add = T,
           fixmax = 120000,
           inches=0.1,
           legend.var2.pos = "bottomleft",
           legend.var2.title.txt = "Score en %\ndes suffrages\nexprimés",
           legend.var2.values.rnd = 2,
           legend.var.pos = "topleft",
           legend.var.style = "e",
           legend.var.title.txt = "Suffrages exprimés",
           legend.var.values.rnd = 0)
layoutLayer(title = "", author = "", 
            sources = "", frame = TRUE, 
            north = TRUE, scale = NULL, col = "black")

# MAP 3

plot(communes.spdf, border = NA, col = NA, bg = "#dbd6ce")
plot(departements.spdf, col = "#dbd6ce", border = "white", lwd = 1,add=T)
plot(communes.spdf, col = "white", border = "#487096", lwd = 0.5, add = TRUE)
choroLayer(spdf = communes.spdf,
           df = results2017,
           var = "tx_jlm2017",
           breaks = breaks2017,
           nclass=nb,
           col = cols,
           border = "white",
           lwd=0.2,
           add = T,
           colNA = "white",
           legend.pos = "bottomleft",
           legend.title.txt = "Score en %\ndes suffrages\nexprimés",
           legend.values.rnd = 2)
plot(departements.spdf, col = NA, border = "white", lwd =1.5, add = TRUE)
layoutLayer(title = "Score de Jean-Luc Mélenchon en 2017", author = "", 
            sources = "", frame = TRUE, 
            north = FALSE, scale = NULL, col = "black")

# MAP 4

plot(communes.spdf, border = NA, col = NA, bg = "#dbd6ce")
plot(departements.spdf, col = "#DDDDDD", border = "white", lwd = 1,add=T)
plot(communes.spdf, col = "#dbd6ce", border = "#EEEEEE", lwd = 0.5, add = TRUE)
plot(departements.spdf, col = NA, border = "white", lwd =1.5, add = TRUE)
propSymbolsChoroLayer(spdf = communes.spdf,
                      df = results2017,
                      var = "exprimés",
                      var2 = "tx_jlm2017",
                      breaks = breaks2017,
                      nclass=nb,
                      col = cols,
                      border = "#DDDDDD",
                      lwd=0.4,
                      add = T,
                      fixmax = 120000,
                      inches=0.1,
                      legend.var2.pos = "bottomleft",
                      legend.var2.title.txt = "Score en %\ndes suffrages\nexprimés",
                      legend.var2.values.rnd = 2,
                      legend.var.pos = "topleft",
                      legend.var.style = "e",
                      legend.var.title.txt = "Suffrages exprimés",
                      legend.var.values.rnd = 0)
layoutLayer(title = "", author = "", 
            sources = "", frame = TRUE, 
            north = FALSE, scale = 20, col = "black")

# Fin de la planche
dev.off()



# **************************
# PLANCHE CARTOGRAPHIQUE 2 *
# **************************
sizes <- getFigDim(spdf = communes.spdf, width = 600, mar = c(0, 0, 1.2, 0))
sizes[1] <- sizes[1] * 2
sizes[2] <- sizes[2] * 1

png(filename = "outputs/planche-carto-2.png", width = sizes[1], height = sizes[2], res = 150)
opar <- par(mar = c(0, 0, 1.2, 0), mfrow = c(1, 2))
sizes[1] <- sizes[1] * 2

results2012$nb <- results2012$nb_jlm2012*100
breaks <-  getBreaks(results2012$tx_jlm2012,6)
cols <-  carto.pal("wine.pal",n1=6)

plot(communes.spdf, border = NA, col = NA, bg = "#dbd6ce")
plot(departements.spdf, col = "#dbd6ce", border = "white", lwd = 1,add=T)
smoothLayer(spdf = communes.spdf, df = results2012, 
            var = 'nb', var2 = 'exprimés',
            span = 4000, beta = 2, 
            breaks = breaks,
            col = cols,
            legend.title.txt = "Vote JLM\n(potentiel 4km)",
            mask=communes.spdf,
            legend.pos = "bottomleft", legend.values.rnd = 0,add=T)
plot(departements.spdf, col = NA, border = "white", lwd = 1,add=T)
layoutLayer(title = "Géographie du vote Mélenchon en 2012", author = "", 
            sources = "", frame = TRUE, 
            north = FALSE, scale = 20, col = "black")
# CARTE 2017

results2017$nb <- results2017$nb_jlm2017*100
breaks <-  getBreaks(results2017$tx_jlm2017,6)
cols <-  carto.pal("wine.pal",n1=6)

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


# ********************
# CARTES D'EVOLUTION *
# ********************

sizes <- getFigDim(spdf = communes.spdf, width = 600, mar = c(0, 0, 1.2, 0))
sizes[1] <- sizes[1] * 2
sizes[2] <- sizes[2] * 1
png(filename = "outputs/evolution.png", width = sizes[1], height = sizes[2], res = 150)
opar <- par(mar = c(0, 0, 1.2, 0), mfrow = c(1, 2))


# Tableau de données

votes.df <- data.frame(results2012, results2017[match(results2012[,"id"], results2017[,"id"]),])
votes.df$diff <- votes.df$nb_jlm2017 - votes.df$nb_jlm2012
votes.df$evol <-  (votes.df$tx_jlm2017 / votes.df$tx_jlm2012)*100

# ABSOLU


plot(communes.spdf, border = NA, col = NA, bg = "#dbd6ce")
plot(departements.spdf, col = "#dbd6ce", border = "white", lwd = 1,add=T)
plot(departements.spdf[departements.spdf@data$id %in% listdep,], col = "#bcbaa7", border = "#897777", lwd = 1,add=T)
propSymbolsLayer(spdf = communes.spdf, df = votes.df,
                 var = "diff",
                 border = "#CCCCCC", lwd = 0.1,
                 col = "#d14a4a", col2 = "#6e8cb7", breakval=0,
                 legend.pos = "bottomleft",
                 legend.title.txt = "Evolution\nabsolue",
                 inches=0.1,
                 legend.style = "e",add=T)
layoutLayer(title = "Evolution du nombre de voix pour Mélenchon 2012-2017", author = "", 
            sources = "", frame = TRUE, 
            north = FALSE, scale = 20, col = "black")
# RELATIF
v <- votes.df$evol
v[v==Inf] <- NA
v <- na.omit(v)
bks <- getBreaks(v =v,nclass = 6,method = "quantile")
bks <- c(39.88,50,100,150, 175,200, 250, 1303)
cols <- carto.pal(pal1 = "blue.pal",n1=2, pal2 = "red.pal",n2=5)
plot(communes.spdf, border = NA, col = NA, bg = "#dbd6ce")
plot(departements.spdf, col = "#dbd6ce", border = "white", lwd = 1,add=T)
choroLayer(spdf = communes.spdf,
           df = votes.df,
           var = "evol",
           breaks =bks,
           nclass=6,
           col = cols,
           border = "white",
           lwd=0.2,
           colNA = "white",
           legend.pos = "bottomleft",
           legend.title.txt = "Evolution\ndes scores\n2012-2017 (%)",
           legend.values.rnd = 2,add=T)
plot(departements.spdf[departements.spdf@data$id %in% listdep,], col = NA, border = "white", lwd = 1,add=T)
layoutLayer(title = "Evolution du vote Mélenchon 2012-2017 (%)", author = "", 
            sources = "", frame = TRUE, 
            north = FALSE, scale = 20, col = "black")
# Fin de la planche
dev.off()

# ************
# REGRESSION *
# ************



# **************
# LES BASTIONS *
# **************

sizes <- getFigDim(spdf = communes.spdf, width = 600, mar = c(0, 0, 1.2, 0))
sizes[1] <- sizes[1] * 2
sizes[2] <- sizes[2] * 2

png(filename = "outputs/planche-multiscalaire.png", width = sizes[1], height = sizes[2], res = 150)
opar <- par(mar = c(0, 0, 1.2, 0), mfrow = c(2, 2))

results2017$dpt <- substr(results2017$id,1,2)

cols <- carto.pal(pal1 = "blue.pal", n1 = 3,pal2 = "wine.pal", n2 = 3)

# DEVIATION REGIONALE
results2017$regionaldev <- gdev(results2017, "nb_jlm2017", "exprimés", type = "rel", ref = NULL)
bks <- c(min(results2017$regionaldev),50,75,100,125,150,max(results2017$regionaldev))
plot(communes.spdf, border = NA, col = NA, bg = "#dbd6ce")
plot(departements.spdf, col = "#dbd6ce", border = "white", lwd = 1,add=T)
plot(communes.spdf, col = "white", border = "#487096", lwd = 0.5, add = TRUE)
choroLayer(spdf = communes.spdf, df = results2017, var = "regionaldev",
           legend.pos = "bottomleft",
           legend.title.txt = "Ecart à la\nmoyenne\nrégionale",
           breaks = bks, border = "white",lwd=0.2,
           col = cols, add=T)
plot(departements.spdf[departements.spdf@data$id %in% listdep,], col = NA, border = "white", lwd = 1,add=T)
layoutLayer(title = "Le score de JLM en 2017 (comparaison régionale)", author = "", 
            sources = "", frame = TRUE, 
            north = FALSE, scale = NULL, col = "black")

# DEVIATION DEPARTEMENTALE
results2017$departementaldev <- tdev(results2017, "nb_jlm2017", "exprimés", type = "rel", key="dpt")
bks <- c(min(results2017$departementaldev),50,75,100,125,150,max(results2017$departementaldev))
plot(communes.spdf, border = NA, col = NA, bg = "#dbd6ce")
plot(departements.spdf, col = "#dbd6ce", border = "white", lwd = 1,add=T)
plot(communes.spdf, col = "white", border = "#487096", lwd = 0.5, add = TRUE)
choroLayer(spdf = communes.spdf, df = results2017, var = "departementaldev",
           legend.pos = "bottomleft",
           legend.title.txt = "Ecart à la\nmoyenne\ndépartementale",
           breaks = bks, border = "white",lwd=0.2,
           col = cols,add=T)
plot(departements.spdf[departements.spdf@data$id %in% listdep,], col = NA, border = "white", lwd = 1,add=T)
layoutLayer(title = "Le score de JLM en 2017 (comparaison départementale)", author = "", 
            sources = "", frame = TRUE, 
            north = FALSE, scale = NULL, col = "black")

# DEVIATION LOCALE
results2017$localdev <- sdev(results2017, "nb_jlm2017", "exprimés", type = "rel", communes.spdf, spdfid = "id", xid = "id", order = 1)
bks <- c(min(results2017$localdev),50,75,100,125,150,max(results2017$localdev))
plot(communes.spdf, border = NA, col = NA, bg = "#dbd6ce")
plot(departements.spdf, col = "#dbd6ce", border = "white", lwd = 1,add=T)
plot(communes.spdf, col = "white", border = "#487096", lwd = 0.5, add = TRUE)
choroLayer(spdf = communes.spdf, df = results2017, var = "localdev",
           legend.pos = "bottomleft",
           legend.title.txt = "Ecart à la\nmoyenne\nlocale",
           breaks = bks, border = "white",lwd=0.2,
           col = cols,add=T)
plot(departements.spdf[departements.spdf@data$id %in% listdep,], col = NA, border = "white", lwd = 1,add=T)
layoutLayer(title = "Le score de JLM en 2017 (comparaison locale)", author = "", 
            sources = "", frame = TRUE, 
            north = FALSE, scale = NULL, col = "black")

# LES PLACES FORTES
synthesis <- mst(spdf = communes.spdf,
                 x = results2017,
                 spdfid = "id",
                 xid = "id",
                 var1 = "nb_jlm2017", 
                 var2 = "exprimés",
                 dist = NULL,
                 key = "dpt",
                 order = 1,
                 mat = NULL,
                 threshold = 125,
                 superior = TRUE)
cols <- c("#f0f0f0", "#fdc785","#ffffab","#fba9b0","#addea6","#ffa100","#fff226","#e30020")
rVal<-c(" .     .   . ",
        "[X]   .   . ",
        " .   [X]  . ",
        "[X] [X]  . ",
        " .    .   [X]",
        "[X]  .   [X]",
        " .   [X] [X]",
        "[X] [X] [X]")

plot(communes.spdf, border = NA, col = NA, bg = "#dbd6ce")
plot(departements.spdf, col = "#dbd6ce", border = "white", lwd = 1,add=T)
plot(communes.spdf, col = "white", border = "#487096", lwd = 0.5, add = TRUE)
typoLayer(spdf = communes.spdf, df = synthesis, var = "mst",
          border = "#D9D9D9",legend.values.order = 0:7, 
          col = cols,
          lwd = 0.25,
          legend.pos = "n",
          add=T)
plot(departements.spdf[departements.spdf@data$id %in% listdep,], col = NA, border = "white", lwd = 1,add=T)
layoutLayer(title = "Synthèse multicalaire du vote Mélenchon en 2017", author = "", 
            sources = "", frame = TRUE, 
            north = FALSE, scale = NULL, col = "black")
legendTypo(col = cols, categ = rVal,
           title.txt = "Typologie\nmultiscalaire\n(>125)",
           nodata = FALSE, pos = "bottomleft")
# Fin de la planche
dev.off()



