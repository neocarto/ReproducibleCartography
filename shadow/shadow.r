# COMMENT CREER UNE OMBRE PORTEE ?
getwd()
# Chargement des packages utiles

library("cartography")
library("rgeos")

# choix d'un fond de carte (disponible dans le package cartography)

data("nuts2006")
spdf <- nuts0.spdf

# Agrégation des géométries 

buff.spdf <- gBuffer(spgeom=spdf, byid=FALSE, id=NULL, width=1.0, quadsegs=5, capStyle="ROUND", joinStyle="ROUND", mitreLimit=1.0)


# Translation
x = 20000 ; y = -30000
shadow.spdf <- raster::shift(buff.spdf, x, y)

# On dessine la carte (carte basé sur un exemple du package cartography)

opar <- par(mar = c(0,0,1.2,0), mfrow = c(1,1))
size <- getFigDim(spdf, width = 1500, mar = par("mar"))
png(filename = "map.png",width = size[1], height = size[2],res=150)
opar <- par(mar = c(0,0,1.2,0), mfrow = c(1,1))

data("nuts2006")

plot(nuts2.spdf, border=NA)
plot(world.spdf,col="#DBBFB650", border=NA,add=T)
plot(shadow.spdf,col="#2D3F4580",border="NA",add=T) # ici affichage de l'ombre

nuts2.df$unemprate <- nuts2.df$unemp2008/nuts2.df$act2008*100
choroLayer(spdf = nuts2.spdf,
           df = nuts2.df,
           var = "unemprate",
           method = "quantile",
           nclass = 8,
           col = carto.pal(pal1 = "orange.pal", n1 = 8),
           border = "#FFFFFF50",
           add = TRUE,
           legend.pos = "topright",
           legend.title.txt = "Unemployment rate\nin 2008 (%)",
           legend.values.rnd = 1,
           lwd=0.3,
           )
plot(spdf,col=NA, border="#FFFFFF80",lwd=0.8,add=T)
plot(buff.spdf,col=NA, border="#2D3F45",lwd=1,add=T)

layoutLayer(title = "How to draw a shadow on a map with R?", sources = "https://neocarto.hypotheses.org",
            author = "Eurostat, 2008", theme = "grey.pal")

dev.off()

