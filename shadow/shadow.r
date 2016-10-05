# COMMENT CREER UNE OMBRE PORTEE ?

# Chargement des packages utiles

library("cartography")
library("rgeos")

# choix d'un fond de carte (disponible dans le package cartography)

data("nuts2006")
spdf <- nuts0.spdf

# Agrégation des géométries 

buff.spdf <- gBuffer(spgeom=spdf, byid=FALSE, id=NULL, width=1.0, quadsegs=5, capStyle="ROUND", joinStyle="ROUND", mitreLimit=1.0)
shadow.spdf <- buff.spdf

# Création d'une fonction de translation

translate <- function(vertices, dx, dy){
  p <- vertices
  p2 <- p
  p2[,1] <- p[,1] + dx 
  p2[,2] <- p[,2] + dy
  p2
}

# On applique la fonction à chaque polygone de l'objet "buff" en définissant les parametres dx et dy

dx = 20000 ; dy = -30000
nb <- length(shadow.spdf@polygons[[1]]@Polygons)
for (i in 1:nb){
  shadow.spdf@polygons[[1]]@Polygons[[i]]@coords <- translate(shadow.spdf@polygons[[1]]@Polygons[[i]]@coords, dx, dy)
}

# On dessine la carte (carte basé sur un exemple du package cartography)

opar <- par(mar = c(0,0,1.2,0), mfrow = c(1,1))
size <- getFigDim(spdf, width = 1500, mar = par("mar"))

png(filename = "/home/nlambert/Documents/R/ReproducibleCartography/shadow/map.png",width = size[1], height = size[2],res=150)

data("nuts2006")


plot(nuts2.spdf, border=NA)
plot(world.spdf,col="#DBBFB650", border=NA,add=T)
plot(shadow.spdf,col="#2D3F4580",border="NA",add=T)

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
           legend.title.txt = "Unemployment raten(%)",
           legend.values.rnd = 1,
           lwd=0.3,
           )
plot(spdf,col=NA, border="#FFFFFF80",lwd=0.8,add=T)
plot(buff.spdf,col=NA, border="#2D3F45",lwd=1,add=T)

layoutLayer(title = "How to draw a shadow on a map with R?", sources = "https://neocarto.hypotheses.org",
            author = "Eurostat, 2008", theme = "grey.pal")

dev.off()

