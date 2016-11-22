library("cartography")
library("rgdal")


setwd("")

# Fond de carte
data("nuts2006")

# Données

data.df <- read.csv( "input/eurostat/hsw_mi01_1_Data.csv",header=TRUE,sep=",",dec=".",encoding="latin1",comment.char = "",quote = "\"")
fatal.df <- fatal.df[data.df$TIME == 2014,]
fatal.df <-fatal.df[,c("GEO","Value")]
colnames(fatal.df) <- c("id","fatal")
fatal.df$fatal <- as.numeric(as.character(fatal.df$fatal))
fatal.eu.df <- fatal.df[1:3,]
fatal.df <- fatal.df[4:dim(fatal.df)[1],]

# Cartography
pdf("output/Fatal.pdf")
plot(countries.spdf,col="#CCCCCC")
plot(nuts0.spdf[!nuts0.spdf@data$id %in% c("TR","CH","IS","LI","NO"),], col="#AAAAAA",add=T)
plot(nuts0.spdf[nuts0.spdf@data$id %in% c("IS","FI"),], col="white",add=T)
propSymbolsLayer(spdf = countries.spdf, df = fatal.df,
                 var = "fatal", 
                 symbols = "square", col =  "#920000",
                 legend.pos = "right",
                 legend.title.txt = "Nb de morts au travail (2014)",
                 legend.style = "c",inches = 0.6)

dev.off()

# Graphiques

FR.df <- data.df[data.df$GEO == "FR",c("GEO","TIME","Value")]
DE.df <- data.df[data.df$GEO == "DE",c("GEO","TIME","Value")]
IT.df <- data.df[data.df$GEO == "IT",c("GEO","TIME","Value")]
ES.df <- data.df[data.df$GEO == "ES",c("GEO","TIME","Value")]

pdf("output/FatalGraph.pdf")
plot(FR.df$TIME,as.numeric(as.character(FR.df$Value)),pch=15, xlim=c(2008,2014),ylim=c(200,800),xlab="Années", ylab="Nombre d'accidents mortels", type="o",col="blue")
par(new=T)
plot(DE.df$TIME,as.numeric(as.character(DE.df$Value)),,pch=15, xlim=c(2008,2014),ylim=c(200,800),xlab="Années", ylab="Nombre d'accidents mortels", type="o",col="red")
par(new=T)
plot(IT.df$TIME,as.numeric(as.character(IT.df$Value)),,pch=15, xlim=c(2008,2014),ylim=c(200,800),xlab="Années", ylab="Nombre d'accidents mortels", type="o",col="green")
par(new=T)
plot(ES.df$TIME,as.numeric(as.character(ES.df$Value)),,pch=15, xlim=c(2008,2014),ylim=c(200,800),xlab="Années", ylab="Nombre d'accidents mortels", type="o",col="purple")
dev.off()
