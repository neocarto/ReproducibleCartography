# ********************************
# VOTE PCF
# ********************************

library("rgdal")
library("cartography")

# Import

setwd("/home/nlambert/Documents/R/ReproducibleCartography/votePCF")

dpts.spdf <-readOGR(dsn ="input",layer = "DEPARTEMENT")
pcf.df <-read.csv( "input/pcf.csv",header=TRUE,sep="\t",dec=".",encoding="utf-8")

head(dpts.spdf@data)

# *************************
# Vote Mélenchon en tx

pdf("output/jlm_tx.pdf")
opar <- par(mar = c(0,0,1.2,0), mfrow = c(1,2))

# 2011
data.df <- pcf.df[,c("id","name","Exprimés_2011","Melenchon_2011")]
data.df$pct_jlm2011 <- (data.df$Melenchon_2011/data.df$Exprimés_2011)*100
head(data.df)

Bks <- c(min(data.df$pct_jlm2011 ),20,30,40,50,60,70,80,max(data.df$pct_jlm2011 ))

choroLayer(spdf = dpts.spdf,
           df = data.df,
           dfid = "id", spdfid = "CODE_DEPT", 
           var = "pct_jlm2011",
           col = carto.pal(pal1 = "blue.pal", n1 = 4, pal2 =,"red.pal", n2 = 4),
           border = "grey40",
           breaks = Bks,
           add = FALSE,
           legend.pos = "topright",
           legend.title.txt = "vote Mélenchon, 2011",
           legend.values.rnd = 1)

# 2016

data.df <- pcf.df[,c("id","name","Exprimés_2016","Option1_2016")]
data.df$pct_jlm2016 <- (data.df$Option1_2016/data.df$Exprimés_2016)*100
head(data.df)

Bks <- c(min(data.df$pct_jlm2016 ),20,30,40,50,60,70,80,max(data.df$pct_jlm2016 ))

choroLayer(spdf = dpts.spdf,
           df = data.df,
           dfid = "id", spdfid = "CODE_DEPT", 
           var = "pct_jlm2016",
           col = carto.pal(pal1 = "blue.pal", n1 = 4, pal2 =,"red.pal", n2 = 4),
           border = "grey40",
           breaks = Bks,
           add = FALSE,
           legend.pos = "topright",
           legend.title.txt = "Option 1\n(vote Mélenchon), 2016",
           legend.values.rnd = 1)


dev.off()

# *************************
# Vote Mélenchon en tx + abs

pdf("output/jlm_txabs.pdf")
opar <- par(mar = c(0,0,1.2,0), mfrow = c(1,2))

# 2011
data.df <- pcf.df[,c("id","name","Exprimés_2011","Melenchon_2011")]
data.df$pct_jlm2011 <- (data.df$Melenchon_2011/data.df$Exprimés_2011)*100
head(data.df)


Bks <- c(min(data.df$pct_jlm2011 ),20,30,40,50,60,70,80,max(data.df$pct_jlm2011 ))

plot(dpts.spdf,col="#CCCCCC")
propSymbolsChoroLayer(spdf = dpts.spdf,
                     df = data.df,
                     dfid = "id", spdfid = "CODE_DEPT", 
                     var = "Exprimés_2011", var2 = "pct_jlm2011", 
                     inches = 0.5,symbols="square",
                     col = carto.pal(pal1 = "blue.pal", n1 = 3, pal2 =,"red.pal", n2 = 5),
                     breaks= Bks,fixmax=3144,
                     legend.var.pos = "topright", legend.var2.pos = "right",
                     legend.var2.title.txt = "Vote Mélenchon, 2011",
                     legend.var.title.txt = "Nb de vote exprimés",
                     legend.var.style = "e",add=T)

# 2016

data.df <- pcf.df[,c("id","name","Exprimés_2016","Option1_2016")]
data.df$pct_jlm2016 <- (data.df$Option1_2016/data.df$Exprimés_2016)*100
head(data.df)

Bks <- c(min(data.df$pct_jlm2016),20,30,40,50,60,70,80,max(data.df$pct_jlm2016 ))


plot(dpts.spdf,col="#CCCCCC")
propSymbolsChoroLayer(spdf = dpts.spdf,
                      df = data.df,
                      dfid = "id", spdfid = "CODE_DEPT", 
                      var = "Exprimés_2016", var2 = "pct_jlm2016", 
                      inches = 0.5,symbols="square",
                      col = carto.pal(pal1 = "blue.pal", n1 = 3, pal2 =,"red.pal", n2 = 5),
                      breaks= Bks,fixmax=3144,
                      legend.var.pos = "topright", legend.var2.pos = "right",
                      legend.var2.title.txt = "Vote Mélenchon, 2016",
                      legend.var.title.txt = "Nb de vote exprimés",
                      legend.var.style = "e",add=T)


dev.off()



# *************************
# Evolution en points

pdf("output/jlm_evol_pts.pdf")

opar <- par(mar = c(0,0,1.2,0), mfrow = c(1,1))

data.df <- pcf.df[,c("id","name","Exprimés_2011","Melenchon_2011","Exprimés_2016","Option1_2016")]
data.df$tx2011 <- (data.df$Melenchon_2011/data.df$Exprimés_2011)*100
data.df$tx2016 <- (data.df$Option1_2016/data.df$Exprimés_2016)*100
data.df$pts <- data.df$tx2016 -  data.df$tx2011

View(data.df)

# summary(data.df$pts)
# var <- data.df$pts
# hist(var, probability = TRUE, nclass = 30)
# rug(var)
# moy <- mean(var)
# med <- median(var)
# abline(v = moy, col = "red", lwd = 3)
# abline(v = med, col = "blue", lwd = 3)

Bks <- c(min(data.df$pts),-50,-40,-30,-20,-10,0,10,20,30,max(data.df$pts))

choroLayer(spdf = dpts.spdf,
           df = data.df,
           dfid = "id", spdfid = "CODE_DEPT", 
           var = "pts",
           col = carto.pal(pal1 = "blue.pal", n1 = 6, pal2 =,"red.pal", n2 = 4),
           border = "grey40",
           breaks = Bks,
           add = FALSE,
           legend.pos = "topright",
           legend.title.txt = "Evolution (en points de %), 2016",
           legend.values.rnd = 1)
dev.off()


# *************************
# Regression

data.df <- pcf.df[,c("id","name","Exprimés_2011","Melenchon_2011","Exprimés_2016","Option1_2016")]

# Variable quantitative explicative (X) = 2011
nomX <- "2011"
data.df$X <- (data.df$Melenchon_2011/data.df$Exprimés_2011)*100

# Variable quantitative à expliquer (Y) = 2016
nomY <- "2016"
data.df$Y<- (data.df$Option1_2016/data.df$Exprimés_2016)*100

# Tableau minimal
data.df <- data.df[,c("id","name","Y","X")]

# Creation de variables X et Y standardisées par moyenne et écart-typ
data.df$Ystand<-(data.df$Y-mean(data.df$Y))/sd(data.df$Y)
data.df$Xstand<-(data.df$X-mean(data.df$X))/sd(data.df$X)
data.df$Ystand<-round(data.df$Ystand,2)
data.df$Xstand<-round(data.df$Xstand,2)


plot.new()
par(mfrow=c(2,2))

hist(data.df$X,main=nomX,breaks=10)
hist(data.df$Y,main=nomY,breaks=10)
boxplot(data.df$X,main=nomX, horizontal=TRUE)
boxplot(data.df$Y,main=nomY, horizontal=TRUE)

plot(data.df$X,data.df$Y,
     main="Relation entre X et Y",
     xlab=nomX, # titre horizontal
     ylab=nomY, # titre vertical
     type="p",  # type de point
     pch=20,    # taille des points
     cex=0.7)   # coefficient multiplicateur de taille

text(data.df$X,data.df$Y,
     labels=data.df$name,    # nom à ajouter sur le graphique
     adj=c(0.5,-1), # position par rapport au point
     cex=0.7)       # coefficient multiplicateur de taille

cor(data.df$X,data.df$Y)
cor.test(data.df$X,data.df$Y) # une relatio est signification si p-value < 0.05 (5% d'erreur)

MonModele <- lm(data.df$Y~data.df$X)
summary(MonModele)
names(MonModele)

data.df$Yest<-MonModele$fitted.values
data.df$Yres<-MonModele$residuals
data.df$Yres_std<-data.df$Yres/(sd(data.df$Yres))
head(data.df)


par(mfrow=c(1,2))

# 8.1 Graphique de régression
plot(data.df$X,data.df$Y,
     main="Régression",
     xlab=nomX, # titre horizontal
     ylab=nomY, # titre vertical
     type="p",  # type de point
     pch=20,    # taille des points
     cex=0.7)   # coefficient multiplicateur de taille
#text(X,Y,
#     labels=nom,    # nom à ajouter sur le graphique
#     adj=c(0.5,-1), # position par rapport au point
#     cex=0.4)       # coefficient multiplicateur de taille

abline(MonModele,
       col="red")

summary()

Bks<-c(min(data.df$Yres_std),-2,-1,-0.5,0,0.5,1,2,max(data.df$Yres_std))
head(data.df)
choroLayer(spdf = dpts.spdf,
           df = data.df,
           dfid = "id", spdfid = "CODE_DEPT", 
           var = "Yres_std",
           col = carto.pal(pal1 = "blue.pal", n1 = 4, pal2 =,"red.pal", n2 = 4),
           border = "grey40",
           breaks = Bks,
           add = FALSE,
           legend.pos = "topright",
           legend.title.txt = "Carte des résidus",
           legend.values.rnd = 1)


