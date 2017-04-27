onzieme <- c("77296","77326","77384","77285","77039","77495","77251","77038","77445","77067","77447")

# IMPORT ET MISE EN FORME

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

votes.df <- data.frame(results2012, results2017[match(results2012[,"id"], results2017[,"id"]),])

votes.df <- votes.df[votes.df$id %in% onzieme,]

write.csv(votes.df,file = "data/11e.csv")
