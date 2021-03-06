### ------------------------------------------------------
### WEBSCRAPPING DONNEES ELECTORALES MINISTERE INTERIEUR (2017)
### RESULTATS PAR COMMUNES EN IDF
### ------------------------------------------------------

library(xml2)
library(rvest)

# Liens des pages à lire

site <- "http://elections.interieur.gouv.fr/presidentielle-2017/011"
departements <- c("075","077","078","091","092","093","094","095")

# PARIS
  dep <- departements[1]
  page <- paste(dep,"html",sep=".")
  url <- paste (site,dep,page,sep="/")
  webpage <- read_html(x = url)
  l <- webpage %>% html_nodes(".offset2") %>% html_nodes("a")%>%  html_attr("href")
  l <- gsub("../../011/075/", "", l)
  l <- paste(dep,l,sep="/")
  links <- l
  links
  
# AUTRES DEPARTEMENTS
for(i in 2:length(departements)){
  dep <- departements[i]
  page <- paste(dep,"html",sep=".")
  url <- paste (site,dep,page,sep="/")
  webpage <- read_html(x = url)
  l <- webpage %>% html_nodes(".offset2") %>% html_nodes("a")%>%  html_attr("href")
  #l <- l[3:(length(l)-1)]
  tmp <- paste("../../01/",dep,sep="")
  l <- gsub(paste("../../011/",dep,"/",sep=""), "", l)
  l <- paste(dep,l,sep="/")
  if (i ==2){l2 <- l} else {l2 <- c(l2,l)}
}

for (i in 1:length(l2)) {
  url <- paste(site,l2[i],sep="/")
  w <- read_html(x = url)
  l <- w %>% html_nodes(".offset2") %>% html_nodes("a")%>%  html_attr("href")
  #l <- l[4:(length(l)-1)]
  l <- gsub("../../011/", "", l)
  start <- length(grep("[A-Z]", l))+1
  stop <-   length(l)
  l <- l[start:stop]
  #l <- strsplit(l2[i],"/")[[1]][1]
  links <- c(links,l)
  } 

  

  ################################################"

# Création d'un premier dataframe avec les resultats
df<-data.frame()
for(i in 1:length(links)){
df[i,"link"] <- links[i]
url <- paste (site,links[i],sep="/")
webpage <- read_html(x = url)
name <- webpage %>% html_nodes(".row-fluid .pub-fil-ariane")%>% html_nodes("a")%>% html_text() 
df[i,"name"] <- name[4]
results <- webpage %>% html_nodes("table") %>%html_table(header=T)
for (j in 1:11){
if (results[[2]][j,1] == "M. Jean-Luc MÉLENCHON"){index <- j}
}
df[i,"nb_jlm2017"] <- as.numeric(gsub("\\D", "", results[[2]][index,2]))
df[i,"tx_jlm2017"] <- as.numeric(gsub("\\D", ".", results[[2]][index,4]))
df[i,"abstention"] <- as.numeric(gsub("\\D", "", results[[3]][2,2]))
df[i,"exprimés"] <- as.numeric(gsub("\\D", "", results[[3]][6,2]))
}
# Export du fichier
write.csv(df,file = "data/results_comidf_2017.csv")

