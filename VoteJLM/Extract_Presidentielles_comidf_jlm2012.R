### ------------------------------------------------------
### WEBSCRAPPING DONNEES ELECTORALES MINISTERE INTERIEUR (2012)
### RESULTATS PAR COMMUNES EN IDF
### ------------------------------------------------------

library(xml2)
library(rvest)

# Liens des pages à lire

site <- "http://www.interieur.gouv.fr/Elections/Les-resultats/Presidentielles/elecresult__PR2012/(path)/PR2012/011"
departements <- c("075","077","078","091","092","093","094","095")

# PARIS
  dep <- departements[1]
  page <- paste(dep,"html",sep=".")
  url <- paste (site,dep,page,sep="/")
  webpage <- read_html(x = url)
  l <- webpage %>% html_nodes("#content-wrap") %>% html_nodes("a")%>%  html_attr("href")
  l <- l[4:(length(l)-1)]
  l <- paste(dep,l,sep="/")
  links <- l

  
# AUTRES DEPARTEMENTS
for(i in 2:length(departements)){
  dep <- departements[i]
  page <- paste(dep,"html",sep=".")
  url <- paste (site,dep,page,sep="/")
  webpage <- read_html(x = url)
  l <- webpage %>% html_nodes("#content-wrap") %>% html_nodes("a")%>%  html_attr("href")
  l <- l[3:(length(l)-1)]
  l <- paste(dep,l,sep="/")
  if (i ==2){l2 <- l} else {l2 <- c(l2,l)}
}

for (i in 1:length(l2)) {
  url <- paste(site,l2[i],sep="/")
  w <- read_html(x = url)
  l <- w %>% html_nodes("#content-wrap") %>% html_nodes("a")%>%  html_attr("href")
  l <- l[4:(length(l)-1)]
  start <- length(grep("[A-Z]", l))+1
  stop <-   length(l)
  l <- l[start:stop]
  toto <- strsplit(l2[i],"/")[[1]][1]
  l <- paste(toto,l,sep="/")
  links <- c(links,l)
  } 

  
################################################"

# Création d'un premier dataframe avec les resultats
df<-data.frame()
for(i in 1:length(links)){
df[i,"link"] <- links[i]
url <- paste (site,links[i],sep="/")
webpage <- read_html(x = url)
name <- webpage %>% html_nodes("#content-wrap") %>% html_node("strong")%>%html_text()
name <- strsplit(as.character(name),split = ">")[[1]][4]
df[i,"name"] <- gsub("^.","",name)
results <- webpage %>% html_nodes("table") %>%html_table(header=T)
df[i,"nb_jlm2012"] <- as.numeric(gsub("\\D", "", results[[4]][4,2]))
df[i,"tx_jlm2012"] <- as.numeric(gsub("\\D", ".", results[[4]][4,3]))
df[i,"abstention"] <- as.numeric(gsub("\\D", "", results[[3]][2,2]))
df[i,"exprimés"] <- as.numeric(gsub("\\D", "", results[[3]][5,2]))
}

# Export du fichier
write.csv(df,file = "data/results_comidf_2012.csv")

