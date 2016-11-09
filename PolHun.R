setwd('c:/gergo/fp7')

adat1 <- read.csv('cordis-fp7projects.csv', header=T, sep=";", dec=",")#[,c(1:5, 17:20)]

# van a projektben lengyel vagy magyar partner
adat1 <- adat1[unique(c(which(adat1$coordinatorCountry %in% c("PL", "HU")), grep(c("PL|HU"), adat1$participantCountries))),]

# EÜ kutprogrammból való projekt
adat1 <- adat1[adat1$programme == "FP7-HEALTH",]

adat1$participantCountries <- as.character(adat1$participantCountries)
adat1$coordinatorCountry <- as.character(adat1$coordinatorCountry)

adat2 <- read.csv('cordis-h2020projects.csv', header=T, sep=";", dec=",")

# van a projektben legyel / magyar partner
adat2 <- adat2[unique(c(which(adat2$coordinatorCountry %in% c("PL", "HU")), grep("PL|HU", adat2$participantCountries))),]

# egészségügyi célú kiírás
adat2 <- adat2[grep(c("H2020-EU.3.1."), adat2$programme),]

# ez az elsõdleges elemzési adatbázis
adat <- rbind(adat1, adat2)
rm(list = c('adat1','adat2'))

proj_countries <- lapply(strsplit(paste0(adat$coordinatorCountry,";",adat$participantCountries), ";"), unique)

collect_edges <- NULL

for(i in 1:length(proj_countries))
{
  collect_edges <- rbind(collect_edges, expand.grid(proj_countries[i][[1]], proj_countries[i][[1]]))
}

collect_edges      <- collect_edges[-which(as.character(collect_edges[,1]) == as.character(collect_edges[,2])),]
collect_edges.sort <- t(apply(collect_edges, 1, sort))
collect_edges      <- collect_edges[!duplicated(collect_edges.sort),]

collect_nodes <- unique(c(as.character(collect_edges[,1]), as.character(collect_edges[,2])))

# get ecContrib from projects data
fp7_org <- read.csv('cordis-fp7organizations.csv', header=T, sep=";", dec=",")
fp7_org <- fp7_org[which(fp7_org$projectReference %in% adat$reference[adat$programme=="FP7-HEALTH"]),]
length(unique(fp7_org$projectReference))

h2020_org <- read.csv('cordis-h2020organizations.csv', header=T, sep=";", dec=",")
h2020_org <- h2020_org[which(h2020_org$projectReference %in% adat$reference[adat$programme!="FP7-HEALTH"]),]
length(unique(h2020_org$projectReference))

# aggregate ecContrib by country
fp7_ecbycountry <- aggregate(fp7_org$ecContribution ~ fp7_org$country, data=adat, FUN=sum)
h2020_ecbycountry <- aggregate(h2020_org$ecContribution ~ h2020_org$country, data=adat, FUN=sum)
rm(list = c('fp7_org','h2020_org'))

colnames(fp7_ecbycountry) <- c('country', 'ecContrib')
colnames(h2020_ecbycountry) <- c('country', 'ecContrib')

ecbycountry <- rbind(fp7_ecbycountry, h2020_ecbycountry)

ecbycountry <- aggregate(ecbycountry$ecContrib ~ ecbycountry$country, data=ecbycountry, FUN=sum)
colnames(ecbycountry) <- c('country', 'ecContrib')

collect_nodes <- as.data.frame(collect_nodes)
colnames(collect_nodes) <- c('country')

# update nodes data with ecContrib for weighting
collect_nodes <- merge(collect_nodes, ecbycountry, by.x="country", by.y="country", all.x=T)

# update edges by joint projects data
