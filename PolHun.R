library(Hmisc)

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

adat$allCountries <- paste0(adat$coordinatorCountry, ";", adat$participantCountries)

collect_edges$total_joint <- 0

for(i in 1:nrow(collect_edges))
{
  collect_edges$total_joint[i] <- sum(adat$ecMaxContribution[grep(paste0(collect_edges[i,1], "|", collect_edges[i,2]), adat$allCountries)])
}

collect_edges$total_groups <- as.character(cut2(collect_edges$total_joint, g=10)) 

collect_edges$total_groups[which(collect_edges$total_groups == "[6.00e+05,1.12e+08)")] <- "1"
collect_edges$total_groups[which(collect_edges$total_groups == "[1.12e+08,1.77e+08)")] <- "2"
collect_edges$total_groups[which(collect_edges$total_groups == "[1.77e+08,2.54e+08)")] <- "3"
collect_edges$total_groups[which(collect_edges$total_groups == "[2.54e+08,4.40e+08)")] <- "4"
collect_edges$total_groups[which(collect_edges$total_groups == "[4.40e+08,5.21e+08)")] <- "5"
collect_edges$total_groups[which(collect_edges$total_groups == "[5.21e+08,6.53e+08)")] <- "6"
collect_edges$total_groups[which(collect_edges$total_groups == "[6.53e+08,7.29e+08)")] <- "7"
collect_edges$total_groups[which(collect_edges$total_groups == "[7.29e+08,8.23e+08)")] <- "8"
collect_edges$total_groups[which(collect_edges$total_groups == "[8.23e+08,9.53e+08)")] <- "9"
collect_edges$total_groups[which(collect_edges$total_groups == "[9.53e+08,1.15e+09]")] <- "10"

collect_edges$type <- "Undirected"

colnames(collect_nodes) <- c('id', "weight")
colnames(collect_edges) <- c('source', "target", "weight2", "weight1", "type")
collect_edges <- collect_edges[, c('source', 'target', 'type', 'weight1', 'weight2')]

collect_nodes$weight_group <- as.character(cut2(collect_nodes$weight, g=10))

collect_nodes$weight_group[which(collect_nodes$weight_group == "[       0,5.49e+04)")] <- "1"
collect_nodes$weight_group[which(collect_nodes$weight_group == "[   54899,1.29e+05)")] <- "2"
collect_nodes$weight_group[which(collect_nodes$weight_group == "[  129420,2.53e+05)")] <- "3"
collect_nodes$weight_group[which(collect_nodes$weight_group == "[  252750,9.56e+05)")] <- "4"
collect_nodes$weight_group[which(collect_nodes$weight_group == "[  956383,1.35e+06)")] <- "5"
collect_nodes$weight_group[which(collect_nodes$weight_group == "[ 1345011,3.28e+06)")] <- "6"
collect_nodes$weight_group[which(collect_nodes$weight_group == "[ 3284972,1.04e+07)")] <- "7"
collect_nodes$weight_group[which(collect_nodes$weight_group == "[10435971,3.14e+07)")] <- "8"
collect_nodes$weight_group[which(collect_nodes$weight_group == "[31414116,5.75e+07)")] <- "9"
collect_nodes$weight_group[which(collect_nodes$weight_group == "[57468340,1.66e+08]")] <- "10"

country_geocode <- read.csv("country_geocode.csv", header=T, sep=";")

collect_nodes <- merge(collect_nodes, country_geocode, by.x="id", by.y="country", all.x=T, all.y=F)

colnames(collect_nodes) <- c('id', 'weight', 'weight_group', 'lat', 'lng', 'label')
collect_nodes <- collect_nodes[, c('id','label', 'weight', 'weight_group', 'lat', 'lng')]

# delete countries with 0 budget or NA
collect_nodes <- collect_nodes[-which(is.na(collect_nodes$weight) | collect_nodes$weight == 0),]

collect_edges <- collect_edges[which(collect_edges$source %in% collect_nodes$id & collect_edges$target %in% collect_nodes$id),]

write.csv(collect_nodes, "country_nodes.csv")
write.csv(collect_edges, "country_edges.csv")

# timeline elemzes

library(Hmisc)

setwd('c:/gergo/fp7')

fp7_org <- read.csv('cordis-fp7organizations.csv', header=T, sep=";", dec=",")
fp7_org <- fp7_org[which(fp7_org$projectReference %in% adat$reference[adat$programme=="FP7-HEALTH"]),]

fp7_org <- fp7_org[,which(colnames(fp7_org) %in% c('projectReference', 'ecContribution', 'projectAcronym', 'id', 'name', 'country'))]
fp7_org <- aggregate(ecContribution ~ projectReference+country, data=fp7_org, FUN=sum)
fp7_org$fwp <- "FP7"


h2020_org <- read.csv('cordis-h2020organizations.csv', header=T, sep=";", dec=",")
h2020_org <- h2020_org[which(h2020_org$projectReference %in% adat$reference[adat$programme!="FP7-HEALTH"]),]

h2020_org <- h2020_org[,which(colnames(h2020_org) %in% c('projectReference', 'ecContribution', 'projectAcronym', 'id', 'name', 'country'))]
h2020_org <- aggregate(ecContribution ~ projectReference+country, data=h2020_org, FUN=sum)
h2020_org$fwp <- "H2020"

org_adat <- rbind(fp7_org, h2020_org)
rm(list = c('fp7_org','h2020_org'))

adat_sub <- adat[,which(colnames(adat) %in% c('reference', 'startDate', 'endDate'))]
adat_sub <- merge(org_adat, adat_sub, by.x = "projectReference", by.y= "reference", all.x=F, all.y=F)

monnb <- function(d) {
                      lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"))
                      lt$year*12 + lt$mon
                      }
mondf <- function(d1, d2) { monnb(d2) - monnb(d1) + 1}

adat_sub$duration <- mondf(as.Date(adat_sub$startDate), as.Date(adat_sub$endDate))

adat_sub$ecc_bymonth <- adat_sub$ecContribution / adat_sub$duration

write.csv(adat_sub, "ecc_date.csv")
