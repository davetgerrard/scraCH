# alan sugar
library(ndtv) 
library(network)
library(scraCH)


# TODO many people are given several IDs and appear multiply
# e.g. RAY, michael edward
# Is this because they have duplicate appointments or do they have multiple officer_ids
#       It's because 'secretary' appointments have non-unique ids. The director ids, should be unique.


#Alan Michael SUGAR https://beta.companieshouse.gov.uk/officers/1fox8G7xzfgdlmkfSG5a24fprbM/appointments
setwd("C:/Temp/companies/")

officer_id <- "1fox8G7xzfgdlmkfSG5a24fprbM"
officerAppoints <- getOfficerAppoints(officer_id) 
newCompanies <- as.character(officerAppoints$company_id)
officerTable <- data.frame()
for(company_id in newCompanies)  {
  officerTable <- rbind(officerTable, getCompanyAppointments(company_id))
}

officerTable <- subset(officerTable, officer_role == "Director")


compsTable <- unique(officerAppoints[,c("company_id", "company_name")])
names(compsTable) <- c("id", "name")
compsTable$type <- "company"

peopleTable <- unique(officerTable[,c("officer_id", "officer_name")])
names(peopleTable) <- c("id", "name")
peopleTable$type <- "person"
# check for duplicate names
tail(sort(table(peopleTable$name)))

nodes <- rbind(compsTable,peopleTable)
# might be some duplicate name entries.
nodes <- nodes[match(unique(nodes$id), nodes$id),]   # may delete alternate names for same person
row.names(nodes) <- nodes$id




all.names <- c(unique(as.character(officerTable$officer_id)), unique(as.character(officerTable$company_id)))

link.adjacency <- matrix(0, nrow=length(all.names), ncol= length(all.names), 
                         dimnames=list(all.names, all.names))


link.adjacency[cbind(as.character(officerTable$officer_id), as.character(officerTable$company_id))] <- 1
# and the inverse to make is symmetrical (seems silly)
link.adjacency[cbind(as.character(officerTable$company_id),as.character(officerTable$officer_id))] <- 1
compNet <- network(link.adjacency, directed=F, loops=T)


compNet %v% "ID" <- all.names
compNet %v% "name" <- as.character(nodes[all.names,'name'])
compNet %v% "type" <- as.character(nodes[all.names,'type'])
compNet %v% "col" <-ifelse(compNet %v% "ID" == officer_id, "green", "red")
compNet %v% "col" <- ifelse(compNet %v% "type" == "person",compNet %v% "col" , "blue")
compNet %v% "sides" <- ifelse(compNet %v% "type" == "person", 3, 4)



plot(compNet)
# basiic plot to check details.
#plot(compNet, label=NA, usearrows=F,vertex.sides=compNet %v% 'sides', vertex.col=compNet %v% 'col')
plot(compNet, label=NA, displaylabels=F,usearrows=F,vertex.sides=5, vertex.col=compNet %v% 'col')

render.d3movie(compNet, vertex.tooltip = compNet %v% 'name', vertex.sides=compNet %v% 'sides', vertex.col=compNet %v% 'col',
               launchBrowser=F, filename="AlanSugar.html")

# check status of multiple id dudes
officerTable[grep("RAY, Michael Edward", officerTable$officer_name), c("officer_name", "officer_id")]
#table(officerTable$officer_name, officerTable$officer_name)
officerIDTable <- table(unique(officerTable[,c("officer_name", "officer_id")]))
tail(sort(rowSums(officerIDTable)))
#colSums(officerIDTable)

#http://help.companycheck.co.uk/hc/en-us/articles/203741219-More-than-one-director-report-for-the-same-person

# I should look at this guy and see why he has so many duplicate records...

officerTable[grep("RAY, Michael Edward", officerTable$officer_name), c("officer_address", "officer_id")]    # some IDs have several addresses (so that is not the only reason)

