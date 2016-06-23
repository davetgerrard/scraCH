

networkFromOfficerId <- function(officer_id, depth=1, queryCountStart=0, make.cache=TRUE, use.cache=cache) {
  #officer_id <- "1fox8G7xzfgdlmkfSG5a24fprbM"
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
  
  #return(nodes)
  all.names <- unique(as.character(nodes$id))
  
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
  
  return(compNet)
}