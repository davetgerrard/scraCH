

# Graph some companies data
# Working. Needs to be made into a function.
# Could extend to make network dynamic with connections appearing 
#   and disappearing over time (with names 'exploding' off the screen!)
# TODO, try with a new person. Alan Michael SUGAR https://beta.companieshouse.gov.uk/officers/1fox8G7xzfgdlmkfSG5a24fprbM/appointments
# TODO fix the multi-page problem getting data.


setwd("C:/Temp/companies/")

# already made a table of James Dyson's appointment AND another table list all the directors of those companies.

load("test.James.Dyson.links.Rdata")
# officerTable, officer_id, officerAppoints

# try something like the interactive charts on https://rpubs.com/kateto/netviz
# install.packages("ndtv", dependencies=T)


library(ndtv)   # for rendering networks
#net3


library(network)   # for creating networks

# make a network

head(officerAppoints)
head(officerTable)


#net3 <- network(links,  vertex.attr=nodes, matrix.type="edgelist", 
   #             loops=F, multiple=F, ignore.eval = F)

#links <- officerTable[,c("officer_id", "company_id")]
#names(links) <- c("from", "to")
#links$from <- as.character(links$from)
#links$to <- as.character(links$to)

compsTable <- unique(officerAppoints[,c("company_id", "company_name")])
names(compsTable) <- c("id", "name")
compsTable$type <- "company"

peopleTable <- unique(officerTable[,c("officer_id", "officer_name")])
names(peopleTable) <- c("id", "name")
peopleTable$type <- "person"
nodes <- rbind(compsTable,peopleTable)
# not sure if this is necessary.
#for(i in 1:ncol(nodes)) {
#  nodes[,i] <- as.character(nodes[,i])
#  
#}

row.names(nodes) <- nodes$id




#list.network.attributes(compNet)

all.names <- c(unique(as.character(officerTable$officer_id)), unique(as.character(officerTable$company_id)))

link.adjacency <- matrix(0, nrow=length(all.names), ncol= length(all.names), 
                         dimnames=list(all.names, all.names))

#link.adjacency[officerTable[,c("officer_id", "company_id")]]
link.adjacency[cbind(as.character(officerTable$officer_id), as.character(officerTable$company_id))] <- 1
# and the inverse to make is symmetrical (seems silly)
link.adjacency[cbind(as.character(officerTable$company_id),as.character(officerTable$officer_id))] <- 1
compNet <- network(link.adjacency, directed=F, loops=T)


compNet %v% "ID" <- all.names
compNet %v% "name" <- as.character(nodes[all.names,'name'])
compNet %v% "type" <- as.character(nodes[all.names,'type'])
compNet %v% "col" <-ifelse(compNet %v% "name" == "DYSON, James, Sir", "green", "red")
compNet %v% "col" <- ifelse(compNet %v% "type" == "person",compNet %v% "col" , "blue")
compNet %v% "sides" <- ifelse(compNet %v% "type" == "person", 3, 4)

 

plot(compNet)
# basiic plot to check details.
#plot(compNet, label=NA, usearrows=F,vertex.sides=compNet %v% 'sides', vertex.col=compNet %v% 'col')
plot(compNet, label=NA, displaylabels=F,usearrows=F,vertex.sides=5, vertex.col=compNet %v% 'col')

render.d3movie(compNet, vertex.tooltip = compNet %v% 'name', vertex.sides=compNet %v% 'sides', vertex.col=compNet %v% 'col',
               launchBrowser=F, filename="Company-Network.html")



stopifnot(FALSE)

# DEVELOPMENT --------------------------------

compNet <- network(links, directed=F, loops=T)
compNet <- network(links,  directed=F, matrix.type="edgelist", 
                   loops=T, multiple=F, ignore.eval = F)
?network

#set.vertex.attribute(compNet, "name", as.character(nodes$name))   # working   FAXCTORS!  - NO i think the names are wrong.
#set.vertex.attribute(compNet, "type", as.character(nodes$type))
#set.vertex.attribute(compNet, "sides", ifelse(as.character(nodes$type) == "person", 3, 4) )
#set.vertex.attribute(compNet, "col", ifelse(as.character(nodes$name) == "DYSON, James, Sir", "green", "red") )

#render.d3movie(compNet, vertex.tooltip = compNet %v% 'name', vertex.shape="square")
#library(igraph)
#render.d3movie(compNet, vertex.tooltip = compNet %v% 'name', vertex.shape=compNet %v% 'shape')
# FAILED with  , layout=layout.fruchterman.reingold(compNet)

get.vertex.attribute(compNet, "name")

# load it into the same file:-
render.d3movie(compNet, vertex.tooltip = compNet %v% 'name', vertex.sides=compNet %v% 'sides', vertex.col=compNet %v% 'col',
               launchBrowser=F, filename="Company-Network.html")

rowSums(as.matrix(compNet))[officer_id]  # check that the network as the righ number of interactions.


compNet %v% 'name'
compNet %v% 'type'
ifelse(compNet %v% 'type' == "person", "circle","square")

seq_len(network.size(compNet))


, usearrows = F, displaylabels = F, bg="#111111", 
               vertex.border="#ffffff", vertex.col =  net3 %v% "col",
               vertex.cex = (net3 %v% "audience.size")/8, 
               edge.lwd = (net3 %e% "weight")/3, edge.col = '#55555599',
               vertex.tooltip = paste("<b>Name:</b>", (net3 %v% 'media') , "<br>",
                                      "<b>Type:</b>", (net3 %v% 'type.label')),
               edge.tooltip = paste("<b>Edge type:</b>", (net3 %e% 'type'), "<br>", 
                                    "<b>Edge weight:</b>", (net3 %e% "weight" ) ),
               launchBrowser=F, filename="Media-Network.html", output.mode='inline') 



plot(compNet)

#row.names(nodes) <- nodes$id
#compNet <- network(links, vertex.attr=as.list(nodes), matrix.type="edgelist", 
#                   loops=F, multiple=F, ignore.eval = F)

?set.vertex.attribute

# graphs look wrong, check the data from some large nodes.
focal.id <- as.character(peopleTable$id[grep("ZENG", peopleTable$name)])
officerTable[grep(focal.id, officerTable$officer_id),]

head(links)
grep(focal.id, links$from)
grep(focal.id, links$to)




#example
edata <-data.frame(
  tails=c(1,2,3),
  heads=c(2,3,1),
  love=c('yes','no','maybe'),
  hate=c(3,-5,2),
  stringsAsFactors=FALSE
)

g<-network.edgelist(edata,network.initialize(4),ignore.eval=FALSE)
as.sociomatrix(g,attrname='hate')
g%e%'love'

g<-network.edgelist(links,network.initialize(nrow(nodes)),ignore.eval=FALSE)

