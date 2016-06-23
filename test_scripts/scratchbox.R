



officer_id <- "Bwx3j9lLWQRa33DqXJgPMV56BE8"
officerAppoints <- getOfficerAppoints(officer_id)  
dim(officerAppoints)


company_id <- "OC313276"
officerTable <- getCompanyAppointments(company_id)
dim(officerTable)




# test if there is duplication in names 
# simplest case would be identical name entry but alternate IDs.

# Current workflow with focal person
# Get list of companies from person's officer_id  using getOfficerAppoints(officer_id)

# then extract list of company ids and get list of officers for each company using
#     getCompanyAppointments(company_id)

# officerTable
head(officerTable[,c("officer_id", "officer_name", "officer_role")])
# search for a specific officer
officerTable[grep("RAY", officerTable$officer_name),c("officer_id", "officer_name", "officer_role")]

# these revealed that Michael RAY's many officer_ids are as Secretary, his directorships all have the same iid
# therefore,  filter officers on officer_role before building network. 



#TODO - function to handle production of network from officerTable and companyTable
#  BUT - later want to be able to produce networks of arbitrary depths...
# complicated by requiring multiple objects to be returned. 

officer_id <- "Bwx3j9lLWQRa33DqXJgPMV56BE8"
focalNetwork <- networkFromOfficerId(officer_id)

plot(focalNetwork)


# now try it on someone with many connections
# yDYw1ffWuspV8MVb4WOT9TTGr5A

officer_id <- "yDYw1ffWuspV8MVb4WOT9TTGr5A"
focalNetwork <- networkFromOfficerId(officer_id)

plot(focalNetwork)

render.d3movie(focalNetwork, vertex.tooltip = focalNetwork %v% 'name', vertex.sides=focalNetwork %v% 'sides', vertex.col=focalNetwork %v% 'col',
               launchBrowser=F, filename="CC_McGill.FULL.html")


# Ian Rapley (one of two IDs
officer_id <- "7AAAn6kfdwEQa1-0NI4KOQ-ci64"
focalNetwork <- networkFromOfficerId(officer_id)

plot(focalNetwork)
render.d3movie(focalNetwork, vertex.tooltip = focalNetwork %v% 'name', vertex.sides=focalNetwork %v% 'sides', vertex.col=focalNetwork %v% 'col',
               launchBrowser=F, filename="I_Rapley.1.html")


# TODO - Build caching facility storing parsed queries as dated local files. e.g. scraCH.company.company_id.2016-01-01.tsv
# TODO - stats on appointments (distribution of appointment lengths. (or gannt chart style?)
# TODO - find company directors with most directorships.

