



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






# TODO - Build caching facility storing parsed queries as dated local files. e.g. scraCH.company.company_id.2016-01-01.tsv


