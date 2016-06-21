



# http://download.companieshouse.gov.uk/en_output.html
# explanation of columns
#http://resources.companieshouse.gov.uk/toolsToHelp/pdf/freeDataProductDataset.pdf


setwd("C:/Temp/companies/")

dir()


?zip.file.extract

?read.csv

data.1 <- read.csv(unz("BasicCompanyData-2016-06-01-part1_5.zip","BasicCompanyData-2016-06-01-part1_5.csv" ))


head(data.1)



dim(data.1)



# how many unique post-codes are there

length(unique(data.1$RegAddress.PostCode))


tail(sort(table(data.1$RegAddress.PostCode)))  # some postcodes have 1000s of addresses - remember, this is still onyy a portion of the total data.

graphics:::hist.POSIXt(as.Date( data.1$IncorporationDate))

hist(as.Date( head(data.1$IncorporationDate, n = 200)), breaks=50)  # need to format axis


# PROBLEM - above data does not contain Director's name or address. Need this to glue together groups.



head(unique(data.1$RegAddress.CareOf), n=30)
tail(sort(table(data.1$RegAddress.CareOf))) 



head(data.1[data.1$RegAddress.CareOf == "UMBRELLA ACCOUNTANTS LLP",])




# TODO will need to scrape data for directors. Data IS in the html source.

# e.g. https://beta.companieshouse.gov.uk/company/09269679/officers  
# also contains a (temporary ?) link to a page for that person listing all their directorships (and other recorded positions).
# for the above example: https://beta.companieshouse.gov.uk/officers/oE34_b99Fu3jX4vF9f3KLX_5bVw/appointments



# from that page, found that there is some sort of API in beta
# https://developer.companieshouse.gov.uk/api/docs/
# and an active forum
#http://forum.aws.chdev.org/

# there is some concern from directors that their personal info is now exposed
#https://companieshouse.blog.gov.uk/2016/01/21/our-register-advice-on-protecting-your-personal-information/

require(XML)
require(RCurl)
input<-"R%statistical%Software"
url <- paste0("http://www.google.com/search?q=",input)
doc <- htmlParse(url)
result <- lapply(doc['//span[@class="st"]'],xmlValue)
result[1]

# test on companies house:-

https://beta.companieshouse.gov.uk/company/09269679/officers
url <- "https://beta.companieshouse.gov.uk/company/09269679/officers"
doc <- xmlParse(url)
doc <- htmlTreeParse(url)
result <- lapply(doc['//span[@class="st"]'],xmlValue)
result[1]


# data is in json ?
library(jsonlite)
url <- 'http://www.juntadeandalucia.es/export/drupaljda/ayudas.json'
url <- "https://beta.companieshouse.gov.uk/company/09269679/officers"
# read url and convert to data.frame
document <- fromJSON(txt=url)
dim(document)

# the simplistic way

allLines <- readLines(url)

grep("officer-name", allLines, value=T)
grep("officers.*appointments", allLines, value=T)


#"You can make up to 600 requests within a five-minute period. If you excee"  https://developer.companieshouse.gov.uk/api/docs/index/gettingStarted/rateLimiting.html

officerNameIndex <- grep("officer-name", allLines)
officerAppointmentsIndex <- intersect(officerNameIndex +1 , grep("officers.*appointments", allLines))

allLines[officerAppointmentsIndex]
thisIndex <- officerAppointmentsIndex[1]
officer_id <- sub("/appointments.*", "",sub(".*officers/","", allLines[thisIndex]))
officer_name <- sub("</a>", "",sub("[^>]*>?","", allLines[[thisIndex]]))


companies <- as.character(sample(data.1$CompanyNumber, 20, replace=F))

officerTable <- data.frame()
for(company_id in companies)  {
  
  url <- paste0("https://beta.companieshouse.gov.uk/company/", company_id,"/officers")
  allLines <- readLines(url)
  officerNameIndex <- grep("officer-name", allLines)
  officerAppointmentsIndex <- intersect(officerNameIndex +1 , grep("officers.*appointments", allLines))
  print(paste(company_id, ":", length(officerAppointmentsIndex), "officers found"))
  for(thisIndex in officerAppointmentsIndex)  {
    officer_id <- sub("/appointments.*", "",sub(".*officers/","", allLines[thisIndex]))
    officer_name <- sub("</a>", "",sub("[^>]*>?","", allLines[[thisIndex]]))
    thisRow <- data.frame(company_id,officer_id, officer_name)
    officerTable <- rbind(officerTable, thisRow)
  }
    
}
officerTable



# second version, 
# use "officer-name" to get number of officers listed, then create custom greps for each officer.






  

# TODO - allow for multi-page results.  
# DONE, re-write this as a function.  
getCompanyAppointments <- function(company_id)  {  
  officerTable <- data.frame()
  url <- paste0("https://beta.companieshouse.gov.uk/company/", company_id,"/officers")
  allLines <- readLines(url)
  #officerNameIndex <- grep("officer-name", allLines)
  n.officers <- length(grep("officer-name", allLines))
  #officerAppointmentsIndex <- intersect(officerNameIndex +1 , grep("officers.*appointments", allLines))
  print(paste(company_id, ":", n.officers, "officers found"))
  for(i in 1:n.officers)  {
    officerNameIndex <- grep(paste0("officer-name-",i,"[^0-9]"), allLines, fixed=F) + 1     # need the "[^0-9]" to separate 1 from 10,11 etc without using '\'
    officer_id <- ifelse(length(officerNameIndex) < 1, NA,sub("/appointments.*", "",sub(".*officers/","", allLines[officerNameIndex])))
    officer_name <- ifelse(length(officerNameIndex) < 1, NA,sub("</a>", "",sub("[^>]*>?","", allLines[[officerNameIndex]])))
    
    officerAddressIndex <- grep(paste0("officer-address-value-",i,"[^0-9]"), allLines) + 1
    officer_address <- ifelse(length(officerAddressIndex) < 1, NA,sub("<.*","", allLines[[officerAddressIndex]]))
    
    officerRoleIndex <- grep(paste0("officer-role-",i,"[^0-9]"), allLines) + 1
    officer_role <- ifelse(length(officerRoleIndex) < 1, NA,sub("^ *","", allLines[[officerRoleIndex]]) )   
    
    officerDOBIndex <- grep(paste0("officer-date-of-birth-",i,"[^0-9]"), allLines) + 1
    officer_dob <- ifelse(length(officerDOBIndex) < 1, NA,sub("^ *","", allLines[[officerDOBIndex]])  )
    
    officerAppIndex <- grep(paste0("officer-appointed-on-",i,"[^0-9]"), allLines) + 1
    officer_appointed_on <- ifelse(length(officerAppIndex) < 1, NA,sub("^ *","", allLines[[officerAppIndex]]) )
  
    officerResIndex <- grep(paste0("officer-resigned-on-",i,"[^0-9]"), allLines) + 1
    officer_resigned_on <- ifelse(length(officerResIndex) < 1, NA,sub("^ *","", allLines[[officerResIndex]]) )
    
    officerNatIndex <- grep(paste0("officer-nationality-",i,"[^0-9]"), allLines) + 1
    officer_nationality <- ifelse(length(officerNatIndex) < 1, NA,sub("^ *","", allLines[[officerNatIndex]]) )
    
    officerOccIndex <- grep(paste0("officer-occupation-",i,"[^0-9]"), allLines) + 1
    officer_occupation <- ifelse(length(officerOccIndex) < 1, NA,sub("^ *","", allLines[[officerOccIndex]]) )
  
     # "officer-address-value", "officer-role", "officer-date-of-birth","officer-appointed-on","officer-resigned-on","officer-nationality" , "officer-occupation"
    
    thisRow <- data.frame(company_id,officer_id, officer_name, officer_role, officer_address, officer_role, officer_dob, officer_appointed_on,  officer_resigned_on, officer_nationality, officer_occupation)
    officerTable <- rbind(officerTable, thisRow)
  }
  return(officerTable)
}


#"officer-name"
companies <- as.character(sample(data.1$CompanyNumber, 500, replace=F))
#company_id <- companies[1]
officerTable <- data.frame()
for(company_id in companies)  {
  officerTable <- rbind(officerTable, getCompanyAppointments(company_id))
}

#officerTable

dim(officerTable)

#c( "officer-address-value", "officer-role", "officer-date-of-birth","officer-appointed-on","officer-resigned-on","officer-nationality" , "officer-occupation")




# what to do next?   could download officers info from all companies. 
# approx calc    600 per 5 mins = 7200 per hour.
# their are approx 850000 * 4.5 =3.825M company records  ~ 531.25 hours.(and lots of data).


# Would be quite good to be able to put in a person or a company and see the connections 
# size nodes by value of company or number of directors
# size people by age or by number of appointments (active or inactive), or by number of people that 


# report builder for people or companies, need to limit depth of search. 


# could also do six degrees of separation style thing with connections between people (though parsing maybe intensive).


# Prioritisation function - given a search goal start looking through people and companies and then update priorities based on parial results. 
# e.g. if a person, company or address is over-represented.


# start with someone well known but of relatively modest scope - James Dyson 
focal.officer_id <- "Bwx3j9lLWQRa33DqXJgPMV56BE8"

url <- paste0("https://beta.companieshouse.gov.uk/officers/", focal.officer_id,"/appointments")
#url <- paste0("https://beta.companieshouse.gov.uk/company/", company_id,"/officers")
allLines <- readLines(url)


# ! NOTE - appointment limited to XX per page, therefore may need to query further pages.  # may be able to search on "page-number-"



allLines[grep("company-name-", allLines) + 1]

#officer_id <- "Bwx3j9lLWQRa33DqXJgPMV56BE8"
# TODO - allow for multi-page results.  
# this function mainly to get table of companies, but as an officer can server in several roles for the same company (or be director more than once), strictly this is a table of appointments.
getOfficerAppoints <- function(officer_id) {
  url <- paste0("https://beta.companieshouse.gov.uk/officers/", officer_id,"/appointments")
  allLines <- readLines(url)
  n.appoints <- length(grep("company-name-", allLines))
  #
  appoints.table <- data.frame()
  print(paste(officer_id, ":", n.appoints, "appointments found"))
  
  for(i in 1:n.appoints) {
    
    #company-name-
      companyNameIndex <- grep(paste0("company-name-",i,"[^0-9]"), allLines, fixed=F) + 1 # need the "[^0-9]" to separate 1 from 10,11 etc without using '\'

    company_id <- ifelse(length(companyNameIndex) < 1, NA,sub("\".*", "",sub(".*company/","", allLines[companyNameIndex])))  # beware the tricksy |" combo
    company_name <- ifelse(length(companyNameIndex) < 1, NA,sub("</a>", "",sub("[^>]*>?","", allLines[[companyNameIndex]])))
      
      
      #company-status-value-  
    companyStatusIndex <- grep(paste0("company-status-value-",i,"[^0-9]"), allLines) + 1
    company_status <- ifelse(length(companyStatusIndex) < 1, NA,sub("^ *","", allLines[[companyStatusIndex]]) )  
     
    
    #correspondence-address-value-
    corrAddressIndex <- grep(paste0("correspondence-address-value-",i,"[^0-9]"), allLines) 
    correspondence_address <- ifelse(length(corrAddressIndex) < 1, NA,sub("</dd>", "",sub("[^>]*>?","", allLines[[corrAddressIndex]])))   # TODO remove trailing whitespace
    
    #  appointment-type-value24
    appTypeIndex <- grep(paste0("appointment-type-value",i,"[^0-9]"), allLines) + 1
    appointment_type <- ifelse(length(appTypeIndex) < 1, NA,sub("^ *","", allLines[[appTypeIndex]]) )  
    
    #appointed-value24
    appOnIndex <- grep(paste0("appointed-value",i,"[^0-9]"), allLines) + 1
    appointed_on <- ifelse(length(appOnIndex) < 1, NA,sub("^ *","", allLines[[appOnIndex]]) )      
    
    #resigned-value-
    resignedIndex <- grep(paste0("resigned-value-",i,"[^0-9]"), allLines) + 1
    resigned_on <- ifelse(length(resignedIndex) < 1, NA,sub("^ *","", allLines[[resignedIndex]]) )  
      
     # nationality-value24
    nationalityIndex <- grep(paste0("nationality-value",i,"[^0-9]"), allLines) + 1
    nationality <- ifelse(length(nationalityIndex) < 1, NA,sub("^ *","", allLines[[nationalityIndex]]) )  
    
    #country-of-residence-value
    countryResIndex <- grep(paste0("country-of-residence-value",i,"[^0-9]"), allLines) + 1
    country_of_residence <- ifelse(length(countryResIndex) < 1, NA,sub("^ *","", allLines[[countryResIndex]]) )  
    
    #occupation-value-2
    occupationIndex <- grep(paste0("occupation-value-",i,"[^0-9]"), allLines) + 1
    occupation <- ifelse(length(occupationIndex) < 1, NA,sub("^ *","", allLines[[occupationIndex]]) )  
    
    thisRow <- data.frame(officer_id,company_id, company_name, company_status, correspondence_address,
                           appointment_type, appointed_on, resigned_on, nationality, country_of_residence, occupation   )
    appoints.table <- rbind(appoints.table, thisRow)
  }
  
  return(appoints.table)
}


officer_id <- "Bwx3j9lLWQRa33DqXJgPMV56BE8"
officerAppoints <- getOfficerAppoints(officer_id)    # not really companies but appointments
dim(officerAppoints)
head(officerAppoints)


newCompanies <- as.character(officerAppoints$company_id)
# now find all the appointment for 
# should probably limit to 600
length(newCompanies)

officerTable <- data.frame()
for(company_id in newCompanies)  {
  officerTable <- rbind(officerTable, getCompanyAppointments(company_id))
}

dim(officerTable)
# expect their to be some redundancy in the individuals in these companies (James Dyson himself should appear at least 35 times).
length(unique(officerTable$officer_id))
length(unique(officerTable$officer_name))
length(unique(officerTable$officer_address))
length(unique(officerTable$officer_dob))

tail(sort(table(officerTable$officer_name)))


# TODO - tidy this up. 
# plot relationships  - may be better in Cytoscape?  Ask Jamie S.
#           https://rpubs.com/kateto/netviz


#maybe save these tables 
getwd()
save(officerTable, officer_id, officerAppoints, file="test.James.Dyson.links.Rdata")






# TODO - some directors have multiple entries, write a function to cluster these entries. One reason is due to change of address.
#  e.g. same company, similar name  




