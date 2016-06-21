# TODO - allow for multi-page results.  
# this function mainly to get table of companies, but as an officer can server in several roles for the same company (or be director more than once), strictly this is a table of appointments.
getOfficerAppoints <- function(officer_id) {
  url <- paste0("https://beta.companieshouse.gov.uk/officers/", officer_id,"/appointments")
  allLines <- readLines(url)
  n.appoints <- as.integer(sub(".*Total number of appointments *","",grep("Total number of appointments", allLines, value=T)))
  appoints.table <- data.frame()
  print(paste(officer_id, ":", n.appoints, "appointments found"))
  page.counter <- 1

  while(nrow(appoints.table) < n.appoints)  {  
   if(page.counter > 1)  {
     url <- paste0("https://beta.companieshouse.gov.uk/officers/", officer_id,"/appointments\\?page=", page.counter) 
     allLines <- readLines(url)
   }
    page.appoints <- length(grep("company-name-", allLines))
  #

  
  for(i in 1:page.appoints) {    # they are number 1: page.appoints on each new page 
    
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
    page.counter <- page.counter + 1
  }
  return(appoints.table)
}
