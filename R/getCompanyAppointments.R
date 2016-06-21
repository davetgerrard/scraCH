# TODO - allow for multi-page results.  
# DONE, re-write this as a function.  
getCompanyAppointments <- function(company_id)  {  
  officerTable <- data.frame()
  url <- paste0("https://beta.companieshouse.gov.uk/company/", company_id,"/officers")
  allLines <- readLines(url)
  #officerNameIndex <- grep("officer-name", allLines)
  
  total.index <- grep('total-appointments' , allLines) 
  n.officers <- as.integer(sub("^ *", "", allLines[total.index + 1]))  +
    as.integer(sub("[^0-9]*","",allLines[total.index + 3]))
  
  
  #allLines[total.index + 3]   # number of resignations (not really needed at this point.
  #n.officers <- length(grep("officer-name", allLines))
  #officerAppointmentsIndex <- intersect(officerNameIndex +1 , grep("officers.*appointments", allLines))
  print(paste(company_id, ":", n.officers, "officers found"))
  page.counter <- 1
  
  while(nrow(officerTable) < n.officers)  {  
    if(page.counter > 1)  {
      url <- paste0("https://beta.companieshouse.gov.uk/company/", company_id,"/officers\\?page=", page.counter) 
      allLines <- readLines(url)
    }
    page.officers <- length(grep("officer-name", allLines))
    
  
  for(i in 1:page.officers)  {
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
  page.counter <- page.counter + 1  
  }
  return(officerTable)
}