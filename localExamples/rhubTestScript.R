# The following script will test the functionality of the PWFSLSmoke Package on various platforms
#
# NOTE: May have to validate or revalidate email address prior to running...

library(rhub)

rhubTestScript <- function(email='',...) {
  
  if ( email == '' ) {
  stop("Requred argument email was left blank")
  }
  
  argsList <- list(...)
  
  argsList$email <- email

  # NOTE:  All available platforms:  rub::platforms()[['name]]

  if ( !'platform' %in% argsList ) {
    argsList$platform <- c("debian-gcc-devel",
                           "debian-gcc-release",
                           "ubuntu-gcc-devel",
                           "ubuntu-gcc-release",
                           "windows-x86_64-devel",
                           "windows-x86_64-release")
  }
  
  do.call(rhub::check,argsList)
  
}