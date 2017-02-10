# The following script will test the functionality of the PWFSLSmoke Package on various platforms
# remember to run library(rhub) prior to using
# NOTE: May have to validate or revalidate email address prior to running...

rhubTestScript <- function(email='RexS.Thompson@gmail.com',...) {
  
  argsList <- list(...)
  
  argsList$email <- email

  #platforms()[['name']][c(1,3,9,10,11,14)]
  if ( !'platform' %in% argsList ) {
    argsList$platform <- c("debian-gcc-devel","debian-gcc-release","ubuntu-gcc-devel","ubuntu-gcc-release","windows-x86_64-devel","windows-x86_64-release")
  }
  
  do.call(rhub::check,argsList)
  
}