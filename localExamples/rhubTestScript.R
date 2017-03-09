# The following script will test the functionality of the PWFSLSmoke Package on various platforms
#
# NOTE: May have to validate or revalidate email address prior to running...

library(rhub)

rhubTestScript <- function(path=".",
                           email='',
                           check_args=c("--as-cran"),
                           show_status=FALSE,
                           ...) {
  
  if ( email == '' ) {
    stop("Requred argument 'email' was left blank")
  }
  
  argsList <- list(...)
  
  argsList$path <- path
  argsList$email <- email
  argsList$check_args <- check_args
  argsList$show_status <- show_status
  
  # NOTE:  All available platforms:  rub::platforms()[['name]]
  
  if ( !'platform' %in% argsList ) {
    argsList$platform <- c("debian-gcc-devel",
                           "debian-gcc-release",
                           "linux-x86_64-centos6-epel-rdt",
                           "linux-x86_64-rocker-gcc-san",
                           "ubuntu-gcc-devel",
                           "ubuntu-gcc-release",
                           "windows-x86_64-devel",
                           "windows-x86_64-release")
  }
  
  do.call(rhub::check, argsList)
  
}