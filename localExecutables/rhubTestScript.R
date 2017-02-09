# The following script will test the functionality of the PWFSLSmoke Package on various platforms

rhubTestScript <- function(windows=TRUE,linux=TRUE,debian=TRUE,ubuntu=TRUE,email='RexS.Thompson@gmail.com') {
  
  if ( windows ) {
    rhub::check_on_windows(email=email)
  }
  if ( linux ) {
    rhub::check_on_linux(email=email)
  }
  if ( debian ) {
    rhub::check_on_debian(email=email)
  }
  if ( ubuntu ) {
    rhub::check_on_ubuntu(email=email)
  }
  
  #rhub::check(platform = rhub::platforms()[['name']][1], email=email)

}