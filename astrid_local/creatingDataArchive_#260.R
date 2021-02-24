###################################################################
# Author: Astrid Sanna
# Date: 2/23/2021
# Object: Creating a Data Archive (#260)
###################################################################
devtools::install_github("MazamaScience/AirSensor")
library(MazamaCoreUtils)
library(AirSensor)
library(dplyr)

pas_ex <- AirSensor::example_pas
id <- pas_getDeviceDeploymentIDs(pas_ex, "^Seattle$")

# ----- STEP 1: pas_load MVCCA and save it locally
setArchiveBaseDir(NULL)
setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1/")
mvcaa =
  pas_load() %>% # Load the most recent archived 'pas'(PurpleAir Synoptic)
  pas_filter(stringr::str_detect(label, "MV Clean Air Ambassador"))
head(mvcaa)

pas_leaflet(mvcaa)

# to save objects as files - save(pas, file = "FILE_PATH")
save(mvcaa, file = "C:/Users/astri/Mirror/Mazamascience/Data/AirSensor/pas_MVCAA.rda")

# ----- STEP 2: use setArchiveBaseDir()
setArchiveBaseDir("C:/Users/astri/Mirror/Mazamascience/Data/AirSensor/my_archive/")
getArchiveBaseDir() #OK

# ----- STEP 3: get list of deviceDeploymentIDs for MVCAA sensors
pas = get(load("C:/Users/astri/Mirror/Mazamascience/Data/AirSensor/pas_MVCAA.rda"))
ID =  pas_getDeviceDeploymentIDs(pas = pas)
# output:
# [1] "ab5dca99422f2c0d_13669" "f6c44edd41c941c7_10182" "49215ad49d1a87e3_10188"
# [4] "f736fd3fb21fc4da_13667" "db5d6b3b79f5830e_39237" "4f19d256e1787973_10166"
# [7] "f592adb5067ad9d3_13675" "4a47b9252e16e558_15077" "0cbfeb2ce4c1553c_13661"
# [10] "2e3b5ceea86a885b_10168" "f96deab8c29aa42b_10134" "96b108298883ca47_64441"
# [13] "b56c0ef677852913_81495"

# ----- STEP 4: in a for loop, use pat_createNew() create 'pat' objects for
# each sensor. I think it will get the last week if you don't specify startdate
# and enddate

ID_test = c("ab5dca99422f2c0d_13669","f6c44edd41c941c7_10182")

# try a test of 2 IDs
for (id in ID_test){
  filename = paste0(local_dir, "/", id, "_202009", ".rda")
  pat = pat_createNew(id = id,
                      pas = mvcaa,
                      startdate = 20200901,
                      enddate = 20200930)
  save(pat, file = filename)
}

# ----- STEP 5: create my_archive and save pat files there
my_archive_dir = "C:/Users/astri/Mirror/Mazamascience/Data/AirSensor/my_archive"
pat = "pat/2020/09"
dir.create(file.path(my_archive_dir, pat), recursive = TRUE)

# save the new directory as an object
pat_202009 = paste0(my_archive_dir,"/", pat)

# try a test of 2 IDs saved in my_archive
setArchiveBaseDir(NULL)
for (id in ID_test){
  filename = paste0(pat_202009, "/", "pat_", id, "_202009", ".rda")
  pat = pat_createNew(id = id,
                      pas = mvcaa,
                      startdate = 20200901,
                      enddate = 20200930)
  save(pat, file = filename)
}


# ----- STEP 6: load data (trying different approaches)
setArchiveBaseDir(my_archive_dir)
getArchiveBaseDir()
pat_load(id = "ab5dca99422f2c0d_13669")
# C:/Users/astri/Mirror/Mazamascience/Data/AirSensor/my_archive

setArchiveBaseDir(pat_202009)
getArchiveBaseDir()
pat_load(id = "ab5dca99422f2c0d_13669")

setArchiveBaseDir("C:/my_archive/")
getArchiveBaseDir()
pat_load(id = "ab5dca99422f2c0d_13669")
