## Create the Docker Image ##

A quick refresher on docker commands is available at the [docker cheatsheet](https://github.com/wsargent/docker-cheat-sheet).

A docker image with all required prerequisites can be built with the `Makefile` in this directory:

```
make operational_build
```

This is just shorthand for the following `docker build` line:

```
$ docker build --no-cache -t mazamascience/pwfslsmoke:v1.0.12 -t mazamascience/pwfslsmoke:latest .
$ docker images
REPOSITORY                        TAG                 IMAGE ID            CREATED             SIZE
mazamascience/pwfslsmoke          latest              c2403de11f34        About a minute ago   1.74GB
mazamascience/pwfslsmoke          v1.0.12             c2403de11f34        About a minute ago   1.74GB
...
```

> It is best practice to create versioned images and tag the most recent one with "latest".

Spatial data required by the **MazamaSpatialUtils** package already exists in the docker image in `/home/mazama/data/Spatial`.


## Test the Docker Image ##

Having built the docker image we can now test it. The following output was obtained on December 12, 2017:

```
docker run -ti mazamascience/pwfslsmoke R --vanilla
...
library(PWFSLSmoke)
airnow <- airnow_loadLatest()
wa <- monitor_subset(airnow, stateCodes='wa')
maxValues <- sort(apply(wa$data[,-1], 2, max, na.rm=TRUE), decreasing=TRUE)[1:6]
ids <- names(maxValues)
df <- wa$meta[ids,c('siteName','countyName')]
df$max_pm25 <- maxValues
print(df)                 siteName   countyName max_pm25
530770005         Sunnyside-S 16th       YAKIMA    193.0
530270011   Taholah-Quinault Tribe GRAYS HARBOR    141.2
530610020        Darrington-Fir St    SNOHOMISH     93.6
530630021 Spokane-Augusta Ave (SO)      SPOKANE     83.0
530611007       Marysville-7th Ave    SNOHOMISH     81.8
530090017    Port Angeles-E 5th St      CLALLAM     79.7
```


## Publish the Docker Image ##

```
docker login
...
docker push mazamascience/pwfslsmoke:v1.0.12
```


## Download the Docker Image ##

A recent image can also be obtained from DockerHub with:

```
docker pull mazamascience/pwfslsmoke:v1.0.12
```

