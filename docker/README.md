## Create a Docker Image ##

A quick refresher on docker commands is available at the [docker cheatsheet](https://github.com/wsargent/docker-cheat-sheet).

A docker image with all required prerequisites can be built with the `Dockerfile` in this directory:

```
$ docker build --no-cache -t mazamascience/pwfslsmoke:v0.99.16 .
$ docker tag mazamascience/pwfslsmoke:v0.99.15 mazamascience/pwfslsmoke:latest
$ docker images
REPOSITORY                   TAG                 IMAGE ID            CREATED             SIZE
mazamascience/pwfslsmoke     latest              964859fc3d6c        21 minutes ago      1.258 GB
mazamascience/pwfslsmoke     v0.99.16            964859fc3d6c        21 minutes ago      1.258 GB
mazamascience/spatialutils   v0.4.9              be5b4f8f8ca5        22 hours ago        1.193 GB
...
```

> It is best practice to create versioned images and tag the most recent one with "latest".

Spatial data required by the **MazamaSpatialUtils** package already exists in the docker image in `/home/mazama/data/Spatial`.

## Download a Docker Image ##

A recent image can also be obtained from DockerHub with:

```
docker pull mazamascience/pwfslsmoke:v0.99.16
```

## Docker Run ##

Having built the docker image we can now test it with:

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
print(df)
                    siteName countyName max_pm25
530610020  Darrington-Fir St  SNOHOMISH     92.8
530470009    Twisp-Glover St     CHELAN     69.2
530770009     Yakima-4th Ave     YAKIMA     59.0
530730015 Bellingham-Yew St     WHATCOM     58.3
530530029        Tacoma-L St     PIERCE     57.3
530370002 Ellensburg-Ruby St     CHELAN     55.0
```

