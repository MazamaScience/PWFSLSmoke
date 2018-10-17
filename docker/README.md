## Create the Docker Image ##

A quick refresher on docker commands is available at the [docker cheatsheet](https://github.com/wsargent/docker-cheat-sheet).

A docker image with all required prerequisites can be built with the `Makefile` in this directory:

```
make operational_build
```

This is just shorthand for the following `docker build` line:

```
$ docker build --no-cache -t mazamascience/pwfslsmoke:1.0.34 -t mazamascience/pwfslsmoke:latest-2018 .
$ docker images
REPOSITORY                     TAG                 IMAGE ID            CREATED             SIZE
mazamascience/pwfslsmoke       latest-2018         f4945f0c24e6        4 minutes ago       1.75GB
mazamascience/pwfslsmoke       1.0.34              f4945f0c24e6        4 minutes ago       1.75GB
...
```

> It is best practice to create versioned images and tag the most recent one with "latest-2018".

Spatial data required by the **MazamaSpatialUtils** package already exists in the docker image in `/home/mazama/data/Spatial`.


## Test the Docker Image ##

Having built the docker image we can now test it. The following output was obtained on May 08, 2018:

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
530650002_01       Wellpinit-Spokane Tribe    Stevens    125.0
530330030_01         Seattle-10th & Weller       King     28.0
530639999_01 Airway Heights-West 12th (US)    Spokane     26.0
530470010_01           Winthrop-Chewuch Rd   Okanogan     25.8
530331011_01            Seattle-South Park       King     25.5
530330057_01              Seattle-Duwamish       King     25.1
```


## Publish the Docker Image ##

```
docker login
...
docker push mazamascience/pwfslsmoke:1.0.34
```


## Download the Docker Image ##

A recent image can also be obtained from DockerHub with:

```
docker pull mazamascience/pwfslsmoke:1.0.34
```

