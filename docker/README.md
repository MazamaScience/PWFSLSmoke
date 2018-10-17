## Create the Docker Image ##

A quick refresher on docker commands is available at the [docker cheatsheet](https://github.com/wsargent/docker-cheat-sheet).

A docker image with all required prerequisites can be built with the `Makefile` in this directory:

```
make operational_build
```

This is just shorthand for the following `docker build` line:

```
$ docker build --no-cache -t mazamascience/pwfslsmoke:1.0.34 mazamascience/pwfslsmoke:latest-2018 .
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
docker run -ti mazamascience/pwfslsmoke:1.0.34 R --vanilla
...
library(PWFSLSmoke)
wrcc <- wrcc_loadLatest()
df <- monitor_currentData(wrcc)
df$monitoringSiteUrl
 [1] "http://tools.airfire.org/monitoring/v4/#!/?monitors=lon_.105.124_lat_39.714_wrcc.sm11"
 [2] "http://tools.airfire.org/monitoring/v4/#!/?monitors=lon_.105.124_lat_39.714_wrcc.sm13"
 [3] "http://tools.airfire.org/monitoring/v4/#!/?monitors=lon_.105.930_lat_40.077_wrcc.sm15"
 [4] "http://tools.airfire.org/monitoring/v4/#!/?monitors=lon_.105.124_lat_39.714_wrcc.sm19"
 [5] "http://tools.airfire.org/monitoring/v4/#!/?monitors=lon_.106.098_lat_40.074_wrcc.sm21"
 [6] "http://tools.airfire.org/monitoring/v4/#!/?monitors=lon_.105.124_lat_39.714_wrcc.sm24"
 [7] "http://tools.airfire.org/monitoring/v4/#!/?monitors=lon_.105.124_lat_39.714_wrcc.sm69"
 [8] "http://tools.airfire.org/monitoring/v4/#!/?monitors=lon_.105.124_lat_39.714_wrcc.s215"
 [9] "http://tools.airfire.org/monitoring/v4/#!/?monitors=lon_.105.504_lat_39.514_wrcc.s216"
[10] "http://tools.airfire.org/monitoring/v4/#!/?monitors=lon_.108.516_lat_37.461_wrcc.e925"
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

