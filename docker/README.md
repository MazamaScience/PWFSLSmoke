## Create the Docker Image ##

A quick refresher on docker commands is available at the [docker cheatsheet](https://github.com/wsargent/docker-cheat-sheet).

A docker image with all required prerequisites can be built with the `Makefile` in this directory:

```
make operational_build
```

This is just shorthand for the following `docker build` line:

```
$ docker build --no-cache -t mazamascience/pwfslsmoke:v1.0.18 -t mazamascience/pwfslsmoke:latest .
$ docker images
REPOSITORY                     TAG                 IMAGE ID            CREATED             SIZE
mazamascience/pwfslsmoke       latest              f4945f0c24e6        4 minutes ago       1.75GB
mazamascience/pwfslsmoke       v1.0.18             f4945f0c24e6        4 minutes ago       1.75GB
...
```

> It is best practice to create versioned images and tag the most recent one with "latest".

Spatial data required by the **MazamaSpatialUtils** package already exists in the docker image in `/home/mazama/data/Spatial`.


## Test the Docker Image ##

Having built the docker image we can now test it. The following output was obtained on Marh 06, 2018:

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
530611007_01       Marysville-7th Ave  Snohomish     55.8
530530029_01              Tacoma-L St     Pierce     46.4
530630021_01 Spokane-Augusta Ave (SO)    Spokane     42.0
530610020_01        Darrington-Fir St  Snohomish     39.5
530610005_01           Lynnwood-212th  Snohomish     38.8
530470009_01          Twisp-Glover St     Chelan     38.0
```


## Publish the Docker Image ##

```
docker login
...
docker push mazamascience/pwfslsmoke:v1.0.18
```


## Download the Docker Image ##

A recent image can also be obtained from DockerHub with:

```
docker pull mazamascience/pwfslsmoke:v1.0.18
```

