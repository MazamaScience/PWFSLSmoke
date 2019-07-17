## Create the Docker Image ##

A quick refresher on docker commands is available at the [docker cheatsheet](https://github.com/wsargent/docker-cheat-sheet).

A docker image with all required prerequisites can be built with the `Makefile` in this directory:

```
make production_build
```

You should then be able to see something like the following:

```
$ docker images
REPOSITORY                          TAG                     IMAGE ID            CREATED             SIZE
mazamascience/pwfslsmoke            1.2.11                  1824e396e3f3        34 seconds ago      2.61GB
mazamascience/pwfslsmoke            latest                  1824e396e3f3        34 seconds ago      2.61GB
...
```

> It is best practice to create versioned images and tag the most recent one with "latest".

Spatial data required by the **MazamaSpatialUtils** package already exists in 
the docker image in `/home/mazama/data/Spatial`.


## Test the Docker Image ##

Having built the docker image we can now test it. The following output was 
obtained on April 03, 2019:

```
docker run -ti mazamascience/pwfslsmoke R --vanilla
...
library(PWFSLSmoke)
wa <- airnow_loadLatest() %>%
  monitor_subset(stateCodes='WA')
maxValues <- sort(apply(wa$data[,-1], 2, max, na.rm=TRUE), decreasing=TRUE)[1:6]
ids <- names(maxValues)
df <- wa$meta[ids,c('siteName','countyName')]
df$max_pm25 <- maxValues
print(df)
                               siteName countyName max_pm25
840530390006_01     Mobile-White Salmon  Klickitat     63.2
TT1010003_01                  Inchelium       <NA>     51.8
530650002_01    Wellpinit-Spokane Tribe    Stevens     51.3
840530739992_01   Maple Falls-Azure Way    Whatcom     49.7
530070011_01         Wenatchee-Fifth St     Chelan     49.4
530610020_01          Darrington-Fir St  Snohomish     49.0
```


## Publish the Docker Image ##

```
docker login
...
docker push mazamascience/pwfslsmoke:1.2.11
```


## Download the Docker Image ##

A recent image can also be obtained from DockerHub with:

```
docker pull mazamascience/pwfslsmoke:1.2.11
```

