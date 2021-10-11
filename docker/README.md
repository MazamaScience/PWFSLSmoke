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
mazamascience/pwfslsmoke            1.2.116                 1824e396e3f3        34 seconds ago      2.61GB
mazamascience/pwfslsmoke            latest                  1824e396e3f3        34 seconds ago      2.61GB
...
```

> It is best practice to create versioned images and tag the most recent one with "latest".

Spatial data required by the **MazamaSpatialUtils** package already exists in 
the docker image in `/home/mazama/data/Spatial`.


## Test the Docker Image ##

Having built the docker image we can now test it. The following output was 
obtained on July 08, 2020 -- Fourth of July Fireworks!!!:


```
docker run -ti --rm mazamascience/pwfslsmoke R --vanilla
...
library(PWFSLSmoke)
wa <- airnow_loadLatest() %>%
  monitor_subset(stateCodes='WA')
maxValues <- sort(apply(wa$data[,-1], 2, max, na.rm=TRUE), decreasing=TRUE)[1:6]
ids <- names(maxValues)
df <- wa$meta[ids,c('siteName','countyName')]
df$max_pm25 <- maxValues
print(df)
                          siteName   countyName max_pm25
530330080_01   Seattle-Beacon Hill         King    943.0
530272002_01  Aberdeen-Division St Grays Harbor    203.7
530611007_01    Marysville-7th Ave    Snohomish    134.0
530650005_01     Colville-E 1st St      Stevens    123.0
530110024_01 Vancouver-NE 84th Ave        Clark    115.0
530350007_01  Bremerton-Spruce Ave       Kitsap    113.0
```


## Publish the Docker Image ##

```
make production_publish
```

## Download the Docker Image ##

This image can also be pulled from DockerHub with:

```
docker pull mazamascience/pwfslsmoke:1.2.116
```

