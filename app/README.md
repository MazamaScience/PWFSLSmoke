# Web Service App #

This directory contains code to set up a simple web service based on the functionality
provided by the **PWFSLSmoke** package. It is built on top of the **jug** package

From https://github.com/Bart6114/jug:

> Jug is a small web development framework for R which relies heavily upon the `httpuv` package.
> It’s main focus is to make > building APIs for your code as easy as possible.
> 
> Jug is not supposed to be either an especially performant nor an uber stable web framework.
> Other tools (and languages) might be more suited for that. It focuses on maximizing the ease
> with wich you can create web APIs for your R code. However, the flexibility of Jug means that,
> in theory, you could built an extensive web framework with it.

The `latestDataApp.R` file uses **jug** (similar to python [Flask](http://flask.pocoo.org)) to create a set of URL paths that
demonstrate functionality available in the package. In this minimal example we provide functionality at the following paths:

 * `airnow`
 * `airsis`
 * `wrcc`
 
Each URL accepts arguments of the form `?state=<state code>` and will print out a summary of 
latest data available form each source of PM2.5 data.

The accompanying `Dockerfile` lets you build a self-contained webservice image that can be managed by docker.

Here is a quick exmaple of how to build and launch the webservice on localhost port 8080

```
$ docker build -t latestdata-app:latest .
...
$ docker images
REPOSITORY                   TAG                 IMAGE ID            CREATED             SIZE
mazamascience/pwfslsmoke     latest              964859fc3d6c        About an hour ago   1.26 GB
mazamascience/pwfslsmoke     v0.99.9             964859fc3d6c        About an hour ago   1.26 GB
latestdata-app               latest              64295f80b8f9        19 hours ago        1.26 GB
...
$ docker run --rm -p 8080:8080 latestdata-app
Loading required package: magrittr

Attaching package: ‘jug’

The following object is masked from ‘package:base’:

    get

Loading required package: dplyr

Attaching package: ‘dplyr’

The following objects are masked from ‘package:stats’:

    filter, lag

The following objects are masked from ‘package:base’:

    intersect, setdiff, setequal, union

Loading required package: maps
Loading required package: MazamaSpatialUtils
Serving the jug at http://0.0.0.0:8080
```

Now you can try the following URLs in your browser:

http://localhost:8080/airnow?state=id

```
monitorID,stateCode,siteName,maxPM25,longitude,latitude
160570005,ID,Moscow,7.0,-116.954597,46.728199
160690014,ID,Reubens-ID,3.2,-116.535797,46.3349
160850002,ID,McCall,8.7,-116.1065,44.890197
160210002,ID,Bonners Ferry Kootenai Tribe of Idaho,7.7,-116.368965,48.705551
160150002,ID,Garden Valley,12.5,-115.972386,44.104498
160090010,ID,St. Maries,20.0,-116.570297,47.3167
160690013,ID,Lapwai-ID,5.7,-116.806198,46.397202
160830007,ID,Twin Falls PM25,13.0,-114.494717,42.565089
160530003,ID,Minidoka National Historic Site,8.0,-114.2525,42.68
160690012,ID,Lewiston,8.0,-116.991402,46.408501
160790017,ID,Pinehurst,34.0,-116.236867,47.536712
160010010,ID,St. Lukes Meridian,6.0,-116.347853,43.600699
160490003,ID,Kamiah-ID,7.8,-116.027496,46.2094
160170003,ID,Sandpoint U of I,10.0,-116.556557,48.291821
160050015,ID,Pocatello Garrett and Gould,8.1,-112.460297,42.876701
160270002,ID,Nampa Fire Station,10.0,-116.562676,43.58031
160590004,ID,Salmon PM2.5,15.0,-113.892998,45.178699
160150001,ID,Idaho City,17.0,-115.838557,43.823017
160550003,ID,Lancaster,8.0,-116.8044,47.7889
160090011,ID,Plummer CDA Tribe,19.2,-116.885902,47.3386
160190011,ID,Idaho Falls / Penford,10.0,-112.053101,43.4683
160410001,ID,Franklin,21.0,-111.80916,42.013333
160490002,ID,Grangeville,28.0,-116.115196,45.931301
160050020,ID,Shoshone Bannock Tribes,9.1,-112.458889,42.589833
```

http://localhost:8080/airsis

```
monitorID,stateCode,siteName,maxPM25,longitude,latitude
USFS1041__001,NM,Santa Fe-Dale Ball Trailhead,-Inf,-105.895628929138,35.6870484352112
X1026.Kernville.EBAM__002,CA,Kernville-Buena Vista Drive,-Inf,-118.408713340759,35.7688236236572
X1026.Kernville.EBAM__001,CA,Kernville-Sierra Way,20.0,-118.417489528656,35.755090713501
X1013.Johnsondale__001,CA,Camp Nelson-Abandoned County Road,-Inf,-118.541278839111,35.9698390960693
```

http://localhost:8080/wrcc

```
monitorID,stateCode,siteName,maxPM25,longitude,latitude
Smoke..1__001,WA,Usk-Honeysuckle Drive,8.0,-117.26,48.344
Smoke..22__001,CO,Dolores-Colorado 184,1.0,-108.51,37.461
Smoke.USFS.R1.52__001,MT,Eureka-Zach Road,18.0,-115.05,48.898
Smoke.USFS.R2.69__001,CO,Lake George-Highway 24,2.0,-105.34,38.979
Smoke.USFS.R2.923__001,CO,Denver-South Kipling Street,-Inf,-105.11,39.714
Smoke.USFS.R2.924__001,CO,Denver-South Kipling Street,41.0,-105.11,39.714
```
