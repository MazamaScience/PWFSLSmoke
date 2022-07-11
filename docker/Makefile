################################################################################
# Makefile for building and running docker containers
#
# Version 1.2.117 uses R 4.1.1 and mazamascience/spatialutils:0.7.6

# PRODUCTION version -----------------------------------------------------------
production_build:
	docker build --no-cache -t mazamascience/pwfslsmoke:1.2.117 -t mazamascience/pwfslsmoke:latest .

production_publish:
	docker login && docker push mazamascience/pwfslsmoke:1.2.117
