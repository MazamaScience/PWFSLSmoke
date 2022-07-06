################################################################################
# Makefile for building and running docker containers
#
# Version 1.3.x uses mazamascience/spatialutils:0.7.6

# PRODUCTION version -----------------------------------------------------------
production_build:
	docker build --no-cache -t mazamascience/pwfslsmoke:1.3.1 -t mazamascience/pwfslsmoke:latest .

production_publish:
	docker login && docker push mazamascience/pwfslsmoke:1.3.1
