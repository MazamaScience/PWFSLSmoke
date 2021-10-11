################################################################################
# Makefile for building and running docker containers

# PRODUCTION version -----------------------------------------------------------
production_build:
	docker build --no-cache -t mazamascience/pwfslsmoke:1.2.116 -t mazamascience/pwfslsmoke:latest .

production_publish:
	docker login && docker push mazamascience/pwfslsmoke:1.2.116
