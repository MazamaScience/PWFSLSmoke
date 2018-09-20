################################################################################
# Makefile for building and running docker containers

# NOTE:  The commands that are echoed to the screen will contain this string but
# NOTE:  the actual commands that are executed will evalute the backticked command.
DATE=`date --rfc-3339='date'`


# PRODUCTION version -----------------------------------------------------------
production_build:
	docker build --no-cache -t mazamascience/pwfslsmoke:1.0.33 -t mazamascience/pwfslsmoke:latest .

