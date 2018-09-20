FROM mazamascience/spatialutils:0.5.4

#######################################################################
# For PWFSLSmoke
RUN apt-get update \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/ \
  && rm -rf /tmp/downloaded_packages/ /tmp/*.rds \
  ## Install R packages from CRAN
  && install2.r --error -r "https://cran.rstudio.com" \
    PWFSLSmoke

# For the latest updates in PWFSLSmoke
RUN installGithub.r \
    mazamascience/pwfslsmoke

