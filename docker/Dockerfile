FROM mazamascience/spatialutils:0.6.4

#######################################################################
# For PWFSLSmoke
RUN apt-get update \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/ \
  && rm -rf /tmp/downloaded_packages/ /tmp/*.rds \
  ## Install R packages from CRAN
  && install2.r --error -r "https://cran.rstudio.com" \
    PWFSLSmoke \
    optparse

# For the latest updates in PWFSLSmoke
RUN installGithub.r \
    mazamascience/MazamaCoreUtils \
    milesmcbain/slippymath \
    mazamascience/PWFSLSmoke

