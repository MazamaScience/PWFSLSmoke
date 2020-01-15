FROM mazamascience/spatialutils:0.6.1

#######################################################################
# For PWFSLSmoke
RUN apt-get update \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/ \
  && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

# Update to latest CRAN version of currently installed R packages
RUN r -e "update.packages(repos = 'https://cran.rstudio.com', \
                          ask = FALSE)"

# Install extra R packages from CRAN
RUN install2.r --error -r "https://cran.rstudio.com" \
    optparse \
    PWFSLSmoke

# Install latest updates from GitHub
RUN installGithub.r \
    mazamascience/MazamaCoreUtils \
    milesmcbain/slippymath \
    mazamascience/PWFSLSmoke

