FROM mazamascience/spatialutils:0.7.6

#######################################################################
# For PWFSLSmoke

# Install extra R packages from CRAN
RUN install2.r --error -r "https://cran.rstudio.com" \
    optparse \
    PWFSLSmoke

# Install latest updates from GitHub
#RUN installGithub.r \
#    mazamascience/PWFSLSmoke

