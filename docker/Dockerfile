FROM mazamascience/spatialutils:v0.4.9

#######################################################################
# For PWFSLSmoke
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    libv8-dev \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/ \
  && rm -rf /tmp/downloaded_packages/ /tmp/*.rds \
  && installGithub.r \
    mazamascience/PWFSLSmoke

