FROM mazamascience/pwfslsmoke

#######################################################################
# For the spatialUtilsApp
RUN install2.r --error -r "https://cran.rstudio.com" \
  && installGithub.r \
    bart6114/jug

#######################################################################
# jug instance configuration
ENV JUG_HOST 0.0.0.0
ENV JUG_PORT 8080

EXPOSE 8080

COPY latestDataApp.R .

CMD [ "Rscript", "latestDataApp.R" ]

