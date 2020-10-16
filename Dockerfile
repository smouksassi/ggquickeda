FROM rocker/shiny-verse:4.0.2

RUN apt-get update && \
    apt-get install -y --no-install-recommends curl

RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site

RUN install2.r colourpicker Formula GGally ggpmisc ggrepel ggstance ggpubr \
    gridExtra Hmisc plotly quantreg scales shinyjs shinyjqui survminer table1 \
    shinyFiles RPostgres

COPY data /home/shiny/pkg/data
COPY inst /home/shiny/pkg/inst
COPY man /home/shiny/pkg/man
COPY R /home/shiny/pkg/R
COPY DESCRIPTION /home/shiny/pkg/DESCRIPTION
COPY NAMESPACE /home/shiny/pkg/NAMESPACE
COPY .Rbuildignore /home/shiny/pkg/.Rbuildignore

RUN R -e "devtools::install('/home/shiny/pkg')"

# install shiny app
RUN rm -rf /srv/shiny-server/*
RUN cp -r /home/shiny/pkg/inst/shinyapp/* /srv/shiny-server
RUN rm -rf /tmp/*

# Required for the Workspace integrations
EXPOSE 8080

# Set up entrypoint
COPY docker/mini-app /usr/bin/mini-app
RUN chmod 755 /usr/bin/mini-app

# run
# CMD /usr/bin/mini-app && tail -f /dev/null
ENTRYPOINT /usr/bin/mini-app
