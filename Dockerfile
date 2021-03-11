FROM rocker/shiny-verse:4.0.2

RUN apt-get update && \
    apt-get install -y --no-install-recommends curl

RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site

# Install CRAN ggquickeda
RUN install2.r ggquickeda

# Install Additional packages required for Aridhia
RUN install2.r shinyFiles RPostgres

# Copy over ggquickeda Shiny app
COPY inst/shinyapp /home/ggquickeda

# Required for the Workspace integrations
EXPOSE 8080

# Set up entrypoint
COPY docker/mini-app /usr/bin/mini-app
RUN chmod 755 /usr/bin/mini-app

# Run
ENTRYPOINT /usr/bin/mini-app
