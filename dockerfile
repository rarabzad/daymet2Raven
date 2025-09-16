# Use the official R Shiny image as base
FROM rocker/shiny:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    unzip \
    git \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c(
    'shiny', 'shinyjs', 'sf', 'dplyr', 'progress', 'ncdf4', 'rmapshaper', 
    'lubridate', 'imputeTS', 'raster', 'daymetr', 'zip'
), repos='https://cloud.r-project.org/')"

# Copy app files
COPY app.R /srv/shiny-server/

# Expose port 3838 for Shiny
EXPOSE 3838

# Run the Shiny app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/', host='0.0.0.0', port=3838)"]
