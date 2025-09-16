# Use the official R Shiny image as base
FROM rocker/shiny:latest

# Install system dependencies for sf, zip, etc.
RUN apt-get update && apt-get install -y \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    unzip \
    && rm -rf /var/lib/apt/lists/*

# Install R packages needed for the app
RUN R -e "install.packages(c('shiny', 'shinyjs', 'sf', 'zip', 'httr', 'raster'), repos='https://cloud.r-project.org/')"

# Copy app files into the container
# Assumes your app.R and logo.png are in the same folder as the Dockerfile
COPY app.R /srv/shiny-server/
COPY logo.png /srv/shiny-server/

# Set working directory
WORKDIR /srv/shiny-server/

# Expose port 3838 (Shiny default)
EXPOSE 3838

# Set environment variable for Shiny timeout
ENV SHINY_SESSION_TIMEOUT=300

# Run the Shiny app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/', host='0.0.0.0', port=3838)"]
