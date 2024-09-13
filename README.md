# daymet2Raven_nc Function

The `daymet2Raven_nc` function is designed to process spatial climate data from the Daymet database and prepare it for use in the Raven Hydrological Modelling Framework. It generates a NetCDF file and grid weight information, integrating spatial data from a shapefile with gridded climatic variables.

## Function Overview

**Purpose:** Download Daymet climate data for specified locations, generate spatial data in NetCDF format, and create grid weight files necessary for hydrological modeling in Raven.

**Basic Usage:**

```r
daymet2Raven_nc(hru_shp_file,
                start_date,
                d_date,
                grid_size,
                HRU_ID="HRU_ID",
                nc_file="RavenInput.nc",
                grid_weight_file="weights.txt",
                plot=T)
```

**Parameters:**

- **`hru_shp_file`**: Path to the shapefile representing the hydrological response units (HRUs).
- **`start_date`**: The starting date for data retrieval in `YYYY-MM-DD` format.
- **`end_date`**: The ending date for data retrieval in `YYYY-MM-DD` format.
- **`grid_size`**: Desired size of the grid cells in decimal degrees.
- **`HRU_ID`**: Column name in the shapefile that contains unique HRU identifiers (default is `"HRU_ID"`).
- **`nc_file`**: The name of the output NetCDF file (default is `"RavenInput.nc"`).
- **`grid_weight_file`**: The name of the output file where grid weights will be saved (default is `"weights.txt"`).
- **`plot`**: logical, whether to plot the grid cells overlied by the `"hru_shp_file"`.

**How It Works:**

1. **Shapefile Processing:** Reads and transforms the input shapefile to a valid spatial format, then simplifies and buffers the boundary to create grid cells.

2. **Data Retrieval:** Downloads daily minimum temperature, maximum temperature, and precipitation data from the Daymet database for each grid cell.

3. **NetCDF Creation:** Organizes the retrieved climate data and altitude information into arrays. Writes the arrays into a NetCDF file with appropriate metadata.

4. **Grid Weight Calculation:** Intersects grid cells with HRUs to calculate area-based weights and writes the grid weights into a text file for Raven.

5. **Metadata Logging:** Generates metadata files containing the configuration of grid forcings and other relevant information.

## Installation

To use this function, ensure you have the following R libraries installed:

- `daymetr`
- `sf`
- `dplyr`
- `progress`
- `ncdf4`
- `rmapshaper`
- `lubridate`
- `imputeTS`
- `raster`

Install these packages using:

```r
install.packages(c("daymetr",
                   "sf",
                   "dplyr",
                   "progress",
                   "ncdf4",
                   "rmapshaper",
                   "lubridate",
                   "raster",
                   "imputeTS"))
```

## Example

```r
# Create a directory and set it as the working directory
dir.create("c:/daymet")
setwd("c:/daymet")

# Load the function
source("https://raw.githubusercontent.com/rarabzad/daymet2Raven/main/daymet2Raven_nc.R")
# Download and unzip HRU shapefile
download.file("https://github.com/rarabzad/RDRS/raw/main/data/hru.zip", "hru.zip")
unzip("hru.zip")
hru_shp_file <- "hru/finalcat_hru_info.shp"
HRU_ID <- "HRU_ID"
grid_size <- 0.1
start_date <- as.Date("2020-06-01")
end_date <- as.Date("2022-06-30")
grid_weight_file <- "weights.txt"
nc_file <- "RavenInput.nc"
plot<-T

# Run the function
daymet2Raven_nc(hru_shp_file,
                start_date,
                end_date,
                grid_size,
                HRU_ID,
                nc_file,
                grid_weight_file,
                plot)
```

## Author

Rezgar Arabzadeh, Sept 2024
