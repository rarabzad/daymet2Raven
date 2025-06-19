# Understanding NetCDF Files for Spatiotemporal Datasets

NetCDF (Network Common Data Form) is a widely-used file format designed for the efficient storage and sharing of array-oriented scientific data, especially in the Earth and atmospheric sciences.

## 📦 What is NetCDF?

NetCDF files store multidimensional data such as:

- Time series of satellite images
- Climate model outputs
- Oceanographic measurements
- Gridded environmental data

These datasets often span multiple **dimensions**, most commonly:
- `time`
- `latitude`
- `longitude`
- (sometimes `altitude` or `depth`)

---

## 🧱 Data Structure

NetCDF organizes data into variables and dimensions. Each variable (e.g., temperature, precipitation) is an N-dimensional array indexed by dimensions such as `time`, `lat`, and `lon`.

Below is a schematic illustration of a spatiotemporal dataset stored in a 3D NetCDF cube:

                              ↑ Time
                     +---+---+---+---+
                    / * / * / * / * /|
                   +---+---+---+---+ |
                  / * / * / * / * /| |
                 +---+---+---+---+ | |
                / * / * / * / * /| | |
               +---+---+---+---+ | | |
              / * / * / * / * /| | | |
             +---+---+---+---+ | | | +
            | * | * | * | * | | | | +----→ Lon
            |   |   |   |   | | | |/
            |   |   |   |   | | |/
            +---+---+---+---+ | +
            | * | * | * | * | | |/
            +---+---+---+---+ +
            | * | * | * | * | |/
            +---+---+---+---+
            | * | * | * | * |/
            +---+---+---+---+
         ↑ Lat

Each "slice" is a 2D grid: Latitude × Longitude

> 🗺️ This cube represents one variable (e.g., temperature). Each horizontal slice is a 2D spatial grid at a specific time step.

---

## 📂 File Components

A NetCDF file typically contains:

- **Dimensions**: Named sizes of axes (e.g., `time = 100`, `lat = 50`, `lon = 75`)
- **Variables**: Multidimensional arrays associated with dimensions (e.g., `temperature[time][lat][lon]`)
- **Attributes**: Metadata about the file or variables (e.g., units, description, source)

---

## 🛠️ Example Structure

Here’s an example of what a NetCDF structure might look like:

netcdf example { dimensions: time = 12 ; lat = 180 ; lon = 360 ;

variables: float temperature(time, lat, lon) ; temperature:units = "Kelvin" ; temperature:long_name = "Monthly Mean Temperature" ;

float precipitation(time, lat, lon) ; precipitation:units = "mm" ; precipitation:long_name = "Monthly Total Precipitation" ;

double time(time) ; time:units = "days since 2000-01-01" ;

float lat(lat) ; lat:units = "degrees_north" ;

float lon(lon) ; lon:units = "degrees_east" ; }

---

## 📊 Why Use NetCDF?

✅ **Self-describing**: Contains metadata  
✅ **Portable**: Platform-independent binary format  
✅ **Efficient**: Supports compression and chunking  
✅ **Widely Supported**: Tools exist in Python (`xarray`, `netCDF4`), R, MATLAB, C/C++, and more

---

## 📚 Learn More

- [NetCDF Format Specification](https://www.unidata.ucar.edu/software/netcdf/docs/)
- [xarray (Python package)](https://docs.xarray.dev/en/stable/)
- [Panoply Viewer](https://www.giss.nasa.gov/tools/panoply/) – GUI for NetCDF files

---

## 🧪 Example Code (Python + xarray)

```python
import xarray as xr

# Open a NetCDF file
ds = xr.open_dataset("example.nc")

# Explore the dataset
print(ds)

# Access a variable
temperature = ds["temperature"]

# Plot the first timestep
temperature.isel(time=0).plot()
```

# daymet2Raven_nc Function

The `daymet2Raven_nc` function is designed to process spatial climate data from the Daymet database and prepare it for use in the Raven Hydrological Modelling Framework. It generates a NetCDF file and grid weight information, integrating spatial data from a shapefile with gridded climatic variables.

# 📡 Daymet Dataset Overview

Daymet provides high-resolution, gridded estimates of daily weather parameters across North America. These data are widely used in hydrological, ecological, and climate modeling.

---

## 🌍 Dataset Description

- **Source:** [ORNL DAAC – Daymet](https://daymet.ornl.gov/)
- **Coverage:** North America (including Hawaii and Puerto Rico)
- **Spatial Resolution:** 1 km x 1 km
- **Temporal Coverage:** 1980 – Present (updates annually)
- **Temporal Resolution:** Daily
- **Projection:** Lambert Conformal Conic (LCC)

### 📦 Available Variables

| Variable | Description                       | Units             |
|----------|-----------------------------------|-------------------|
| `prcp`   | Daily total precipitation         | mm/day            |
| `tmin`   | Daily minimum 2-m air temperature | °C                |
| `tmax`   | Daily maximum 2-m air temperature | °C                |
| `srad`   | Daily incident shortwave radiation| W/m²              |
| `vp`     | Daily average vapor pressure      | Pa                |
| `dayl`   | Day length                        | seconds           |
| `swe`    | Snow water equivalent (Alaska only)| kg/m²            |

---

## 📁 Data Access Methods

### 1. Web Interface
- Browse and download from: [https://daymet.ornl.gov/](https://daymet.ornl.gov/)

### 2. THREDDS / OPeNDAP
- Use NetCDF via remote access:  
  `https://thredds.daac.ornl.gov/thredds/catalog/ornldaac/Daymet/catalog.html`

## Function Overview

**Purpose:** Download Daymet climate data for specified locations, generate spatial data in NetCDF format, and create grid weight files necessary for hydrological modeling in Raven.

**Basic Usage:**

```r
daymet2Raven_nc(hru_shp_file,
                start_date,
                end_date,
                grid_size,
                lat = NULL,
                lon = NULL,
                HRU_ID = "HRU_ID",
                outdir = getwd(),
                plot = T)
```

**Parameters:**

- **`hru_shp_file`**: Path to the shapefile representing the hydrological response units (HRUs).
- **`start_date`**: The starting date for data retrieval in `YYYY-MM-DD` format.
- **`end_date`**: The ending date for data retrieval in `YYYY-MM-DD` format.
- **`grid_size`**: Desired size of the grid cells in decimal degrees.
- **`lat`**: required when `"grid_size"` has not been provided. A vector of latitude(s) where data are collected.
- **`lon`**: required when `"grid_size"` has not been provided. A vector of longitude(s) where data are collected.
- **`HRU_ID`**: Column name in the shapefile that contains unique HRU identifiers (default is `"HRU_ID"`).
- **`outdir`**: directory where the output files go there, default is set to the currect directory.
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
plot<-T

# Run the function
daymet2Raven_nc(hru_shp_file = hru_shp_file,
                start_date = start_date,
                end_date = end_date,
                grid_size = grid_size,
                HRU_ID = HRU_ID,
                plot = plot)
```

## Author

Rezgar Arabzadeh, Sept 2024
