daymet2Raven_nc<-function(hru_shp_file,
                          start_date,
                          end_date,
                          grid_size,
                          HRU_ID="HRU_ID",
                          nc_file="RavenInput.nc",
                          grid_weight_file="weights.txt")
{
  library(daymetr)
  library(sf)
  library(dplyr)
  library(progress)
  library(ncdf4)
  library(rmapshaper)
  library(lubridate)
  hru <- st_make_valid(st_transform(st_read(hru_shp_file), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs"))[,HRU_ID]
  boundary <- ms_simplify(st_cast(st_simplify(st_union(hru),dTolerance = sum(area(as_Spatial(hru)))/1e6/20),"POLYGON"),0.99)
  buffered_boundary <- st_buffer(boundary, dist = grid_size*1e5 / 2,singleSide = T)
  r<-raster(extent(as_Spatial(buffered_boundary)),resolution=grid_size,crs=crs(buffered_boundary))
  r[]<-rnorm(prod(dim(r)))
  grid_cells<-rasterToPolygons(r)
  locations<-as.data.frame(round(coordinates(grid_cells),4))
  colnames(locations)<-c("Longitude","Latitude")
  locations<-locations[order(locations$Latitude,decreasing = T),][order(locations$Longitude),]
  tmin_list <- list()
  tmax_list <- list()
  prcp_list <- list()
  altitude_list <- numeric(length = nrow(locations))
  time_list <- NULL
  pb <- progress_bar$new(
    format = "  Downloading [:bar] :percent in :elapsed",
    total = nrow(locations),
    clear = FALSE,
    width = 60
  )
  for (i in 1:nrow(locations))
  {
    pb$tick()
    lat <- locations$Latitude[i]
    lon <- locations$Longitude[i]
    daymet_data <- download_daymet(
      lat = lat,
      lon = lon,
      start = as.numeric(substr(start_date, 1, 4)),
      end = as.numeric(substr(end_date, 1, 4)),
      internal = TRUE,silent = T)
    tmin_list[[i]] <- daymet_data$data$tmin..deg.c.
    tmax_list[[i]] <- daymet_data$data$tmax..deg.c.
    prcp_list[[i]] <- daymet_data$data$prcp..mm.day.
    altitude_list[i] <- daymet_data$altitude
  }
  time_list<-as.Date(paste0(daymet_data$data$year,"-01-01"))+days(daymet_data$data$yday)
  lat<-unique(locations$Latitude)
  lon<-unique(locations$Longitude)
  num_lats <- length(lat)
  num_lons <- length(lon)
  num_times <- length(time_list)
  prcp_array<-tmin_array <-tmax_array<-array(NA,dim = c(num_lats,num_lons,num_times),dimnames = list(lat,lon,time_list))
  altitude_array<-prcp_array[,,1]
  for(i in 1:length(lon))
  {
    for(j in 1:length(lat))
    {
      id<-which(as.numeric(colnames(prcp_array)[i]) == locations$Longitude & as.numeric(rownames(prcp_array)[j]) == locations$Latitude)
      prcp_array[j,i,]<-prcp_list[[id]]
      tmin_array[j,i,]<-tmin_list[[id]]
      tmax_array[j,i,]<-tmax_list[[id]]
      altitude_array[j,i]<-altitude_list[id]
    }
  }
  lon_dim  <- ncdim_def("lon", "degrees_east", lon)
  lat_dim  <- ncdim_def("lat", "degrees_north", lat)
  time_dim <- ncdim_def("time", sprintf("days since %s",time_list[1]), seq_along(time_list))
  tmin_var <- ncvar_def("tmin", "degrees_C", list(lat_dim,lon_dim, time_dim), missval = NA)
  tmax_var <- ncvar_def("tmax", "degrees_C", list(lat_dim,lon_dim, time_dim), missval = NA)
  prcp_var <- ncvar_def("prcp", "mm/day", list(lat_dim,lon_dim , time_dim), missval = NA)
  altitude_var <- ncvar_def("altitude", "meters", list(lat_dim, lon_dim), missval = NA)
  nc <- nc_create(nc_file, list(tmin_var, tmax_var, prcp_var, altitude_var))
  ncvar_put(nc, tmin_var, tmin_array)
  ncvar_put(nc, tmax_var, tmax_array)
  ncvar_put(nc, prcp_var, prcp_array)
  ncvar_put(nc, altitude_var, altitude_array)
  nc_close(nc)
  grid_cells$Cell_ID<-(1:nrow(grid_cells))-1
  grid_cells$layer<-NULL
  grid_hru<-as_Spatial(st_intersection(st_as_sf(grid_cells),hru))
  grid_hru$area<-area(grid_hru)
  weight_data<-grid_hru@data
  hru_id<-unique(weight_data$HRU_ID)
  weight_data$weight<-NA
  for(i in 1:length(hru_id))
  {
    id<-hru_id[i]==weight_data$HRU_ID
    weight_data[id,]$weight<-weight_data[id,]$area/sum(weight_data[id,]$area)
  }
  weight_data$area<-NULL
  L1<-":GridWeights"
  L2<-sprintf(":NumberHRUs\t%s",length(unique(weight_data$HRU_ID)))
  L3<-sprintf(":NumberGridCells\t%s",nrow(grid_cells))
  L4<-"# [HRU ID]\t[Cell #]\t[w_kl]"
  Lweights<-apply(weight_data[,c(2,1,3)],1,paste,collapse="\t")
  Lend<-":EndGridWeights"
  weights_mat_data<-c(L1,L2,L3,L4,Lweights,Lend)
  writeLines(weights_mat_data,grid_weight_file)
  var_names <- names(nc$var)
  create_info_block <- function(var_name) {
    var_info <- nc$var[[var_name]]
    dim_names <- sapply(var_info$dim, function(d) d$name)
    info_block <- c(
      paste0(":GriddedForcing \t\t\t ", var_name),
      paste0("\t:ForcingType \t\t\t ", "TBD"),
      paste0("\t:FileNameNC \t\t\t ", "YourNetCDFFileName.nc"),
      paste0("\t:VarNameNC \t\t\t ", var_name),
      paste0("\t:DimNamesNC \t\t\t ", paste(dim_names, collapse = ", ")),
      paste0("\t:ElevationVarNameNC \t\t ", ifelse("altitude" %in% var_names, "altitude", "none")),
      paste0("\t:RedirectToFile \t\t ", "grid_weights_path"),
      ":EndGriddedForcing\n"
    )
    return(info_block)
  }
  all_blocks <- unlist(lapply(var_names, create_info_block))
  writeLines(all_blocks, con = "model.rvt")
  writeLines(text = capture.output(nc),con = "nc_file_content.txt")
  cat("Done!")
}