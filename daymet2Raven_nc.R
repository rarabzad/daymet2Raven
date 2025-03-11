daymet2Raven_nc<-function(hru_shp_file,
                          start_date,
                          end_date,
                          grid_size=NULL,
                          lat=NULL,
                          lon=NULL,
                          HRU_ID="HRU_ID",
                          outdir = getwd(),
                          plot=T)
{
  library(daymetr)
  library(sf)
  library(dplyr)
  library(progress)
  library(ncdf4)
  library(rmapshaper)
  library(lubridate)
  library(imputeTS)
  library(raster)
  if(!is.null(lat) & !is.null(lon)) grid_size<-c(mean(diff(lon)),mean(diff(lat)))
  if(!dir.exists(outdir)) dir.create(outdir)
  nc_file<-file.path(outdir,"RavenInput.nc")
  grid_weight_file<-file.path(outdir,"weights.txt")
  plot_file<-file.path(outdir,"plot.pdf")
  grid_file<-file.path(outdir,"grids_polygons.shp")
  grid_file_json<-file.path(outdir,"grids_polygons.json")
  rvt_file<-file.path(outdir,"model.rvt")
  nc_content_file<-file.path(outdir,"nc_file_content.txt")
  hru<-st_transform(st_read(hru_shp_file),crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs")
  get_utm_zone <- function(lon) floor((lon + 180) / 6) + 1
  lon <- mean(st_coordinates(st_centroid(hru))[, "X"])
  utm_zone <- get_utm_zone(lon)
  epsg_utm <- 32600 + utm_zone
  hru_projected <- st_transform(hru, epsg_utm)
  hru_valid <- st_make_valid(hru_projected)
  hru_simplified <- st_simplify(hru_valid, preserveTopology = TRUE, dTolerance = 100)
  hru_valid <- st_make_valid(hru_simplified)
  hru_union <- st_union(st_geometry(hru_valid))
  hru_buffer <- st_buffer(hru_union, dist = max(grid_size)*1e5 / 1.9 ,singleSide = T)
  hru_buffer_sf <- st_sf(geometry = hru_buffer)
  buffered_boundary <- st_transform(hru_buffer_sf, 4326)
  
  if(!is.null(lat) & !is.null(lon))
  {
    xmn<-min(lon)-abs(mean(diff(lon)))/2
    xmx<-max(lon)+abs(mean(diff(lon)))/2
    ymn<-min(lat)-abs(mean(diff(lat)))/2
    ymx<-max(lat)+abs(mean(diff(lat)))/2
    r<-raster(nrows=length(lat),
              ncols=length(lon),
              xmn=xmn,xmx=xmx,ymn=ymn,ymx=ymx,
              crs=crs(buffered_boundary))
    raster_extent <- extent(r)
    shape_extent <- extent(as_Spatial(boundary))
    if (!all(raster_extent@xmin <= shape_extent@xmin,
             raster_extent@xmax >= shape_extent@xmax,
             raster_extent@ymin <= shape_extent@ymin,
             raster_extent@ymax >= shape_extent@ymax))
    {
      stop("The raster does NOT cover the shapefile extent.")
    }
  }else{
    r<-raster(extent(as_Spatial(buffered_boundary)),
              resolution=grid_size,
              crs=crs(buffered_boundary))
  }
  r[]<-rnorm(prod(dim(r)))
  grid_cells<-rasterToPolygons(r)
  locations<-as.data.frame(round(coordinates(grid_cells),4))
  colnames(locations)<-c("Longitude","Latitude")
  locations<-locations[order(locations$Latitude,decreasing = T),][order(locations$Longitude),]
  tmin_list <- tmax_list <- prcp_list <- list()
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
  
  dta<-list(tmin_list,tmax_list,prcp_list)
  tmin_list<-dta[[1]];tmax_list<-dta[[2]];prcp_list<-dta[[3]]
  
  time_all<-seq(as.Date(start_date),as.Date(end_date),"day")
  time_list<-as.Date(paste0(daymet_data$data$year,"-01-01"))+days(daymet_data$data$yday-1)
  time_list_complete<-seq(as.Date(paste0(range(year(time_list))[1],"-01-01")),as.Date(paste0(range(year(time_list))[2],"-12-31")),"day")
  missing_leap_days<-which(is.na(match(time_list_complete,time_list)))
  if(length(missing_leap_days)>0)
  {
    for(j in 1:nrow(locations))
    {
        tmin<-tmin_list[[j]]
        tmax<-tmax_list[[j]]
        prcp<-prcp_list[[j]]
        tmin_tmp<-tmax_tmp<-prcp_tmp<-rep(NA,length(time_list_complete))
        id<-match(time_list,time_list_complete)
        tmin_tmp[id]<-tmin
        tmax_tmp[id]<-tmax
        prcp_tmp[id]<-prcp
        id<-match(time_all,time_list_complete)
        tmin_tmp<-tmin_tmp[id]
        tmax_tmp<-tmax_tmp[id]
        prcp_tmp<-prcp_tmp[id]
        tmin_list[[j]]<-na_ma(tmin_tmp,1)
        tmax_list[[j]]<-na_ma(tmax_tmp,1)
        prcp_list[[j]]<-na_ma(prcp_tmp,1)
      }
  }
  lon<-sort(unique(locations$Longitude),decreasing = F)
  lat<-sort(unique(locations$Latitude),decreasing = T)
  num_lats <- length(lat)
  num_lons <- length(lon)
  num_times <- length(time_all)
  prcp_array<-tmin_array <-tmax_array<-array(NA,dim = c(num_lats,num_lons,num_times),dimnames = list(lat,lon,time_all))
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
  time_dim <- ncdim_def("time", sprintf("days since %s",time_all[1]), seq_along(time_all))
  tmin_var <- ncvar_def("tmin", "degrees_C", list(lat_dim,lon_dim, time_dim), missval = NA)
  tmax_var <- ncvar_def("tmax", "degrees_C", list(lat_dim,lon_dim, time_dim), missval = NA)
  prcp_var <- ncvar_def("prcp", "mm", list(lat_dim,lon_dim , time_dim), missval = NA)
  altitude_var <- ncvar_def("altitude", "meters", list(lat_dim, lon_dim), missval = NA)
  nc <- nc_create(nc_file, list(tmin_var, tmax_var, prcp_var, altitude_var))
  ncvar_put(nc, tmin_var, tmin_array)
  ncvar_put(nc, tmax_var, tmax_array)
  ncvar_put(nc, prcp_var, prcp_array)
  ncvar_put(nc, altitude_var, altitude_array)
  grid_cells<-grid_cells[as.numeric(rownames(locations)),]
  grid_cells$Cell_ID<-(1:nrow(grid_cells))-1
  grid_cells$layer<-NULL
  grid_hru<-as_Spatial(st_intersection(st_as_sf(grid_cells),hru))
  grid_hru$area<-area(grid_hru)
  weight_data<-grid_hru@data
  weight_data<-weight_data[,c(HRU_ID,"Cell_ID","area")]
  hru_id<- unique(weight_data [,HRU_ID])
  weight_data$weight<-NA
  for(i in 1:length(hru_id))
  {
    id<-hru_id[i]==weight_data[,HRU_ID]
    weight_data[id,]$weight<-weight_data[id,]$area/sum(weight_data[id,]$area)
  }
  weight_data$area<-NULL
  L1<-":GridWeights"
  L2<-sprintf(":NumberHRUs\t%s",length(unique(weight_data$HRU_ID)))
  L3<-sprintf(":NumberGridCells\t%s",nrow(grid_cells))
  L4<-"# [HRU ID]\t[Cell #]\t[w_kl]"
  Lweights<-apply(weight_data,1,paste,collapse="\t")
  Lend<-":EndGridWeights"
  weights_mat_data<-c(L1,L2,L3,L4,Lweights,Lend)
  writeLines(weights_mat_data,grid_weight_file)
  var_names <- names(nc$var)
  variableBlocks<-c(":GriddedForcing \t\t\t RavenVarName",
                    "\t:ForcingType \t\t\t RavenForcingType",
                    "\t:FileNameNC \t\t\t netcdf_path",
                    "\t:VarNameNC \t\t\t var_name",
                    "\t:DimNamesNC \t\t\t dims",
                    "\t:ElevationVarNameNC \t\t ele_var",
                    "\t:RedirectToFile \t\t grid_weights_path",
                    ":EndGriddedForcing\n")
  vars<-nc$var
  vars_lookup_infor<-data.frame(var=c("tmin","tmax","prcp"),
                                GriddedForcing=c("TEMPERATURE","TEMPERATURE","PRECIPITATION"),
                                RavenForcingType=c("TEMP_MIN","TEMP_MAX","PRECIP"))
  rvt<-c()
  for(i in 1:length(vars))
  {
    if(names(vars)[i] != "altitude")
    {
      variableBlocks_tmp<-variableBlocks
      variableBlocks_tmp<-gsub("RavenVarName",vars_lookup_infor$GriddedForcing[which(names(vars)[i]==vars_lookup_infor$var)],variableBlocks_tmp)
      variableBlocks_tmp<-gsub("RavenForcingType",vars_lookup_infor$RavenForcingType[which(names(vars)[i]==vars_lookup_infor$var)],variableBlocks_tmp)
      variableBlocks_tmp<-gsub("netcdf_path",nc_file,variableBlocks_tmp)
      variableBlocks_tmp<-gsub("var_name",vars[[i]]$name,variableBlocks_tmp)
      variableBlocks_tmp<-gsub("ele_var",vars$altitude$name,variableBlocks_tmp)
      dimNames<-c()
      for(j in 1:length(vars[[i]]$dim)) dimNames<-c(dimNames,vars[[i]]$dim[[j]]$name)
      dimNames<-paste(dimNames,collapse = " ")
      variableBlocks_tmp<-gsub("dims",dimNames,variableBlocks_tmp)
      rvt<-c(rvt,variableBlocks_tmp)
    }
  }
  writeLines(paste(rvt,collapse = "\n"), con = rvt_file)
  writeLines(text = capture.output(nc),con = nc_content_file)
  nc_close(nc)
  st_write(st_as_sf(grid_cells), dsn=grid_file,  driver="ESRI Shapefile", delete_layer = TRUE)
  if(file.exists(grid_file_json)) file.remove(grid_file_json)
  st_write(st_as_sf(grid_cells), dsn=grid_file_json, driver="GeoJSON")
  if(plot)
  {
    pdf(file = plot_file)
    plot(grid_cells,col="lightgrey")
    if(nrow(grid_cells)<500)
    {
      points(rasterToPoints(r)[,1:2],pch=19,cex=0.5,col="red")
      text(x=coordinates(r)[,1],y=coordinates(r)[,2],labels=grid_cells$Cell_ID,col="white",cex=0.6)
      legend("topleft",
             legend = c("grid","HRU","centroid"),
             pch=c(4,4,19),
             col=c("black","lightgrey","red"),
             cex=c(.7,.7,.7),
             bty="n")
    }
    plot(grid_cells[grid_cells$Cell_ID %in% unique(weight_data[,1]),],add=T,col="darkgrey")
    plot(hru,col = rgb(0.7, 0.5, 0.5, 0.5), border = "white", lwd = 0.1,add=T)
    x_range <- par()$usr[1:2]
    y_range <- par()$usr[3:4]
    x_scale <- diff(x_range) / 5
    y_scale <- diff(y_range) / 5
    ruler_x <- seq(x_range[1], x_range[2], by = x_scale)
    ruler_y <- seq(y_range[1], y_range[2], by = y_scale)
    axis(1, at = ruler_x, labels = FALSE, tck = -0.02)
    axis(2, at = ruler_y, labels = FALSE, tck = -0.02)
    mtext(round(ruler_x, 1), side = 1, at = ruler_x, line = 1, cex = 0.7,las=2)
    mtext(round(ruler_y, 1), side = 2, at = ruler_y, line = 1, cex = 0.7,las=2)
    abline(v=ruler_x,col="green",lty=2)
    abline(h=ruler_y,col="green",lty=2)
    dev.off()
  }
  cat("Done!")
}
