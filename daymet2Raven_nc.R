daymet2Raven_nc<-function(hru_shp_file,
                          start_date,
                          end_date,
                          grid_size,
                          HRU_ID="HRU_ID",
                          nc_file="RavenInput.nc",
                          grid_weight_file="weights.txt",
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
  hru <- st_make_valid(st_transform(st_read(hru_shp_file), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs"))[,HRU_ID]
  boundary <- ms_simplify(st_cast(st_simplify(st_union(hru),dTolerance = sum(area(as_Spatial(hru)))/1e6/20),"POLYGON"),0.99)
  buffered_boundary <- st_buffer(boundary, dist = grid_size*1e5 / 2,singleSide = T)
  r<-raster(extent(as_Spatial(buffered_boundary)),resolution=grid_size,crs=crs(buffered_boundary))
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
    for(j in 1:1:nrow(locations))
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
  lat<-unique(locations$Latitude)
  lon<-unique(locations$Longitude)
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
  writeLines(paste(rvt,collapse = "\n"), con = "model.rvt")
  writeLines(text = capture.output(nc),con = "nc_file_content.txt")
  nc_close(nc)
  st_write(st_as_sf(grid_cells), dsn="grids_polygons.shp",  driver="ESRI Shapefile", delete_layer = TRUE)
  st_write(st_as_sf(grid_cells), dsn="grids_polygons.json", driver="GeoJSON", delete_layer = TRUE)

  
  if(plot)
  {
    pdf(file = "plot.pdf")
    plot(grid_cells,col="lightgrey")
    if(nrow(grid_cells)<500)
    {
      points(rasterToPoints(r)[,1:2],pch=19,cex=0.5,col="red")
      text(x=coordinates(r)[,1],y=coordinates(r)[,2],labels=grid_cells$Cell_ID,col="white",cex=0.6)
      legend("topleft",
             legend = c("grid","HRU","centroid"),
             pch=c(4,4,19,),
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
