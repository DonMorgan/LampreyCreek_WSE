# Copyright 2022 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

Wetlands <- read_sf(file.path(spatialOutDir,"Wetlands.gpkg"))

roads_file <- file.path(spatialOutDir,"roadsSR.tif")
if (!file.exists(roads_file)) {
  roadsSR<-raster(file.path(spatialOutDirP,'roadsSR.tif'))
  roadsSR <- roadsSR %>%
      terra::mask(AOI) %>%
      terra::crop(AOI)
  writeRaster(roadsSR, filename=file.path(spatialOutDir,'roadsSR'), format="GTiff", overwrite=TRUE)
  roads_sf_all<-st_read(file.path(spatialOutDirP,"roads_clean.gpkg")) %>%
    st_intersection(AOI)
  roads_sf<-roads_sf_all %>%
    dplyr::filter(RoadUse<3)
  saveRDS(roads_sf,file='tmp/roads_sf')
  write_sf(roads_sf, file.path(spatialOutDir,"roads_sf.gpkg"), layer_options = "OVERWRITE=true")
  } else {
  roadsSR<-raster(file.path(spatialOutDir,'roadsSR.tif'))
  roads_sf<-st_read(file.path(spatialOutDir,"roads_sf.gpkg"))
}

file500 <- file.path(spatialOutDir,"roads500m.gpkg")
if (!file.exists(file500)) {
  roads500m1<-roads_sf %>%
    st_buffer(dist=500)
  roads500m2 <- geos::as_geos_geometry(roads500m1)
  roads500m3 <- roads500m2 %>%
    geos::geos_make_collection() %>%
    geos::geos_unary_union()
  roads500m <- sf::st_as_sf(roads500m3)
  clgeo_IsValid(as(roads500m,'Spatial'), verbose = FALSE)
  write_sf(roads500m, file.path(spatialOutDir,"roads500m.gpkg"), layer_options = "OVERWRITE=true")
} else {
  roads500m<-st_read(file.path(spatialOutDir,"roads500m.gpkg"))
}


fileWetRd1 <- file.path(spatialOutDir,"wetsRoads1.gpkg")
#if (!file.exists(fileWetRd1)) {
#  wetsRoads1a<-Wetlands %>%
#  st_intersection(roads500m)
  #write_sf(wetsRoads1, file.path(spatialOutDir,"wetsRoads1.gpkg"))
#  write_sf(wetsRoads1a, file.path(spatialOutDir,"wetsRoads1a.gpkg"))
#} else {
#  wetsRoads1a<-st_read(file.path(spatialOutDir,"wetsRoads1a.gpkg")) #17714
#}

#Modify to use exactextract - way faster than st_intersection
roads500mr <- fasterize(roads500m,ProvRast)
writeRaster(roads500mr, filename=file.path(spatialOutDir,'roads500mr'), format="GTiff", overwrite=TRUE)

#Number of 1ha road buffer cells in polygon
Wetlands_Erd <- data.frame(areaHa500=exact_extract(roads500mr, Wetlands, 'sum'))
Wetlands_Erd$wet_id <-seq.int(nrow(Wetlands_Erd))

wetsRoads1 <- Wetlands %>%
  left_join(Wetlands_Erd) %>%
  #st_drop_geometry() %>%
  mutate(pcentIn500Buf=round(areaHa500/area_Ha*100)) %>%
  dplyr::select(wet_id, WTLND_ID, areaHa500, area_Ha) %>%
  mutate(pcentIn500Buf=round(areaHa500/area_Ha*100))
write_sf(wetsRoads1, file.path(spatialOutDir,"wetsRoads1.gpkg"))

#SubBoreal PEM wetlands
#Number of 1ha road buffer cells in polygon
#PEM_wetlands<-st_read(file.path(spatialOutDir,"PEM_wetlands.gpkg"))
#Wetlands_Eprd <- data.frame(areaHa500=exact_extract(roads500mr, PEM_wetlands, 'sum'))
#Wetlands_Eprd$wet_id_FWCP <-seq.int(nrow(Wetlands_Eprd))

#wetsRoads1pem <- PEM_wetlands %>%
#  left_join(Wetlands_Eprd) %>%
  #st_drop_geometry() %>%
#  mutate(pcentIn500Buf=round(areaHa500/area_Ha*100)) %>%
#  dplyr::select(wet_id_FWCP, WTLND_ID, areaHa500, area_Ha) %>%
#  mutate(pcentIn500Buf=round(areaHa500/area_Ha*100))

#write_sf(wetsRoads1pem, file.path(spatialOutDir,"wetsRoads1pem.gpkg"))

#Add Roads
#roadsSR<-raster(file.path(spatialOutDir,'roadsSR.tif'))
Roads_LUT<-data.frame(Value=c(1,2,3), Rd_Disturb_Code=c(11,9,6))

roadsDist <- raster::subs(roadsSR, Roads_LUT, by='Value', which='Rd_Disturb_Code')
RdDisturb_LUT<-data.frame(RdDisturbCode=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13),
                        RdDisturbType=c('not disturbed', 'historic fire','recent fire',
                                      'historic cutblocks','rangeland','current fire',
                                      'low use roads','current cutblocks',
                                      'agriculture and rural and recreation',
                                      'mod use roads', 'ROW, Power, Rail, OGC geophysical',
                                      'high use roads','mine, OGC infrastructure','urban'))
saveRDS(RdDisturb_LUT,file='tmp/RdDisturb_LUT')
RdDisturb<-max(roadsDist, na.rm=TRUE)
writeRaster(RdDisturb,filename=file.path(spatialOutDir,"RdDisturb.tif"), format="GTiff", overwrite=TRUE)

gc()

message('Breaking')
break

#fix na wetlands so dont have to re-run intersection
#wetsRoads1<-wetsRoads1 %>% #17714
#  dplyr::filter(!(is.na(BEC_BCWF))) #17684

#### 50M
file50 <- file.path(spatialOutDir,"roads50m.gpkg")
if (!file.exists(file50)) {
  #Buffer roads 50m and 500m for checking
  roads50m1<-roads_sf %>%
    st_buffer(dist=50)
  #write_sf(roads50m1, file.path(spatialOutDir,"roads50m1.gpkg"))
  roads50m2 <- geos::as_geos_geometry(roads50m1)
  roads50m3 <- roads50m2 %>%
    geos::geos_make_collection() %>%
    geos::geos_unary_union()
  roads50m <- sf::st_as_sf(roads50m3)
  clgeo_IsValid(as(roads50m,'Spatial'), verbose = FALSE)
  write_sf(roads50m, file.path(spatialOutDir,"roads50m.gpkg"))
} else {
  roads50m<-st_read(file.path(spatialOutDir,"roads50m.gpkg"))
}

#Now calculate per cent overlap of wetlands with 50 m to
#identify wetlands that may be dropped since close to road
#st_intersection runs very slow
start.time <- Sys.time()
wets50 <- Wetlands  %>%
  #st_intersects(roads50m)
  st_intersection(roads50m)
write_sf(wets50, file.path(spatialOutDir,"wets50.gpkg"))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

wets50mToDrop<-wets50 %>%
  mutate(areaHa50=as.numeric(st_area(wets50)*0.0001)) %>%
  mutate(pcentIn50Buf=as.numeric(areaHa50/as.numeric(area_Ha)*100)) %>%
  st_drop_geometry() %>%
  dplyr::filter(pcentIn50Buf>1) %>%
  # mutate(WetToDrop=1) %>%
  dplyr::select(WTLND_ID, pcentIn50Buf, areaHa50) #5404 wetlands to drop
