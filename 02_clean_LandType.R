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

#Provincial Human Disturbance Layers - compiled for CE
#Needs refinement to differentiate rural/urban and old vs young cutblocks, rangeland, etc.

ProvRast<-raster(file.path(spatialOutDirP,'ProvRast.tif'))

lc_file<-file.path(spatialOutDirP,'LandCoverPS.tif')
if (!file.exists(lc_file)) {
  LandCoverin<-raster(file.path(DataDir,"WESPdata/BC_Land_Cover/BC_Land_Cover/BC_LandCover_20210512.tif")) #%>%
   #mask(AOI) %>%
   #crop(AOI)

  writeRaster(LandCoverin, filename=file.path(spatialOutDir,'LandCoverin'), format="GTiff", overwrite=TRUE)

  LandCover1<- LandCoverin %>%
    raster::aggregate(fact=5, fun=modal, na.rm=TRUE)
  writeRaster(LandCover1, filename=file.path(spatialOutDir,'LandCover1'), format="GTiff", overwrite=TRUE)

  LandCover<-LandCover1 %>%
    raster::resample(ProvRast, method='ngb')
  writeRaster(LandCover, filename=file.path(spatialOutDir,'LandCover'), format="GTiff", overwrite=TRUE)

  saveRDS(LandCover, file=lc_file)
#  LandCover_LUT<-read_csv(file.path(DataDir,"WESPdata/BC_Land_Cover/BC_LandCover_Key.csv"),
  #               skip=2,
 #                col_names=TRUE)

  #write out LandCover_LUT for classification
 # WriteXLS(LandCover_LUT,file.path(DataDir,'LandCover_LUT.xlsx'))

#Freq of original raster - takes a long time to run
# LandCover_Tbl <- raster::freq(LandCover)
# WriteXLS(LandCover_Tbl,file.path(DataDir,'LandCover_Tbl.xlsx'))

#In excel add an 'LTypeCode' column to the LandCover_LUT
#Read the file back in and use with 'sub' command
  #New Codes for land cover to group from 20 units to 10 groups
  #1	Barren Land
  #2	Coniferous Forest
  #7	Cropland
  #3	Deciduous Forest
  #9	Estuary
  #9	Fresh Water
  #6	Grassland
  #9	Intertidal Zone
  #9	Marine Water
  #4	Mixed Forest
  #2	Riparian - Coniferous Forest
  #3	Riparian - Deciduous Forest
  #6	Riparian - Grassland
  #4	Riparian - Mixed Forest
  #5	Riparian - Shrubland
  #5	Shrubland
  #8	Snow and Ice
  #9	Subtidal Zone
  #10	Unclassified
  #1	Urban and Built-up
  #9	Wetland

  LandCover_LUT<-data.frame(read_excel(file.path(DataDir,'LandCover_LUT.xlsx'))) %>%
    dplyr::select(LTypeCode,Value)

  LandCoverS<-subs(LandCover,
                   LandCover_LUT, by='Value',which='LTypeCode')
  writeRaster(LandCoverS, filename=file.path(spatialOutDirP,'LandCoverPS'), format="GTiff", overwrite=TRUE)

  #Give names to the new LandCover' to's 10  categroies
  LandCoverType_LUT<-data.frame(LandCCode=c(0,1,2,3,4,5,6,7,8,9,10),
                                LandCoverType=c('not typed', 'Urban Barren','Coniferous',
                                                'Deciduous','Mixed Forest','Shrubland',
                                                'Grassland','Cropland','Snow and Ice',
                                                'Water','Unclassified'))

  saveRDS(LandCoverType_LUT,file='tmp/LandCoverType_LUT')

} else {
  LandCoverPS<-raster(file.path(spatialOutDirP,'LandCoverPS.tif'))
  LandCoverType_LUT<-readRDS(file='tmp/LandCoverType_LUT')
}

#Geology
Bedrock <- st_read(file.path(spatialOutDirP,"BedrockP.gpkg")) %>%
  st_intersection(AOI) %>%
  mutate(rastV=as.numeric(rownames(.)))
write_sf(Bedrock, file.path(spatialOutDir,"Bedrock.gpkg"))
Bedrockr <- fasterize(st_cast(Bedrock,"POLYGON"),ProvRast,field='rastV')
writeRaster(Bedrockr, filename=file.path(spatialOutDir,'Bedrockr'), format="GTiff", overwrite=TRUE)




