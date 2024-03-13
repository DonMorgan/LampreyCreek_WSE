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

#Generate AOI specific disturbance layer
disturbance_sfR<-raster(file.path(spatialOutDirP,'disturbance_sfR.tif')) %>%
  mask(AOI) %>%
  crop(AOI)
writeRaster(disturbance_sfR, filename=file.path(spatialOutDir,'disturbance_sfR'), format="GTiff", overwrite=TRUE)
#disturbance_sfR<-raster(file.path(spatialOutDir,'disturbance_sfR'))

#Add Fires
FireR<-raster(file.path(spatialOutDirP,"FireR.tif")) %>%
  mask(AOI) %>%
  crop(AOI)
writeRaster(FireR, filename=file.path(spatialOutDir,'FireR'), format="GTiff", overwrite=TRUE)

#Add Roads
roadsSR<-raster(file.path(spatialOutDir,'roadsSR.tif'))

#Combine human disturbance + fires + roads
#human disturbed from AreaDisturbance_LUT:
#0 not disturbed
#1 for historic Fire <=1990
#2 for recent historic Fire <=2015 & >=1991
#3 for historic cutblocks
#4 rangeland
#5 for recent fire - >2016
#6 Low use Roads
#7 for current cutblocks
#8 for agriculture and rural and recreation
#9 Mod use Roads
#10 ROW, Power, Rail, OGC geophysical
#11 High use Roads
#12 Mining, OGC infrastructure
#13 Urban

#Assign values to roads
Roads_LUT<-data.frame(Value=c(1,2,3), Rd_Disturb_Code=c(11,9,6))

roadsDist <- raster::subs(roadsSR, Roads_LUT, by='Value', which='Rd_Disturb_Code')

Disturb_LUT<-data.frame(DisturbCode=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13),
                        DisturbType=c('not disturbed', 'historic fire','recent fire',
                                      'historic cutblocks','rangeland','current fire',
                                      'low use roads','current cutblocks',
                                      'agriculture and rural and recreation',
                                      'mod use roads', 'ROW, Power, Rail, OGC geophysical',
                                      'high use roads','mine, OGC infrastructure','urban'))
saveRDS(Disturb_LUT,file='tmp/Disturb_LUT')

#Make a raster stack of disturbance layers
disturbStackL <- list(disturbance_sfR,FireR,roadsDist)
disturbStack <- stack(disturbStackL)
LandDisturb<-max(disturbStack, na.rm=TRUE)
#LandDisturbP<-max(disturbance_sfR, FireR, na.rm=TRUE)
writeRaster(LandDisturb,filename=file.path(spatialOutDir,"LandDisturb.tif"), format="GTiff", overwrite=TRUE)



