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

#Read in wetlands buffer and LandCover files
WetlandsB<-read_sf(file.path(spatialOutDir,"WetlandsB.gpkg"))
LandCover<-raster(file.path(spatialOutDirP,'LandCoverPS.tif'))
Wetlands<-read_sf(file.path(spatialOutDir,"Wetlands.gpkg"))
LandCoverType_LUT<-readRDS(file='tmp/LandCoverType_LUT')

#Take max disturbance and assign to wetland, such if any urban then urban disturbance, etc
#alternative is to take most common - but likley larger effect if more severe disturbance
#Set water land type to NA so selection only uses terrestrial types
LandCover[LandCover==9] <- NA

#WetlandsE1 <- raster::extract(LandDisturb, WetlandsB, sp=TRUE)
#WetlandsE3 <- exact_extract(LandDisturb, WetlandsB2) - returns propotion of each
Wetlands_E <- data.frame(LandCCode=exact_extract(LandCover, WetlandsB, 'mode')) #should be mode
Wetlands_E$wet_id <-as.numeric(rownames(Wetlands_E))

#if no land cover around wetland - typically a wetland in a lake then reset back to water
Wetlands_E$LandCCode[is.na(Wetlands_E$LandCCode)] <- 9

Wetlands_LC1 <- Wetlands %>%
  mutate(wet_id=as.numeric(rownames(WetlandsB))) %>%
  left_join(Wetlands_E) %>%
  left_join(LandCoverType_LUT)

ObserveColumns<-c(Observed_Landcover=NA,Observed_Disturbance=NA)
#Dont deal with observed if none in data set

  #Check if a site has an observed landcover type
#drop the LandCCode - since it will be wrong for observed sites and rejoin the LUT to populate it correctly
Wetlands_LC <- Wetlands_LC1 %>%
  #dplyr::select(-c(Observed_Landcover)) %>%
  add_column(!!!ObserveColumns[!names(ObserveColumns) %in% names(.)]) %>%
  mutate(LandCoverType=if_else(!is.na(Observed_Landcover),as.character(Observed_Landcover),LandCoverType)) %>%
  dplyr::select(-c(LandCCode)) %>%
  left_join(LandCoverType_LUT)





write_sf(Wetlands_LC, file.path(spatialOutDir,"Wetlands_LC.gpkg"))
#Wetlands_LC<-read_sf(file.path(spatialOutDir,"Wetlands_LC.gpkg"))
LCcheck<-Wetlands_LC %>% dplyr::filter(is.na(LandCoverType))
write_sf(LCcheck, file.path(spatialOutDir,"LCcheck.gpkg"))
#Function to nibble into water features to ensure wetlands land in a non-water
#dominant adjacent feature - inspired by this link https://stackoverflow.com/questions/36960974/how-to-replace-raster-values-less-than-0-to-na-in-r-code/49159943
#Fill with highest value - will be oldest forest or if non-forest then human disturbed, then shrubs
#fill.na <- function(x, i=5) {
#  if( is.na(x)[i] ) {
#    return( modal(x, ties='highest',na.rm=TRUE))
#  } else {
#    return( round(x[i],0) )
#  }
#}
#Pass the fill.na function to raster::focal and check results.
#The pad argument creates virtual rows/columns of NA values to keep the
#vector length constant along the edges of the raster.
#This is why we can always expect the fifth value of the vector to be
#the focal value in a 3x3 window thus, the index i=5 in the fill.na function.
#Do the fill twice to nibble into large lakes sufficient to assign areas
#where wetlands may occur to their largest neighbour

#LCstrataFilled <- focal(LCstrata, w = matrix(1,3,3), fun = fill.na,
#                               pad = TRUE, na.rm = FALSE ) %>%
#  focal(w = matrix(1,3,3), fun = fill.na,pad = TRUE, na.rm = FALSE )
#writeRaster(LCstrataFilled, filename=file.path(spatialOutDir,"LCstrataFilled.tif"), format="GTiff", overwrite=TRUE)

#Read in the point coverage of wetland centroids
#waterpt<-st_read(file.path(spatialOutDir,"waterptRoad.gpkg"))
#waterpt<-st_read(file.path(spatialOutDir,"waterpt.gpkg"))

lt.ls <- Wetlands_LC %>%
  st_drop_geometry() %>%
  left_join(LandCoverType_LUT) %>%
  dplyr::select(WTLND_ID, LandCoverType, LandCCode)

WriteXLS(lt.ls,file.path(dataOutDir,paste('ltls.xlsx',sep='')))

## generate a list summarizing land types, and the number and % of wetlands, then save
unique(lt.ls$LandCoverType)
prop.site <- lt.ls %>%
  group_by(LandCoverType)%>%
  dplyr::summarise(no.pts = n()) %>%
  #st_drop_geometry() %>%
  mutate(perc = ceiling(no.pts / sum(no.pts)*100))

#WriteXLS(prop.site,file.path(dataOutDir,paste('ESILandTypexWetland.xlsx',sep='')))
WriteXLS(prop.site,file.path(dataOutDir,paste('ESI_Wetland_Strata_LT.xlsx',sep='')),SheetNames='LandType')

gc()
