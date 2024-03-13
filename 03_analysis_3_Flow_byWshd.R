# Copyright 2019 Province of British Columbia
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

#Variation of the 03_analysis_3_Flow.R to speed up processing by breaking into
# major watersheds for processing

Wetlands<-read_sf(file.path(spatialOutDir, "Wetlands.gpkg")) %>%
  st_cast("POLYGON")

ws<-read_sf(file.path(spatialOutDir,"ws.gpkg")) #From 02_clean_Plains.R

#Make map of lakes+rivers
Lakes <- read_sf(file.path(spatialOutDir,"Lakes.gpkg")) %>%
  mutate(lake_id=as.numeric(rownames(.))) %>%
  st_buffer(50) %>%
  st_cast("POLYGON")

#mapview(lakeNB) + mapview(Lakes_ws) + mapview(Wetlands_ws)

#Function to evaluate Lakes near wetlands
NearLakeFn <- function(wsNum){
    lake_ws<-st_intersection(Lakes, ws[wsNum,])
    lake_ws$lake_ws_id<-as.numeric(rownames(lake_ws))
    wet_ws<-st_intersection(Wetlands, ws[wsNum,])
    wet_ws$wet_ws_id<-as.numeric(rownames(wet_ws))
    lakeNB <-lake_ws %>%
      st_intersects(wet_ws) %>%
      tibble::enframe(name = 'lake_ws_id', value = 'wet_ws_id') %>%
      tidyr::unnest(wet_ws_id) %>%
      left_join(lake_ws, by=c("lake_ws_id")) %>%
      left_join(wet_ws, by=c("wet_ws_id")) %>%
      dplyr::select(lake_id, lake_ws_id, wet_id, wet_ws_id)
     return(lakeNB)
}

#loop through each of the 21 major watersheds in the AOI and generate a list of what lakes are close to what wetlands
ws_list<-list()
ws_list <-lapply(c(1:21), function(i) NearLakeFn(i))
saveRDS(ws_list,file='tmp/ws_list')

#tt<-NearLakeFn(2)
#Take resulting list and combine into a single data.frame
LakeNearBy<-ldply(ws_list,data.frame)

LakeNearBy <- LakeNearBy %>%
  mutate(Lake=ifelse(lake_id>0, 1, 0)) %>%
  left_join(Lakes) %>%
  dplyr::select(lake_id, wet_id, Lake, LakeHa=AREA_HA)

Rivers<-read_sf(file.path(spatialOutDir,"Rivers.gpkg"))
RiversB <- Rivers %>%
  mutate(river_id=as.numeric(rownames(.))) %>%
  st_buffer(50) %>%
  st_cast("POLYGON")

#wet_id wasnt joining properly so will define after read
#Wetlands <- Wetlands %>%
#  mutate(wet_id2=as.numeric(rownames(Wetlands)))
#unique(st_geometry_type(waterbodiesB))
#unique(st_geometry_type(Wetlands))

NearRiverFn <- function(wsNum){
  river_ws<-st_intersection(RiversB, ws[wsNum,])
  river_ws$river_ws_id<-as.numeric(rownames(river_ws))
  wet_ws<-st_intersection(Wetlands, ws[wsNum,])
  wet_ws$wet_ws_id<-as.numeric(rownames(wet_ws))
  riverNB <-river_ws %>%
    st_intersects(wet_ws) %>%
    tibble::enframe(name = 'river_ws_id', value = 'wet_ws_id') %>%
    tidyr::unnest(wet_ws_id) %>%
    left_join(river_ws, by=c("river_ws_id")) %>%
    left_join(wet_ws, by=c("wet_ws_id")) %>%
    dplyr::select(river_id, river_ws_id, wet_id, wet_ws_id)
  return(riverNB)
}

#loop through each of the 21 major watersheds in the AOI and generate a list of what lakes are close to what wetlands
ws_listR<-list()
ws_listR <-lapply(c(1:21), function(i) NearRiverFn(i))
saveRDS(ws_listR,file='tmp/ws_listR')

#tt<-NearLakeFn(2)
#Take resulting list and combine into a single data.frame
RiverNearBy<-ldply(ws_listR,data.frame)

RiverNearBy <- RiverNearBy %>%
  mutate(River=ifelse(river_id>0, 1, 0))

#join the 2 data frames with 1 record per wetland
WaterNearBy <- LakeNearBy %>%
  full_join(RiverNearBy, by='wet_id') %>%
  group_by(wet_id) %>%
  dplyr::summarise(nRiver=sum(River), nLake=sum(Lake), LakeHa=max(LakeHa)) %>%
  mutate(Water=ifelse((nRiver || nLake)>0, 1, 0)) %>%
  mutate(LakeSize=case_when(
    LakeHa >5 ~ 1,
    LakeHa <=5 & LakeHa >1 ~ 2,
    LakeHa <=1 ~ 3,
    TRUE ~ 0
  ))
 # mutate(LargeLake=if_else(LakeSize>1,1,0)) %>%
 # mutate(MediumLake=if_else(LargeLake==1,0,1)) %>%
 # mutate(SmallLake=if_else(LakeSize<=1,1,0))

WetlandsN<-Wetlands %>%
  left_join(WaterNearBy, by='wet_id')
WetlandsN$Water[is.na(WetlandsN$Water)]<-0
WetlandsN$LakeSize[is.na(WetlandsN$LakeSize)]<-0
WetlandsN$nLake[is.na(WetlandsN$nLake)]<-0
WetlandsN$nRiver[is.na(WetlandsN$nRiver)]<-0

write_sf(WetlandsN, file.path(spatialOutDir,"WetlandsN.gpkg"))
#WetlandsN<-st_read(file.path(spatialOutDir,"WetlandsN.gpkg"))

#Organize flow related variables for classifying wetlands
WetFlow<-WetlandsN %>%
  #st_drop_geometry() %>%
  dplyr::select("WTLND_ID",
                "split_by_stream", "stream_end", "stream_start",
                "max_stream_order", "Verticalflow", "Bidirectional", "Throughflow",
                "Outflow", "Inflow", "nRiver","nLake",'LakeSize',"Water") %>%
  mutate(ConnectedWet=ifelse(Throughflow=="Yes" | Bidirectional=="Yes", 2, 0)) %>%
  mutate(UnconnectedWet=ifelse(Verticalflow=="Yes" | Outflow=="Yes" | Inflow=="Yes",1, 0)) %>%
  #mutate(AdjacentWaterWet=ifelse(Water>0, 2, 0)) %>%
  mutate(AdjacentWaterWet=ifelse(Water>0, 3, 0)) %>%
  mutate(FlowCode=pmax(ConnectedWet,UnconnectedWet,AdjacentWaterWet, na.rm=FALSE)) %>%
  dplyr::select(WTLND_ID,FlowCode, Water, nRiver, nLake, LakeSize)
#if 0 ie no cases from connected, unconnected or adjacent water then assign to unconnected
WetFlow$FlowCode[WetFlow$FlowCode==0]<-1

#unique(WetFlow$FlowCode)
write_sf(WetFlow, file.path(spatialOutDir,"WetFlow.gpkg"))

#evaluate prevelance of each category
# Generate a breakdown of how many and % wetlands in each flow group
flow.site <- WetFlow %>%
  st_drop_geometry() %>%
  group_by(FlowCode)%>%
  dplyr::summarise(no.pts = n()) %>%
  mutate(perc = ceiling(no.pts / sum(no.pts)*100))

WriteXLS(st_drop_geometry(WetFlow),file.path(dataOutDir,paste('WetFlow.xlsx',sep='')))
WriteXLS(flow.site,file.path(dataOutDir,paste('ESIFlowxWetland.xlsx',sep='')))


