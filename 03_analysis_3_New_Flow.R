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

#Notes until new flow is working need to join back in flow info
# from WeltansAll.gpkg in WESP_data_prep out spatial directory
#Attributes generated:
#1. stream_intersect,river_intersect,mmwb_intersect,lake_intersect,
#2. FlowCode, Water, nRiver, nLake, LakeSize,LargeWetland
#3. stream_start,stream_end
#4. split_by_stream
#5. Verticalflow, Bidirectional,Throughflow, Outflow, Inflow,
#max_stream_order -not used
#

Wetlands<-read_sf(file.path(spatialOutDir, "Wetlands.gpkg")) #%>%
  #st_intersection(AOI)
clgeo_IsValid(as(Wetlands,'Spatial'), verbose = FALSE)

###. stream_intersect,river_intersect,mmwb_intersect,lake_intersect
#Pre-processing of Streams, Rivers, Lakes and Manmade Water bodies
#Streams
#Select wetlands that intersect filtered FWA streams
#AOI<-TestAOI
Streams<-read_sf(file.path(spatialOutDirP,"StreamsP.gpkg")) %>%
  dplyr::filter(EDGE_TYPE %in% c('1000', '1050', '1100', '1150', '1250', '2000', '2300')) %>%
  st_intersection(AOI)
write_sf(Streams, file.path(spatialOutDir,"Streams.gpkg"))
Streams<-st_read(file.path(spatialOutDir,"Streams.gpkg"))

#Use Geos to get stream ends and starts - works better than st_ version
st1 <- as_geos_geometry(Streams) %>%
  geos_unnest(keep_multi = FALSE)

#Buffer to identify streams next to wetlands
StreamsB <- Streams %>%
  st_buffer(25) %>%
  st_cast("POLYGON")
StreamsNearBy1<-StreamsB %>%
  st_intersects(Wetlands) %>%
  tibble::enframe(name = 'Streams', value = 'wet_id') %>%
  tidyr::unnest(wet_id)
StreamsNearBy <- StreamsNearBy1 %>%
  st_drop_geometry() %>%
  mutate(Streams=ifelse(Streams>0, 1, 0))

#Lakes
Lakes <- read_sf(file.path(spatialOutDirP,"Lakes.gpkg")) %>%
  st_intersection(AOI) %>%
  mutate(lake_id=as.numeric(rownames(.))) %>%
  st_buffer(25) %>%
  st_cast("POLYGON") %>%
  mutate(LakeSize=case_when(
    AREA_HA >5 ~ 3,
    AREA_HA <=5 & AREA_HA >1 ~ 2,
    AREA_HA <=1 ~ 1,
    TRUE ~ 0
  ))
write_sf(Lakes, file.path(spatialOutDir,"Lakes.gpkg"))
write_sf(Wetlands, file.path(spatialOutDir,"WetlandsLakes.gpkg"))

LakeNearBy1<-Lakes %>%
  st_intersects(Wetlands) %>%
  tibble::enframe(name = 'lake_id', value = 'wet_id') %>%
  tidyr::unnest(wet_id)
LakeNearBy <- LakeNearBy1 %>%
  mutate(Lake=ifelse(lake_id>0, 1, 0)) %>%
  left_join(Lakes) %>%
  group_by(wet_id) %>%
  dplyr::summarise(nLake=sum(Lake),
                   LakeHa=sum(AREA_HA)) %>%
  dplyr::select(wet_id, nLake, LakeHa)

#Rivers
Rivers<-read_sf(file.path(spatialOutDirP,"Rivers.gpkg")) %>%
  st_intersection(AOI)
RiversB <- Rivers %>%
  st_buffer(25) %>%
  st_cast("POLYGON")
RiverNearBy<-RiversB %>%
  st_intersects(Wetlands) %>%
  tibble::enframe(name = 'River', value = 'wet_id') %>%
  tidyr::unnest(wet_id)
RiverNearBy <- RiverNearBy %>%
  group_by(wet_id) %>%
  dplyr::summarise(nRiver=sum(River)) %>%
  mutate(River=ifelse(nRiver>0, 1, 0))

#Man made Waterboidies
MMWB<-st_read(file.path(spatialOutDirP,"MMWB.gpkg")) %>%
  st_intersection(AOI) %>%
  mutate(mmwb_id=as.numeric(rownames(.))) %>%
  st_buffer(25) %>%
  st_cast("POLYGON") %>%
  mutate(MMWBSize=case_when(
    area_Ha >5 ~ 3,
    area_Ha <=5 & area_Ha >1 ~ 2,
    area_Ha <=1 ~ 1,
    TRUE ~ 0
  ))
MMWBNearBy1<-MMWB %>%
  st_intersects(Wetlands) %>%
  tibble::enframe(name = 'mmwb_id', value = 'wet_id') %>%
  tidyr::unnest(wet_id)
MMWBNearBy <- MMWBNearBy1 %>%
  mutate(MMWB=ifelse(mmwb_id>0, 1, 0)) %>%
  left_join(MMWB) %>%
  dplyr::select(mmwb_id, wet_id, MMWB, area_Ha) %>%
  group_by(wet_id) %>%
  dplyr::summarise(nMMWB=sum(MMWB),
                   MMWBHa=sum(area_Ha))

#join the water data frames with 1 record per wetland
WaterNearBy <- LakeNearBy %>%
  full_join(RiverNearBy, by='wet_id') %>%
  full_join(StreamsNearBy, by='wet_id') %>%
  full_join(MMWBNearBy, by='wet_id') %>%
  group_by(wet_id) %>%
  dplyr::summarise(nRiver=sum(River), nStream=sum(Streams), nLake=sum(nLake), nMMWB=sum(nMMWB),
                   LakeHa=max(LakeHa), MMWBHa=max(MMWBHa)) %>%
  mutate(Water=ifelse((nRiver | nStream | nLake | nMMWB)>0, 1, 0)) %>%
  mutate(LakeSize=case_when(
    LakeHa >5 ~ 1,
    LakeHa <=5 & LakeHa >1 ~ 2,
    LakeHa <=1 ~ 3,
    TRUE ~ 0
  ))

###### stream_start,stream_end
#uses streams data set
source('03_analysis_3_Flow_StreamStartEnd.R')
#returns - non-spatial Strm_start_end (for join below)
Strm_start_end<-read_xlsx(file.path(dataOutDir,paste0('Strm_start_end.xlsx')))


#Join water features
WetlandsN<-Wetlands %>%
  left_join(Strm_start_end, by='WTLND_ID') %>%
  left_join(WaterNearBy, by='wet_id')
WetlandsN$Water[is.na(WetlandsN$stream_start)]<-0
WetlandsN$Water[is.na(WetlandsN$stream_end)]<-0
WetlandsN$Water[is.na(WetlandsN$Water)]<-0
WetlandsN$LakeSize[is.na(WetlandsN$LakeSize)]<-0
WetlandsN$nLake[is.na(WetlandsN$nLake)]<-0
WetlandsN$nRiver[is.na(WetlandsN$nRiver)]<-0
WetlandsN$nStream[is.na(WetlandsN$nStream)]<-0
WetlandsN$nMMWB[is.na(WetlandsN$nMMWB)]<-0

write_sf(WetlandsN, file.path(spatialOutDir,"WetlandsN.gpkg"))
#WetlandsN<-st_read(file.path(spatialOutDir,"WetlandsN.gpkg"))

#Organize flow related variables for classifying wetlands
WetFlow1<-WetlandsN %>%
  #st_drop_geometry() %>%
  dplyr::select("WTLND_ID",'wet_id',
                'stream_start','stream_end',
                "nRiver","nStream","nLake","nMMWB",'LakeSize',"Water") %>%
  mutate(ConnectedWet=ifelse(nRiver>0 | nLake>0 | nMMWB>0, 2, 0)) %>%
  mutate(UnconnectedWet=ifelse(ConnectedWet==0, 1,0)) %>%
  mutate(AdjacentWaterWet=ifelse(Water>0, 3, 0)) %>%
  mutate(FlowCode=pmax(ConnectedWet,UnconnectedWet,AdjacentWaterWet, na.rm=FALSE)) %>%
  dplyr::select(WTLND_ID,wet_id,stream_start,stream_end,
                FlowCode, Water, nRiver, nStream,nLake, LakeSize, nMMWB)

WetFlow2<-WetFlow1 %>%
  mutate(stream_intersect=ifelse(nStream>0,'Yes', 'No')) %>%
  mutate(river_intersect=ifelse(nRiver>0,'Yes', 'No')) %>%
  mutate(mmwb_intersect=ifelse(nMMWB>0,'Yes', 'No')) %>%
  mutate(lake_intersect=ifelse(nLake,'Yes', 'No'))
saveRDS(WetFlow2,file='tmp/WetFlow2')

#######   split_by_stream
#Identify wetlands with a stream running through them - used as a proxy
# to identify wetlands with throughflow (i.e. a linked basin)
StreamsB1 <- Streams %>%
  st_buffer(1) %>%
  st_cast("POLYGON") %>%
  mutate(StreamsB1_id=as.numeric(rownames(.))) %>%
  dplyr::select(StreamsB1_id)
write_sf(StreamsB1, file.path(spatialOutDir,"StreamsB1.gpkg"))
#StreamsB1<-st_read(file.path(spatialOutDir,"StreamsB1.gpkg"))

Wetlands.sp<-as(st_make_valid(Wetlands),'Spatial')
StreamsB1.sp<-as(st_make_valid(StreamsB1),'Spatial')

StrmsThrough.g<- gDifference(Wetlands.sp,StreamsB1.sp, byid=FALSE)
StrmsThrough.sf<-st_as_sf(StrmsThrough.g) %>%
    st_cast("POLYGON")
  write_sf(StrmsThrough.sf, file.path(spatialOutDir,"StrmsThrough.sf.gpkg"))
  #StrmsThrough.sf<-st_read(file.path(spatialOutDir,"StrmsThrough.sf.gpkg"))
  StrmsThrough<-StrmsThrough.sf %>%
    st_intersects(st_as_sf(Wetlands.sp)) %>%
    as.data.frame(.) %>%
    dplyr::rename(wet_id=col.id) %>%
    group_by(wet_id) %>%
    dplyr::summarise(NStreamThrough=n())
  saveRDS(StrmsThrough,file='tmp/StrmsThrough')

#########
WetFlow2<-readRDS(file='tmp/WetFlow2')

WetFlow3<-WetFlow2 %>%
  left_join(StrmsThrough, by='wet_id') %>%
  mutate(split_by_stream=ifelse(NStreamThrough>1,'Yes','No')) %>%
  st_drop_geometry()
#write_sf(WetFlow3, file.path(spatialOutDir,"WetFlow3.gpkg"))

########  Verticalflow, Bidirectional,Throughflow, Outflow, Inflow
WetFlow<-WetFlow3 %>%
  mutate(Verticalflow=ifelse(stream_intersect == 'No' &  river_intersect == 'No' &
                               lake_intersect == 'No' & mmwb_intersect == 'No', 'Yes', 'No')) %>%
  mutate(Bidirectional=ifelse((lake_intersect == 'Yes' |  mmwb_intersect == 'Yes') &
                                !(river_intersect == 'Yes' | stream_intersect == 'Yes'), 'Yes', 'No')) %>%
  mutate(Throughflow=ifelse((river_intersect == 'Yes' |  split_by_stream == 'Yes'), 'Yes', 'No')) %>%
  mutate(Outflow=ifelse((stream_start == 'Yes' &
                           !(split_by_stream == 'Yes' | river_intersect == 'Yes')), 'Yes', 'No')) %>%
  mutate(Inflow=ifelse(((stream_end == 'Yes' &  stream_start == 'No') &
                          !(split_by_stream=='Yes' | river_intersect == 'Yes')) &
                          !(lake_intersect == 'Yes' | mmwb_intersect  == 'Yes'), 'Yes', 'No')) %>%
  dplyr::select(-c(wet_id))

#Generate flow attributes data set
#if 0 ie no cases from connected, unconnected or adjacent water then assign to unconnected
WetFlow$FlowCode[WetFlow$FlowCode==0]<-1

#unique(WetFlow$FlowCode)

#evaluate prevelance of each category
# Generate a breakdown of how many and % wetlands in each flow group
flow.site <- WetFlow %>%
  st_drop_geometry() %>%
  group_by(FlowCode)%>%
  dplyr::summarise(no.pts = n()) %>%
  mutate(perc = ceiling(no.pts / sum(no.pts)*100))

WriteXLS(WetFlow,file.path(dataOutDir,paste('WetFlow.xlsx',sep='')))
#WriteXLS(flow.site,file.path(dataOutDir,paste('ESIFlowxWetland.xlsx',sep='')))


message('Breaking')
break

FlowCheck  <- Wetlands %>%
  #dplyr::rename(BEC_BCWF=BEC) %>%
  left_join(WetFlow, by='WTLND_ID')
write_sf(FlowCheck, file.path(spatialOutDir,"FlowCheck.gpkg"))
#######exploratory code

#WetlandsB<-st_buffer(Wetlands, dist=50) %>%
#  st_collection_extract("POLYGON")

#LakesR<- fasterize(LakesE,ProvRast,field='LakeSize', fun='max')
#LakesR[is.na(LakesR)]<-0
#writeRaster(LakesR,filename=file.path(spatialOutDir,"LakesR.tif"), format="GTiff", overwrite=TRUE)


Wetlands_E <- data.frame(LakeSizeCode=exact_extract(LakesR, wetlandsB, 'max')) #select for largest near by lake
Wetlands_E$wet_id <-as.numeric(rownames(Wetlands_E))

#if no lake around wetland
Wetlands_E$LakeSizeCode[is.na(Wetlands_E$LakeSizeCode)] <- 0

#Give names to the new LandCover' to's 10  categroies
LakeSize_LUT<-data.frame(LakeSizeCode=c(0,1,2,3),
                         LakeSize=c('None', 'Small','Medium','Large'))
saveRDS(LakeSize_LUT,file='tmp/LakeSize_LUT')

Wetlands_L <- Wetlands %>%
  mutate(wet_id=as.numeric(rownames(wetlandsB))) %>%
  left_join(Wetlands_E) %>%
  left_join(LakeSize_LUT)

write_sf(Wetlands_L, file.path(spatialOutDir,"Wetlands_L.gpkg"))

#######exploratory code
StreamsLS1<-st_cast(st_cast(Streams, "MULTILINESTRING"),"LINESTRING")
touching_list = st_touches(StreamsLS1)
g = graph.adjlist(touching_list)#, mode="in")
c = components(g)
#add a new attribute identifying which group a vertice belongs to
StreamsLS1$groups = c$membership
#write_sf(StreamsLS1, file.path(spatialOutDir,"StreamsLS1.gpkg"))


#https://gis.stackexchange.com/questions/295806/r-turn-off-automatic-ordering-of-linestrings-when-applying-sfst-intersection
reorder_segments <- function(src, parts){
  joints = st_intersection(parts)
  joints = joints[st_is(joints,"POINT"),]
  jgraph = igraph::graph_from_edgelist(do.call(rbind, joints$origins), directed=FALSE)
  ends = which(igraph::degree(jgraph) == 1)
  sps = igraph::shortest_paths(jgraph, ends[1], ends[2])
  path = sps$vpath[[1]]
  return(parts[path,])
}
df<-StreamsLS1
df_in <- df %>%
  group_by(groups) #%>% st_intersection(grid)
rs = reorder_segments(df, df_in)
show_order(rs,cex=3)
stream_start<-st_startpoint(rs)#21,872

g1<-graph_from_literal(StreamsLS1)

#Merge streams, first make the streams linestring
#StreamsLS1<-do.call(rbind,lapply(1:nrow(Streams),function(i){st_cast(Streams[i,],"LINESTRING")}))
StreamsLS1<-st_cast(st_cast(Streams, "MULTILINESTRING"),"LINESTRING")

g1<-graph_from_literal(StreamsLS1)

#StreamsLS1<-Streams %>%
#  st_cast("LINESTRING") %>%
#  transmute(stream_id = 1:n()) #21,872
#write_sf(StreamsLS1, file.path(spatialOutDir,"StreamsLS1.gpkg"))
#identify which vertices touch
touching_list = st_touches(StreamsLS1)
g = graph.adjlist(touching_list)#, mode="in")
c = components(g)
#add a new attribute identifying which group a vertice belongs to
StreamsLS1$groups = c$membership
write_sf(StreamsLS1, file.path(spatialOutDir,"StreamsLS1.gpkg"))

#Use group_by to join groups of vertices
StreamsLS2 <- StreamsLS1 %>%
  #group_by(section = as.character({{groups}})) %>%
  group_by(section = as.character(groups)) %>%
  dplyr::summarize() #3,311

#StreamsLS3<-st_cast(StreamsLS2,"LINESTRING")
#StreamsLS3<-st_cast(st_cast(StreamsLS2, "MULTILINESTRING"),"LINESTRING")

StreamsLS<-StreamsLS2
#lwgeom::st_make_valid() %>%
#  st_combine()
#StreamsLS<-do.call(st_combine,

#tt<-lapply(1:nrow(StreamsLS2), function(i){st_combine(StreamsLS2[[i]])})
#StreamsLS1<-do.call(rbind,lapply(1:nrow(Streams),function(i){st_cast(Streams[i,],"LINESTRING")}))
#st_buffer(dist=0) %>%
#st_cast(st_cast(st_cast('MULTILINESTRING'),'LINESTRING'),'MULTILINESTRING')
#StreamsLS<-  st_cast(st_cast(StreamsLS2,'MULTILINESTRING'),'LINESTRING')
write_sf(StreamsLS, file.path(spatialOutDir,"StreamsLS.gpkg"))
#StreamsLS<-st_read(file.path(spatialOutDir,"StreamsLS.gpkg"))
stream_start<-st_startpoint(StreamsLS)#21,872
stream_end<-st_endpoint(StreamsLS)#21,872
write_sf(stream_start, file.path(spatialOutDir,"stream_start.gpkg"))
write_sf(stream_end, file.path(spatialOutDir,"stream_end.gpkg"))

#########
#geos::geos_make_collection() %>%
#  geos::geos_unary_union()
#overGeomGeom(p1,p2)
#converted back to sf
#using sf::st_as_sf().
#DiffTest<-st_difference(Wetlands,StreamsB1)
#StreamsRunThrough2<-Wetlands %>%
#  dplyr::select(wet_id) %>%
#rmapshaper::ms_erase(StreamsB1, remove_slivers=TRUE)
#write_sf(StreamsRunThrough2, file.path(spatialOutDir,"StreamsRunThrough2.gpkg"))
#clgeo_IsValid(as(Wetlands_ws,'Spatial'), verbose = FALSE)
#clgeo_IsValid(as(StreamsB1_ws,'Spatial'), verbose = FALSE)

#StreamsRunTFn<-function(wsNum){
  Wetlands_ws<-as(st_make_valid(st_intersection(Wetlands, AOI_ws[wsNum,])),'Spatial')
  StreamsB1_ws<-as(st_make_valid(st_intersection(StreamsB1, AOI_ws[wsNum,])),'Spatial')

  #loop through each of the watersheds in the AOI
  ws_list<-list()
  ws_list <-lapply(c(3:3), function(i) StreamsRunTFn(i))
  ws_list.5<-ws_list
  #ws_list <-lapply(c(1:nrow(AOI_ws)), function(i) StreamsRunTFn(i))
  #StreamsRunThroughtt<-ws_list[[1]]
  #
  StreamsRunThrough2<-ldply(ws_list,data.frame)
  saveRDS(StreamsRunThrough2,file='tmp/StreamsRunThrough2')

