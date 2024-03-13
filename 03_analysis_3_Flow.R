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

#
Wetlands<-read_sf(file.path(spatialOutDir, "Wetlands.gpkg"))

#Lakes
Lakes <- read_sf(file.path(spatialOutDir,"Lakes.gpkg")) %>%
  st_intersection(AOI) %>%
  mutate(lake_id=as.numeric(rownames(.))) %>%
  st_buffer(50) %>%
  st_cast("POLYGON") %>%
  mutate(LakeSize=case_when(
    AREA_HA >5 ~ 3,
    AREA_HA <=5 & AREA_HA >1 ~ 2,
    AREA_HA <=1 ~ 1,
    TRUE ~ 0
  ))
LakeNearBy1<-Lakes %>%
  st_intersects(Wetlands) %>%
  tibble::enframe(name = 'lake_id', value = 'wet_id') %>%
  tidyr::unnest(wet_id)
LakeNearBy <- LakeNearBy1 %>%
  mutate(Lake=ifelse(lake_id>0, 1, 0)) %>%
  left_join(Lakes) %>%
  dplyr::select(lake_id, wet_id, Lake, LakeHa=AREA_HA)

#Rivers
Rivers<-read_sf(file.path(spatialOutDir,"Rivers.gpkg")) %>%
  st_intersection(AOI)
RiversB <- Rivers %>%
  st_buffer(50) %>%
  st_cast("POLYGON")
RiverNearBy<-RiversB %>%
  st_intersects(Wetlands) %>%
  tibble::enframe(name = 'River', value = 'wet_id') %>%
  tidyr::unnest(wet_id)
RiverNearBy <- RiverNearBy %>%
  mutate(River=ifelse(River>0, 1, 0))

#join the water data frames with 1 record per wetland
WaterNearBy <- LakeNearBy %>%
  full_join(RiverNearBy, by='wet_id') %>%
  group_by(wet_id) %>%
  dplyr::summarise(nRiver=sum(River), nLake=sum(Lake),
                   LakeHa=max(LakeHa)) %>%
  mutate(Water=ifelse((nRiver | nLake)>0, 1, 0)) %>%
  mutate(LakeSize=case_when(
    LakeHa >5 ~ 1,
    LakeHa <=5 & LakeHa >1 ~ 2,
    LakeHa <=1 ~ 3,
    TRUE ~ 0
  ))

#Join water features
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
  dplyr::select("WTLND_ID",'wet_id',
                "nRiver","nLake",'LakeSize',"Water") %>%
  mutate(ConnectedWet=ifelse(nRiver>0 | nLake>0, 2, 0)) %>%
  mutate(UnconnectedWet=ifelse(ConnectedWet==0, 1,0)) %>%
  mutate(AdjacentWaterWet=ifelse(Water>0, 3, 0)) %>%
  mutate(FlowCode=pmax(ConnectedWet,UnconnectedWet,AdjacentWaterWet, na.rm=FALSE)) %>%
  dplyr::select(WTLND_ID,
                FlowCode, Water, nRiver,nLake, LakeSize)

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

message('Breaking')
break


#######exploratory code
wetlandsB<-read_sf(file.path(spatialOutDir,"wetlandsB.gpkg"))

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


