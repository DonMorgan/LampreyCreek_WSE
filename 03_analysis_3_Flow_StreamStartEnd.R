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
#testing data set
#mapview(TestAOI) + mapview(AOI)
#AOI<-TestAOI
WetlandsT<-Wetlands #%>%
  st_intersection(AOI)
#write_sf(WetlandsT, file.path(spatialOutDir,"WetlandsT.gpkg"))

DEM_AOI<-rast(file.path(spatialOutDir,paste0('DEMtp_',WetlandAreaShort,'.tif'))) #%>%
  #mask(vect(AOI)) %>%
  #crop(vect(AOI))
#writeRaster(DEM_AOI, filename=file.path(spatialOutDir,'DEM_AOI.tif'), overwrite=TRUE)

#DEM_AOI<-rast(DEM_AOI)

#Assign average elevation to stream segment
#Streams<-st_read(file.path(spatialOutDir,"Streams.gpkg"))
StreamsT<-Streams %>%
  dplyr::filter(EDGE_TYPE %in% c('1000', '1050', '1100', '1150', '1250', '2000', '2300')) %>%
  st_intersection(AOI) %>%
  mutate(stream_id=seq.int(nrow(.)))
StreamsT<- st_cast(st_cast(StreamsT,"MULTILINESTRING"),"LINESTRING")
write_sf(StreamsT, file.path(spatialOutDir,"StreamsT.gpkg"))
StreamsT<-st_read(file.path(spatialOutDir,"StreamsT.gpkg"))

#Merge contiguous stream segments - this helps to determine relative elevation of stream touching a wetland
#https://gis.stackexchange.com/questions/310462/cluster-geometries-that-touch-each-other
touching_list = st_touches(StreamsT)
g = graph.adjlist(touching_list)
c = components(g)
StreamsT$groups = c$membership

StreamsT.grouped <- StreamsT %>%
  group_by(groups) %>%
  dplyr::summarise() %>%
  mutate(stream_id=seq.int(nrow(.)))
write_sf(StreamsT.grouped, file.path(spatialOutDir,"StreamsT.grouped.gpkg"))

#flip streams to Terra format for extract to assign elevation to stream reach
# can then determine if a wetland is above or below the mean elevation of the reach
StreamsT.v<-vect(StreamsT)
StreamsT.v$ID_UF <- 1:nrow(StreamsT.v)
#find mean stream elevation
StreamsT.v.d<-terra::extract(DEM_AOI,StreamsT.v,mean,bind=TRUE) #max height of network
# any wetlands within network will be lower ie recieving water
# any wetalnds contributing water should be higer - but no.....
StreamsTD<-st_as_sf(StreamsT.v.d) %>%
  dplyr::rename(StrmElevation=names(DEM_AOI))
write_sf(StreamsTD, file.path(spatialOutDir,"StreamsTD.gpkg"))
#StreamsTD<-st_read(file.path(spatialOutDir,"StreamsTD.gpkg"))

#Use Geos to get stream ends and starts - works better than st_ version
#https://dewey.dunnington.ca/post/2021/stream-networks-using-r-and-geos/
start1 <- StreamsTD %>%
  as_geos_geometry(.) %>%
  geos_unnest(keep_multi = FALSE) %>%
  #start
  geos_point_start() %>%
  st_as_sf() %>%
  mutate(end_id=as.numeric(rownames(.)))
end1 <- StreamsTD %>%
  as_geos_geometry(.) %>%
  geos_unnest(keep_multi = FALSE) %>%
  #end
  geos_point_end() %>%
  st_as_sf() %>%
  mutate(end_id=as.numeric(rownames(.)))

#########
#Start & End - add elevation
start1.v<-vect(start1)
start1.v$ID_UF <- 1:nrow(start1.v)
start1.v.d<-terra::extract(DEM_AOI,start1.v,max,method='bilinear',bind=TRUE)
start1D<-st_as_sf(start1.v.d) %>%
  dplyr::rename(PtElevation=names(DEM_AOI))
#write_sf(start1D, file.path(spatialOutDir,"start1D.gpkg"))

end1.v<-vect(end1)
end1.v$ID_UF <- 1:nrow(end1.v)
end1.v.d<-terra::extract(DEM_AOI,end1.v,max,method='bilinear',bind=TRUE)
end1D<-st_as_sf(end1.v.d) %>%
  dplyr::rename(PtElevation=names(DEM_AOI))
#write_sf(end1D, file.path(spatialOutDir,"end1D.gpkg"))

##End Points
#function to get only those points at end or beginning of linestring
#Start
start_intersection<-start1D %>%
  st_intersection(StreamsTD) %>%
  dplyr::select(end_id,stream_id, StrmElevation,PtElevation) %>%
  st_drop_geometry() %>%
  group_by(end_id) %>%
  dplyr::summarize(n=n(),PtElevation=max(PtElevation),StrmElevation=max(StrmElevation),
                   stream_id=first(stream_id)) %>%
  dplyr::filter(n==1)

stream_start<-start1 %>%
  left_join(start_intersection) %>%
  dplyr::filter(end_id %in% start_intersection$end_id) #keep only nodes with no neighbour
#dplyr::filter(end_id %in% end2$end_id & n==1) #keep only nodes with no neighbour
write_sf(stream_start, file.path(spatialOutDir,"stream_start.gpkg"))

###
#End
end_intersection<-end1D %>%
  st_intersection(StreamsTD) %>%
  dplyr::select(end_id,stream_id, PtElevation,StrmElevation) %>%
   #write_sf(end_intersection, file.path(spatialOutDir,"end_intersection.gpkg"))
  st_drop_geometry() %>%
  group_by(end_id) %>%
  dplyr::summarize(n=n(),PtElevation=max(PtElevation),StrmElevation=max(StrmElevation),
                   stream_id=first(stream_id)) %>%
  dplyr::filter(n==1)

stream_end<-end1 %>%
  left_join(end_intersection) %>%
  dplyr::filter(end_id %in% end_intersection$end_id) #keep only nodes with no neighbour
#dplyr::filter(end_id %in% end2$end_id & n==1) #keep only nodes with no neighbour
write_sf(stream_end, file.path(spatialOutDir,"stream_end.gpkg"))

stream_start_end<-rbind(stream_start, stream_end) %>%
  mutate(end=case_when(
    PtElevation > StrmElevation ~ "start",
    PtElevation <= StrmElevation ~ "end",
    TRUE ~ "other"))
table(stream_start_end$end)
write_sf(stream_start_end, file.path(spatialOutDir,"stream_start_end.gpkg"))

##############

#Start in Wetland
WetlandsTendB<-st_buffer(WetlandsT,dist=10)
Strm_start_end <- st_join(WetlandsTendB,stream_start_end,join = st_covers) %>%
  dplyr::filter(!(is.na(WTLND_ID))) %>%
  st_drop_geometry() %>%
  mutate(starting=ifelse(end=='start',1,0)) %>%
  mutate(ending=ifelse(end=='end',1,0)) %>%
  group_by(WTLND_ID) %>%
  dplyr::summarize(n=n(),nEnd=sum(ending),nStart=sum(starting)) %>%
  #mutate(stream_start=ifelse(nEnd>0 & nStart==0,'Yes','No')) %>%
  #mutate(stream_end=ifelse(nStart>0 & nEnd==0,'Yes','No')) %>%
  #mutate(stream_StartEnd=ifelse(nEnd>0 & nStart>0,'Yes','No'))
  mutate(stream_end=ifelse(nEnd>0 & nStart==0,'Yes','No')) %>%
  mutate(stream_start=ifelse(nStart>0 & nEnd==0,'Yes','No')) %>%
  mutate(stream_StartEnd=ifelse(nEnd>0 & nStart>0,'Yes','No'))

tt<-Strm_start_end %>%
  dplyr::filter(n>1)

write_sf(Strm_start_end, file.path(spatialOutDir,"Strm_start_end.gpkg"))
Strm_start_end.dat<-Strm_start_end %>% st_drop_geometry()
WriteXLS(Strm_start_end.dat,file.path(dataOutDir,'Strm_start_end.xlsx'),SheetNames = NULL)

message('Breaking')
break







######exploratory code
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


End_Fn <- function(terminus) {
  end2<-terminus %>%
    st_intersects(StreamsTD)

  end2<-StreamsT %>%
    st_intersects(terminus) %>% # get intersection of end nodes with source stream lines
    #tibble::enframe(.)
    tibble::enframe(name = 'Streams_id', value = 'end_id') %>%
    tidyr::unnest(end_id) %>%
    group_by(end_id) %>%
    dplyr::summarize(n=n(),Streams_id=first(Streams_id)) #%>% #identify all nodes that have no neighbour ie a true end
  #dplyr::filter(n==1)

  stream_end<-termunus %>%
    left_join(end2) %>%
    dplyr::filter(end_id %in% end2$end_id) #keep only nodes with no neighbour
  #dplyr::filter(end_id %in% end2$end_id & n==1) #keep only nodes with no neighbour
  write_sf(stream_end, file.path(spatialOutDir,"stream_end.gpkg"))

  return(stream_end)
}


