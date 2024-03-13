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

wetsRoads1<-st_read(file.path(spatialOutDir,"wetsRoads1.gpkg"))

#wetsRoads1C<-wetsRoads1 %>% st_drop_geometry()

wetsRoads<-wetsRoads1 %>%
  st_drop_geometry() %>%
  #dplyr::filter(pcentInBuf>75.00) %>%
  mutate(win500=if_else((pcentIn500Buf>75.00),1,0)) #%>%
  #Flag wetlands within 50m of road
  #dplyr::filt  mutate(win500=if_else((pcentIn500Buf>75.00),1,0)) %>%
  #mutate(win50=if_else((WTLND_ID %in% wets50mToDrop$WTLND_ID),1,0))

# make a list of Wetlands and their road status
road.ls <- wetsRoads %>%
  dplyr::select(WTLND_ID, pcentIn500Buf, win500)
#dplyr::select(WTLND_ID, pcentIn500Buf, win500, win50)
#unique(disturb.ls$DisturbType)
WriteXLS(road.ls,file.path(dataOutDir,paste('road.ls.xlsx',sep='')))
