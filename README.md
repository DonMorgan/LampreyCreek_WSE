# LampreyCreek_WSE
R code for conducting data collation, analysis and figures for the Lamprey Creek WSE

Lamprey Creek Watershed Status Evaluation (WSE)
================================================
Under British Columbia’s Forest and Range Practices Act Government Actions Regulation, and the Oil and Gas Activities Act Environmental Management and Protection Regulation, watersheds with significant fish values and watershed sensitivity can be designated as Fisheries Sensitive Watersheds (FSWs). Effectiveness monitoring and assessment is critical to ensure that FSW designations succeed in achieving the intended goals of maintaining natural functions and processes to conserve healthy fish habitats and associated fish populations. Integrated GIS-based (Tier I) and field-based (Tier II) methods for assessing the habitat status of FSWs have been developed jointly by government and private partners through watershed status evaluation monitoring protocols. 

The Lamprey Creek WSE Tier I GIS-based methods, as described in Porter et al. (2013), is used to assess the watershed indicator “risk” status the watershed. Measured values of habitat indicators within the watersheds were compared to indicator benchmarks defined in Porter et al. (2013) to assess risk “status” (i.e., GIS-derived watershed values relative to indicator benchmarks). The defined indicator benchmarks represent one of three risk levels associated with fish habitat impact: (1) low, (2) moderate, and (3) high. 

### Usage

There are a set of scripts that help organize the data for the Tier I and Tier II analysis:    
Control scripts - set up the analysis environment;   
Load scripts - loads base data;    
Clean scripts - cleans spatial layers for attributing watershd; and    
Analysis scripts - generate watershed Tier I and II indicators.    

#Control Scripts:   
run_all.R	Sets local variables and directories used by scripts, presents script order.   
header.R	loads R packages, sets global directories, and attributes.  

#Load Scripts:	
01_base_load.R	Loads core spatial layers used by routines.  

#Clean Scripts:   
02_cleanRoads_AOI.R	Clean road data for watershed.   
02_clean_disturb_AOI.R	Clean Provincial disturbance data for watershed.  

#Analysis Scripts:   
03_analysis_1_Roads.R	Assign road indicators.    
03_analysis_3_Flow_byWshd.R	if needed to process large study areas.  
03_analysis_3_New_Flow.R	New base wetlands - generate flow attributes.  
03_analysis_3_Flow_StreamStartEnd.R	Determine stream start and ends for each wetland.   
03_analysis_4_Disturbance.R	Assign disturbance indicators by wetland.   
03_analysis_4_RdDisturbance.R	Assign Road Disturbance indicators by wetland.  
03_analysis_5_LandCover.R	Assign Land Cover indicators by wetland.  

### Project Status

The set of R WSE scripts are continually being modified and improved, including adding new watersheds as sampling is initiated.

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an [issue](https://github.com/BCWF-Wetlands/WESP_data_prep/issues/).

### How to Contribute

If you would like to contribute, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

### License

```
### License

```
Copyright 2022 Province of British Columbia

Licensed under the Apache License, Version 2.0 (the &quot;License&quot;);
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an &quot;AS IS&quot; BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and limitations under the License.
```
---
