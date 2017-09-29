#################### Program to create the soils File in PIHM from Data in SSurgo and other resources##################
################# Felipe Montes ##########################
################# 2015 11 13 ############################


###############################################################################################################
#                          Loading Packages and setting up working directory                        
###############################################################################################################



#  Tell the program where the package libraries are  #####################


.libPaths("C:/Felipe/Sotware&Coding/R_Library/library")  ;

#  Set Working directory


setwd("C:/Felipe/PIHM-CYCLES/PIHM/PIHM_Felipe/CNS/WE-38/WE38_Files_PIHM_Cycles20170208/SWATPIHMRcode") ; 

########### Tell the program where the package libraries are  #####################

########### Call the library packages needed for the program to work #############



library(rgdal);

library(sp);
   
library(RColorBrewer) ;
   
library(lattice) ;
   
library(ggplot2)  ;
   
library(rgeos)   ;
   
library(tmap) ;
  
library(dplyr)  ;

library(tidyr)  ;
   


##### Read the shape files form the shape files used in preparation of the mesh file #######


WE38.mesh<-readOGR("../../input/ShapeFiles/MergeVectorLayer000_q25_a50000.shp","MergeVectorLayer000_q25_a50000" ); 

WE38.river<-readOGR("../../input/ShapeFiles/Stream12000_sln30_xln_Decomp_Decomp_Decomp.shp","Stream12000_sln30_xln_Decomp_Decomp_Decomp" ); 


######### Import the shape File into R

GSurgoSoils.shp<-readOGR("C:/Felipe/PIHM-CYCLES/PIHM/PIHM_Felipe/CNS/Manhantango/HydroTerreFullManhantango/Soils/GsurgoSoilsExtract.shp","GsurgoSoilsExtract")   ;

#  GSurgoSoils.shp<-readOGR("E:\\ManhantangoPIHM\\Soils\\Gsurgosoils.shp","Gsurgosoils")   ;

#  C:/Felipe/PIHM-CYCLES/PIHM/PIHM_Felipe/CNS/Manhantango/HydroTerreFullManhantango/Soils/GsurgoSoilsExtract.shp

############ Querying the data in the shape file

names(GSurgoSoils.shp@data)<-c("Ele_ID","MUKEY") ; 

#converting the MUKEY as factor to use the levels to identify which MUKEY are present


GSurgoSoils.shp@data$Factor.Mukey<-as.factor(GSurgoSoils.shp@data$MUKEY) ;

MUKEYS.Manhantango<-as.numeric(levels(GSurgoSoils.shp@data$Factor.Mukey)) ;

#Write a table with the MUKEYs present

write(MUKEYS.Manhantango, file="E:\\ManhantangoPIHM\\Soils\\MUKEYS.Manhantango.txt", ncolumns=1)  ;



