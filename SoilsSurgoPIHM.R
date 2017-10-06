#################### Program to create the soils File in PIHM from Data in SSurgo and other resources##################
################# Felipe Montes ##########################
################# 2015 11 13 ############################
################# REvised 2017/09/29 ####################

###############################################################################################################
#                          Loading Packages and setting up working directory                        
###############################################################################################################



#  Tell the program where the package libraries are  #####################


.libPaths("C:/Felipe/Sotware&Coding/R_Library/library")  ;

#  Set Working directory


setwd("C:/Felipe/PIHM-CYCLES/PIHM/PIHM_Felipe/CNS/WE-38/WE38_Files_PIHM_Cycles20170208/SWATPIHMRcode") ; 

########### Install packages  #####################


# install.packages('raster', dep=TRUE)
# install.packages('plyr', dep=TRUE)
# install.packages('Hmisc', dep=TRUE)
# install.packages('soilDB', dep=TRUE) # stable version from CRAN + dependencies
# install.packages("soilDB", repos="http://R-Forge.R-project.org") # most recent copy from r-forge
# install.packages("SSOAP", repos = "http://www.omegahat.org/R", type="source") # SSOAP and XMLSchema
# install.packages("foreign")
# install.packages("httr", dep=TRUE)
# install.packages("rgdal", dep = TRUE)
# install.packages("raster", dep = TRUE)
# install.packages("rgeos", dep = TRUE)
# install.packages("RColorBrewer")
# install.packages("latticeExtra")A
# install.packages("reshape")

########### Call the library packages needed for the program to work #############

# load libraries
library(Hmisc) ;
library(soilDB) ;
library(plyr) ;
library(raster) ;
library(aqp) ;
library(sp) ;
library(rgdal) ;
library(raster) ;
library(rgeos) ; 
library(lattice) ;
library(MASS) ;
library(RColorBrewer) ;
library(ggplot2)  ;
#library(tmap) ;
library(dplyr)  ;
library(tidyr)  ;
library(devtools) ;
   


########################## import the raster file with the GSSURGO Data for the watershed in PIHM ####################


Manhatango_GSSURGO<- raster('C:\\Felipe\\PIHM-CYCLES\\PIHM\\PIHM_Felipe\\CNS\\Manhantango\\gssurgo_g_pa\\GSSURGO_PA_TIFF\\MahatangoGSSG1.tif') ;

# generate a RAT via raster package functionality
Manhatango_GSSURGO <- ratify(Manhatango_GSSURGO) ;

# extract RAT to a data.frame
MUKEYS <- levels(Manhatango_GSSURGO)[[1]] ;


################################ Query the Soil Data access database with SQL through R #################


# from https://sdmdataaccess.sc.egov.usda.gov/queryhelp.aspx
# and https://sdmdataaccess.sc.egov.usda.gov/documents/ReturningSoilTextureRelatedAttributes.pdf


# --Sample query begins.
# --Note that a pair of dashes denotes the beginning of a comment. 
# SELECT
# saversion, saverest, -- attributes from table "sacatalog"
# l.areasymbol, l.areaname, l.lkey, -- attributes from table "legend"
# musym, muname, museq, mu.mukey, -- attributes from table "mapunit"
# comppct_r, compname, localphase, slope_r, c.cokey, -- attributes from table "component"
# hzdept_r, hzdepb_r, ch.chkey, -- attributes from table "chorizon"
# sandtotal_r, silttotal_r, claytotal_r, --total sand, silt and clay fractions from table "chorizon"
# sandvc_r, sandco_r, sandmed_r, sandfine_r, sandvf_r,--sand sub-fractions from table "chorizon"
# texdesc, texture, stratextsflag, chtgrp.rvindicator, -- attributes from table "chtexturegrp"
# texcl, lieutex, -- attributes from table "chtexture"
# texmod -- attributes from table "chtexturemod"
# FROM sacatalog sac
# INNER JOIN legend l ON l.areasymbol = sac.areasymbol AND l.areatypename = 'Non-MLRA Soil Survey Area'
# INNER JOIN mapunit mu ON mu.lkey = l.lkey
# AND mu.mukey IN
# ('107559','107646','107674','107682','107707','107794','107853','107854','107865','107867','107869','107870','107871')
# LEFT OUTER JOIN component c ON c.mukey = mu.mukey
# LEFT OUTER JOIN chorizon ch ON ch.cokey = c.cokey
# LEFT OUTER JOIN chtexturegrp chtgrp ON chtgrp.chkey = ch.chkey
# LEFT OUTER JOIN chtexture cht ON cht.chtgkey = chtgrp.chtgkey
# LEFT OUTER JOIN chtexturemod chtmod ON chtmod.chtkey = cht.chtkey
# --WHERE.
# --ORDER BY l.areaname, museq, comppct_r DESC, compname, hzdept_r -- standard soil report ordering
# --Sample query ends. 

# extract the map unit keys from the RAT, and format for use in an SQL IN-statement
in.statement2 <- format_SQL_in_statement(MUKEYS$ID)


# format query in SQL- raw data are returned

Pedon.query<- paste0("SELECT component.mukey, component.cokey, compname, comppct_r, majcompflag, slope_r, hzdept_r, hzdepb_r, hzname, awc_r, sandtotal_r, silttotal_r, claytotal_r, om_r,dbtenthbar_r, dbthirdbar_r, dbfifteenbar_r, fraggt10_r, frag3to10_r, sieveno10_r, sieveno40_r, sieveno200_r, ksat_r  FROM component JOIN chorizon ON component.cokey = chorizon.cokey AND mukey IN ", in.statement2," ORDER BY mukey, comppct_r DESC, hzdept_r ASC") ;

# now get component and horizon-level data for these map unit keys
Pedon.info<- SDA_query(Pedon.query);
head(Pedon.info) ;

# filter components that are the major components of each unit map with the Flag majcompflag=='Yes'

Pedon.info.MajorC<-Pedon.info[which(Pedon.info$majcompflag == 'Yes'),]  ;
head(Pedon.info.MajorC) ; 

# check if there are mukeys with more than one dominant component

Pedon.info.MajorC$mukey.factor<-as.factor(Pedon.info.MajorC$mukey) ;

Pedon.info.MajorC$cokey.factor<-as.factor(Pedon.info.MajorC$cokey) ;

Pedon.info.MajorC$mukey_comppct_r<-paste(Pedon.info.MajorC$mukey.factor,Pedon.info.MajorC$comppct_r, sep = "_") ;

# Select major component mukeys that have also the highest component percent comppct_r

head(Pedon.info.MajorC)

Dominant<- aggregate(comppct_r ~ mukey.factor, data=Pedon.info.MajorC, FUN="max" , drop=T, simplify=T) ;

head(Dominant)

Dominant$mukey_comppct_r<-paste(Dominant$mukey.factor,Dominant$comppct_r, sep ="_");


Mukey.Pedon<-Pedon.info.MajorC[Pedon.info.MajorC$mukey_comppct_r %in% Dominant$mukey_comppct_r,]



#  Transform the Pedon.info query in to the right format to be converted into a SoilProfileCollection object
#   https://ncss-tech.github.io/AQP/aqp/aqp-intro.html


#Pedon.info$id<-Pedon.info$mukey ;
# Pedon.info$top<-Pedon.info$hzdept_r ;
# Pedon.info$bottom<-Pedon.info$hzdept_r ;
#Pedon.info$name<-Pedon.info$hzname ;

depths(Mukey.Pedon)<-mukey ~ hzdept_r + hzdepb_r  ;
str(Mukey.Pedon) ;


plot(Mukey.Pedon[1:20], name='hzname',color='dbthirdbar_r')  ;




