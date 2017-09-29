##### Working with the Package AQP #######
##### http://ncss-tech.github.io/AQP/soilDB/gSSURGO-SDA.html#####
#### Felipe Montes 2016 09 09 #############


#  Tell the program where the package libraries are  #####################


.libPaths("C:/Felipe/Sotware&Coding/R_Library/library")  ;



#### Install pakages #####

install.packages('raster', dep=TRUE)
install.packages('plyr', dep=TRUE)
install.packages('Hmisc', dep=TRUE)
install.packages('soilDB', dep=TRUE) # stable version from CRAN + dependencies
install.packages("soilDB", repos="http://R-Forge.R-project.org") # most recent copy from r-forge
install.packages("SSOAP", repos = "http://www.omegahat.org/R", type="source") # SSOAP and XMLSchema
install.packages("foreign")
install.packages("httr", dep=TRUE)
install.packages("rgdal", dep = TRUE)
install.packages("raster", dep = TRUE)
install.packages("rgeos", dep = TRUE)
install.packages("RColorBrewer")
install.packages("latticeExtra")A
install.packages("reshape")


# function for computing profile-total water storage
co.sum.whc <- function(i) {
    wt <- i$comppct_r[1] # keep the first component pct (they are all the same)
    thick <- with(i, hzdepb_r - hzdept_r) # compute horizon thickness
    whc <- thick * i$awc_r # compute water storage by horizon
    whc.total <- sum(whc, na.rm=TRUE) # sum to get profile water storage
    data.frame(whc=whc.total, wt=wt) # return profile water storage and component pct
}

# function for computing weighted-mean whc within a map unit
mu.mean.whc <- function(i) {
    whc <- wtd.mean(i$whc, weights=i$wt) # safely compute wt. mean water storage
    data.frame(whc=whc) # return wt. mean water storage
}



# load libraries
library(Hmisc)
library(soilDB)
library(plyr)
library(raster)
library(aqp)
library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(lattice)
library(MASS)

# load chunk of gSSURGO
data(gSSURGO.chunk, package='soilDB')

# convert into a raster + RAT
gSSURGO.chunk <- ratify(gSSURGO.chunk, count=TRUE)

# save RAT to new object, will use later
rat <- levels(gSSURGO.chunk)[[1]]

# extract the map unit keys from the RAT, and format for use in an SQL IN-statement
in.statement <- format_SQL_in_statement(rat$ID)


# format query in SQL- raw data are returned
q <- paste("SELECT component.mukey, component.cokey, compname, comppct_r, hzdept_r, hzdepb_r, hzname, awc_r
FROM component JOIN chorizon ON component.cokey = chorizon.cokey
AND mukey IN ", in.statement, "ORDER BY mukey, comppct_r DESC, hzdept_r ASC", sep="")

# now get component and horizon-level data for these map unit keys
res <- SDA_query(q)
head(res)


q2 <- paste0("SELECT component.mukey, component.cokey, compname, comppct_r, majcompflag, slope_r, hzdept_r, hzdepb_r, hzname, awc_r, sandtotal_r, silttotal_r, claytotal_r FROM component JOIN chorizon ON component.cokey = chorizon.cokey AND mukey IN ", in.statement," ORDER BY mukey, comppct_r DESC, hzdept_r ASC") ;
           
res2 <- SDA_query(q2)
head(res2,20)

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



# aggregate by component, retaining map unit keys
co.whc <- ddply(res, c('mukey', 'cokey'), co.sum.whc)

# aggregate by map unit
mu.whc <- ddply(co.whc, 'mukey', mu.mean.whc)

# check: there should be a single water storage value per map unit key
head(mu.whc)

# change first colum name from 'mukey' to 'ID', so that it matches our RAT
names(mu.whc)[1] <- 'ID'

# combine RAT with aggregate data via "left" join
rat.new <- join(rat, mu.whc, type='left')

# put modified RAT back into our raster object
levels(gSSURGO.chunk) <- rat.new

# convert into standard raster based on new column
r.new <- deratify(gSSURGO.chunk, att='whc')

# check: OK
plot(r.new)


library(plyr)
library(raster)
library(foreign)

# load exported gSSURGO chunk, cell values are NOT map unit keys


r <- raster('C:\\Felipe\\PIHM-CYCLES\\PIHM\\PIHM_Felipe\\CNS\\Manhantango\\gssurgo_g_pa\\GSSURGO_PA_TIFF\\MahatangoGSSG1.tif') ;

# generate a RAT via raster package functionality
r <- ratify(r) ;

# extract RAT to a data.frame
rat <- levels(r)[[1]] ;

# extract the map unit keys from the RAT, and format for use in an SQL IN-statement
in.statement2 <- format_SQL_in_statement(rat$ID)



# format query in SQL- raw data are returned

q3 <- paste0("SELECT component.mukey, component.cokey, compname, comppct_r, majcompflag, slope_r, hzdept_r, hzdepb_r, hzname, awc_r, sandtotal_r, silttotal_r, claytotal_r, om_r,dbtenthbar_r, dbthirdbar_r, dbfifteenbar_r, fraggt10_r, frag3to10_r, sieveno10_r, sieveno40_r, sieveno200_r  FROM component JOIN chorizon ON component.cokey = chorizon.cokey AND mukey IN ", in.statement2," ORDER BY mukey, comppct_r DESC, hzdept_r ASC") ;

# now get component and horizon-level data for these map unit keys
res3 <- SDA_query(q3);
head(res3) ;

res3$id<-res3$mukey ;
res3$top<-res3$hzdept_r ;
res3$bottom<-res3$hzdepb_r ;
res3$name<-res3$hzname ;

depths(res3)<-id ~ top + bottom  ;
str(res3) ;


plot(res3[1:10], name='name', color='frag3to10_r')


# load ESRI-specific RAT, generated when gSSURGO was exported
mu <- read.dbf('C:\\Felipe\\PIHM-CYCLES\\PIHM\\PIHM_Felipe\\CNS\\Manhantango\\gssurgo_g_pa\\GSSURGO_PA_TIFF\\MahatangoGSSG1.tif.vat.dbf') ;

# re-name the first coulmn to match our new RAT
names(mu)[1] <- 'ID'

# convert map unit keys from character to integer
mu$MUKEY <- as.integer(mu$MUKEY)


# join map unit keys to gSSURGO integer indices
rat.new <- join(rat, mu, by='ID', type='left')

# over-write original RAT with new one, containing map unit keys
levels(r) <- rat.new

# make a new raster, this time with map unit keys used as the cell values
r.mu <- deratify(r, att='MUKEY')

plot(r.mu)
