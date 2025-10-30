install.packages("terra")
library(terra)
install.packages("tidyterra")
library(tidyterra)
install.packages("FedData")
library(FedData)

nlcd_meve16 <-get_nlcd(template = FedData::meve,
                       label = "meve",
                       year = 2016,
                       extraction.dir = "Z:/zwang2/github_lw/GEOG331/data")
terra::plot(nlcd_meve16)
cavm <- vect("Z:/zwang2/github_lw/GEOG331/data/cp_veg_la_shp")

cavm
