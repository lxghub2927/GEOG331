library(terra)
library(tidyterra)
library(FedData)

f <- list.files("Z:/zwang2/github_lw/GEOG331/data(lw)/landsat", full.names = T)

lc <- rast(f[3:10])
lc[5]
summary(lc[[5]])
plot(lc[[5]])
plot(lc[[5]]*0.0000275-0.2) #shows that we can directly perform mathematical operations in plot *Note: This is not permanent. Consider creating a new object if required.

ndvi <- (lc[[5]]-lc[[4]])/(lc[[5]]+lc[[4]])
names(ndvi) <- "names"
plot(ndvi)

dec_lands <- vect("Z:/zwang2/github_lw/GEOG331/data(lw)/NYS_DEC_Lands")
mad_dec <- dec_lands[dec_lands$COUNTY == "Madison",]
