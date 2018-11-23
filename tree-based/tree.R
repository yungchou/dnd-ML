library(rattle); rattle()
#library(Rcmdr)

glass <-  read.csv('n:/dataset/glass.data')
names(glass)
colnames(glass) <-
  c('ID','RI','Na','Mg','Al','Si','K','Ca','Ba','Fe','Type')
library(plyr)
glass$Type <- mapvalues(glass$Type,from=c(1,2,3,5,6,7),
    to=c('bldg_windows_float','bldg_windows_not_float',
         'vehicle_windows_float','containers','tableware','headlamps'))

str(glass)
bar(glass$Type)

library(tree)
model <- tree(Pollution ~ . , Pollute)
Pollute <- read.table("c:\\temp\\Pollute.txt",header=T)
attach(Pollute)
names(Pollute)
