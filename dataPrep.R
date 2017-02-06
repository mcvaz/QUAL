## Start from scratch
# include species names and initial DBH and date from CTFS database

# include maximum DBH

# include leaf areas

# include light measurements


## Started from previous work

# All architecture variables
raw = read.csv("MAO16tmpTraits.csv",header=T,as.is=T)
str(raw)
head(raw)
raw$sp = paste(raw$genus,raw$epithet)

# Maximum size values
maxSizes = read.csv("maxsizes.csv",header=T,as.is=T)
str(maxSizes)
head(maxSizes)
maxSizes$sp = paste(maxSizes$Genus,maxSizes$epithet)
Hmax = apply(maxSizes[,c("q975","q975F")],1,max,na.rm=T)
mS = cbind(maxSizes,Hmax)
mS = subset(mS,select=c("sp","Hmax"))
subset(mS,sp=="Dinizia excelsa")
subset(mS,sp=="Pausandra macropetala")
hist(Hmax)

# Combining both into a same dataframe
unique(raw$sp)[!unique(raw$sp)%in%unique(maxSizes$sp)]
raw2 = merge(raw,mS,all.x=T)
write.csv(raw2,"raw2.csv")












