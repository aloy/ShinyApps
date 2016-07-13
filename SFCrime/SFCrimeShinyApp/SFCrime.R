
#"http://data.sfgov.org/resource/cuks-n6tp.csv?$where=date >'2014-01-01T00:00:00'")
#------------------------------------
df.sf_crime <- read.csv("SFPD.csv")
opar <- par()

library(rgdal)
df_neigh <- spTransform(readOGR(".",'planning_neighborhoods') ,CRS("+proj=longlat +ellps=WGS84"))
df_water <- spTransform(readOGR(".",'phys_waterbodies') ,CRS("+proj=longlat +ellps=WGS84"))

par(bty='n',yaxt='n',mar=c(7,0,0,0))
barplot(sort(table(df.sf_crime$Category)),cex.names=.4,las=2)
popcrime <- tail(names(sort(table(df.sf_crime$Category))),10)




#########################################################
par(opar)
for(i in 4:4){
  par(bty='n',xaxt='n',yaxt='n',mar=c(0,0,0,0))
  plot(df_neigh)
  plot(df_water,add=TRUE,col='lightblue')
  with(df.sf_crime[df.sf_crime$Category %in% popcrime[i],], points(X,Y,pch = 19,col=rainbow(40,alpha = .5)[Category],cex=.2))
}

####################################################################

subst <- df.sf_crime$Category == 'WARRANTS'

df_neigh_tmp <- df_neigh
sp <- SpatialPoints(df.sf_crime[subst,c('X','Y')],CRS("+proj=longlat +ellps=WGS84"))
spdf <- SpatialPointsDataFrame(sp,df.sf_crime[subst,])
df_neigh_tmp$crimes <- table(over(spdf,df_neigh_tmp))

n = 5 #Number of categories
pal <- colorRampPalette(c('white','DarkBlue'))(n+1)[2:(n+1)]
plot(df_neigh_tmp,col=pal[cut(df_neigh_tmp$crimes,quantile(df_neigh_tmp$crimes,cumsum(c(0,rep(1/n,n)))))])



