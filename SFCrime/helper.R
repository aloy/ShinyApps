#These need to be in the server file so that they are only run once.
#df.sf_crime <- read.csv("SFPD.csv")

#library(rgdal)
#df_neigh <- spTransform(readOGR(".",'planning_neighborhoods') ,CRS("+proj=longlat +ellps=WGS84"))
#df_water <- spTransform(readOGR(".",'phys_waterbodies') ,CRS("+proj=longlat +ellps=WGS84"))

#########################################################
#This function will need to be updated with the appropriate variable names from df.sf_crime and the names of the input variables coming from the widgets.
createsub <- function(dat,input){
	if(input$Category != 'All Crimes'){
		dat <- dat[dat$Category == input$Category,]
	}
	if(input$Month != 'All Months'){
		dat <- dat[dat$Month == input$Month,]
	}
	if(input$DayOfWeek != 'All Days'){
		dat <- dat[dat$DayOfWeek == input$DayOfWeek,]
	}
	dat <- dat[dat$Times >= input$times[1] & dat$Times <= input$times[2],]
	return(dat)
}

#Pass output from createsub into mappoints
mappoints <- function(subdat){
	par(bty='n',xaxt='n',yaxt='n',mar=c(0,0,0,0))
	colpalette <- rainbow(40,alpha = .5)
	plot(df_neigh)
	plot(df_water,add=TRUE,col='lightblue')
	with(subdat, points(X,Y,pch = 19,col=colpalette[Category],cex=.2))
}

#Or pass output from createsub into mapneigh
mapneigh <- function(subdat){
	df_neigh_tmp <- df_neigh
	sp <- SpatialPoints(subdat[,c('X','Y')],CRS("+proj=longlat +ellps=WGS84"))
	spdf <- SpatialPointsDataFrame(sp,subdat)
	df_neigh_tmp$crimes <- table(over(spdf,df_neigh_tmp))

	n = 5 #Number of categories
	pal <- colorRampPalette(c('white','DarkBlue'))(n+1)[2:(n+1)]
	plot(df_neigh_tmp,col=pal[cut(df_neigh_tmp$crimes,quantile(df_neigh_tmp$crimes,cumsum(c(0,rep(1/n,n)))))])
}



