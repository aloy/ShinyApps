


#This function will need to be updated with the appropriate variable names from df.sf_crime and the names of the input variables coming from the widgets.
createsub <- function(dat,input){
	if(input$crime != 'ALL CRIMES'){
		dat <- dat[dat$Category == input$crime,]
	}
	if(input$day != 'All days'){
		dat <- dat[dat$DayOfWeek == input$day,]
	}
	if(input$district != "All districts"){                             
		dat <- dat[dat$PdDistrict == input$district,]
	}
	dat <- dat[dat$Dates >= input$dates[1] & dat$Dates<= input$dates[2],]
	dat <- dat[dat$Times >= input$time[1] & dat$Times <= input$time[2],]
	return(dat)       
}

#Pass output from createsub into mappoints
mappoints <- function(subdat,df_neigh,df_water){
	par(bty='n',xaxt='n',yaxt='n',mar=c(0,0,0,0))
	colpalette <- rainbow(40,alpha = .5)
	plot(df_neigh)
	plot(df_water,add=TRUE,col='lightblue')
	with(subdat, points(X,Y,pch = 19,col=colpalette[Category],cex=.2))
}

#Or pass output from createsub into mapneigh
mapneigh <- function(subdat,df_neigh,df_water){
	df_neigh_tmp <- df_neigh
	sp <- SpatialPoints(subdat[,c('X','Y')],CRS("+proj=longlat +ellps=WGS84"))
	spdf <- SpatialPointsDataFrame(sp,subdat)
	df_neigh_tmp$crimes <- table(over(spdf,df_neigh_tmp))

	n = 5 #Number of categories
	pal <- colorRampPalette(c('white','DarkBlue'))(n+1)[2:(n+1)]
	plot(df_neigh_tmp,col=pal[cut(df_neigh_tmp$crimes,quantile(df_neigh_tmp$crimes,cumsum(c(0,rep(1/n,n)))))])
}


#Or pass output from createsub into mapheat
mapheat <- function(subdat,df_neigh,df_water){
  df_neigh_tmp <- df_neigh
  sp <- SpatialPoints(subdat[,c('X','Y')],CRS("+proj=longlat +ellps=WGS84"))
  spdf <- SpatialPointsDataFrame(sp,subdat)
  knl <- kde.points(spdf,h=.01,n=50,lims = df_neigh)
  pal <- list(breaks = c(10,50,100,500,1000),cols = c('#FFFFFF',brewer.pal(6,'Reds')))
  level.plot(knl,shades=pal)
  plot(df_neigh,add=TRUE)
  plot(df_water,add=TRUE,col='lightblue')
  #Add legend (could adjust the breaks for the color groupings - want it to stay consistent)

}

mapfunc <- function(subdat,df_neigh,df_water,types){
  if(types =="Points"){
    mappoints(subdat,df_neigh,df_water)
  }
  
  if(types == "Neighborhood"){
    
    mapneigh(subdat,df_neigh,df_water)
  }
  
  if(types == "Heatmap"){
    
   mapheat(subdat,df_neigh,df_water)
  }
}

