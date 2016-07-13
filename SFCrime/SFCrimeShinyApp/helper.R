


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

	brk <- pretty(df_neigh_tmp$crimes,n=10,min.n=0)
	n = length(brk)-1 #Number of categories
	pal <- colorRampPalette(c('white','DarkBlue'))(n+1)[2:(n+1)]
	crimtab <- df_neigh_tmp$crimes[df_neigh_tmp$neighborho]
	lvl <- cut(crimtab,brk)
	plot(df_neigh_tmp,col=pal[lvl])
	par(cex=.8)
	image.plot( zlim=c(min(df_neigh_tmp$crimes),max(df_neigh_tmp$crimes)), legend.only=TRUE,col=pal,lab.breaks=brk)
}

CUT <- function(x, n = 5, params = NULL){
  tmp <- x[x>10]
  quantileCuts(tmp,n,params)
}

#Or pass output from createsub into mapheat
mapheat <- function(subdat,df_neigh,df_water){
  df_neigh_tmp <- df_neigh
  sp <- SpatialPoints(subdat[,c('X','Y')],CRS("+proj=longlat +ellps=WGS84"))
  spdf <- SpatialPointsDataFrame(sp,subdat)
  knl <- kde.points(spdf,h=.01,n=50,lims = df_neigh)
  shades <- auto.shading(data.frame(knl)[,1], n = 10, cols = c('white',brewer.pal(9, "Reds")),cutter = CUT)
  par(mar=c(5,4,4,5))
  level.plot(knl,shades=shades)
  plot(df_neigh,add=TRUE)
  plot(df_water,add=TRUE,col='lightblue')
  par(cex=.8)
 #choro.legend(331089, 384493, shades, fmt = "%g", title = "Count of Dwellings")
  #image.plot( zlim=c(0,max(data.frame(knl)[,1])), legend.only=TRUE,col=shades$cols,lab.breaks=signif(c(0,shades$breaks,max(data.frame(knl)[,1])),2))
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

