#These need to be in the server file so that they are only run once.
#df.sf_crime <- read.csv("../SFPD.csv")

#df.sf_crime$Times <- sapply(strsplit(as.character(df.sf_crime$Time),":"),function(v){ as.numeric(v[1])+as.numeric(v[2])/60 })

#df.sf_crime$Month <- factor(month.name[as.numeric(substr(df.sf_crime$Date,1,2))],levels=month.name)


#########################################################

graphvar1 <- function(var1){
	if(length(levels(var1)) > 10){
			topct <- tail(row.names(sort(table(var1))),10)
			var1 = factor(var1[var1 %in%  topct],levels=topct)
	}

	if(class(var1) == 'factor'){
		par(mfrow=c(1,2))
		barplot(table(var1),las = 2,cex.names= .5,col='darkblue',main='Bar Plot')
		pie(table(var1),cex= .5,col=rainbow(50)[c(seq(1,50,by=3),seq(2,50,by=3))],main='Pie Chart')
	}else{
		par(mfrow=c(1,2))
		hist(var1,freq=FALSE,col='darkblue',main='Histogram',xlab = 'Time of Day (24h)')
		#lines(density(var1,from=0,to = 24,adjust=.8),col='green',lwd=2)
		boxplot(var1,main='Boxplot',ylab = 'Time of Day (24h)')
	}
}

numsum1 <- function(var1){
  if(length(levels(var1)) > 10){
    topct <- tail(row.names(sort(table(var1))),10)
    var1 = factor(var1[var1 %in%  topct],levels=topct)
  }
  
  if(class(var1) == 'factor'){
    paste('The frequencies for the following categories (', paste(row.names(sort(table(var1))),collapse=', '),') are: ',paste(round(sort(table(var1)),2),collapse=', '),sep='')
  }else{
    paste('The center is ',round(mean(var1),2),'/',round(median(var1),2),' (mean/median). The spread is ',round(sd(var1),2),'/',round(IQR(var1),2),'/',round(diff(range(var1)),2),' (sd/IQR/range).' ,sep='')
  }
}

numsum12 <- function(var1){
  if(length(levels(var1)) > 10){
    topct <- tail(row.names(sort(table(var1))),10)
    var1 = factor(var1[var1 %in%  topct],levels=topct)
  }
  
  if(class(var1) == 'factor'){
    paste('The relative frequencies for only the following categories (', paste(row.names(sort(table(var1))),collapse=', '),') are: ',paste(round(prop.table(sort(table(var1))),2),collapse=', '),sep='')
  }else{
    paste('The 25th percentile is ',round(quantile(var1,probs=.25),2),'. The 75th percentile is ',round(quantile(var1,probs=.75),2),'.',sep='')
  }
}

graphvar2 <- function(var1,var2){
  if(length(levels(var1)) > 10 | length(levels(var2)) > 10 ){
    if(length(levels(var1)) > 10 ){
      topct <- tail(row.names(sort(table(var1))),10)
      var2 = var2[var1 %in%  topct]
      var1 = factor(var1[var1 %in%  topct],levels=topct)
    }
    if(length(levels(var2)) > 10 ){
      topct <- tail(row.names(sort(table(var2))),10)
      var1 = var1[var2 %in%  topct]
      var2 = factor(var2[var2 %in%  topct],levels=topct)
    }
  }

  if(class(var1) == 'factor' & class(var2) == 'factor'){
    par(mar=c(5,4,4,2))
    if(length(levels(var2)) > length(levels(var1))){
      cols = rainbow(length(levels(var2)))
      mosaicplot(table(var1,var2),cex=.5,col=cols,las=1,main='Mosaic Plot',xlab=' ',ylab= ' ')
    }else{
      cols = rainbow(length(levels(var1)))
      mosaicplot(table(var2,var1),cex=.5,col=cols,las=1,main='Mosaic Plot',xlab=' ',ylab= ' ')	
    }
  }else if((class(var1) == 'numeric' | class(var1) == 'integer') & (class(var2) == 'numeric' | class(var2) == 'integer')){
    plot(var1,var2)
  }else{
    par(xpd = TRUE,mar=c(5,4,4,13),mfrow=c(2,1))
    if((class(var1) == 'numeric' | class(var1) == 'integer') ){
      cols = rainbow(length(levels(var2)))
      lty = rep(1:2,10)
      plot(density(var1),type='n',ylim = c(0,.14),main='Density Plot',xlab = 'Time of Day (24h)')
      sapply(1:length(levels(var2)),function(i){
        foo = hist(var1[var2==levels(var2)[i]],breaks=seq(0,24,by=1),plot=FALSE)
        lines(foo$mids,foo$density,col=cols[i],lwd=2,lty=lty[i])
      })
      legend(27,.15,legend=levels(var2),lty=1:2,col=cols,cex=.8,lwd=2)
      
      plot(density(var1),type='n',ylim=c(0,4000),main='Frequency Plot',xlab = 'Time of Day (24h)',ylab='Frequency')
      sapply(1:length(levels(var2)),function(i){
        foo = hist(var1[var2==levels(var2)[i]],breaks=seq(0,24,by=1),plot=FALSE)
        lines(foo$mids,foo$counts,col=cols[i],lwd=2,lty=lty[i])
      })
      legend(27,4500,legend=levels(var2),lty=1:2,col=cols,cex=.8,lwd=2)
    }
    if((class(var2) == 'numeric' | class(var2) == 'integer') ){
      cols = rainbow(length(levels(var1)))
      lty = rep(1:2,10)
      plot(density(var2),type='n',ylim = c(0,.10),main='Density Plot',xlab = 'Time of Day (24h)')	
      sapply(1:length(levels(var1)),function(i){
        foo = hist(var2[var1==levels(var1)[i]],breaks=seq(0,24,by=1),plot=FALSE)
        lines(foo$mids,foo$density,col=cols[i],lwd=2,lty=lty[i])
      })    
      legend(27,.11,legend=levels(var1),lty=1:2,col=cols,cex=.8,lwd=2)	
      
      plot(density(var2),type='n',ylim=c(0,3000),main='Frequency Plot',xlab = 'Time of Day (24h)',ylab='Frequency')
      sapply(1:length(levels(var1)),function(i){
        foo = hist(var2[var1==levels(var1)[i]],breaks=seq(0,24,by=1),plot=FALSE)
        lines(foo$mids,foo$counts,col=cols[i],lwd=2,lty=lty[i])
      })
      legend(27,3500,legend=levels(var1),lty=1:2,col=cols,cex=.8,lwd=2)
    }
  }
}

numsum2 <- function(var1,var2){
  if(length(levels(var1)) > 20 | length(levels(var2)) > 20 ){
    if(length(levels(var1)) > 20 ){
      topct <- tail(row.names(sort(table(var1))),20)
      var2 = var2[var1 %in%  topct]
      var1 = factor(var1[var1 %in%  topct],levels=topct)
    }
    if(length(levels(var2)) > 20 ){
      topct <- tail(row.names(sort(table(var2))),20)
      var1 = var1[var2 %in%  topct]
      var2 = factor(var2[var2 %in%  topct],levels=topct)
    }
  }
  
  if(class(var1) == 'factor' & class(var2) == 'factor'){
    table(var1,var2) 
  }else{
    if((class(var1) == 'numeric' | class(var1) == 'integer') ){
      tapply(var1, var2, summary)
    }
    if((class(var2) == 'numeric' | class(var2) == 'integer') ){
      tapply(var2, var1, summary)
    }
  }
}

