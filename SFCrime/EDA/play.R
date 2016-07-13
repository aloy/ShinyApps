#df.sf_crime <- read.csv("../SFPD.csv")

#df.sf_crime$Times <- sapply(strsplit(as.character(df.sf_crime$Time),":"),function(v){ as.numeric(v[1])+as.numeric(v[2])/60 })

#df.sf_crime$Month <- factor(month.name[as.numeric(substr(df.sf_crime$Date,1,2))],levels=month.name)



var1 = df.sf_crime$Times
var2 = df.sf_crime$Category


cols = rainbow(length(levels(var2)))
lty = rep(1:2,10)


plot(density(var1),type='n',ylim=c(0,5000),main=' ',xlab = 'Time of Day (24h)',ylab='Frequency')
sapply(1:length(levels(var2)),function(i){
  foo = hist(var1[var2==levels(var2)[i]],breaks=seq(0,24,by=1),plot=FALSE)
  lines(foo$mids,foo$counts,col=cols[i],lwd=2,lty=lty[i])
})
legend(27,.08,legend=levels(var2),lty=1:2,col=cols,cex=.8,lwd=2)