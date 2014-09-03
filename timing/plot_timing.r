
# Load helpful functions
source("functions.r")

## Load timing comparison: netcdf vs ncio
times  = read.table("compare_timing.txt",header=TRUE)
timesb = read.table("compare_timing_10x.txt",header=TRUE)


## Plot timing comparison ##

col.axis = "grey50"
xlim     = range(times$nloops)
ylim     = range(times$netcdf,times$ncio)

col = c("grey20","blue","cyan")
pch = c(15,20,20)
pt.cex = c(1,1.5,1.5)

myfigure("./","compare_ncio",date=TRUE,asp=2.2,pointsize=10,type="pdf")
par(plt=c(0.1,0.48,0.14,0.95),
    col=col.axis,col.lab=col.axis,col.axis=col.axis)

plot(xlim,ylim,type="n",ann=FALSE,log="xy")
grid()
mtext(side=1,line=1.6,"Number of repititions")
mtext(side=2,line=3.1,las=0,"Run time (s)")

points(times$nloops,times$netcdf,col=col[1],pch=pch[1],cex=pt.cex[1])
lines(times$nloops,times$netcdf,col=col[1])

points(times$nloops,times$ncio,col=col[2],pch=pch[2],cex=pt.cex[2])
lines(times$nloops,times$ncio,col=col[2])

points(times$nloops,times$ncio_ncid,col=col[3],pch=pch[3],cex=pt.cex[3])
lines(times$nloops,times$ncio_ncid,col=col[3])

# ## 10x-10x larger array ##

# points(timesb$nloops,timesb$netcdf,col=col[1],pch=pch[1],cex=pt.cex[1])
# lines(timesb$nloops,timesb$netcdf,col=col[1],lty=2)

# points(timesb$nloops,timesb$ncio,col=col[2],pch=pch[2],cex=pt.cex[2])
# lines(timesb$nloops,timesb$ncio,col=col[2],lty=2)

# points(timesb$nloops,timesb$ncio_ncid,col=col[3],pch=pch[3],cex=pt.cex[3])
# lines(timesb$nloops,timesb$ncio_ncid,col=col[3],lty=2)

legend("topleft",bty="n",inset=0.02,col=col,pch=pch,lwd=1,pt.cex=pt.cex,
       c("Native NetCDF","NCIO","NCIO-ncid"))

box(col=col.axis)

xlim     = range(times$nloops)
ylim     = range(2,times$ncio/times$netcdf)

x.at     = c(1,10,100,1000,10000)
y.at     = c(2,5,10,20,50,100,200)

par(new=TRUE,plt=c(0.60,0.98,0.14,0.95))

plot(xlim,ylim,type="n",ann=FALSE,log="xy")
abline(v=x.at,h=y.at,col="lightgrey",lty=3)
mtext(side=1,line=1.6,"Number of repititions")
mtext(side=2,line=2.1,las=0,"Relative run time")

points(times$nloops,times$ncio/times$netcdf,col=col[2],pch=pch[2],cex=pt.cex[2])
lines(times$nloops,times$ncio/times$netcdf,col=col[2])

points(times$nloops,times$ncio_ncid/times$netcdf,col=col[3],pch=pch[3],cex=pt.cex[3])
lines(times$nloops,times$ncio_ncid/times$netcdf,col=col[3])

points(timesb$nloops,timesb$ncio/timesb$netcdf,col=col[2],pch=pch[2],cex=pt.cex[2])
lines(timesb$nloops,timesb$ncio/timesb$netcdf,col=col[2],lty=2)

points(timesb$nloops,timesb$ncio_ncid/timesb$netcdf,col=col[3],pch=pch[3],cex=pt.cex[3])
lines(timesb$nloops,timesb$ncio_ncid/timesb$netcdf,col=col[3],lty=2)


box(col=col.axis)

graphics.off()