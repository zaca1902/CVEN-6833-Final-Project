library(maps)
library(akima)
library(fields)

nrows=72
ncols=20
ntime = 115    #Dec-Feb 1906 - Dec-Feb 2020

nyrs = ntime    #1906 - 2020

nglobe = nrows*ncols
N = nrows*ncols


### Lat - Long grid..

ygrid=seq(-22.5,72.5,by=5)
ny=length(ygrid)

xgrid=seq(27.5,382.5,by=5)
#xgrid[xgrid > 180]=xgrid[xgrid > 180]-360	#longitude on 0-360 grid if needed
nx=length(xgrid)

xygrid=matrix(0,nrow=nx*ny,ncol=2)

i=0
for(iy in 1:ny){
for(ix in 1:nx){
i=i+1
xygrid[i,1]=ygrid[iy]
xygrid[i,2]=xgrid[ix]
}

}

# REad Kaplan SST data..


data=readBin("https://civil.colorado.edu/%7Ebalajir/CVEN6833/HWs/HW-2/data.r4", what="numeric", n=(nrows * ncols * ntime), size=4,endian="swap")


data <- array(data = data, dim=c( nrows, ncols, ntime ) )


data1=data[,,1]
	

# the lat -long data grid..

index=1:(nx*ny)

index1=index[data1 < 20 & data1 != "NaN"]	# only non-missing data.
xygrid1=xygrid[index1,]

x1=xygrid1[,2]
#x1[x1 < 0]= x1[x1 < 0] + 360
#xygrid1[,2]=x1

nsites=length(index1)
data2=data1[index1]

### SSTdata matrix - rows are seasonal (i.e. one value per year)
## and columns are locations
sstdata=matrix(NA,nrow=nyrs, ncol=nsites)


for(i in 1:nyrs){
data1=data[,,i]
index1=index[data1 < 20 & data1 != "NaN"]
data2=data1[index1]
sstdata[i,]=data2
}

sstannavg = sstdata
indexgrid = index1
rm("data")	#remove the object data to clear up space


## write out the grid locations..
write(t(xygrid1),file="kaplan-sst-locs-4Zach.txt",ncol=2)

