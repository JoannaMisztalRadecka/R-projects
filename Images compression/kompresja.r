library(biOps)
library(som)

setwd("D:/UJ/sieci neuronowe/kompresja/")


bc1<-readJpeg("albert-einstein.jpg")
bc1<-imgRGB2Grey(bc1)
#writeJpeg("albert-einstein.jpg",bc1)
nrOfClasses = 100
xSize = dim(bc1)[1]
ySize = dim(bc1)[2]

d = 8

xDim = floor(xSize/d)
yDim = floor(ySize/d)
xyDim = xDim*yDim


tmp<-matrix(0,xDim,yDim)
	for (i in 1:yDim)
		for(j in 1:xDim)
			tmp[j,i] = i+yDim*(j-1)
			

mx<-matrix(bc1,xSize,ySize)
mx<-mx/256

splitMatrix<-array(0, dim=c(d,d,xyDim))
n=0
 for(i in 1:xDim)
	for(j in 1:yDim)
	{
	n=n+1
		splitMatrix[,,n]<-mx[(d*(i-1)+1):(d*(i-1)+d),(d*(j-1)+1):(d*(j-1)+d)]
}

splitVectors<-matrix(,64,xyDim)

for(i in 1:xyDim)
	splitVectors[,i] <- c(splitMatrix[,,i])
	

som<-som(t(splitVectors),xdim=floor(nrOfClasses^(1/2)),ydim=floor(nrOfClasses^(1/2)),topol="rect",neigh="gaussian",rlen=5)
compressedClasses<-matrix(,xyDim,1)
for(i in 1:xyDim)
	compressedClasses[i]<-som$visual$x[i]+(som$visual$y[i]*nrOfClasses^(1/2))+1

compressedVectors<-matrix(,xyDim,64)
for (i in 1:xyDim){
	compressedVectors[i,]<-som$code[compressedClasses[i],]
	}
	
compressedMatrix<-array(0, dim=c(d,d,xyDim))
for (i in 1:xyDim)
	compressedMatrix[,,i] = matrix(compressedVectors[i,],ncol=d,nrow=d)

	

	
	cMx<-matrix(0,xDim*d,yDim*d)
	for (i in 1:yDim)
		for(j in 1:xDim)
			cMx[(d*(j-1)+1):(d*(j-1)+d),(d*(i-1)+1):(d*(i-1)+d)]<-compressedMatrix[,,tmp[j,i]]
				
				cMx<-cMx*256
img<-imagedata(cMx)
writeJpeg("afterCompression.jpg",img)

compressionError<-function(som){
return(som$visual$qerror)
}

averageError<-function(som){
return(som$qerror)
}

	