library(seqRFLP)
library(som)



#testGen<-function(fileName){
	sequencesData <- read.fasta("gen2.txt")
	len <- length(sequencesData)
	sequenceLength <- nchar(sequencesData[2])
	inputVector <- vector("list", len/2)
	sumMatrix<-matrix(ncol=sequenceLength, nrow=4, 0)
	somDimX<-2
	somDimY<-2
	somDim<-somDimX*somDimY
	partToDivide<-0.15*(len/2)
	nameVector<- vector("list",len/2)
	dataMatrix<-matrix(0,ncol=4*sequenceLength,nrow=len/2)
	makeSOM(sequencesData)
#}

##wyœwietlanie zgodnie z poziomem podzia³u
display<-function(level,tekst){
cat("\n")
for(i in 1: level)cat("	")
cat(tekst)
}


##rekurencyjne dzielenie klasy, w ktorej jest za du¿o wektorów
divideCluster<-function(clusterNr,nobs,som,level,nrOfVectors,genList){
	mx<-matrix(0,ncol=4*sequenceLength,nrow=nobs)
	n<-0
	display(level,paste("Poziom podzia³u: ",level," dla klasy: ",clusterNr))
	for(i in 1:nrOfVectors)
		if(((som$visual[i,]$x==som$code.sum$x[clusterNr])) && ((som$visual[i,]$y==som$code.sum$y[clusterNr]))){
			n<-n+1
			mx[n,]<-som$data[i,]		
		}
	##tworzenie nowej mapy
	som<-som(mx,xdim=somDimX,ydim=somDimY,topol="rect",neigh="gaussian")
	for (i in 1:somDim){
		newGenList<-c()
		display(level,paste("Klasa: ",i,"Liczba genów: ",som$code.sum[i,]$nobs))
		display(level,paste("Geny w klasie",i,": "))
		if(som$code.sum[i,]$nobs>0)
		{
			for(j in 1:(nobs)){
				if(((som$visual[j,]$x==som$code.sum$x[i])) && ((som$visual[j,]$y==som$code.sum$y[i]))){
					cat(genList[[j]],", ")
					newGenList<-append(newGenList,genList[[j]])
				}
			}
		}
		if((som$code.sum[i,]$nobs)>partToDivide){
		#jeœli po podziale jest to samo, to przerywamy
			if(nobs==nrOfVectors)
				display(level,"Klasa nie da siê ju¿ podzieliæ.")
			else
				mx<-divideCluster(i,som$code.sum[i,]$nobs,som,level+1,nobs,newGenList)
		}
	}
	return(mx)	
}





##tworzenie sieci
makeSOM<-function(sequences){
## zapisz dane jako 4 elementowe wektory

	for(i in 1:len)
	{
		if(i%%2==1)
		nameVector[[(i+1)/2]]<-sequences[i]
		else{
			text<-sequences[i]
			resultMatrix <- matrix(ncol=sequenceLength, nrow=4, 0)
			for(j in 1:sequenceLength)
			{
				char<-substr(text,j,j)
				if(char=="A"){
					sumMatrix[1,j]<-sumMatrix[1,j]+1;
					resultMatrix[1,j]=1
				}
				else if(char=="T"){
					sumMatrix[2,j]<-sumMatrix[2,j]+1;
					resultMatrix[2,j]=1
				}
				else if(char=="C"){
					sumMatrix[3,j]<-sumMatrix[3,j]+1;
					resultMatrix[3,j]=1
				}
				else if(char=="G"){
					sumMatrix[4,j]<-sumMatrix[4,j]+1;
					resultMatrix[4,j]=1
				}
				else{
					resultMatrix[,j]=0.25
				}
			}
		inputVector[[floor(i/2)]]<-resultMatrix;
		}
	}

	## ignoreList zawiera liste pozycji, gdzie dane wejsciowe sa takie same
	ignoreList<-c()
	for( i in 1:sequenceLength ){
		for(j in 1:4){
			if(sumMatrix[j,i]==len/2)
			{
				ignoreList<-append(ignoreList,i)
			}
		}
	
	## przykladowe pobieranie danych - c(inputVector[[1]])

	##zamiana na jeden wektor

	for (i in 1:(len/2))
		dataMatrix[i,]<- c(inputVector[[i]])

}
	som<-som(dataMatrix,xdim=somDimX,ydim=somDimY,topol="rect",neigh="gaussian")
	##sprawdzanie, czy gdzieœ jest za du¿o wektorów i wypisywanie wyników
	for (i in 1:somDim){
		genList<-c()
		cat("\n Klasa: ",i,"Liczba genów: ",som$code.sum[i,]$nobs,'\n Geny w klasie',i,": \n")
		if(som$code.sum[i,]$nobs>0)
			for(j in 1:(len/2))
				if(((som$visual[j,]$x==som$code.sum$x[i])) && ((som$visual[j,]$y==som$code.sum$y[i]))){
					genList<-append(genList,nameVector[[j]])
				}
		cat(genList)		
		if((som$code.sum[i,]$nobs)>partToDivide)
			mx<-divideCluster(i,som$code.sum[i,]$nobs,som,1,len/2,genList)
	}
}
	
