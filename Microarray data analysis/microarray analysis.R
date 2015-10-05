library(Biobase)
library(GEOquery)
library(kernlab)
library(fastICA)
library("e1071")
library(wccsom)
library(fastcluster)
library(graphics)
library(stringr)



gdclassif<-function(sample.name,selection.method="pca",classif.method="svm"){
	sample.name<-"GSE10072"
	#selection.method="pca"#"variance"
	#classif.method = "svm"#"som"

	#import danych

	getGEOfile(sample.name, ".")
	file.name<-paste(sample.name,".soft.gz",sep="")
	file.training <- file.path(".", file.name)
	training.geo<-getGEO(filename=file.training,GSEMatrix=TRUE)
	training.title<-Meta(training.geo)$title

	#wczytywanie danych
	training.sample.names <- names(GSMList(training.geo))
	training.sample.nb <-length(training.sample.names)
	training.probe.ids <- as.vector(Table(GSMList(training.geo)[[1]])$ID_REF)
	training.probe.nb <- length(training.probe.ids)

	dataset <- numeric()
	for (x in training.sample.names) {
	   dataset <- c(dataset, as.numeric(Table(GSMList(training.geo)[[x]])$VALUE))
	}
	training.raw <- data.frame(matrix(dataset, training.probe.nb, training.sample.nb))
	colnames(training.raw) <- training.sample.names
	rownames(training.raw) <- training.probe.ids

	# training.class.factor<-c()

	# for (i in 1:length(attributes(GSMList(training.geo))$names)){
		# class.name<-split(Meta(GSMList(training.geo)[[i]])$title," ")[1]
		# training.class.factor<-rbind(training.class.factor,class.name)
	# }
	
	
	# Meta(GSMList(training.geo)$"GSM254731")$source_name_ch1

	training.class.file<-file.path(".",'list_training.tab')
	training.class <- read.table(training.class.file, row.names=1)
	names(training.class) <- "training_class"
	training.class.factor <- training.class[,1]
	training.class.nb<-length(attributes(training.class.factor)$levels)


	#normalizacja 
	genemedians <- apply(training.raw, 1, median)
	training.normalized <- training.raw / genemedians

	#gdy 2 klasy
	if(training.class.nb==2){

		normal<-c()
		for (i in 1:length(training.class.factor)){
		if(training.class.factor[i] == attributes(training.class.factor)$levels[1])
		normal<-append(normal,i)}

		tumor<-c()
		for (i in 1:length(training.class.factor)){
		if(training.class.factor[i] == attributes(training.class.factor)$levels[2])
		tumor<-append(tumor,i)}

		training.normal<-training.normalized.max[,normal]
		training.tumor<-training.normalized.max[,tumor]


#		if(selection.method=="variance"){
			#badanie jednorodnoœci wariancji
			var.table<-data.frame()
			for (i in 1:nrow(training.normalized.max)){
				x<-as.vector(as.matrix(training.normal[i,]))
				y<-as.vector(as.matrix(training.tumor[i,]))
				test.var<-var.test(x,y)
				var.table<-rbind(var.table, data.frame(id=training.filtered.probe.names[i],p.value=test.var$p.value))
			}
			var.table<-var.table[order(var.table$p.value,decreasing=FALSE),]

			#ró¿na wariancja
			last.significant.var <- max(which(var.table$p.value <= 0.05))
			selected.var <- 1:last.significant.var
			diff.genes.factor.var <- var.table$id[selected.var]
			diff.genes.var <- as.vector(diff.genes.factor.var)
			training.selected.var<-training.normalized[diff.genes.var,]
			training.selected.var.names<-rownames(training.selected.var)

			#test Wilcoxona dla ró¿nych wariancji - sprawdzanie równoœci œrednich 
			wilc.table<-data.frame()
			training.selected.normal<-training.selected.var[,normal]
			training.selected.tumor<-training.selected.var[,tumor]
			
			for (i in 1:nrow(training.selected.var)){
				x<-as.vector(as.matrix(training.selected.normal[i,]))
				y<-as.vector(as.matrix(training.selected.tumor[i,]))
				test.wilc<-wilcox.test(x,y)
				wilc.table<-rbind(wilc.table, data.frame(id=training.selected.var.names[i],
														   p.value=test.wilc$p.value))
			}

			wilc.table<-wilc.table[order(wilc.table$p.value,decreasing=FALSE),]
			last.significant.wilc <- max(which(wilc.table$p.value <= 0.05))
			selected.wilc <- 1:last.significant.wilc
			diff.genes.factor.wilc <- wilc.table$id[selected.wilc]
			diff.genes.wilc <- as.vector(diff.genes.factor.wilc)
			training.selected.wilc<-training.normalized.max[diff.genes.wilc,]

			# równa wariancja - tstudent
			selected.var.eq<-last.significant.var:nrow(var.table)
			diff.genes.factor.var.eq<- var.table$id[selected.var.eq]
			diff.genes.var.eq <- as.vector(diff.genes.factor.var.eq)
			training.selected.var.eq<-training.normalized.max[diff.genes.var.eq,]
			training.selected.var.eq.names<-rownames(training.selected.var.eq)

			#test tstudenta dla rownych wariancji - sprawdzanie równoœci œrednich 
			ttest.table<-data.frame()
			training.selected.normal<-training.selected.var.eq[,normal]
			training.selected.tumor<-training.selected.var.eq[,tumor]
			for (i in 1:nrow(training.selected.var.eq)){
				x<-as.vector(as.matrix(training.selected.normal[i,]))
				y<-as.vector(as.matrix(training.selected.tumor[i,]))
				test.t<-t.test(x,y)
				ttest.table<-rbind(ttest.table, data.frame(id=training.selected.var.eq.names[i],
														   p.value=test.t$p.value))
			}
				
			ttest.table<-ttest.table[order(ttest.table$p.value,decreasing=FALSE),]
			last.significant.t <- max(which(ttest.table$p.value <= 0.05))
			selected.t <- 1:last.significant.t
			diff.genes.factor.t <- ttest.table$id[selected.t]
			diff.genes.t <- as.vector(diff.genes.factor.t)
			training.selected.t<-training.normalized.max[diff.genes.t,]

			training.selected<-rbind(training.selected.wilc,training.selected.t)
		}
		
		#jesli wiecej niz 2 klasy
		else if(training.clases.nb>2){
					
			#kruskal wallis - kiedy wiecej klas
			kruskal.wallis.alpha <- 0.01
			kruskal.wallis.table <- data.frame()
			for (i in 1:training.filtered.probe.nb) {
			  ## Run the KW test on on gene
			  x <- as.vector(as.matrix(training.filtered[i,]))
			  ks.test <- kruskal.test(x, g=training.class.factor)
			  ## Store the result in the data frame
			  kruskal.wallis.table <- rbind(kruskal.wallis.table,
											data.frame(id=training.filtered.probe.names[i],p.value=ks.test$p.value))
			}
			nb.tests <- training.filtered.probe.nb



			kruskal.wallis.table$E.value <- kruskal.wallis.table$p.value * nb.tests
			kruskal.wallis.table$FWER <- pbinom(q=0, p=kruskal.wallis.table$p.value, size=nb.tests, lower.tail=FALSE)
			kruskal.wallis.table <- kruskal.wallis.table[order(kruskal.wallis.table$p.value, decreasing=FALSE), ]
			kruskal.wallis.table$q.value.factor <- nb.tests / 1:nb.tests
			kruskal.wallis.table$q.value <- kruskal.wallis.table$p.value * kruskal.wallis.table$q.value.factor

			last.significant.element.ks <- max(which(kruskal.wallis.table$q.value <= kruskal.wallis.alpha))
			selected.ks <- 1:last.significant.element.ks
			diff.genes.factor.ks <- kruskal.wallis.table$id[selected.ks]
			diff.genes.ks <- as.vector(diff.genes.factor.ks)
			training.selected<-training.normalized.max[diff.genes,]
		}
		
		training.selected.trans<-t(training.selected)
		
		if(classif.method=="svm")
			training.svm<-ksvm(training.selected.trans,training.class.factor)
			
		else if(classif.method=="som"){
			training.som<-wccxyf(training.selected.trans,as.numeric(training.class.factor))
			plot(training.som,type="mapping",labels=training.class.factor,col=as.numeric(training.class.factor))
			plot(training.som,type="counts",labels=training.class.factor,palette.name = heat.colors,ncolor=4)
		}
#	}

#	else if(selection.method=="pca"){
#
		#principal component analysis of the selected genes
#		training.trans<-t(training.normalized.max)
#		training.kpc <- kpca(training.trans,kernel="rbfdot")
#		if(classif.method=="svm")
#			training.svm<-ksvm(pcv(training.kpc),training.class.factor)
#		
#		else if (classif.method=="som"){
#			training.som<-wccxyf(pcv(training.kpc),as.numeric(training.class.factor))
#			plot(training.som,type="mapping",labels=training.class.factor,col=as.numeric(training.class.factor))
#			plot(training.som,type="counts",labels=training.class.factor,palette.name = heat.colors,ncolor=4)
#		}	

#	}


}