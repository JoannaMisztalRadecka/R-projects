library(audio)

processWav<-function(wav,txt){
sound<-load.wave(wav)
sound<-sound[1:50000]
sound<-spectrum(sound)
write(sound,txt,sep='\n')
}

readDataSpectrum<-function(path){
t(read.table(path))
}

learning<-function(patterns,letters) {
gr <- somgrid(3, 3, "rectangular")
x <- wccxyf(patterns,letters,grid=gr, trwidth=20, rlen=100)
x
}

prediction<-function(x, newData){
y<-predict(x,newData)
round(y$predictions)
}

spectrum<-function(data){
fourier<-fft(data)
 magnitude<-Mod(fourier)
 phase<-Arg(fourier)
 magnitude_firsthalf <- magnitude[1:(length(magnitude)/2)] 
 phase_firsthalf<-phase[1:(length(magnitude)/2)]
 x.axis <- 1:length(magnitude_firsthalf)/length(magnitude)
 magnitude_firsthalf
#x11() 
#plot(x=x.axis,y=magnitude_firsthalf,type="l")

}

initialize<-function(){
initial.dir<-getwd()
library(wccsom)

T1<-readDataSpectrum("T1.txt")
T2<-readDataSpectrum("T2.txt")
T3<-readDataSpectrum("T3.txt")
T4<-readDataSpectrum("T4.txt")
T5<-readDataSpectrum("T5.txt")
N1<-readDataSpectrum("N1.txt")
N2<-readDataSpectrum("N2.txt")
N3<-readDataSpectrum("N3.txt")
N4<-readDataSpectrum("N4.txt")
N5<-readDataSpectrum("N5.txt")
setwd(initial.dir)
letters<-c(rep(1.0,5),rep(2.0,5))
patterns<-rbind(T1,T2,T3,T4,T5,N1,N2,N3,N4,N5)
x<-learning(patterns,letters)
x
}





