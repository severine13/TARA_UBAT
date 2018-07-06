# script Martini Severine MBARI
# 2017 - 2018
# read and plot all data from UBAT for Tara Expeditions

# packages
library(ggplot2)
Tarastation = "NZ-NC"

pdf("TaraBiolum.pdf",width=20,height=10,bg="white")

# read all datasets in the repository
TaraDir = "F:/Oceano/sev-projet-autres/TARA_UBAT/UBAT_tara_pacific/"
setwd(paste(TaraDir,Tarastation,sep=""))

# list all files in the repo
files <- list.files(pattern = "\\.log$")

# plot all data in one file
for (i in 1:length(files)){
inFile = read.table(files[i],sep=",",skip=1,fill=T)
# remove non-entire lines
inFile = inFile [ complete.cases(inFile), ]

#inFile=inFile[-(which(ncol(inFile)!=70)),]
names(inFile)=c("UBAT","record_nb","cal","average_bl","pump","volt","flow","HV","res1","res2","60HzdigitAD")

# recuperer toutes les raw dans une matrice
raw0_59 = inFile[11:70]
vect_raw = unlist(raw0_59)
# longueur des donnees
nr = nrow(inFile)
nc = 60
mat_raw = matrix(vect_raw,nrow=nr,ncol=nc)
# verif table(dat$raw_0-mat_raw[,1]) --> OK

# vecteur temporel consecutif OK
mat_vec=""
mat_vec$Biolum = as.vector(t(mat_raw))
# matrix en photons.s-1
## cal = rep(dat$cal_striing,length.out=length(mat_vec))
## vect_cal = cal*mat_vec


# recreer le data.frame
mat_vec$UBAT=rep(inFile$UBAT,each=60)
mat_vec$timeS=1:length(mat_vec$Biolum)
mat_vec$record_nb=rep(inFile$record_nb,each=60)
mat_vec$cal=rep(inFile$cal,each=60)
mat_vec$average_bl=rep(inFile$average_bl,each=60)
mat_vec$pump=rep(inFile$pump,each=60)
mat_vec$volt=rep(inFile$volt,each=60)
mat_vec$flow=rep(inFile$flow,each=60)
mat_vec$HV=rep(inFile$HV,each=60)
mat_vec$res1=rep(inFile$res1,each=60)
mat_vec$res2=rep(inFile$res2,each=60)


# calibration avec une valeur approxi de flow rate calibration ~0.00055
# flow rate (l.s)=flow (RPM)*0.00055
mat_vec$calib1 = mat_vec$average_bl/(mat_vec$flow*0.00055)
mat_vec=as.data.frame(mat_vec)

# calibration 60Hz en ph.s
mat_vec$calib60 = mat_vec$Biolum*mat_vec$cal


# plot data
tt=ggplot(mat_vec,aes(y=calib60,x=(timeS-timeS[1])/3600))+geom_line()+ggtitle(files[i])+ylab("Bioluminescence (ph.s)")+
  xlab("Time (min)")

print(tt)

# plot data average biolum 1Hz - notify if flowrate =0
if (max(mat_vec$flow==0,na.rm=T)){
tt2=ggplot(mat_vec,aes(y=average_bl,x=(record_nb-record_nb[1])/60))+geom_line()+ggtitle(paste(files[i],"/ flow rate=0, average biolum ploted"))+ylab("Bioluminescence (ph.L)")+
    xlab("Time (min)")
print(tt2)
} else {
tt2=ggplot(mat_vec,aes(y=calib1,x=(record_nb-record_nb[1])/60))+geom_line()+ggtitle(files[i])+ylab("Bioluminescence (ph.L)")+
  xlab("Time (min)")
print(tt2)
}

}

dev.off()


