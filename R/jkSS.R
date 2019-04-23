if (FALSE){
  library(r4ss)
  library(stringr)
  library(plyr)
  library(doParallel)
  library(foreach)
  library(tMSE)
  
  cl = makeCluster(3)
  registerDoParallel(cl)
  
  x="/home/laurence/Desktop/sea++/swonmse/inputs/2017/jk/_SWO_DATA.dat"
  
  jk=tMSE:::runJK(x)
  }

if (FALSE){
  cl = makeCluster(3)
  registerDoParallel(cl)
  
  x="/home/laurence/Desktop/sea++/swonmse/inputs/2017/jk/_SWO_DATA.dat"
  
  hindcast=runHcst(x)
  
ggplot(subset(ss$cpue,Fleet_name!="SPN_1"))+
    geom_point(aes(Yr,Obs),col="grey")+
    geom_line(aes(Yr,Exp))+
    geom_line(aes(year,hat,group=tail,col=as.character(year-tail)),
            data=transform(subset(hindcast,name!="SPN_1"),Yr=year,Obs=obs,Fleet_name=name))+
    facet_wrap(~Fleet_name,scale="free")+
    theme_bw()+
    xlab("Year")+ylab("CPUE")+
    theme(axis.text.x=element_text(angle=-30),legend.position="none")
  
ggplot(ddply(subset(ss$cpue,Fleet_name!="SPN_1"),.(Fleet_name), subset, Yr>max(Yr-6)))+
  geom_point(aes(Yr,Obs))+
  geom_line(aes(Yr,Exp),col="grey")+
  geom_line(aes(year,hat,group=tail,col=as.character(year-tail)),
            data=transform(subset(hindcast,name!="SPN_1"),Yr=year,Obs=obs,Fleet_name=name))+
  facet_wrap(~Fleet_name,scale="free")+
  theme_bw()+
  xlab("Year")+ylab("CPUE")+
  theme(axis.text.x=element_text(angle=-90),legend.position="none")
  }


if (FALSE){
  x="/home/laurence/Desktop/Dropbox/baltic-ss/xval/EBcod_base_dat.ss"

  codh=tMSE:::runHcstYr(x,new=TRUE)
  }