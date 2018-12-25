require(r4ss)
require(stringr)
require(plyr)

require(doParallel)
require(foreach)


## Sets up the files for the jackknife
setJK<-function(x){
  
  ## process file
  dirNow=getwd()
  
  #get rid of comments in data file
  dfl =str_trim(scan(x,sep="\n",what=as.character()))
  dfl=dfl[substr(dfl,1,1)!="#"]
  dfl=dfl[nchar(dfl)!=0]
  
  # function to get count number of rows
  rw<-function(x) as.numeric(strsplit(x,"\\s+")[[1]][1])
  
  # number of fleets and surveys  
  nFlt=rw(dfl[6])
  nSry=rw(dfl[7])
  
  # rows with data
  rCtc=seq(rw(dfl[17]))+17
  rU  =max(rCtc)+1+nFlt+nSry+seq(rw(dfl[max(rCtc)+1]))
  nDsc=rw(dfl[max(rU)+1])
  rDsc=max(rU)+nDsc+2+seq(rw(dfl[max(rU)+nDsc+2]))
  nLnc=rw(dfl[max(rDsc)+9])
  rLnc=(max(rDsc)+12):(length(dfl)-13)
  
  ## key for obs, i.e. indices and length comps
  u  =mdply(rU,  function(i) as.numeric(strsplit(dfl[i],"\\s+")[[1]])[1:5])[,-1]
  
  ## No length comps for now
  #lnc=mdply(rLnc,function(i) as.numeric(strsplit(dfl[i],"\\s")[[1]])[1:75])
  
  names(u)=c("year","season","fleet","obs","cv")
  
  u=cbind(u,row=rU)
  
  list(dfl=dfl,u=u)}

mkTmp<-function(){
  dr=tempfile()
  system(paste("mkdir",dr))
  
  dr}

jkU<-function(i,u,tfl,dat){
  #print(u[i,])
  
  ## copy files from target 
  dirNow=getwd()
  dirTmp=mkTmp()
  setwd(dirTmp)
  
  system(paste("cp",file.path(dirname(dat),"*.*"),dirTmp))
  
  #leave out obs
  u[,"fleet"]=-u[,"fleet"]
  for (j in i)
    tfl[j]=paste(unlist(subset(u[,-5],j==row)),sep=" ",collapse=" ")
  cat(tfl,sep="\n",file=file.path(dirTmp,substr(dat,nchar(dirname(dat))+2,nchar(dat))))
  
  #run
  system2("./ss3_3.24z",args="-nohess",stdout=NULL)
  
  #get results
  ssf=SS_output(getwd(), 
                forecast  =FALSE, 
                covar     =FALSE,
                checkcor  =FALSE,
                verbose   =FALSE, 
                printstats=FALSE, 
                hidewarn  =TRUE, 
                NoCompOK  =TRUE)
  
  rfs=c("SSB_Unfished",    "TotBio_Unfished", "SmryBio_Unfished", "Recr_Unfished",  "SSB_Btgt",        
        "SPR_Btgt",        "Fstd_Btgt",       "TotYield_Btgt",    "SSB_SPRtgt",     "Fstd_SPRtgt",     
        "TotYield_SPRtgt", "SSB_MSY",         "SPR_MSY",          "Fstd_MSY",       "TotYield_MSY",    
        "RetYield_MSY") 
  rf=subset(ssf$derived_quants,Label%in%rfs)
  ts=ssf$timeseries[,1:8]
  
  #clean up  
  setwd(dirNow)
  system(paste("rm -R",dirTmp))
  
  list(u=ssf$cpue,rf=rf,ts=ts)}

runHcst<-function(x,n=6){
  
  ## process files
  fls=tMSE:::setJK(x)
  key=ddply(fls$u,.(fleet), transform, maxyr=max(year))
  key=subset(key,year>=(maxyr-n)&year<maxyr)[,-7]
  
  dir=dirname(x)
  dir.create(dirX)
  
  hRsd=foreach(i=seq(dim(key)[1]),
       .multicombine=TRUE,
       .combine     =rbind.fill,
       .packages    ="tMSE") %dopar%{

       iRw=subset(fls$u,fleet==key[i,"fleet"]&year>key[i,"year"])[,"row"]
       res=tMSE:::jkU(iRw,fls$u,fls$dfl,x)
       rtn=cbind(tail =key[i,"year"],
                 naive=fls$u[as.numeric(dimnames(subset(fls$u,fleet==key[i,"fleet"]&year==key[i,"year"]))[[1]]),"obs"],
                 subset(res$u,Fleet==key[i,"fleet"]&Yr>key[i,"year"]))[,1:13]

       write.table(res[[1]],file=file.path(dir, "hcast",paste("rsd",i,".csv",sep="")))
       write.table(res[[2]],file=file.path(dir, "hcast",paste("ref",i,".csv",sep="")))
       write.table(res[[3]],file=file.path(dir, "hcast",paste("ts" ,i,".csv",sep="")))

       names(rtn)[3:13]=c("fleet","name","year","season","year.","vuln","obs","hat","q","eff","se")

       rtn}
 
  rsdl=mdply(data.frame(i=seq(dim(key)[1])),function(i)
    read.csv(file.path(dir,"hcast",paste("prd",i,".csv",sep="")),header=T,sep=" "))
  names(rsdl)=c("fleet","name","year","season","year.","vulnerable","obs","hat","q","q.","se",
                "dev","like","like.","sp","use")
  
  ts  =mdply(data.frame(i=seq(dim(key)[1])),function(i) 
    read.csv(file.path(dir,"hcast",paste("ts",i,".csv",sep="")),header=T,sep=" "))
  names(ts)=c("i","area","year","era","season","biomass","biomass.","ssb")
  
  rf  =mdply(data.frame(i=seq(dim(key)[1])),function(i)   
    read.csv(file.path(dir,"hcast",paste("ref",i,".csv",sep="")),header=T,sep=" "))
  rf=rf[,1:3]
  names(rf)=c("i","variable","value")
  
  return(list(hindcast=hRsd,residuals=rsdl,timeseries=ts,refpts=rf,key=key))}

runJK<-function(x){ 
  
  ## process files
  dirNow=getwd()
  fls   =setJK(x)
  dir   =dirname(x)
  dat   =substr(x,nchar(dir)+2,nchar(x))
  
  dirX=file.path(dir, "xval")
  dir.create(dirX)
  
  pRsd=NULL
  pRsd=foreach(i=seq(length(fls$u$row)),
     .multicombine=TRUE,
     .combine     =rbind,
     .packages    ="r4ss") %dopar%{
                
     ## copy files from target 
     dirTmp=tMSE:::mkTmp()
     setwd(dirTmp)
                
     system(paste("cp",file.path(dir,"*.*"),dirTmp))
     iRw=fls$u[i,"row"]
     res=tMSE:::jkU(iRw,fls$u,fls$dfl,file.path(dirTmp,dat))
                
     write.table(res[[1]],file=file.path(dir, "xval",paste("prd",i,".csv",sep="")))
     write.table(res[[2]],file=file.path(dir, "xval",paste("ref",i,".csv",sep="")))
     write.table(res[[3]],file=file.path(dir, "xval",paste("ts" ,i,".csv",sep="")))
                
     #clean up  
     setwd(dirNow)
     system(paste("rm -R",dirTmp))
                
     res[[1]][i,]}
  
  pRsd=pRsd[,1:11]
  names(pRsd)=c("fleet","name","year","season","year.","vulnerable","obs","hat","q","q.","se")
  
  #rsdl=mdply(data.frame(i=seq(length(fls$u$row))),function(i)
  #  read.csv(file.path(dirX,paste("prd",i,".csv",sep="")),header=T,sep=" ")[i,])

  ts  =mdply(data.frame(i=seq(length(fls$u$row))),function(i) 
    read.csv(file.path(dirX,paste("ts",i,".csv",sep="")),header=T,sep=" "))
  names(ts)=c("i","area","year","era","season","biomass","biomass.","ssb")
  
  rf  =mdply(data.frame(i=seq(length(fls$u$row))),function(i)   
    read.csv(file.path(dirX,paste("ref",i,".csv",sep="")),header=T,sep=" "))
  rf=rf[,1:3]
  names(rf)=c("i","variable","value")
  
  return(list(prediction=pRsd,timeseries=ts,refpts=rf))}

