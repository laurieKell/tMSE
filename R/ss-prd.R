
getRefpts<-function(object,value=TRUE){
  rfs=c("SSB_Unfished",    "TotBio_Unfished",  "SmryBio_Unfished", "Recr_Unfished",    
        "SSB_Btgt",        "SPR_Btgt",         "Fstd_Btgt",        "TotYield_Btgt",   
        "SSB_SPRtgt",      "Fstd_SPRtgt",      "TotYield_SPRtgt",  "SSB_MSY",         
        "SPR_MSY",          "Fstd_MSY",         "TotYield_MSY",    "RetYield_MSY") 
  rf=subset(object$derived_quants,Label%in%rfs[c(1,12,15)])[,2:3]
  names(rf)=c("value","var")
  dimnames(rf)[[1]]=c("k","bmsy","msy")
  rf=t(as.matrix(rf))
  rf["var",]=rf["var",]^2
  rf=as.data.frame(rf)
  
  rf[ifelse(value,1,2),]}

getPellaT<-function(object){
  rfs=c("SSB_Unfished",    "TotBio_Unfished",  "SmryBio_Unfished", "Recr_Unfished",    
        "SSB_Btgt",        "SPR_Btgt",         "Fstd_Btgt",        "TotYield_Btgt",   
        "SSB_SPRtgt",      "Fstd_SPRtgt",      "TotYield_SPRtgt",  "SSB_MSY",         
        "SPR_MSY",          "Fstd_MSY",         "TotYield_MSY",    "RetYield_MSY") 
  rf=subset(object$derived_quants,Label%in%rfs[c(1,12,15)])[,2:3]
  names(rf)=c("value","var")
  dimnames(rf)[[1]]=c("k","bmsy","msy")
  rf=t(as.matrix(rf))
  rf["var",]=rf["var",]^2
  rf=as.data.frame(rf)
  
  pt=transmute(rf["value",],shape=bmsy/k,
               k    =k,
               p    =optimise(function(x,y) (y-(1/(1+x))^(1/x))^2,
                              c(-0.9999,10),y=shape)$minimum,
               r    =(1+p)*(msy/bmsy))
  
  pt[,c("k","r","p","shape")]}

productionFn<-function(b,r,k,p){
  t1=b*r/p
  t3=(b/k)^p
  t1*(1-t3)}
