kobeJabba<-function(object){
  
  kobe=object[[1]][,c(1,2,10,13,3)]
  names(kobe)=c("scenario","year","stock","harvest","catch")

  refs=object[[3]][!duplicated(object[[3]][,1]),c(1,4,7,8)]
  names(refs)=c("scenario","hmsy","msy","k")
  refs=transform(refs,bmsy=msy/hmsy)
  kobe=merge(kobe,refs,by="scenario")
  
  kobe}

#head(ddply(jabbaSall[[2]],.(scenario,name),with,data.frame(obs=obs,hat=hat,diagsFn(data.frame(residual=residual)))))




