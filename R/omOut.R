#' omSmry
#' 
#' @title omSmry 
#' 
#' @description create time series of summary statistics from \code{FLStock} relative to reference points
#' @author Laurence Kell, Sea++
#' @export omSmry  
#' @name omSmry
#' @param x \code{FLStock} 
#' @param y \code{FLBRP} or \code{FLPar} with reference points, optional 
#' @param z \code{FLPar} with `a` and `b` of length weight relationship \code{FLPar(a-0.001,b=3)} by default
#' 
#' @export omSmry
#' @docType method
#' @rdname omSmry
#' 
#' @seealso smryStat
#' 
#' @examples
#' \dontrun{
#' res=omSmry(om,eq)
#' }
setGeneric('omSmry', function(object,...) standardGeneric('omSmry'))

setMethod('omSmry', signature(object="FLStock"),
          function(object,y="missing",z=FLPar(a=0.001,b=3),...)
            
            omSmryFn(object,y,z))

omSmryFn<-function(x,y="missing",z=FLPar(a=0.001,b=3)){
  
  nms=c("iter","year","ssb","stock","rec","catch","catchjuv","fbar",
        "crash_harvest","virgin_rec","virgin_ssb","msy_harvest","msy_ssb","msy_yield","rec_hat",
        "swt","cwt","sage","cage","sln","cln") 
  
  res=omStock(x)
  
  if ("FLBRP" %in% is(y)){
    if (dims(y)$iter==1)
      y=propagate(y,dim(x)[6])
    
    res=merge(res,omRefs(refpts(y)))
    
    rec=as.data.frame((params(y)["a"]%*%ssb(x))%/%(params(y)["b"]%+%ssb(x)))
    
    names(rec)[(seq(dim(rec)[2]))[names(rec)=="data"]]="rec_hat"
    res=merge(res,rec)
  }
  else if ("FLPar" %in% is(y)){
    
    if (dims(y)$iter==1)
      y=propagate(y,dim(x)[6])
    
    rs2=omRefs(y)
    res=merge(res,rs2,by="iter")
  }
  
  if ("FLPar" %in% is(z))
    if (all(c("a","b") %in% dimnames(z)$params))
      res=merge(res,lenFn(x,z))
  
  res=res[,names(res)[names(res)%in%nms]]
  
  res=res[do.call(order,res[,c("iter","year")]),]
  
  return(res)}

omStock<-function(object){
  sage<-function(object) apply(stock.n(object)%*%ages(stock.n(object)),2:6,sum)%/%
    apply(stock.n(object),2:6,sum)
  cage<-function(object) apply(catch.n(object)%*%ages(catch.n(object)),2:6,sum)%/%
    apply(catch.n(object),2:6,sum) 
  swt<-function(object) apply(stock.n(object)%*%stock.wt(object),2:6,sum)%/%
    apply(stock.n(object),2:6,sum)
  cwt<-function(object) apply(catch.n(object)%*%catch.wt(object),2:6,sum)%/%
    apply(catch.n(object),2:6,sum) 
  hvt   <-function(object) catch(object)/stock(object)
  
  recs  <-function(object) {res=rec(object)
  dimnames(res)[[1]]="all"
  res}
  
  catchJuv<-function(object) apply(catch.n(object)%*%(1-mat(object))%*%catch.wt(object),2:6,sum)
  
  res=FLQuants(object,"ssb"=FLCore:::ssb,"stock"=FLCore:::stock,"rec"=recs,"catch"=FLCore:::catch,"catchjuv"=catchJuv,
               "fbar"=FLCore:::fbar,
               "swt"=swt,"cwt"=cwt,"sage"=sage,"cage"=cage)
  
  model.frame(mcf(res),drop=TRUE)}

omRefs<-function(object){
  
  refs=rbind(as.data.frame(object["crash",c("harvest")]),
             as.data.frame(object["virgin",c("rec","ssb")]),
             as.data.frame(object["msy",c("yield","ssb","harvest")]))
  refs=cast(refs,iter~refpt+quant,value="data")
  
  refs}

lenFn<-function(x,y){
  sln<-function(object) apply(stock.n(object)%*%exp(log(stock.wt(object)%/%y["a"])%/%y["b"]),2:6,sum)%/%
    apply(stock.n(object),2:6,sum)
  cln<-function(object) apply(catch.n(object)%*%exp(log(catch.wt(object)%/%y["a"])%/%y["b"]),2:6,sum)%/%
    apply(catch.n(object),2:6,sum) 
  
  model.frame(FLQuants(x,"sln"=sln,"cln"=cln),drop=TRUE)}
