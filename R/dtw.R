if (FALSE}){
library("dtw")
data("aami3a")
ref <-ts(subset(dgs,factor=="base"&level=="wg"&name=="Age-4")$obs)
test<-ts(subset(dgs,factor=="base"&level=="wg"&name=="Age-5+")$obs)

plot(dtw(test,ref,k=TRUE),type="two",off=1,match.lty=2,match.indices=20)
}