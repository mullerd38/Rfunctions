# y<-rnorm(12,mean=10,sd=1)
# x1<-gl(n=2,k=6,length=12,label=c("fille","garcon"))
# x2<-gl(n=2,k=3,length=12,label=c("bleu","rouge"))


# fit<-lm(y~x1+x2)
# summary(fit)

PRE <-function(model){
  library(car)
  A<-Anova(model,type="III")
  R <- A[1]
  dim <-dim(R)
  d <- dim[1]
  SSE <-R[d,1]
  ETASQP <- R/(SSE+R)
  ANOVA<-cbind(A,ETASQP); ANOVA<-round(ANOVA, digits = 3)
  colnames(ANOVA)<-c("SumSq","Df","F","p","PRE")
  ANOVA[d,c(3,4,5)]<-""
  print(ANOVA)
}

#PRE(fit)

