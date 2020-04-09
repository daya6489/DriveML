## ----setup, include=FALSE------------------------------------------------
library(rmarkdown)
library(SmartEDA)
library(SmartML)
library(knitr)
library(scales)
library(ggplot2)

## ----eda-c3-r, warning=FALSE,eval=F--------------------------------------
#  install.packages("SmartML")
#  library("SmartML")
#  library("SmartEDA")
#  ## Load sample dataset from ISLR pacakge
#  heart = SmartML::heart

## ----od_1,warning=FALSE,eval=F,include=T---------------------------------
#  # Overview of the data - Type = 1
#  ExpData(data=heart,type=1)
#  
#  # Structure of the data - Type = 2
#  ExpData(data=heart,type=2)

## ----od_2,warning=FALSE,eval=T,include=F---------------------------------
ovw_tabl <- ExpData(data=heart,type=1)
ovw_tab2 <- ExpData(data=heart,type=2)

## ----od_3,warning=FALSE,eval=T,render=ovw_tabl,echo=F--------------------
kable(ovw_tabl, "html")

## ----od_31,warning=FALSE,eval=T,render=ovw_tab2,echo=F-------------------
kable(ovw_tab2, "html")

## ----snc1,warning=FALSE,eval=T,include=F---------------------------------
snc = ExpNumStat(heart,by="GA",gp="target_var",Qnt=seq(0,1,0.1),MesofShape=2,Outlier=TRUE,round=2)
rownames(snc)<-NULL

## ----snc2, warning=FALSE,eval=F,include=T--------------------------------
#  ExpNumStat(heart,by="GA",gp="target_var",Qnt=seq(0,1,0.1),MesofShape=2,Outlier=TRUE,round=2)

## ----snc3,warning=FALSE,eval=T,render=snc,echo=F-------------------------
paged_table(snc)

## ----bp3.1,warning=FALSE,eval=T,include=T,fig.align='center',fig.height=7,fig.width=7----
plot4 <- ExpNumViz(heart,target="target_var",type=1,nlim=3,fname=NULL,Page=c(2,2),sample=8)
plot4[[1]]

## ----ed3.3, eval=T,include=F---------------------------------------------
et100 <- ExpCTable(heart,Target="target_var",margin=1,clim=10,nlim=3,round=2,bin=NULL,per=F)
rownames(et100)<-NULL

## ----ed3.4, warning=FALSE,eval=F,include=T-------------------------------
#  ExpCTable(Carseats,Target="Urban",margin=1,clim=10,nlim=3,round=2,bin=NULL,per=F)

## ----ed3.5,warning=FALSE,eval=T,render=et100,echo=F,out.height=8,out.width=8----
kable(et100,"html")

## ----ed3.10,warning=FALSE,eval=T,include=T,fig.align='center',fig.height=7,fig.width=7----
plot5 <- ExpCatViz(heart,target = "target_var", fname = NULL, clim=5,col=c("slateblue4","slateblue1"),margin=2,Page = c(2,1),sample=2)
plot5[[1]]

## ----ktana, eval=T,include=F---------------------------------------------
ana1 <- ExpOutliers(heart, varlist = c("oldpeak","trestbps","chol"), method = "boxplot",  treatment = "mean", capping = c(0.1, 0.9))
outlier_summ <- ana1[[1]]

## ----out1, warning=FALSE,eval=F,include=T--------------------------------
#  ExpOutliers(heart, varlist = c("oldpeak","trestbps","chol"), method = "boxplot",  treatment = "mean", capping = c(0.1, 0.9))

## ----out11,warning=FALSE,eval=T,render=outlier_summ,echo=F,out.height=8,out.width=8----
kable(outlier_summ,"html")

## ---- warning=FALSE,eval=T,include=T-------------------------------------
dateprep <- autoDataprep(data = heart,
                             target = 'target_var',
                             missimpute = 'default',
                             auto_mar = FALSE,
                             mar_object = NULL,
                             dummyvar = TRUE,
                             char_var_limit = 15,
                             aucv = 0.002,
                             corr = 0.98,
                             outlier_flag = TRUE,
                             uid = NULL,
                             onlykeep = NULL,
                             drop = NULL)
printautoDataprep(dateprep)
train_data <- dateprep$master_data

## ---- warning=FALSE,eval=T,include=T-------------------------------------
mymodel <- autoMLmodel( train = heart,
                        test = NULL,
                        target = 'target_var',
                        testSplit = 0.2,
                        maxLevels = 100,
                        tuneIters = 10,
                        tuneType = "random",
                        resampleMethod = "CV",
                        resampleIters = 5,
                        models = "all",
                        varImp = 10,
                        liftGroup = 50,
                        maxObs = 4000,
                        uid = NULL,
                        seed = 1991)

## ----out00,warning=FALSE,eval=T,render=mymodel, echo=F,out.height=8,out.width=8----
performance <- mymodel$results
kable(performance, "html")


## ----o1,warning=FALSE,render=mymodel,eval=T,include=T,fig.align='center',fig.height=4,fig.width=7----
TrainROC <- mymodel$trainedModels$randomForest$modelPlots$TrainROC
TrainROC

## ----o10,warning=FALSE,render=mymodel,eval=T,include=T,fig.align='center',fig.height=4,fig.width=7----
TestROC <- mymodel$trainedModels$randomForest$modelPlots$TestROC
TestROC

## ----o11,warning=FALSE,render=mymodel,eval=T,include=T,fig.align='center',fig.height=4,fig.width=7----
VarImp <- mymodel$trainedModels$randomForest$modelPlots$VarImp
VarImp

## ----o12,warning=FALSE,render=mymodel,eval=T,include=T,fig.align='center',fig.height=4,fig.width=7----
Threshold <- mymodel$trainedModels$randomForest$modelPlots$Threshold
Threshold

