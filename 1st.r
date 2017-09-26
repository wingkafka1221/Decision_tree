# Decision_tree
#取数
loc<-"http://archive.ics.uci.edu/ml/machine-learning-databases/"
ds<-"breast-cancer-wisconsin/breast-cancer-wisconsin.data"
url<-paste(loc,ds,sep="")
breast<-read.table(url,sep=",",header = FALSE,na.strings = "?")
names(breast)<-c("ID","clumpThickness","sizeUniformity","shapeUniformity","maginalAdhesion","singleEpithelialCellSize","bareNuclei","blandChromatin","normalNucleoli","mitosis","class")
df<-breast[-1]

#随即提取训练集和测试集
train<-sample(nrow(df$class),0.7*nrow(df$class))
df.train<-df[train,]
df.validate<-df[-train,]
table(df.train$class)
table(df.validate$class)

#生成决策树
library(rpart)
set.seed(1234)
dtree<-rpart(class~.,data = df.train,method="class",parms=list(split="information"))
dtree$cptable
plotcp(dtree)

#剪枝
dtree.pruned<-prune(dtree,cp=.0125)
library(rpart.plot)
prp(dtree.pruned,type=2,extra=104,fallen.leaves=TRUE,main="Decision Tree")
