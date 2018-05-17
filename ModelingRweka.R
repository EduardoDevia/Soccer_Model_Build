Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_161')
#install.packages("DMwR")
options(java.parameters = "-Xmx10g") 
library(libcoin)
library(mvtnorm)
library(rpart)
library(rJava)
library(grid)
library(partykit)
library(RWeka)
library(partykit)
library(FSelector)
library(e1071)
library(lattice)
library(ggplot2)
library(caret)
library(RWekajars)
library(evaluate)
library(proto)
library(gsubfn)
library(DBI)
library(RSQLite)
library(DBI)
library(tcltk)
library(sqldf)
library(plyr)
library(dplyr)
library(DMwR) #----For SMOTE
library(caTools)
library(foreach)
library(iterators)
library(snow)
library(doSNOW)


memory.limit(80000)
memory.limit()
memory.size(TRUE)
memory.size()
memory.profile()

#help(make_Weka_associator)
#WOW("M5P")
#Train total rows


#=============Use one league
Belgium<-subset(Match,Match$name=='Belgium')
England<-subset(Match,Match$name=='England')
France<-subset(Match,Match$name=='France')
Germany<-subset(Match,Match$name=='Germany')
Italy<-subset(Match,Match$name=='Italy')
Netherlands<-subset(Match,Match$name=='Netherlands')
Portugal<-subset(Match,Match$name=='Portugal')
Scotland<-subset(Match,Match$name=='Scotland')
Spain<-subset(Match,Match$name=='Spain')


#=============Run each league with 50% of the data=====

Belgium_table<-AutoML(Belgium,0.8,90,"Y")
England_table<-AutoML(England,0.8,90,"Y")
France_table<-AutoML(France,0.8,90,"Y")
Germany_table<-AutoML(Germany,0.8,90,"Y")
Italy_table<-AutoML(Italy,0.8,90,"Y")
Netherlands_table<-AutoML(Netherlands,0.8,90,"Y")
Portugal_table<-AutoML(Portugal,0.8,90,"Y")
Scotland_table<-AutoML(Scotland,0.8,90,"Y")
Spain_table<-AutoML(Spain,0.8,90,"Y")


#=============Add a column to know the table league

Belgium_table$League<-"Belgium"
England_table$League<-"England"
France_table$League<-"France"
Germany_table$League<-"Germany"
Italy_table$League<-"Italy"
Netherlands_table$League<-"Netherlands"
Portugal_table$League<-"Portugal"
Scotland_table$League<-"Scotland"
Spain_table$League<-"Spain"


#================Join everything in one table

New_Table<-rbind(Belgium_table,England_table,France_table,Germany_table,Italy_table,Netherlands_table,Portugal_table,
                 Scotland_table,Spain_table
                 )
New_Table_Original<-New_Table
New_Table <- New_Table[order(New_Table$Accuracy),] 
rownames(New_Table) <- NULL
New_Table$Order_Accuracy<-rownames(New_Table)
New_Table <- New_Table[order(New_Table$Accuracy_Test),] 
rownames(New_Table) <- NULL
New_Table$Order_Accuracy_Test<-rownames(New_Table)
New_Table$Order_Accuracy<-as.numeric(New_Table$Order_Accuracy)
New_Table$Order_Accuracy_Test<-as.numeric(New_Table$Order_Accuracy_Test)
New_Table$Top<-New_Table$Order_Accuracy_Test+New_Table$Order_Accuracy
New_Table$Top<-as.numeric(New_Table$Top)
New_Table <- New_Table[order(-New_Table$Top),] 
rownames(New_Table) <- NULL
New_Table$Top<-rownames(New_Table)
New_Table$Top<-as.numeric(New_Table$Top)
New_Table<-subset(New_Table, select = c(-15,-16))
New_Table_Original<-New_Table
New_Table<-subset(New_Table, New_Table$Top<=5)



write.table(New_Table, file = "S:/chedevia/New_Table.csv", col.names = NA,  sep = ",",  qmethod = "double") 

#================Select the best league the two best models
#================ In this case is Germany, Portugal and Portugal2 are the best 3 





Germany_Top_2_Models<-subset(New_Table_Original, New_Table_Original$League=='Germany')
rownames(Germany_Top_2_Models)<-NULL
Germany_Top_2_Models$Top<-as.numeric(rownames(Germany_Top_2_Models))
Germany_Top_2_Models<-subset(Germany_Top_2_Models, Germany_Top_2_Models$Top<=2 )

Portugal_Top_2_Models<-subset(New_Table_Original, New_Table_Original$League=='Portugal')
rownames(Portugal_Top_2_Models)<-NULL
Portugal_Top_2_Models$Top<-as.numeric(rownames(Portugal_Top_2_Models))
Portugal_Top_2_Models<-subset(Portugal_Top_2_Models, Portugal_Top_2_Models$Top<=2 )

#Germany
Model1<-"weka.classifiers.trees.RandomForest" 
Model2<-"weka.classifiers.lazy.IBk" 




#===========Sorting the file to find the top 2
#Portugal_Final<-subset(Portugal_Final, select = c(-14))
Portugal_Final <- Portugal_Final[order(Portugal_Final$Accuracy),] 
rownames(Portugal_Final) <- NULL
Portugal_Final$Order_Accuracy<-rownames(Portugal_Final)
Portugal_Final <- Portugal_Final[order(Portugal_Final$Cross_Val_Accuracy),] 
rownames(Portugal_Final) <- NULL
Portugal_Final$Order_Cross_Accuracy<-rownames(Portugal_Final)
Portugal_Final$Order_Accuracy<-as.numeric(Portugal_Final$Order_Accuracy)
Portugal_Final$Order_Cross_Accuracy<-as.numeric(Portugal_Final$Order_Cross_Accuracy)
Portugal_Final$Top<-Portugal_Final$Order_Cross_Accuracy+Portugal_Final$Order_Accuracy
Portugal_Final$Top<-as.numeric(Portugal_Final$Top)
Portugal_Final <- Portugal_Final[order(-Portugal_Final$Top),] 
rownames(Portugal_Final) <- NULL
Portugal_Final$Top<-rownames(Portugal_Final)
Portugal_Final$Top<-as.numeric(Portugal_Final$Top)
Portugal_Final<-subset(Portugal_Final, select = c(-15,-14))
Portugal_Final_Original<-Portugal_Final
Portugal_Final<-subset(Portugal_Final, Portugal_Final$Top<=5)



#Portugal
Model1<-"weka.classifiers.trees.RandomForest" 
Model2<-"weka.classifiers.lazy.LMT" 




#================Remove the last column "Name of the League" to be able to join it

Portugal_Final<-subset(Portugal_Final_Original, select = -14)

#================Check if baggin, boosting or ensamble improves the accuracy

Portugal_Final<-Auto_ML_Bag_Bos_Ens(Portugal,100,Model1,Model2,Portugal_Final)

#==============Plotting the data====================
library(ggplot2)
library(reshape2)

# Everything on the same plot with smooth stat
ggplot(Portugal_Final, aes(Models,Cross_Val_Accuracy)) + 
  geom_point() + 
  stat_smooth() 
# Individuals Graphs
ggplot(Portugal_Final, aes(Models,Overfitting)) +
  theme_bw() +
  geom_line() 
#facet_wrap(~ variable)



#================PREDICTION=========================

RandomForest<-make_Weka_classifier("weka/classifiers/trees/RandomForest")
RandomForest_Classifier<-RandomForest(training_set$result~ ., data = training_set)
RandomForest_Train<-summary(RandomForest_Classifier)

LMT_Classifier<-LMT(training_set$result~ ., data = training_set, na.action=NULL)
LMT_Train<-summary(LMT_Classifier)

J48_Classifier<-J48(training_set$result~ ., data = training_set)
J48_Train<-summary(J48_Classifier)
J48_Train$confusionMatrix

MultilayerPerceptron<-make_Weka_classifier("weka/classifiers/functions/MultilayerPerceptron")
MultilayerPerceptron_Classifier<-MultilayerPerceptron(training_set$result~ ., data = training_set)
MultilayerPerceptron_Train<-summary(MultilayerPerceptron_Classifier)

IBk_Classifier<-IBk(training_set$result~ ., data = training_set,control=Weka_control(K=1))
IBK_Train<-summary(IBk_Classifier)
TEST_IBK<-table( predict(IBk_Classifier,newdata=test_Portugal_data),test_Portugal_data$result )
(TEST_IBK[1,1]+TEST_IBK[2,2])/(TEST_IBK[1,1]+TEST_IBK[2,2]+TEST_IBK[1,2]+TEST_IBK[2,1])

#================ The best model was with the IBk now we find the best % split of data
#####################################
#############Here I am############
#####################################
#####################################
#Germany_Final<-AutoML(Germany,100)


train_file<-Portugal

for (i in 1:9) {
#Sample function
  resample<-make_Weka_filter("weka/filters/supervised/instance/Resample")
#Size start with 10%  
Size<-i*10
#Resample file with % of i
train_file_clean<-resample(result~ .,data=train_file,control=Weka_control(Z=Size))


RandomForest<-make_Weka_classifier("weka/classifiers/trees/RandomForest")
RandomForest_Classifier<-RandomForest(train_file_clean$result~ ., data = train_file_clean)
RandomForest_Train<-summary(RandomForest_Classifier)
#Cross Validation
RandomForest_CV <- evaluate_Weka_classifier(RandomForest_Classifier, numFolds = 10, complexity = FALSE, seed = 1, class = TRUE)
if(!exists("RandomForest_Train")){RandomForest_Train<-summary(ZeroR_Classifier)}
if(!exists("RandomForest_CV")){RandomForest_CV<-evaluate_Weka_classifier(ZeroR_Classifier, numFolds = 10, complexity = FALSE, seed = 1, class = TRUE)}

FALSE_Correct_Clasified<-c(RandomForest_Train$confusionMatrix[2,2])
FALSE_Correct_Clasified_CV<-c(RandomForest_CV$confusionMatrix[2,2])
TRUE_Correct_Clasified<-c(RandomForest_Train$confusionMatrix[1,1])
TRUE_Correct_Clasified_CV<-c(RandomForest_CV$confusionMatrix[1,1])

Percentage<-c(Size)
Table_Models<-data.frame(Percentage,FALSE_Correct_Clasified,TRUE_Correct_Clasified,FALSE_Correct_Clasified_CV,TRUE_Correct_Clasified_CV)
TN<-summary(train_file_clean$result)[2]#True Negative
TP<-summary(train_file_clean$result)[1]#True Positive
Table_Models$Accuracy<-((FALSE_Correct_Clasified+TRUE_Correct_Clasified)/(TN+TP))*100
Table_Models$Cross_Val_Accuracy<-((FALSE_Correct_Clasified_CV+TRUE_Correct_Clasified_CV)/(TN+TP))*100
Table_Models$Sensitivity<-(TRUE_Correct_Clasified/TP)*100
Table_Models$Sensitivity_CV<-(TRUE_Correct_Clasified_CV/TP)*100
Table_Models$Specificity<-(FALSE_Correct_Clasified/TN)*100
Table_Models$Specificity_CV<-(FALSE_Correct_Clasified_CV/TN)*100
Table_Models$Overfitting<-(Table_Models$Accuracy-Table_Models$Cross_Val_Accuracy)
Table_Models<-rbind(Table_Models,Previous_Table)
Table_Models<-Table_Models[order(Table_Models$Overfitting),]
Previous_Table<-Table_Models
rownames(Table_Models) <- NULL
Table_Models<-subset(Table_Models, Table_Models$Overfitting <= 100)
}



#==============Plotting the data====================
library(ggplot2)
library(reshape2)

# Everything on the same plot with smooth stat
ggplot(Table_Models, aes(Percentage,Sensitivity)) + 
  geom_point() + 
  stat_smooth() 
# Everything on the same plot normal data
ggplot(Table_Models, aes(Percentage,Specificity_CV)) +
  theme_bw() +
  geom_line()
# Individuals Graphs
ggplot(Table_Models, aes(Percentage,Overfitting)) +
  theme_bw() +
  geom_line() 
  #facet_wrap(~ variable)



#===============The best percentage of the data was with 90% of the data
#===============Now check if there is any extra column to be deleted in order to improve accuracy.

Size<-90
train_file<-Portugal2
resample<-make_Weka_filter("weka/filters/supervised/instance/Resample")
train_file_clean<-resample(result~ .,data=train_file,control=Weka_control(Z=Size))


#===============IBk C & M Optimisation===================
WOW(IBk)
OF<-0
CVnew<-0
OFnew<-0
Cnew<-0
Knew<-0
Neighbour<-0

df_IBk <- data.frame(SAMPLE=double(),O_F=double(),OF_New=double(),K_New=double(),CV_New=double(),"OFnew+CVnew"=double(),T_R=double())
for (i in 1:20) {
  Neighbour=i
  IBk_Classifier<-IBk(train_file_clean$result~ ., data = train_file_clean,control=Weka_control(K=Neighbour))
  IBk_Classifier_Summary<-summary(IBk_Classifier)
  TR<-IBk_Classifier_Summary$details[[1]]
  
  #Cross Validation
  IBk_CV <- evaluate_Weka_classifier(IBk_Classifier, numFolds = 10, complexity = FALSE, seed = 1, class = TRUE)
  #Cross Validation
  CV<-IBk_CV$details[[1]]
  OF<-TR-CV
  ifelse(-OFnew+CVnew>-OF+CV,OFnew,OFnew<-OF)
  ifelse(-OFnew+CVnew>-OF+CV,Knew,Knew<-Neighbour)
  ifelse(-OFnew+CVnew>-OF+CV,CVnew,CVnew<-CV)
  df_IBk[nrow(df_IBk) + 1,] = list(SAMPLE=i,O_F=OF,OF_New=OFnew,K_New=Knew,CV_New=CVnew,"OFnew+CVnew"=(CVnew-(OFnew*CVnew)),T_R=TR)
 
}



#=================Optimizing the attributes=================

Correlation_AE <- make_Weka_attribute_evaluator("weka/attributeSelection/CorrelationAttributeEval")
Gain_Ratio_AE_table<-GainRatioAttributeEval(train_file_clean$result~ . , data = train_file_clean)
Info_Gain_AE_table<-InfoGainAttributeEval(train_file_clean$result~ . , data = train_file_clean)
Correlation_AE_table<-Correlation_AE(train_file_clean$result~ . , data = train_file_clean)
Total_Features<-Gain_Ratio_AE_table+Info_Gain_AE_table+Correlation_AE_table
Attribute_Table<-rbind(Gain_Ratio_AE_table,Info_Gain_AE_table,Correlation_AE_table,Total_Features)
Attribute_Table<-t(Attribute_Table)
Attribute_Table<-Attribute_Table[order(Attribute_Table$Total_Features  ),] 

Attribute_Selection<-make_Weka_filter("weka/filters/supervised/attribute/AttributeSelection")
Wrapper_Sub<-Attribute_Selection(result ~., data = train_file_clean, control = Weka_control("E" = "weka.attributeSelection.WrapperSubsetEval -B weka.classifiers.lazy.IBk", "S" = "weka.attributeSelection.GreedyStepwise")) 
CFS_SE<-Attribute_Selection(result ~., data = train_file_clean, control = Weka_control("E" = "weka.attributeSelection.CfsSubsetEval", "S" = "weka.attributeSelection.GreedyStepwise")) 
OneR_AE <-Attribute_Selection(result ~., data = train_file_clean, control = Weka_control("E" = "weka.attributeSelection.OneRAttributeEval", "S" = "weka.attributeSelection.Ranker")) 
Principal_Comp<-Attribute_Selection(result ~., data = train_file_clean, control = Weka_control("E" = "weka.attributeSelection.PrincipalComponents", "S" = "weka.attributeSelection.Ranker")) 
Relief_AE <-Attribute_Selection(result ~., data = train_file_clean, control = Weka_control("E" = "weka.attributeSelection.ReliefFAttributeEval", "S" = "weka.attributeSelection.Ranker")) 
Symmetrical_Uncert_AE <-Attribute_Selection(result ~., data = train_file_clean, control = Weka_control("E" = "weka.attributeSelection.SymmetricalUncertAttributeEval", "S" = "weka.attributeSelection.Ranker")) 


NCOL(Symmetrical_Uncert_AE)
colnames(Symmetrical_Uncert_AE)

train_file_clean_GainR<-subset(train_file_clean, select = c(-1,-2))

train_file_clean_InfoGain<-subset(train_file_clean, select = c(-1,-6,-17))

#==================Evaluate with new set InfoGain====================
OF<-0
CVnew<-0
OFnew<-0
Cnew<-0
Mnew<-0
for (i in 1:20) {
  Neighbour<-i+1
  
  IBk_Classifier<-IBk(train_file_clean_InfoGain$class~ ., data = train_file_clean_InfoGain,control=Weka_control(K=Neighbour))
  IBk_Classifier_Summary<-summary(IBk_Classifier)
  TR<-IBk_Classifier_Summary$details[[1]]
  
  #Cross Validation
  IBk_CV <- evaluate_Weka_classifier(IBk_Classifier, numFolds = 10, complexity = FALSE, seed = 1, class = TRUE)
  #Cross Validation
  CV<-IBk_CV$details[[1]]
  OF<-TR-CV
  ifelse(-OFnew+CVnew>-OF+CV,OFnew,OFnew<-OF)
  ifelse(-OFnew+CVnew>-OF+CV,Knew,Knew<-Neighbour)
  ifelse(-OFnew+CVnew>-OF+CV,CVnew,CVnew<-CV)
  print(paste0(" OF ", format(round(OF,digits = 4), nsmall = 4)," OF New :", format(round(OFnew,digits = 4), nsmall = 4), " KNew :", 
               format(round(Knew,digits = 4), nsmall = 4), "  CV New ", format(round(CV,digits = 4), nsmall = 4),"  OFnew+CVnew :", format(round(OFnew+CVnew,digits = 4), nsmall = 4),"  TR New :", format(round(TR,digits = 4), nsmall = 4)
               ,"Value ",i) )
}

#==================Evaluate with new set InfoGain====================
OF<-0
CVnew<-0
OFnew<-0
Cnew<-0
Mnew<-0
for (i in 1:20) {
  Neighbour<-i+1
  
  IBk_Classifier<-IBk(train_file_clean_GainR$class~ ., data = train_file_clean_GainR,control=Weka_control(K=Neighbour))
  IBk_Classifier_Summary<-summary(IBk_Classifier)
  TR<-IBk_Classifier_Summary$details[[1]]
  
  #Cross Validation
  IBk_CV <- evaluate_Weka_classifier(IBk_Classifier, numFolds = 10, complexity = FALSE, seed = 1, class = TRUE)
  #Cross Validation
  CV<-IBk_CV$details[[1]]
  OF<-TR-CV
  ifelse(-OFnew+CVnew>-OF+CV,OFnew,OFnew<-OF)
  ifelse(-OFnew+CVnew>-OF+CV,Knew,Knew<-Neighbour)
  ifelse(-OFnew+CVnew>-OF+CV,CVnew,CVnew<-CV)
  print(paste0(" OF ", format(round(OF,digits = 4), nsmall = 4)," OF New :", format(round(OFnew,digits = 4), nsmall = 4), " KNew :", 
               format(round(Knew,digits = 4), nsmall = 4), "  CV New ", format(round(CV,digits = 4), nsmall = 4),"  OFnew+CVnew :", format(round(OFnew+CVnew,digits = 4), nsmall = 4),"  TR New :", format(round(TR,digits = 4), nsmall = 4)
               ,"Value ",i) )
}
