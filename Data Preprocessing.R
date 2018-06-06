#install.packages("RSQLite") 
#===============================================================================
#=================== Soccer outcome prediction =================================
#===============================================================================

#===============================================================================
#==============================1.Preprocessing =================================
#===============================================================================
#==============================Increase the Java memory=========================
options(java.parameters = "-Xmx12g")
#======================Load Libraries to use SQL================================

library(DBI)
library(RSQLite)
library(proto)
library(gsubfn)
library(RSQLite)
library(sqldf)
library(tcltk)
library(sqldf)
#======================Load Libraries for preprocessing=========================
library(ParamHelpers)
library(mlr)
#======================Load Libraries to use select_if==========================
library(plyr)
library(dplyr)
setwd("C:/Users/chedevia/Desktop/Test")
#==================Load Libraries to use Weka functions========================
#Library to run ML Models
library(RWeka) 
#==============Libraries needed to normalize the numeric values================
library(cluster)
library(MASS)
library(clusterSim)
#===================Library to use SMOTE to balance the classes================
library(grid)
library(DMwR) 
#===========================Library to split values============================
library(caTools)
#===========================Library to graph===================================
library(scales)
library(ggplot2)
#install.packages("formattable")
library(formattable)

#======================connect to the sqlite file===============================
sqlite    <- dbDriver("SQLite")
EuropeSoccer <- dbConnect(sqlite,"database.sqlite")

#======================Load Files from database ================================
#Check the tables in the DB
dbListTables(EuropeSoccer)
#Load the tables from SQLlite
Country<-dbReadTable(EuropeSoccer,"Country")
Match<-dbReadTable(EuropeSoccer,"Match")
Team<-dbReadTable(EuropeSoccer,"Team")
Team_Attributes<-dbReadTable(EuropeSoccer,"Team_Attributes")
# Change the id to Country_id to have different titles between the tables
colnames(Country)[which(names(Country) == "id")] <- "country_id"

#======================Delete Columns no needed=================================
#Remove extra odds and ids, location of a player and live action features like shos, fouls, cards.
summarizeColumns(Match)
Match<-subset(Match,select= c(-1,-(12:85),-(89:115)))
Team<-subset(Team, select = c(-1))

#======================Check the null values====================================
summarizeColumns(Team_Attributes)
sum(is.na(Country))
sum(is.na(Match))
sum(is.na(Team))
sum(is.na(Team_Attributes))
#Team Attributes has several values as NA
summarizeColumns(Team_Attributes)
#For Team Attributes we check the % of rows that have null values
sum(is.na(Team_Attributes$buildUpPlayDribbling))/nrow(Team_Attributes)
#The column "buildUpPlayDribbling" has 66% of the items with NA values, so the column is being removed
Team_Attributes<-subset(Team_Attributes,select=c(-7))
#There are several matches where there was no probabilities assigned to them
#Those values are being removed as they cannot be averaged.
Match<-na.omit(Match)


#======================Join the dataframes=====================================

Team_Joined<- merge(Team_Attributes,Team,by=c("team_fifa_api_id","team_api_id"))
Match_Joined<-merge(Match,Country,"country_id"="id")
#Remove previous dataframes
rm(Country)
rm(Team)
rm(Team_Attributes)
rm(Match)

#======================Converting from char to date===========================
str(Team_Joined)
Team_Joined$date<-as.Date(Team_Joined$date)
Match_Joined$date<-as.Date(Match_Joined$date)

#======================Sort data by team and date============================= 
#======================to calculate a from and to date========================
Team_Joined<-Team_Joined[order(Team_Joined$team_api_id,Team_Joined$date),]
#Rename rows
rownames(Team_Joined) <- NULL

#======================Add a finish date to the Team table ===================
#======================to know the performace of a team in a =================
#==================time frame, the maximum date is 2015-09-10 ================
max(Team_Joined$date) 
#2016-01-01 is the last date assigned as the season was finished in 2015-2016
Team_Joined$LastDate<-'2016-01-01'
#Change the new column to date format
Team_Joined$LastDate<-as.Date(Team_Joined$LastDate)
#set the column number to update with the for script
column_to_update<-which( colnames(Team_Joined)=="LastDate" )
#Having the data organized by team and date allows the below script to copy the previous date to the 
#new column last date to create a "from  to " dates in a single row
for (i in 1:(nrow(Team_Joined)-1)) { ifelse(Team_Joined[i,1]!=Team_Joined[i+1,1],
                                            Team_Joined[i,column_to_update]<-'2016-01-01',
                                            Team_Joined[i,column_to_update]<-Team_Joined[i+1,4]-1) }

#=============Remove Extra columns of the ids used to do the match===========
summarizeColumns(Match_Joined)
Match_Final<-subset(Match_Joined, select = c(-(1:2)))

#=============Join Team_Joined and Match_Joined for Home_Team attributes=====
#FirstCol is used to get the number of the last column before joining the dataframes
firstcol<-NCOL(Match_Final)+1
#Creating a new table
Match_Final<-sqldf("Select * from Match_Final
                   left join Team_Joined on Team_Joined.team_api_id = Match_Final.home_team_api_id 
                   and Match_Final.date > Team_Joined.date
                   and Match_Final.date <= Team_Joined.lastdate")
#Last column of the new table Match_Final Home Columns
lastcol<-NCOL(Match_Final)
#Renaming Home Columns
colnames(Match_Final)[firstcol:lastcol]<-paste("Home", colnames(Match_Final)[firstcol:lastcol],sep= " ")
#Check the columns created to be removed
summarizeColumns(Match_Final)
#Removing the extra columns like team's id and dates
Match_Final<-subset(Match_Final,select = c(-(firstcol:(firstcol+3)),-(lastcol)))

#=============Join Team_Joined and Match_Joined for Away_Team attributes=====
#FirstCol is used to get the number of the last column before joining the dataframes
firstcol<-NCOL(Match_Final)+1
#Creating a new table
Match_Final<-sqldf("Select * from Match_Final
                   left join Team_Joined on Team_Joined.team_api_id = Match_Final.away_team_api_id 
                   and Match_Final.date > Team_Joined.date
                   and Match_Final.date <= Team_Joined.lastdate")

#==========================Renaming Away Columns============================
#Last column of the new table Match_Final Away Team Columns
lastcol<-NCOL(Match_Final)
#Renaming Away team Columns
colnames(Match_Final)[firstcol:lastcol]<-paste("Away", colnames(Match_Final)[firstcol:lastcol],sep= " ")
#Check the columns created to be removed
summarizeColumns(Match_Final)
#Removing the extra columns like team's id and dates
Match_Final<-subset(Match_Final,select = c(-(firstcol:(firstcol+3)),-(lastcol)))
#===== Removing the team's id
summarizeColumns(Match_Final)
Match_Final<-subset(Match_Final,select = c(-4,-5,-6))
#===== Check the missing values
#There are several matches where there is no team attribute date so those matches are removed
#The final model will have this data before hand
summarizeColumns(Match_Final)
sum(is.na(Match_Final))
Match_Final<-na.omit(Match_Final)
#Check the summary of the dataframe
summary(Match_Final)
#The summary is not working because a couple of variables have a charc type instead of factor
#Change character to factor 
columns_to_change<-colnames(select_if(Match_Final, is.character))
#Change all the columns from Character to factor
for(i in 1:length(columns_to_change)){column_number<-which(colnames(Match_Final)==columns_to_change[i])  
Match_Final[,column_number]<-as.factor(Match_Final[,column_number])}
#Now the summary is working 

#==========================Create the target value==========================
#The objective is to find if a soccer Match is going to finish in draw or not
#because the return is higher and the idea is that the prediction will reduce the risk
Match_Final$target<-ifelse(Match_Final$home_team_goal==Match_Final$away_team_goal,"Draw","No Draw")
Match_Final$target<-as.factor(Match_Final$target)


#================Save files to create the graphs in Power BI================
write.table(Match_Final, file = "C:/Users/chedevia/Desktop/Test/Match_Final.csv",  col.names = NA, sep = ",",  qmethod = "double") 

#====Remove the dates and goals that are not going to be used in ML=========
summarizeColumns(Match_Final)
Match_Final$date<-NULL
Match_Final$home_team_goal<-NULL
Match_Final$away_team_goal<-NULL
Match_Final$season<-NULL

#====================Removing tables that are not needed====================
rm(EuropeSoccer)
rm(firstcol)
rm(i)
rm(lastcol)
rm(sqlite)
rm(Match_Joined)
rm(Team_Joined)
rm(column_number)
rm(columns_to_change)
rm(column_to_update)

#===============================================================================
#===================2.Machine Learning Classification===========================
#===============================================================================



#===========Now the function AutoML is run to find the best model===============
General_Model<-AutoML(Match_Final,Split_Value,100,Smote_value)

#============However the model built shows that there is not a =================
#============high prediction that generalizes the model ========================

#============There might be some improvements if the data=======================
#=======================is split between countries==============================
Belgium<-subset(Match_Final,Match_Final$name=='Belgium')
England<-subset(Match_Final,Match_Final$name=='England')
France<-subset(Match_Final,Match_Final$name=='France')
Germany<-subset(Match_Final,Match_Final$name=='Germany')
Italy<-subset(Match_Final,Match_Final$name=='Italy')
Netherlands<-subset(Match_Final,Match_Final$name=='Netherlands')
Portugal<-subset(Match_Final,Match_Final$name=='Portugal')
Scotland<-subset(Match_Final,Match_Final$name=='Scotland')
Spain<-subset(Match_Final,Match_Final$name=='Spain')

#================run the models with 50% of the data============================
#================to find the best subset to focus===============================
# Value to split the data in testing and training
Split_Value<-0.8
Smote_value<-"Y"
#Running the models for each league.
Belgium_ml<-AutoML(Belgium,Split_Value,50,Smote_value)
England_ml<-AutoML(England,Split_Value,50,Smote_value)
France_ml<-AutoML(France,Split_Value,50,Smote_value)
Germany_ml<-AutoML(Germany,Split_Value,50,Smote_value)
Italy_ml<-AutoML(Italy,Split_Value,50,Smote_value)
Netherlandsv<-AutoML(Netherlands,Split_Value,50,Smote_value)
Portugal_ml<-AutoML(Portugal,Split_Value,50,Smote_value)
Scotland_ml<-AutoML(Scotland,Split_Value,50,Smote_value)
Spain_ml<-AutoML(Spain,Split_Value,50,Smote_value)

#==========================Extract the data frame =============================
Belgium_table<-Belgium_ml[[1]]
England_table<-England_ml[[1]]
France_table<-France_ml[[1]]
Germany_table<-Germany_ml[[1]]
Italy_table<-Italy_ml[[1]]
Netherlands_table<-Netherlands_ml[[1]]
Portugal_table<-Portugal_ml[[1]]
Scotland_table<-Scotland_ml[[1]]
Spain_table<-Spain_ml[[1]]

#================add a column to identify the league=============================
Belgium_table$League<-"Belgium"
England_table$League<-"England"
France_table$League<-"France"
Germany_table$League<-"Germany"
Italy_table$League<-"Italy"
Netherlands_table$League<-"Netherlands"
Portugal_table$League<-"Portugal"
Scotland_table$League<-"Scotland"
Spain_table$League<-"Spain"

#================This was run to have a bench mark of===========================
#================the country and model to tune==================================
All_models<-rbind(Belgium_table,England_table,France_table,Germany_table,Italy_table,
                  Netherlands_table,Portugal_table,Scotland_table,Spain_table)
#Make a copy of the original for backup purposes
All_models_Original<-All_models
#Order the models by accuracy
All_models <- All_models[order(All_models$Accuracy),] 
#Rename the rows
rownames(All_models) <- NULL
#Add a new to assign a position based on accuracy
All_models$Order_Accuracy<-rownames(All_models)
#Now wwe sort by Test
All_models <- All_models[order(All_models$Accuracy_Test),] 
#Rename the rows
rownames(All_models) <- NULL
#Add a new to assign a position based on accuracy
All_models$Order_Accuracy_Test<-rownames(All_models)
#Now wwe sort by Cross validation
All_models <- All_models[order(All_models$Accuracy_Cross_Val),] 
#Rename the rows
rownames(All_models) <- NULL
#Add a new to assign a position based on accuracy
All_models$Order_Cross_Validation_Accuracy<-rownames(All_models)
#Convert new row to numeric
All_models$Order_Accuracy<-as.numeric(All_models$Order_Accuracy)
All_models$Order_Accuracy_Test<-as.numeric(All_models$Order_Accuracy_Test)
All_models$Order_Cross_Validation_Accuracy<-as.numeric(All_models$Order_Cross_Validation_Accuracy)
#sum all the new columns
All_models$Top<-All_models$Order_Accuracy_Test+All_models$Order_Accuracy+
  All_models$Order_Cross_Validation_Accuracy
#Sort by the new column to know the top algorightms
All_models <- All_models[order(-All_models$Top),] 
#Clear the row number
rownames(All_models) <- NULL
#Assign row number to new column
All_models$Top<-rownames(All_models)
#Change the new column to numeric
All_models$Top<-as.numeric(All_models$Top)
#Find the number of columns to remove
a<-which( colnames(All_models)=="Order_Accuracy" )
b<-which( colnames(All_models)=="Order_Accuracy_Test" )
c<-which( colnames(All_models)=="Order_Cross_Validation_Accuracy" )
#Remove the new columns used in the calculation
All_models<-subset(All_models, select = c(-a,-b,-c))
#Remove values created
rm(a)
rm(b)
rm(c)
#Select the top 5 
All_models<-subset(All_models, All_models$Top<=5)
#This shows that Germany has the less overfitting among the top 5 models


#=============Selecting the top three models==============================

#Select the top 3 models
Germany_Top_2_Models<-subset(Germany_table, Germany_table$Top<=3 )

#========================Check distribution of the Results=================
Results<-data.frame(group=c("Draw","No Draw"), value = summary(training_set$target))
#Graph the distribution of the Results
ggplot(Results, aes(x="", y=value, fill=group))+ coord_polar("y", start=0)+
  geom_bar(width = 1, stat = "identity")+   
  theme(axis.text.x=element_blank()) +
  geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), 
                label = percent(value/nrow(training_set))), size=5)+ggtitle('Distribution of Results')


#==========Now the objective is to reduce the overfitting=================
#==========1.Eliminate redundant attributes===============================
#==========2.Run all the models again to see if there is a new rank=======
#==========3.Run the ensamble, boosting and bagging methods===============
#==========4.Reduce the size of the file to reduce the noise==============


#==========1.Optimizing the attributes====================================
#Run function to check the attributes.
Attribute_Table<-Attribute_Selection(Germany,RandomForest,5,Split_Value,Size,Smote_value)
#Extract data frames
Attributes_to_remove<-Attribute_Table[[1]]
Optimal_Attributes_table<-as.data.frame(Attribute_Table[[1]])
Attribute_Table<-as.data.frame(Attribute_Table[[3]])
#Heat map to identify the attributes to remove
(formattable(Attribute_Table, list(
  Symmetrical_Uncert_AE = color_tile("white", "green"),
  Relief_AE = color_tile("white", "red"),
  OneR_AE = color_tile("white", "green"),
  CFS_SE = color_tile("white", "red"),
  Wrapper_Sub = color_tile("white", "green"),
  Info_Gain_AE_table = color_tile("white", "red"),
  Gain_Ratio_AE_table= color_tile("white", "green"),
  Correlation_AE= color_tile("white", "red"))))

#==========2.Run all the models again to see if there is a new rank=======
#Remove irrevelant attributes
#Germany<-subset(Germany, select = c (-))
#Check if there is a new model that will improve the accuracy
Germany_ml<-AutoML(Germany,Split_Value,100,Smote_value)
#Extract the table
Germany_table$League<-Germany_ml[[1]]
#Select the top 2 models
Germany_Top_2_Models_new<-subset(Germany_table, Germany_table$Top<=3 )
#The new best model is IBK(Nearest Neighbours) altough it's overfitting is greater than the random forest

#==========3.Run the ensamble, boosting and bagging methods===============
#Top two models to check if bagging, boosting or ensamble improves the accuracy
Model1<-"weka.classifiers.trees.RandomForest" 
Model2<-"weka.classifiers.lazy.IBk"
#Running the models
Germany_with_BBE_ml<-Auto_ML_Bag_Bos_Ens(DataFrame,Split_Value,100,Model1,Model2,Germany_Top_2_Models_new,Smote_value)
#Extract the table
Germany_with_BBE<-Germany_with_BBE_ml[[1]]
#It showed that the AdaBoostM1 reduces the underfitting from -4.83 to -4.53 
#load classifier model
AdaBoostM1_Classifier<-Germany_with_BBE_ml[[3]]
#==========4.Reduce the size of the file to reduce the noise==============

Germany_ML_data_size<-Reduce_noise(Germany,AdaBoostM1,Split_Value,Size,SMOTE)

#Prepare the data for precition

#Predic 200 records to load to Minizinc
#As a sample 200 records were taken
Number_of_records<-200
#Create a dataframe with the team's name and odds
Games<-data.frame(test_set[1:Number_of_records,26],test_set[1:Number_of_records,48],
                  test_set[1:Number_of_records,3])
#Run a prediction
Games$target<-predict(AdaBoostM1_Classifier,newdata=test_set[1:Number_of_records,])
#Rename the columns
colnames(Games)[1] <- "Home Team"
colnames(Games)[2] <- "Away Team"
colnames(Games)[3] <- "Odds Draw"
#Create a new column as a probability
Games$Probability<-(1/Games$`Odds Draw`)/1.06
#Select only the games that finished in Draw
Games<-subset(Games,Games$target=="Draw")
#Rename the rows
rownames(Games)<-NULL
#Create a new column with the Game number
Games$Number<-rownames(Games)

#Convert values to numeric
Games_text<-subset(Games, select = c(-1,-2,-4))
Games_text$Number<-as.numeric(Games_text$Number)
Games_text$`Odds Draw`<-as.numeric(Games_text$`Odds Draw`)
Games_text$Probability<-as.numeric(Games_text$Probability)
#Save file as dzn type to be recognized by minizinc
write.table(Games_text, file = "C:/Users/chedevia/Desktop/Test/Games1.dzn",  sep="\t",col.names = TRUE,qmethod = "double") 

#==================================================================================
#===========================Running Optimization of bets===========================
#==================================================================================

#To select the games to bet to reduce the risk and maximize the profit a model in Minizinc 
#is created using linear and non-linear programing.

#Set the working directory to run optimization tool minizinc
setwd("C:/Program Files/MiniZinc IDE (bundled)")
#Run program minizinc
RESULTS<-(system("mzn-gecode games.mzn games.dzn",intern = TRUE,ignore.stdout = FALSE,
       ignore.stderr = TRUE,wait = TRUE,show.output.on.console = TRUE, minimized = TRUE,
       invisible = TRUE))


