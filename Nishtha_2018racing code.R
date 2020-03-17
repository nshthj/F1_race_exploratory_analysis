############################ webscrapping 
library(rvest)
install.packages("XML")
library(XML)
library(httr)

#laps data
url_race_data <- "https://www.racefans.net/2018-f1-season/2018-f1-statistics/2018-f1-race-data/"
page_race_data <- read_html(url_race_data) #reading the page as html
tables_race <- html_table(page_race_data,header = FALSE) #extracting dataframes out of the html page race data
#View(tables_race)

rownames(tables_race[[1]])<-tables_race[[1]][,1]
tables_race[[1]]<-tables_race[[1]][,-1]
names(tables_race[[1]])<-c("FirstPosition","SecondPosition","ThirdPosition")
write.csv(tables_race[[1]][,1:3], "lapperpos18.csv")

names(tables_race[[2]])<-list("Driver","Started","Classified","Complete")
rownames(tables_race[[2]])<-tables_race[[2]][,1]
tables_race[[2]]<-tables_race[[2]][-1,-1]
write.csv(tables_race[[2]], "started&finished18.csv")

names(tables_race[[3]])<-list("Driver","Full seasons laps led", "Driver\'s season laps led")
rownames(tables_race[[3]])<-tables_race[[3]][,1]
tables_race[[3]]<-tables_race[[3]][-1,-1]
for(i in 1:dim(tables_race[[3]])[1]){
  for(j in 1:2){
    tables_race[[3]][i,j]<-gsub(".*\\(","",tables_race[[3]][i,j])
    tables_race[[3]][i,j]<-as.numeric(gsub("\\%).*","",tables_race[[3]][i,j]))/100
  }
}
write.csv(tables_race[[3]], "lapsled18.csv")

#pit stops data
url_pit_data <- "https://www.racefans.net/2018-f1-season/2018-f1-drivers-teams/2018-f1-strategy-pit-stops/"
page_pit_data <- read_html(url_pit_data) #reading the page as html
tables_pit <- html_table(page_pit_data,header = FALSE) #extracting dataframes out of the html page race data
View(tables_pit)

names(tables_pit[[1]])<-tables_pit[[1]][1,]
rownames(tables_pit[[1]])<-tables_pit[[1]][,1]
tables_pit[[1]]<-tables_pit[[1]][-1,-1]
tables_pit[[1]]["Average Pit Stop"] <- rowMeans(tables_pit[[1]]) #data transformation step
write.csv(tables_pit[[1]], "pitstop18.csv")   

names(tables_pit[[3]])<-list("Driver","Hard","Medium","Soft","Super-soft","Ultra-soft","Hyper-soft","Wet","Intermediate")
rownames(tables_pit[[3]])<-tables_pit[[3]][,1]
tables_pit[[3]]<-tables_pit[[3]][,-1]
for(i in 1:dim(tables_pit[[3]])[1]){
  for(j in 1:8){
    tables_pit[[3]][i,j]<-gsub(".*\\(","",tables_pit[[3]][i,j])
    tables_pit[[3]][i,j]<-as.numeric(gsub("\\%).*","",tables_pit[[3]][i,j]))/100
  }
}
write.csv(tables_pit[[3]], "tyres18.csv")

#penalties dataset

url_penalties_data <- "https://www.racefans.net/2018-f1-season/2018-f1-statistics/2018-f1-retirements-penalties/"
page_penalties_data <- read_html(url_penalties_data) #reading the page as html
tables_penalties <- html_table(page_penalties_data,header = FALSE) #extracting dataframes out of the html page race data
View(tables_penalties)

tables_penalties[[1]]<-data.frame(tables_penalties[[1]])
names(tables_penalties[[1]])<-list("Driver","Accident")
rownames(tables_penalties[[1]])<-tables_penalties[[1]][,1]
tables_penalties[[1]]<-tables_penalties[[1]][,(-3:-8)]
write.csv(tables_penalties[[1]], "accident18.csv")

names(tables_penalties[[2]])<-list("Driver","Penalties due to driver","Penalties due to team","No action")
rownames(tables_penalties[[2]])<-tables_penalties[[2]][,1]
tables_penalties[[2]]<-tables_penalties[[2]][,-1]
write.csv(tables_penalties[[2]], "penalties18.csv")

#starting pole position

url_qualify_data <- "https://www.racefans.net/2018-f1-season/2018-f1-statistics/2018-f1-qualifying-data/"
page_qualify_data <- read_html(url_qualify_data) #reading the page as html
tables_qualify <- html_table(page_qualify_data,header = FALSE) #extracting dataframes out of the html page race data
View(tables_qualify)

names(tables_qualify[[2]])<-list("Driver","Average Pol Pos")
rownames(tables_qualify[[2]])<-tables_qualify[[2]][,1]
tables_qualify[[2]]<-tables_qualify[[2]][,-1]
write.csv(tables_qualify[[2]], "startingpolpos18.csv") #have to delete rest of the data and only keep starting pol pos

#championship points

url_points_data <- "https://www.racefans.net/2018-f1-season/2018-f1-statistics/2018-f1-championship-points/"
page_point_data <- read_html(url_points_data) #reading the page as html
tables_points <- html_table(page_point_data,header = FALSE,fill = TRUE) #extracting dataframes out of the html page race data
View(tables_points)

names(tables_points[[1]])<-tables_points[[1]][1,]
tables_points[[1]]<-tables_points[[1]][-1,-1]
tables_points[[1]][1:3,"Finished"]<-c("First","Second","Third")
tables_points[[1]][4:dim(tables_points[[1]])[1],"Finished"]<-"Lost"
rownames(tables_points[[1]])<-tables_points[[1]][,1]
write.csv(tables_points[[1]], "finished18.csv")

######################################################################## combining the input data set

data_pitstop_18 = read.csv("pitstop18.csv")
data_tyre_18 = read.csv("tyres18.csv")
data_lapsperpos_18 = read.csv("lapperpos18.csv")
data_startfinish_18 = read.csv("started&finished18.csv")
data_lapsled_18 = read.csv("lapsled18.csv")
data_accident_18 = read.csv("accident18.csv")
data_penalties_18 = read.csv("penalties18.csv")
data_startingpolpos_18 = read.csv("startingpolpos18.csv")
data_finished_18 = read.csv("finished18.csv")

names(data_startfinish_18)

combined_18 = merge(combined_18,data_finished_18,by="X")
View(combined_18)

write.csv(combined_18, "Combined_2018.csv")

traindata = read.csv("Combined_2018.csv")
traindata<-traindata[,-1]


#preparing test data
#laps data
url_race_data <- "https://www.racefans.net/2019-f1-season/2019-f1-statistics/2019-f1-race-data/"
page_race_data <- read_html(url_race_data) #reading the page as html
tables_race <- html_table(page_race_data,header = FALSE) #extracting dataframes out of the html page race data
#View(tables_race)

rownames(tables_race[[1]])<-tables_race[[1]][,1]
tables_race[[1]]<-tables_race[[1]][,-1]
names(tables_race[[1]])<-c("FirstPosition","SecondPosition","ThirdPosition")
write.csv(tables_race[[1]][,1:3], "lapperpos19.csv")

names(tables_race[[2]])<-list("Driver","Started","Classified","Complete")
rownames(tables_race[[2]])<-tables_race[[2]][,1]
tables_race[[2]]<-tables_race[[2]][-1,-1]
write.csv(tables_race[[2]], "started&finished19.csv")

names(tables_race[[3]])<-list("Driver","Full seasons laps led", "Driver\'s season laps led")
rownames(tables_race[[3]])<-tables_race[[3]][,1]
tables_race[[3]]<-tables_race[[3]][-1,-1]
for(i in 1:dim(tables_race[[3]])[1]){
  for(j in 1:2){
    tables_race[[3]][i,j]<-gsub(".*\\(","",tables_race[[3]][i,j])
    tables_race[[3]][i,j]<-as.numeric(gsub("\\%).*","",tables_race[[3]][i,j]))/100
  }
}
write.csv(tables_race[[3]], "lapsled19.csv")

#pit stops data
url_pit_data <- "https://www.racefans.net/2019-f1-season/2019-f1-statistics/2019-f1-strategy-pit-stops/"
page_pit_data <- read_html(url_pit_data) #reading the page as html
tables_pit <- html_table(page_pit_data,header = FALSE) #extracting dataframes out of the html page race data
View(tables_pit)

names(tables_pit[[1]])<-tables_pit[[1]][1,]
rownames(tables_pit[[1]])<-tables_pit[[1]][,1]
tables_pit[[1]]<-tables_pit[[1]][-1,-1]
tables_pit[[1]]["Average Pit Stop"] <- rowMeans(tables_pit[[1]]) #data transformation step
write.csv(tables_pit[[1]], "pitstop19.csv")

names(tables_pit[[3]])<-list("Driver","Hard","Medium","Soft","Super-soft","Ultra-soft","Hyper-soft","Wet","Intermediate")
rownames(tables_pit[[3]])<-tables_pit[[3]][,1]
tables_pit[[3]]<-tables_pit[[3]][,-1]
for(i in 1:dim(tables_pit[[3]])[1]){
  for(j in 1:8){
    tables_pit[[3]][i,j]<-gsub(".*\\(","",tables_pit[[3]][i,j])
    tables_pit[[3]][i,j]<-as.numeric(gsub("\\%).*","",tables_pit[[3]][i,j]))/100
  }
}
write.csv(tables_pit[[3]], "tyres19.csv")

#penalties dataset

url_penalties_data <- "https://www.racefans.net/2019-f1-season/2019-f1-statistics/2019-f1-retirements-penalties/"
page_penalties_data <- read_html(url_penalties_data) #reading the page as html
tables_penalties <- html_table(page_penalties_data,header = FALSE) #extracting dataframes out of the html page race data
View(tables_penalties)

tables_penalties[[1]]<-data.frame(tables_penalties[[1]])
names(tables_penalties[[1]])<-list("Driver","Accident")
rownames(tables_penalties[[1]])<-tables_penalties[[1]][,1]
tables_penalties[[1]]<-tables_penalties[[1]][,(-3:-8)]
write.csv(tables_penalties[[1]], "accident19.csv") #extra step is to modify the written file by deleting the driver column

names(tables_penalties[[2]])<-list("Driver","Penalties due to driver","Penalties due to team","No action")
rownames(tables_penalties[[2]])<-tables_penalties[[2]][,1]
tables_penalties[[2]]<-tables_penalties[[2]][,-1]
write.csv(tables_penalties[[2]], "penalties19.csv")

#starting pole position

url_qualify_data <- "https://www.racefans.net/2019-f1-season/2019-f1-statistics/2019-f1-qualifying-data/"
page_qualify_data <- read_html(url_qualify_data) #reading the page as html
tables_qualify <- html_table(page_qualify_data,header = FALSE) #extracting dataframes out of the html page race data
View(tables_qualify)

names(tables_qualify[[2]])<-list("Driver","Average Pol Pos")
rownames(tables_qualify[[2]])<-tables_qualify[[2]][,1]
tables_qualify[[2]]<-tables_qualify[[2]][,-1]
write.csv(tables_qualify[[2]], "startingpolpos19.csv")

#championship points

url_points_data <- "https://www.racefans.net/2019-f1-season/2019-f1-statistics/2019-f1-championship-points/"
page_point_data <- read_html(url_points_data) #reading the page as html
tables_points <- html_table(page_point_data,header = FALSE,fill = TRUE) #extracting dataframes out of the html page race data
View(tables_points)

names(tables_points[[1]])<-tables_points[[1]][1,]
tables_points[[1]]<-tables_points[[1]][-1,-1]
tables_points[[1]][1:3,"Finished"]<-c("First","Second","Third")
tables_points[[1]][4:dim(tables_points[[1]])[1],"Finished"]<-"Lost"
rownames(tables_points[[1]])<-tables_points[[1]][,1]
write.csv(tables_points[[1]], "finished19.csv")

######################################################################## combining the test data set

data_pitstop_19 = read.csv("pitstop19.csv")
data_tyre_19 = read.csv("tyres19.csv")
data_lapsperpos_19 = read.csv("lapperpos19.csv")
data_startfinish_19 = read.csv("started&finished19.csv")
data_lapsled_19 = read.csv("lapsled19.csv")
data_accident_19 = read.csv("accident19.csv")
data_penalties_19 = read.csv("penalties19.csv")
data_startingpolpos_19 = read.csv("startingpolpos19.csv")
data_finished_19 = read.csv(("finished19.csv"))

names(data_startfinish_19)

#iteratively combine data - a bit of a manual process
sum(data_penalties_19[,"X"] %in% data_startingpolpos_19[,"X"]) #turns out only all data is present only for 15 racers 
combined_19 = merge(combined_19,data_finished_19,by="X")
View(combined_19)

write.csv(combined_19, "Combined_2019.csv")

test_data = read.csv("Combined_2019.csv")
test_data<-test_data[,-1]

#correlation
install.packages("ggpubr")
library("ggpubr")

install.packages("polycor")
library("polycor")

cor_train = traindata[2:23]
cor_test = test_data[2:23]
cor_train_filter<-Filter(var,cor_train)
cor_test_filter<-Filter(var,cor_test)
cor_train_filter<-cor_train_filter[-6] #removed hyper soft
cor_test_filter<-cor_test_filter[-2] #removed hard
colnames(cor_train_filter)
colnames(cor_test_filter)

#Principal componenet analysis
princi_cmp=prcomp(cor_train_filter,scale=TRUE)
summary(princi_cmp)

install.packages("factoextra")
library(factoextra)
fviz_eig(princi_cmp) #scree plot
fviz_pca_var(princi_cmp,col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE)
fviz_pca_biplot(princi_cmp,repel = TRUE,col.var = "#2E9FDF",col.ind = "#696969")


CR_train = hetcor(cor_train_filter, ML = FALSE, std.err = TRUE,  use = c("complete.obs", "pairwise.complete.obs"), bins=4, pd=TRUE)
as.matrix(CR_train)
CR_test = hetcor(cor_test_filter, ML = FALSE, std.err = TRUE,  use = c("complete.obs", "pairwise.complete.obs"), bins=4, pd=TRUE)
as.matrix(CR_test)

library(corrgram)
corrgram(cor_train_filter, order=TRUE, lower.panel=panel.shade, text.panel=panel.txt,
         main="Correlation Plot F1 Races 2018")

install.packages("corrplot")
library(corrplot)
corrplot(CR_train$correlations,type = "upper",method = "square",
         title = "Correlation Plot F1 Races 2018",
         tl.col = "black",mar = c(1,1,5,1))

corrgram(cor_test_filter, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Correlation Plot F1 Races 2019")

################################################################################################naivebayes
library(e1071)
library(caret)
set.seed(123)
control <- trainControl(method = "repeatedcv", number = 10, repeats = 20)
NBclassfier=naiveBayes(Finished ~., data=traindata, trControl = control)
print(NBclassfier)

test_data_predict<-test_data[-24]

NB = predict(NBclassfier, test_data_predict)

confusionMatrix(NB, test_data$Finished)
View(test_data)

########################################################################################
library(randomForest)
RFclassifier<-randomForest(Finished ~., data=traindata)

RF = predict(RFclassifier, test_data_predict)
confusionMatrix(RF, test_data$Finished)
