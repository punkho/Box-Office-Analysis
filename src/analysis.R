# Import packages 
library(recharts)
library(wordcloud2)
library(stringr)
library(plyr)
library(DT)

# Read the data
movies<-read.csv("E:\\R\\RStudio-1.1.383\\workspace\\data\\movies.csv")
# Delete invalid information
movies<-na.omit(movies)

# ---------- Draw the histogram of Sales_avg and Appraise_avg in different ratings ----------
# Divide ratings into seven ranks 
rating_1<-subset(movies,Rating<=3)
rating_2<-subset(movies,Rating>3&Rating<=5)
rating_3<-subset(movies,Rating>5&Rating<=6)
rating_4<-subset(movies,Rating>6&Rating<=7)
rating_5<-subset(movies,Rating>7&Rating<=8)
rating_6<-subset(movies,Rating>8&Rating<=9)
rating_7<-subset(movies,Rating>9)

# Build a new frame for different ratings about Sales_avg and Appraise_avg
rating_df<-data.frame(Rating=c("0~3","3~5","5~6","6~7","7~8","8~9","9~10"),
                      Sales_avg=c(mean(rating_1$Sales),mean(rating_2$Sales),
                                  mean(rating_3$Sales),mean(rating_4$Sales),
                                  mean(rating_5$Sales),mean(rating_6$Sales),
                                  mean(rating_7$Sales)),
                      Appraise_avg=c(mean(rating_1$Appraise),mean(rating_2$Appraise),
                                     mean(rating_3$Appraise),mean(rating_4$Appraise),
                                     mean(rating_5$Appraise),mean(rating_6$Appraise),
                                     mean(rating_7$Appraise)))

# Draw the histogram of Sales_avg in different ratings
Sales_eBar<-eBar(rating_df,~Rating,~Sales_avg)+
  eTitle(title="各评分电影平均票房数柱状图")+
  eLegend(show=FALSE)
Sales_eBar

# Draw the histogram of Appraise_avg in different ratings
Appraise_eBar<-eBar(rating_df,~Rating,~Appraise_avg)+
  eTitle(title="各评分电影平均评论数柱状图")+
  eLegend(show=FALSE)
Appraise_eBar


# ---------- Draw the pie chart of types and sales of types ----------
# Extract the type column and divide by /
type<-movies$Type
type<-as.character(type)
type<-strsplit(type,"/")
# Extract the sales column
sales<-movies$Sales

# Create two new lists of type and sales
type_list<-list()
sales_list<-list()
for(i in 1:nrow(movies)) {
  for(j in 1:length(type[[i]])) {
    # Take the every type of each movie and combine it into a new list
    add_list<-list(type[[i]][j])
    type_list<-c(type_list,add_list)
    # Divide the box office of each movies by number of types 
    add_list<-list(sales[[i]]/length(type[[i]]))
    sales_list<-c(sales_list,add_list)
  }
}

# Build two new frames for type and sales
type_df<-data.frame(unlist(type_list))
sales_df<-data.frame(unlist(sales_list))
# Insert the ID column into type_df and sales_df
ID<-c(1:nrow(type_df))
type_df<-data.frame(ID,type_df)
sales_df<-data.frame(ID,sales_df)
# Merge two data frame by "ID" and delete invalid information
type_sales_df<-merge(type_df,sales_df,by="ID")

# Add frequency in type data frame
type_df<-count(type_df$unlist.type_list.)
# Delete invalid information for the last items 
type_df<-type_df[-nrow(type_df),]
# In descending order
type_df<-type_df[order(type_df$freq,decreasing=T),]

# Create a new data frame of sales in each type
sum_sales_list<-list()
for(i in 1:nrow(type_df)) {
  sum_sales<-0
  for(j in 1:nrow(type_sales_df)) {
    if(type_sales_df$unlist.type_list.[j]==type_df$x[i]) {
      sum_sales<-sum_sales+type_sales_df$unlist.sales_list.[j]
    }
  }
  sum_sales_list[i]<-sum_sales
}
sum_sales_df<-data.frame(type_df$x,unlist(sum_sales_list))

# Draw the pie chart of the number of type 
type_ePie<-ePie(type_df,size=c(650, 430))+eLegend(orient="vertical")+
  eTitle(title="各类型电影上榜数量饼图")
type_ePie

# Draw the pie chart of sales in different types
type_sales_ePie<-ePie(sum_sales_df,size=c(650, 430))+eLegend(orient="vertical")+
  eTitle(title="各类型电影票房饼图")
type_sales_ePie


# ---------- Draw the cloud of actors ----------
# Extract the actor column and divide by /
actor<-movies$Actor
actor<-as.character(actor)
actor<-strsplit(actor,"/")

# Take the every actor of each movie and combine it into a new list
actor_list<-list()
for(i in 1:nrow(movies)) {
  add_list<-list(actor[[i]][1],actor[[i]][2],actor[[i]][3])
  actor_list<-c(actor_list,add_list)
}

# Build a new frame for actors 
actor_df<-data.frame(unlist(actor_list))
# Add frequency in actor data frame
actor_df<-count(actor_df$unlist.actor_list.)
# Delete invalid information for the head and tail
actor_df<-actor_df[-c(1:7),]
actor_df<-na.omit(actor_df)

# Extract the actor which appeared more than twice
actor_df<-actor_df[which(actor_df$freq >= 2),]
# In descending order
actor_df<-actor_df[order(actor_df$freq,decreasing=T),]

# Draw the cloud
wordcloud2(actor_df,size=0.3,minRotation=-pi/6,maxRotation=pi/6,rotateRatio=0.9)


# ---------- Using SVM for data mining ----------
# Import packages 
library(ISLR)
library(e1071)

# Read the digital data
digital_movies<-read.csv("E:\\R\\RStudio-1.1.383\\workspace\\data\\digital_movies.csv")

# Extract the data except column 1,3 and 10 as feature vector
x=digital_movies[,-c(1,3,10)]
# Extract the data from column 10 as outcome variable
y=digital_movies[,10]

# Build model: the kernel is polynomial kernel
model=svm(x,y,kernel="polynomial",gamma=if(is.vector(x)) 1 else 1/ncol(x),coef0=2)

summary(model)

# test with train data
pred<-predict(model,x)
# Check accuracy
table(pred,y)

# Calculate accuracy
table<-as.data.frame(table(pred,y))
count<-0
for(i in 1:nrow(table)) {
  if(table$pred[i] == table$y[i]) {
    count<-count+table$Freq[i]
  }
}
accuracy<-count/nrow(x)
accuracy

# Visualize (classes by color, SV by crosses):
plot(cmdscale(dist(x)),
     col=c("purple","red","orange","green")[as.integer(y)],
     pch=c("。","+")[1:200 %in% model$index+1])
legend(-0.95,-0.47,c("Bad","Excellent","Good","Normal"),
       col=c("purple","red","orange","green"),lty=1)


# ---------- Interactive table for showing data  ----------
DT::datatable(movies %>% head(100))