# Read the data
movies<-read.csv("E:\\R\\RStudio-1.1.383\\workspace\\data\\movies.csv")
# Delete invalid information
movies<-na.omit(movies)

# Function for digitization
list.digitization<-function(Factor) {
  # Extract the factor column and divide by /
  factor<-Factor
  factor<-as.character(factor)
  factor<-strsplit(factor,"/")
  # Extract the sales column
  sales<-movies$Sales
  
  # Create two new lists of factor and sales
  factor_list<-list()
  sales_list<-list()
  for(i in 1:nrow(movies)) {
    for(j in 1:length(factor[[i]])) {
      # Take the every factor of each movie and combine it into a new list
      add_list<-list(factor[[i]][j])
      factor_list<-c(factor_list,add_list)
      # Divide the box office of each movies by number of factors 
      add_list<-list(sales[[i]]/length(factor[[i]]))
      sales_list<-c(sales_list,add_list)
    }
  }
  
  # Build two new frames for factor and sales
  factor_df<-data.frame(unlist(factor_list))
  sales_df<-data.frame(unlist(sales_list))
  # Insert the ID column into factor_df and sales_df
  ID<-c(1:nrow(factor_df))
  factor_df<-data.frame(ID,factor_df)
  sales_df<-data.frame(ID,sales_df)
  # Merge two data frame by "ID" and delete invalid information
  factor_sales_df<-merge(factor_df,sales_df,by="ID")
  
  # Add frequency in factor data frame
  factor_df<-count(factor_df$unlist.factor_list.)
  # Delete invalid information for the last items 
  factor_df<-factor_df[-nrow(factor_df),]
  # In descending order
  factor_df<-factor_df[order(factor_df$freq,decreasing=T),]
  
  # Create a new data frame of sales in each factor
  sum_sales_list<-list()
  for(i in 1:nrow(factor_df)) {
    sum_sales<-0
    for(j in 1:nrow(factor_sales_df)) {
      if(factor_sales_df$unlist.factor_list.[j]==factor_df$x[i]) {
        sum_sales<-sum_sales+factor_sales_df$unlist.sales_list.[j]
      }
    }
    sum_sales_list[i]<-sum_sales
  }
  sum_sales_df<-data.frame(factor_df$x,unlist(sum_sales_list))
  
  # Calculated percentage 
  percent<-sum_sales_df$unlist.sum_sales_list./sum(sum_sales_df$unlist.sum_sales_list.)
  sum_sales_df<-data.frame(sum_sales_df,percent)
  
  # Create a new list for factor digitization
  factor_number_list<-list()
  for(i in 1:length(factor)) {
    factor_number<-0
    for(j in 1:length(factor[[i]])) {
      for(k in 1:nrow(sum_sales_df)) {
        if(factor[[i]][j]==sum_sales_df$factor_df.x[k]) {
          factor_number<-factor_number+sum_sales_df$percent[k]
        }
      }
    }
    factor_number_list[i]<-factor_number
  }
  factor_number_list
}

# Digitize these attributes: type, actor, director, area, publish
type_number_list<-list.digitization(movies$Type)
actor_number_list<-list.digitization(movies$Actor)
director_number_list<-list.digitization(movies$Director)
area_number_list<-list.digitization(movies$Area)
publish_number_list<-list.digitization(movies$Publish)

# Build digital data frame for movies
digital_movies<-data.frame(ID=c(1:nrow(digital_movies)),Rating=movies$Rating,Sales=movies$Sales,
                           Appraise=movies$Appraise,Type=unlist(type_number_list),
                           Actor=unlist(actor_number_list),Director=unlist(director_number_list),
                           Area=unlist(area_number_list),Publish=unlist(publish_number_list))

# Function for normalization
normalization<-function(number){
  number=(number-min(number))/(max(number)-min(number))
}

# Normalize the data
digital_movies$Rating<-normalization(digital_movies$Rating)
digital_movies$Appraise<-normalization(digital_movies$Appraise)
digital_movies$Type<-normalization(digital_movies$Type)
digital_movies$Actor<-normalization(digital_movies$Actor)
digital_movies$Director<-normalization(digital_movies$Director)
digital_movies$Area<-normalization(digital_movies$Area)
digital_movies$Publish<-normalization(digital_movies$Publish)

# Evaluate sales as four ranks: Excellent, Good, Normal, Bad
evaluate<-list()
for(i in 1:nrow(digital_movies)) {
  if(digital_movies$Sales[[i]][1]<5) {
    digital_movies$Sales[[i]][1]<-digital_movies$Sales[[i]][1]*10000
  }
  if(digital_movies$Sales[[i]][1]>=100000) {
    evaluate[i]<-"Excellent"
  } else if(digital_movies$Sales[[i]][1]>=10000) {
    evaluate[i]<-"Good"
  } else if(digital_movies$Sales[[i]][1]>=5000) {
    evaluate[i]<-"Normal"
  } else {
    evaluate[i]<-"Bad"
  }
}
evaluate<-data.frame(ID=c(1:nrow(digital_movies)),Evaluate=unlist(evaluate))

# Merge evaluate column into data frame
digital_movies<-merge(digital_movies,evaluate,by="ID")

# Save to local
write.csv(digital_movies,file="E:\\R\\RStudio-1.1.383\\workspace\\data\\digital_movies.csv",row.names=FALSE)
