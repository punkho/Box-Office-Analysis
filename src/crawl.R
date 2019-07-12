# Import packages 
library(rvest)
library(xml2)
library(stringr)

# Define the url to crawl to the website
# Build list of urls because of the multiple pages 
urls<-list()
urls[1]<-"https://www.douban.com/doulist/1295618/"
for (i in 2:36) {
  # Connect the string of web address
  urls[i]<-paste("https://www.douban.com/doulist/1295618/?start=",25*(i-1),"&sort=seq&sub_type=")
  urls[i]<-gsub(" ","",urls[i])
}

# Function of crawlling the data on the webpage 
frame.crawl<-function(webpage){
  # Use CSS selectors to crawl the ranking section 
  rank_data_html<-html_nodes(webpage,'.pos')
  # Convert ranking data into text 
  rank_data<-html_text(rank_data_html)
  # Data preprocessing: convert ranking to digital format 
  rank_data<-as.numeric(rank_data)
  
  # Use the CSS selector to crawl the title section 
  title_data_html<-html_nodes(webpage,'.title')
  # Convert title data into text 
  title_data<-html_text(title_data_html)
  # Data preprocessing: remove '\n'
  title_data<-gsub("\n","",title_data)
  # Data preprocessing: remove whitespace from head and tail
  title_data<-sub("^..............","",title_data)
  title_data<-sub("..........$","",title_data)
  
  # Use the CSS selector to crawl the rating section 
  rating_data_html<-html_nodes(webpage,'.rating')
  # Convert rating data into text 
  rating_data<-html_text(rating_data_html)
  
  # Data preprocessing: remove whitespace, '\n'
  rating_data<-gsub(" ","",rating_data)
  rating_data<-gsub("\n","",rating_data)
  
  # Extract the information about number of appraise from the comment
  appraise_data<-str_extract(rating_data,"\\(.*\\)")
  # Data preprocessing: intercept the appraise part
  appraise_data<-str_extract(appraise_data,"\\d+")
  # Data preprocessing: appraise sales to digital format 
  appraise_data<-as.numeric(appraise_data)
  
  # Data preprocessing: remove ()
  rating_data<-gsub("\\(.*\\)","",rating_data)
  # Data preprocessing: convert rating to digital format 
  rating_data<-as.numeric(rating_data)
  
  # Use the CSS selector to crawl the abstract section 
  abstract_data_html<-html_nodes(webpage,'.abstract')
  # Data preprocessing: remove whitespace and '\n'
  abstract_data<-gsub(" ","",abstract_data_html)
  abstract_data<-gsub("\n","",abstract_data)
  # Data preprocessing: remove relevant tags of head and tail
  abstract_data<-sub("<divclass=\"abstract\">","",abstract_data)
  abstract_data<-sub("</div>","",abstract_data)
  # Data preprocessing: divide by <br>
  abstract_data<-strsplit(abstract_data,"<br>")
  
  # Extract the relevant information from the abstract
  directors_data<-list()
  actors_data<-list()
  types_data<-list()
  areas_data<-list()
  for (i in 1:length(rank_data)) {
    # Extract the information about actor from the abstract
    directors_data[i]<-abstract_data[[i]][1]
    # Extract the information about actor from the abstract
    actors_data[i]<-abstract_data[[i]][2]
    # Extract the information about type from the abstract
    types_data[i]<-abstract_data[[i]][3]
    # Extract the information about area from the abstract
    areas_data[i]<-abstract_data[[i]][4]
  }
  # Data preprocessing: remove relevant tags of head
  directors_data<-gsub(".*:","",directors_data)
  actors_data<-gsub(".*:","",actors_data)
  types_data<-gsub(".*:","",types_data)
  areas_data<-gsub(".*:","",areas_data)
  
  # Use the CSS selector to crawl the comment section
  comment_data_html<-html_nodes(webpage,'.ft')
  # Convert comment data into text 
  comment_data<-html_text(comment_data_html)
  # Data preprocessing: remove whitespace, '\n' and ()
  comment_data<-gsub(" ","",comment_data)
  comment_data<-gsub("\n","",comment_data)
  #comment_data<-gsub("（.*）","",comment_data)
  #comment_data<-gsub("\\(.*\\)","",comment_data)
  
  # Data preprocessing: divide by |
  comment_data<-strsplit(comment_data,"\\|")
  # For the last page 
  if(length(comment_data)<6){
    comment_data[[5]]<-NA
  }
  
  # Extract the information from the comment
  sales_data<-list()
  date_data<-list()
  publish_data<-list()
  for (i in 1:length(rank_data)) {
    # Extract the information about sales from the comment
    sales_data[i]<-comment_data[[i]][1]
    # Extract the information about release date from the comment
    if(lapply(comment_data[i], length)==4) {
      date_data[i]<-comment_data[[i]][3]
    } else { 
      date_data[i]<-comment_data[[i]][2]
    }
    # Extract the information about publish type from the comment
    if(lapply(comment_data[i], length)==4) {
      publish_data[i]<-comment_data[[i]][4]
    } else { 
      publish_data[i]<-comment_data[[i]][3]
    }
  }
  
  # Data preprocessing: remove ()
  sales_data<-gsub("（.*）","",sales_data)
  sales_data<-gsub("\\(.*\\)","",sales_data)
  # Data preprocessing: intercept the digital part
  sales_data<-str_extract(sales_data,"\\d+")
  # Data preprocessing: convert sales to digital format 
  sales_data<-as.numeric(sales_data)
  
  # Data preprocessing: remove ()
  date_data<-gsub("（.*）","",date_data)
  date_data<-gsub("\\(.*\\)","",date_data)
  # Data preprocessing: intercept the date part
  date_data<-sub(".*：","",date_data)
  date_data<-sub(".*&","",date_data)
  # Data preprocessing: convert date to Date format 
  date_data<-as.Date(date_data,format="%Y年%m月%d日")
  
  # Data preprocessing: remove ()
  publish_data<-gsub("（.*）","",publish_data)
  publish_data<-gsub("\\(.*\\)","",publish_data)
  # Data preprocessing: intercept the publish type part
  publish_data<-gsub("\\d.*","",publish_data)
  publish_data<-gsub(".*：","",publish_data)
  
  # Build data frame
  movies_dataframe<-data.frame(Rank=rank_data,Title=title_data,
                               Rating=rating_data,Appraise=appraise_data,
                               Director=directors_data,Actor=actors_data,
                               Type=types_data,Area=areas_data,
                               Sales=sales_data,Date=date_data,
                               Publish=publish_data)
}

# Read the HTML code from the first website 
webpage<-read_html(urls[[1]][1])
# Build data frame
frame<-frame.crawl(webpage)
# Add other data frame
for (i in 2:36) {
  webpage<-read_html(urls[[i]][1])
  add_frame<-frame.crawl(webpage)
  frame<-merge(frame,add_frame,all=TRUE) 
}

# Save to local
write.csv(frame,file="E:\\R\\RStudio-1.1.383\\workspace\\data\\movies.csv",row.names=FALSE)