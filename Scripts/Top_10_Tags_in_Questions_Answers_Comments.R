#######################################################################################################################################
# The following script is answering:
#
#RQ7-1: Are there certain tags that tend to result in more answers or comments?  
#######################################################################################################################################

# STEP 1- LOADING DATA
# 1. Load workspace
## Replace the PATH and the NAME
load("/PATH/NAME.RData")


# STEP 2- DATA PREPROCESSING 

#-------- Begin of - Normalize tags in Questions ----------#
# required libraries 
install.packages("splitstackshape")
library (data.table)
library (stringr)
library (splitstackshape)

# 2. Find the end of each tag by locating ">" and 
# split the tags column by using cSplit fun to 5 distinct columns:
# tags_1, tags_2, tags_3, tags_4, tags_5
questions <- cSplit(questionsNA, "tags", '>')

# 3. Remove all '<' from the 5 columns 
questions$tags_1 <- str_replace_all(questions$tags_1,"<","")
questions$tags_2 <- str_replace_all(questions$tags_2,"<","")
questions$tags_3 <- str_replace_all(questions$tags_3,"<","")
questions$tags_4 <- str_replace_all(questions$tags_4,"<","")
questions$tags_5 <- str_replace_all(questions$tags_5,"<","")
#View(head(questions, n=100))

#------- End of - Normalizing Tags in Questions ------#




#------- Begin of - Counting the number of each distinct tag in questions------#
result1 <- as.data.frame(table(factor(questions$tags_1,levels=unique(tags$tagname))))
colnames(result1) <- c("name","Count")
View(head(result1, n=100))

result2 <- as.data.frame(table(factor(questions$tags_2,levels=unique(tags$tagname))))
colnames(result2) <- c("name","Count")
View(head(result2, n=100))

result3 <- as.data.frame(table(factor(questions$tags_3,levels=unique(tags$tagname))))
colnames(result3) <- c("name","Count")
View(head(result3, n=100))

result4 <- as.data.frame(table(factor(questions$tags_4,levels=unique(tags$tagname))))
colnames(result4) <- c("name","Count")
View(head(result4, n=100))

result5 <- as.data.frame(table(factor(questions$tags_5,levels=unique(tags$tagname))))
colnames(result5) <- c("name","Count")
View(head(result5, n=100))

#sum all the counts per tag
total <- data.frame(result1$Count+result2$Count+result3$Count+result4$Count+result5$Count) 

result <- data.frame(tags$tagname, total)
names(result) <- c("tags", "tcount")
# View(head(result, n=100))
# quesTags is a sorted datafram has all the tags along with their counts in questions
quesTags <- data.frame(result)

View(head(quesTags, n=100))

# Find what tag has the max # of counts
mmax <-max(quesTags$tcount)
MaxTagQues <- quesTags[quesTags$tcount == mmax, ] 
MaxTagQues # C# 

# we have to exclude all the tags with 0 count
mmin <- min (quesTags$tcount)
MinTagQues <- quesTags[quesTags$tcount == mmin, ]
View(head(MinTagQues, n=100))

#----- Sorting the tags in increasing order -----#
x <- data.frame (quesTags)
newdata <- x[order(x$tcount, decreasing = FALSE),]
View (head(newdata, n = 42715))

#------ End of - Counting each tag/question ----#
#
#
#
#
#
# The same to be done for answers and comments 
# For answers:
# -------- Begin of - Normalize tags in Answers ----------#
# required libraries 
library (data.table)
library (stringr)
library (splitstackshape)

# Find the end of each tag by locating ">" and 
# split the tags column by using cSplit fun to 5 distinct columns:
# tags_1, tags_2, tags_3, tags_4, tags_5
answers <- cSplit(answers, "tags", '>')
#View(head(answers, n=100))

# Remove all '<' from the 5 sub-columns 
answers$tags_1 <- str_replace_all(answers$tags_1,"<","")
answers$tags_2 <- str_replace_all(answers$tags_2,"<","")
answers$tags_3 <- str_replace_all(answers$tags_3,"<","")
answers$tags_4 <- str_replace_all(answers$tags_4,"<","")
answers$tags_5 <- str_replace_all(answers$tags_5,"<","")
#View(head(answers, n=100))
#------- End of - Normalizing Tags in Answes ------#




#------- Begin of - Counting the number of each distinct tag in answers------#
result1 <- as.data.frame(table(factor(answers$tags_1,levels=unique(tags$tagname))))
colnames(result1) <- c("name","Count")
# View(head(result1, n=100))

result2 <- as.data.frame(table(factor(answers$tags_2,levels=unique(tags$tagname))))
colnames(result2) <- c("name","Count")
# View(head(result2, n=100))

result3 <- as.data.frame(table(factor(answers$tags_3,levels=unique(tags$tagname))))
colnames(result3) <- c("name","Count")
# View(head(result3, n=100))

result4 <- as.data.frame(table(factor(answers$tags_4,levels=unique(tags$tagname))))
colnames(result4) <- c("name","Count")
# View(head(result4, n=100))

result5 <- as.data.frame(table(factor(answers$tags_5,levels=unique(tags$tagname))))
colnames(result5) <- c("name","Count")
# View(head(result5, n=100))

#sum all the counts per tag
total <- data.frame(result1$Count+result2$Count+result3$Count+result4$Count+result5$Count) 

result <- data.frame(tagsSO$tagname, total)
names(result) <- c("tags", "tcount")
# View(head(result, n=100))

# ansTags is a sorted datafram has all the tags along with their counts in answers
ansTags <- data.frame(result)

View(head(ansTags, n=100))

# Find what tag has the max # of counts
mmax <-max(ansTags$tcount)
MaxTagAns <- ansTags[ansTags$tcount == mmax, ]
MaxTagAns # C# again

# we have to remove tags with 0s count 
mmin <- min (ansTags$tcount)
MinTagAns <- ansTags[ansTags$tcount == mmin, ]
View(head(MinTagAns, n=100))

#----- Sorting the tags in increasing order -----#
x <- data.frame (ansTags)
newdata <- x[order(x$tcount, decreasing = FALSE),]
View (head(newdata, n = 42715))

#------ End of - Counting each tag/answers ----#
#
#
#
#
# For Comments:
#-------- Begin of Normalize tags in Comments ----------#

# required libraries 
library (data.table)
library (stringr)
library (splitstackshape)

# Find the end of each tag by locating ">" and 
# split the tags column by using cSplit fun to 5 distinct columns:
# tags_1, tags_2, tags_3, tags_4, tags_5
comments <- cSplit(comments, "tags", '>')
#View(head(comments, n=100))

# Remove all '<' from the 5 sub-columns 
comments$tags_1 <- str_replace_all(comments$tags_1,"<","")
comments$tags_2 <- str_replace_all(comments$tags_2,"<","")
comments$tags_3 <- str_replace_all(comments$tags_3,"<","")
comments$tags_4 <- str_replace_all(comments$tags_4,"<","")
comments$tags_5 <- str_replace_all(comments$tags_5,"<","")
#View(head(comments, n=100))
#------- End of Normalizing Tags in Comments ------#





#------- Begin of Counting the number of each distinct tag in answers------#
result1 <- as.data.frame(table(factor(comments$tags_1,levels=unique(tags$tagname))))
colnames(result1) <- c("name","Count")
# View(head(result1, n=100))

result2 <- as.data.frame(table(factor(comments$tags_2,levels=unique(tags$tagname))))
colnames(result2) <- c("name","Count")
# View(head(result2, n=100))

result3 <- as.data.frame(table(factor(comments$tags_3,levels=unique(tags$tagname))))
colnames(result3) <- c("name","Count")
# View(head(result3, n=100))

result4 <- as.data.frame(table(factor(comments$tags_4,levels=unique(tags$tagname))))
colnames(result4) <- c("name","Count")
# View(head(result4, n=100))

result5 <- as.data.frame(table(factor(comments$tags_5,levels=unique(tags$tagname))))
colnames(result5) <- c("name","Count")
# View(head(result5, n=100))

#sum all the counts per tag
total <- data.frame(result1$Count+result2$Count+result3$Count+result4$Count+result5$Count) 

result <- data.frame(tags$tagname, total)
names(result) <- c("tags", "tcount")
# View(head(result, n=100))

# comTags is a sorted datafram has all the tags along with their counts in comments
comTags <- data.frame(result)

View(head(comTags, n=100))

# Find what tag has the max # of counts
mmax <-max(comTags$tcount)
MaxTagCom <- comTags[comTags$tcount == mmax, ]
MaxTagCom # C# again!!

# we have to remove tags with 0s count
mmin <- min (comTags$tcount)
MinTagCom <- comTags[comTags$tcount == mmin, ]
View(head(MinTagCom, n=100))

#----- Sorting the tags in increasing order -----#
x <- data.frame (comTags)
newdata <- x[order(x$tcount, decreasing = FALSE),]
View (head(newdata, n = 42715))

#------ End of counting each tag/comments ----#


######################################################################################################


# STEP 3- BAR CHARTS FOR TOP 10 TAGS IN THE QUESTIONS, ANSWERS AND COMMENTS


#Barchart of Top 10 tags in questions# 
# Figure 11
quesTagsOrdered <- quesTags[order(-quesTags$tcount),] 
quesTop=head(quesTagsOrdered,10)
BP <- barplot(quesTop$tcount, main="Top 10 tags in Questions", beside=TRUE, names =  quesTop$tags, las=2, srt= 60)
text(BP, quesTop$tags, quesTop$tcount,cex=1,pos=4, col= "red",srt=90)


#Barchart of Top 10 tags in answers#
# Figure 12
ansTagsOrdered <- ansTags[order(-ansTags$tcount),] 
ansTop=head(ansTagsOrdered,10)
BP2 <- barplot(ansTop$tcount, main="Top 10 tags in Answers", beside=TRUE, names =  ansTop$tags, las=2, srt= 60)
text(BP2, ansTop$tags, ansTop$tcount,cex=1,pos=4, col= "red",srt=90)


#Barchart of Top 10 tags in comments#
# Figure 13
comTagsOrdered <- comTags[order(-comTags$tcount),] 
comTop=head(comTagsOrdered,10)
BP3 <- barplot(comTop$tcount, main="Top 10 tags in Comments", beside=TRUE, names =  comTop$tags, las=2, srt= 60)
text(BP3, comTop$tags, comTop$tcount,cex=1,pos=4, col= "red",srt=90)


######################################################################################################


