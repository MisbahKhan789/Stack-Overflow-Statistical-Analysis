#######################################################################################################################################
# The following script is answering:
#
#RQ7-2: Are there any that tend to attract users with particularly high or low reputation in answers 
#######################################################################################################################################

#STEP 1- LOADING DATA
#Replace PATH
tagsSO <- read.csv("/PATH/tags.csv")
answersRepH <- read.csv("/PATH/answersReputationH.csv")

#STEP 2- EDITING COLUMN HEADERS
names(answersRepH) <- c("user_id", "user_name", "user_reputation", "answer_id", "answer_count", "comment_count", "score", "view_count","tags")
View(head(answersRepH, n=100))

#-------- Normalize tags in Answers ----------#

# required libraries 
library (data.table)
library (stringr)
library (splitstackshape)

#2. Find the end of each tag by locating ">" and 
# split the tags column by using cSplit fun to 5 distinct columns:
# tags_1, tags_2, tags_3, tags_4, tags_5
answersRepH <- cSplit(answersRepH, "tags", '>')

#3. Remove all '<' from the 5 sub-columns 
answersRepH$tags_1 <- str_replace_all(answersRepH$tags_1,"<","")
answersRepH$tags_2 <- str_replace_all(answersRepH$tags_2,"<","")
answersRepH$tags_3 <- str_replace_all(answersRepH$tags_3,"<","")
answersRepH$tags_4 <- str_replace_all(answersRepH$tags_4,"<","")
answersRepH$tags_5 <- str_replace_all(answersRepH$tags_5,"<","")

#------- End of Normalizing Tags in answersRepH ------#

#get top 25% higher reputation#
ansTopHighRep=head(answersRepH,850164)
View(ansTopHighRep)

#get lower 25% user reputation#
ansLowRep <- answersRepH[order(answersRepH$user_reputation),] 
ansTopLowRep=head(ansLowRep,850164)
View(ansTopLowRep)


#------- Counting the number of each distinct tag in ansTopHighRep------#
result1 <- as.data.frame(table(factor(ansTopHighRep$tags_1,levels=unique(tagsSO$tagname))))
colnames(result1) <- c("name","Count")
View(head(result1, n=100))

result2 <- as.data.frame(table(factor(ansTopHighRep$tags_2,levels=unique(tagsSO$tagname))))
colnames(result2) <- c("name","Count")
View(head(result2, n=100))

result3 <- as.data.frame(table(factor(ansTopHighRep$tags_3,levels=unique(tagsSO$tagname))))
colnames(result3) <- c("name","Count")
View(head(result3, n=100))

result4 <- as.data.frame(table(factor(ansTopHighRep$tags_4,levels=unique(tagsSO$tagname))))
colnames(result4) <- c("name","Count")
View(head(result4, n=100))

result5 <- as.data.frame(table(factor(ansTopHighRep$tags_5,levels=unique(tagsSO$tagname))))
colnames(result5) <- c("name","Count")
View(head(result5, n=100))

#sum all the counts per tag
total <- data.frame(result1$Count+result2$Count+result3$Count+result4$Count+result5$Count) 

result <- data.frame(tagsSO$tagname, total)
names(result) <- c("tags", "tcount")
# ansTopHighRepTags is a sorted datafram has all the tags along with their counts in ansTopHighRep
ansTopHighRepTags <- data.frame(result)
View(ansTopHighRepTags)
#order tags in decending order based on their count#
ansTopHighRepTags <- ansTopHighRepTags[order(-ansTopHighRepTags$tcount),] 
#Select Top 10 tags used by high user reputation#
ansTopHighRepTags=head(ansTopHighRepTags,10)
#plot barplot for top 10 tags used by users with high reputation#
barplot(ansTopHighRepTags$tcount, main="Top 10  tags in answers used by users with high reputation")
axis(1,at=mids,labels=ansTopHighRepTags$tags)
View(ansTopHighRepTags)

#------ End Top 10 tags in answer used by users with high reputation  ----#



#------- Counting the number of each distinct tag in ansTopLowRep------#
result1 <- as.data.frame(table(factor(ansTopLowRep$tags_1,levels=unique(tagsSO$tagname))))
colnames(result1) <- c("name","Count")
View(head(result1, n=100))

result2 <- as.data.frame(table(factor(ansTopLowRep$tags_2,levels=unique(tagsSO$tagname))))
colnames(result2) <- c("name","Count")
View(head(result2, n=100))

result3 <- as.data.frame(table(factor(ansTopLowRep$tags_3,levels=unique(tagsSO$tagname))))
colnames(result3) <- c("name","Count")
View(head(result3, n=100))

result4 <- as.data.frame(table(factor(ansTopLowRep$tags_4,levels=unique(tagsSO$tagname))))
colnames(result4) <- c("name","Count")
View(head(result4, n=100))

result5 <- as.data.frame(table(factor(ansTopLowRep$tags_5,levels=unique(tagsSO$tagname))))
colnames(result5) <- c("name","Count")
View(head(result5, n=100))

#sum all the counts per tag
total <- data.frame(result1$Count+result2$Count+result3$Count+result4$Count+result5$Count) 

result <- data.frame(tagsSO$tagname, total)
names(result) <- c("tags", "tcount")
# ansTopLowRepTags is a sorted datafram has all the tags along with their counts in ansTopLowRep
ansTopLowRepTags <- data.frame(result)
View(ansTopLowRepTags)
#order tags in decending order based on their count#
ansTopLowRepTags <- ansTopLowRepTags[order(-ansTopLowRepTags$tcount),] 
#Select Top 10 tags used by lower user reputation#
ansTopLowRepTags=head(ansTopLowRepTags,10)
#plot barplot for top 10 tags used by users with lower reputation#
barplot(ansTopLowRepTags$tcount, main="Top 10  tags in answers used by users with low reputation")
axis(1,at=mids,labels=ansTopLowRepTags$tags)
View(ansTopLowRepTags)

#------ End Top 10 tags in answers used by users with low reputation  ----#

