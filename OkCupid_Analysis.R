library(okcupiddata)
data("profiles")

#Initial exploration
str(profiles)
View(profiles)


# Splitting out swear words -----------------------------------------------

#Start to build a dataframe of "Profiles" with columns for each swear word
Cleaned_profiles <- data.frame(profiles) #created a dataframe

length(grep("fuck",Cleaned_profiles$essay0)) #count of word 'fuck' in entire 'essay' column (146)

#Here are the swear words we are identifying
mystring1<-"fuck" 
mystring2 <- "shit"
mystring3 <- "cunt"
mystring4 <- "bitch"

#Function to count each swear word
countOccurr = function(text,motif) {
  res = gregexpr(motif,text,fixed=T)[[1]]
  ifelse(res[1] == -1, 0, length(res))
}

#Creating the extra columns for each swear word
Cleaned_profiles = cbind(Cleaned_profiles, count_of_Fuck = vapply(Cleaned_profiles$essay0, countOccurr, 1, motif=mystring1))
Cleaned_profiles = cbind(Cleaned_profiles, count_of_Shit = vapply(Cleaned_profiles$essay0, countOccurr, 1, motif=mystring2))
Cleaned_profiles = cbind(Cleaned_profiles, count_of_Cunt = vapply(Cleaned_profiles$essay0, countOccurr, 1, motif=mystring3))
Cleaned_profiles = cbind(Cleaned_profiles, count_of_Bitch = vapply(Cleaned_profiles$essay0, countOccurr, 1, motif=mystring4))


unique(Cleaned_profiles$count_of_F) #This shows us all the categorical answers available
unique(Cleaned_profiles$count_of_Shit)
unique(Cleaned_profiles$count_of_Cunt)
unique(Cleaned_profiles$count_of_Bitch)

Cleaned_profiles$Total_swear_words <- Cleaned_profiles$count_of_F + Cleaned_profiles$count_of_Shit + Cleaned_profiles$count_of_Cunt + Cleaned_profiles$count_of_Bitch

unique(Cleaned_profiles$Total_swear_words)

View(Cleaned_profiles)


# Plotting ----------------------------------------------------------------
sum(Cleaned_profiles$Total_swear_words,na.rm = TRUE) 
#There are 371 instances of the use of the words "fuck", "shit", "cunt" and/or "bitch" in the dataset
#We only want to analyse those profiles which have swear words in them. Therefore we look at a subset of the data

#To only look at rows where swear words is >0:
Contains_swearing<-subset(Cleaned_profiles, Cleaned_profiles$Total_swear_words>0)
str(Contains_swearing) #We can see 350 profiles which have swear words in them (some have multiple)


#From here, we can look at each variable to see which variables have a pattern with swear words
plot(Contains_swearing$Total_swear_words ~ Contains_swearing$age)
plot(Contains_swearing$Total_swear_words ~ Contains_swearing$height)
#For the categorical variables these are currently stored as 'chr' variables, therefore need to push into 'as.factor' variables
plot(Total_swear_words~(as.factor(body_type)),data = Contains_swearing)
plot(Total_swear_words~(as.factor(diet)),data = Contains_swearing)
. . . #to be continued. 




#Code from Paula
# Clustering --------------------------------------------------------------
str(Cleaned_profiles)
library(caret)


s = Cleaned_profiles[1:10000,]
#s <- mutate(s, Total_swear_words = ifelse(Total_swear_words>0, 1, 0))
x = s[,-c(17:19)]
#unique(s$Total_swear_words)
y = s[,19]
str(y)
x.dist1=dist(x,method="euclidean")
x.dist2=dist(x,method="manhattan")
x.dist3=dist(x,method="minkowski")

# Distances
hc.single1=hclust(x.dist1,method="single",members=NULL)
hc.single2=hclust(x.dist2,method="single",members=NULL)
hc.single3=hclust(x.dist3,method="single",members=NULL)
hc.single4=hclust(x.dist1,method="ward",members=NULL)

plot(hc.single1)
plot(hc.single2)
plot(hc.single3)
plot(hc.single4)


hicluste1=cutree(hc.single1,k=2)
hicluste2=cutree(hc.single2,k=2)
hicluste3=cutree(hc.single3,k=2)
hicluste4=cutree(hc.single4,k=2)
#
plot(s, col=hicluste1,main="Single Linkage - Euclidean distance")
plot(s, col=hicluste2,main="Single Linkage - Manhattan distance")
plot(s, col=hicluste3,main="Single Linkage - Minkowski distance")
plot(s, col=hicluste4,main="Ward's minimum variance - Euclidean distance")

table(hicluste1,y)
table(hicluste2,y)
table(hicluste3,y)
table(hicluste4,y)

k.s <- kmeans(x,centers=2,nstart = 10)
plot(x,col=k.s$cluster)

table(y,k.s$cluster)
