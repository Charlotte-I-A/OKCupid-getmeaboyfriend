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
