##########################
####VIDEO GAME SALES######
######DATA MINING#########
##########################

##TO IMPORT THE DATA (CSV File)##
df <- read.csv('https://raw.githubusercontent.com/Sammontalvo/Video-Game-Sales/master/Vgsales.csv?token=ARCXT7AYV3VV26KS6UMYBXS7SWTNI')

##PRELIMINARY EXPLORATROY ANALYSIS##
View(df) #view data in spreadsheet format
dim(df) #checks the dimensions of the data frame
head(df) #display first six rows of data
tail(df) #display last six rows of data

class(df) #checks the object class of data set
class(df$Name) #checks the object class of variable name
class(df$Global_Sales) #checks the object class of variable global sales

summary(df[,7:11]) #generate summary statistics for column 7 though 11

#computes standard deviation of quantitative variables
sd(df$NA_Sales) 
sd(df$EU_Sales)
sd(df$JP_Sales)
sd(df$Other_Sales)
sd(df$Global_Sales)

##DATA CLEANING & TRANFORMATION##
df$Rank<-NULL #deletes variable column rank
View(df) #view the new data frame

#average global sales by platform and storing result in a new data frame 
Global_SalesAVGbyPlatform<-aggregate(Global_Sales~Platform,df,mean)
head(Global_SalesAVGbyPlatform) #view the new structure of the data set

#average NA sales by platform and storing result in new data frame
NA_SalesAVGbyPlatform<-aggregate(NA_Sales~Platform,df,mean)
colnames(NA_SalesAVGbyPlatform)<-c('Platform_2', 'NA_Sales')
head(NA_SalesAVGbyPlatform) #view the new structure of the data set

#average EU sales by platform and storing result in new data frame
EU_SalesAVGbyPlatform<-aggregate(EU_Sales~Platform,df,mean)
colnames(EU_SalesAVGbyPlatform)<-c('Platform_3', 'EU_Sales')
head(EU_SalesAVGbyPlatform) #view the new structure of the data set

#average JP sales by platform and storing result in new data frame
JP_SalesAVGbyPlatform<-aggregate(JP_Sales~Platform,df,mean)
colnames(JP_SalesAVGbyPlatform)<-c('Platform_4', 'JP_Sales')
head(JP_SalesAVGbyPlatform) #view the new structure of the data set

#average other sales by platform and storing result in new data frame
Other_SalesAVGbyPlatform<-aggregate(Other_Sales~Platform,df,mean)
colnames(Other_SalesAVGbyPlatform)<-c('Platform_5', 'Other_Sales')
head(Other_SalesAVGbyPlatform) #view the new structure of the data set

#combines the separate data frames of sales by platform into one TIDY data set
CS_Platform<-cbind(Global_SalesAVGbyPlatform, NA_SalesAVGbyPlatform,
                  EU_SalesAVGbyPlatform, JP_SalesAVGbyPlatform, Other_SalesAVGbyPlatform)

#deletes the extra platform columns
CS_Platform$Platform_2<-NULL
CS_Platform$Platform_3<-NULL
CS_Platform$Platform_4<-NULL
CS_Platform$Platform_5<-NULL
View(CS_Platform)

write.csv(CS_Platform,'TidyCS_Platform.csv') #stores CSV file to desktop

#average global sales by Year and storing result in a new data frame 
Global_SalesAVGbyYear<-aggregate(Global_Sales~Year,df,mean)
head(Global_SalesAVGbyYear) #view the new structure of the data set

#average NA sales by year and storing result in new data frame
NA_SalesAVGbyYear<-aggregate(NA_Sales~Year,df,mean)
colnames(NA_SalesAVGbyYear)<-c('Year_2', 'NA_Sales')
head(NA_SalesAVGbyPlatform) #view the new structure of the data set

#average EU sales by year and storing result in new data frame
EU_SalesAVGbyYear<-aggregate(EU_Sales~Year,df,mean)
colnames(EU_SalesAVGbyYear)<-c('Year_3', 'EU_Sales')
head(EU_SalesAVGbyYear) #view the new structure of the data set

#average JP sales by year and storing result in new data frame
JP_SalesAVGbyYear<-aggregate(JP_Sales~Year,df,mean)
colnames(JP_SalesAVGbyYear)<-c('Year_4', 'JP_Sales')
head(JP_SalesAVGbyYear) #view the new structure of the data set

#average other sales by year and storing result in new data frame
Other_SalesAVGbyYear<-aggregate(Other_Sales~Year,df,mean)
colnames(Other_SalesAVGbyYear)<-c('Year_5', 'Other_Sales')
head(Other_SalesAVGbyYear) #view the new structure of the data set

#combines the separate data frames of sales by year into one TIDY data set
TS_Year<-cbind(Global_SalesAVGbyYear, NA_SalesAVGbyYear,
          EU_SalesAVGbyYear, JP_SalesAVGbyYear, Other_SalesAVGbyYear)

#deletes the extra year columns
TS_Year$Year_2<-NULL
TS_Year$Year_3<-NULL
TS_Year$Year_4<-NULL
TS_Year$Year_5<-NULL
View(TS_Year)

write.csv(TS_Year,'TidyTS_Year.csv') #stores CSV file to desktop

#average global sales by genre and storing result in a new data frame 
Global_SalesAVGbyGenre<-aggregate(Global_Sales~Genre,df,mean)
head(Global_SalesAVGbyGenre) #view the new structure of the data set

#average NA sales by genre and storing result in new data frame
NA_SalesAVGbyGenre<-aggregate(NA_Sales~Genre,df,mean)
colnames(NA_SalesAVGbyGenre)<-c('Genre_2', 'NA_Sales')
head(NA_SalesAVGbyGenre) #view the new structure of the data set

#average EU sales by genre and storing result in new data frame
EU_SalesAVGbyGenre<-aggregate(EU_Sales~Genre,df,mean)
colnames(EU_SalesAVGbyGenre)<-c('Genre_3', 'EU_Sales')
head(EU_SalesAVGbyGenre) #view the new structure of the data set

#average JP sales by genre and storing result in new data frame
JP_SalesAVGbyGenre<-aggregate(JP_Sales~Genre,df,mean)
colnames(JP_SalesAVGbyGenre)<-c('Genre_4', 'JP_Sales')
head(JP_SalesAVGbyGenre) #view the new structure of the data set

#average other sales by genre and storing result in new data frame
Other_SalesAVGbyGenre<-aggregate(Other_Sales~Genre,df,mean)
colnames(Other_SalesAVGbyGenre)<-c('Genre_5', 'Other_Sales')
head(Other_SalesAVGbyGenre) #view the new structure of the data set

#combines the separate data frames of sales by genre into one TIDY dataset
CS_Genre<-cbind(Global_SalesAVGbyGenre, NA_SalesAVGbyGenre,
               EU_SalesAVGbyGenre, JP_SalesAVGbyGenre, Other_SalesAVGbyGenre)

#deletes the extra genre columns
CS_Genre$Genre_2<-NULL
CS_Genre$Genre_3<-NULL
CS_Genre$Genre_4<-NULL
CS_Genre$Genre_4<-NULL
CS_Genre$Genre_5<-NULL
View(CS_Genre)

write.csv(CS_Genre,'TidyCS_Genre.csv') #stores CSV file to desktop

#average global sales by publisher and storing result in a new data frame 
Global_SalesAVGbyPublisher<-aggregate(Global_Sales~Publisher,df,mean)
head(Global_SalesAVGbyPublisher) #view the new structure of the data set

#average NA sales by Publisher and storing result in new data frame
NA_SalesAVGbyPublisher<-aggregate(NA_Sales~Publisher,df,mean)
colnames(NA_SalesAVGbyPublisher)<-c('Publisher_2', 'NA_Sales')
head(NA_SalesAVGbyPublisher) #view the new structure of the data set

#average EU sales by publisher and storing result in new data frame
EU_SalesAVGbyPublisher<-aggregate(EU_Sales~Publisher,df,mean)
colnames(EU_SalesAVGbyPublisher)<-c('Publisher_3', 'EU_Sales')
head(EU_SalesAVGbyPublisher) #view the new structure of the data set

#average JP sales by publisher and storing result in new data frame
JP_SalesAVGbyPublisher<-aggregate(JP_Sales~Publisher,df,mean)
colnames(JP_SalesAVGbyPublisher)<-c('Publisher_4', 'JP_Sales')
head(JP_SalesAVGbyPublisher) #view the new structure of the data set

#average other sales by publisher and storing result in new data frame
Other_SalesAVGbyPublisher<-aggregate(Other_Sales~Publisher,df,mean)
colnames(Other_SalesAVGbyPublisher)<-c('Publisher_5', 'Other_Sales')
head(Other_SalesAVGbyPublisher) #view the new structure of the data set

#combines the separate data frames of sales by publisher into one TIDY data set
CS_Publisher<-cbind(Global_SalesAVGbyPublisher, NA_SalesAVGbyPublisher,
                EU_SalesAVGbyPublisher, JP_SalesAVGbyPublisher, Other_SalesAVGbyPublisher)

#deletes the extra publisher columns
CS_Publisher$Publisher_2<-NULL
CS_Publisher$Publisher_3<-NULL
CS_Publisher$Publisher_4<-NULL
CS_Publisher$Publisher_5<-NULL
View(CS_Publisher)

write.csv(CS_Publisher,'TidyCS_Publisher.csv') #stores CSV file to desktop

#new TIDY data set for names of games
CS_Name<-df
CS_Name$Platform<-NULL
CS_Name$Genre<-NULL
CS_Name$Year<-NULL
CS_Name$Publisher<-NULL
View(CS_Name)

write.csv(CS_Name,'TidyCS_Name.csv') #stores CSV file to desktop

##VISUAL ANALYSIS USING GGPLOT2##
library(ggplot2)
library(ggplot2)
library(plyr) 

##ANALYSIS OF SALES BY YEAR##
#creates a scatter plot showing relationship between avg NA_Sales and Global_Sales
plot_A <- ggplot(TS_Year, aes(NA_Sales,Global_Sales-NA_Sales, colour = factor(Year))) +
  geom_point()
print(plot_A)

plot_B<-plot_A +theme(legend.position="bottom")
print(plot_B)

plot_C<-plot_B + 
  xlab('Average North America Sales (Mil.)') +  #x-axis label
  ylab('Average Global Sales(Mil.)') #y-axis label
plot_C

plot_D<-plot_C + 
  ggtitle('Comparison of Sales') #adds title
plot_D

plot_E<-plot_D +
  ggtitle('Comparison of Sales') + #adds title
  theme(plot.title = element_text(hjust = .5))
plot_E

plot_Line<-ggplot(TS_Year, aes(x = NA_Sales, y = Global_Sales)) + 
  geom_point() +
  geom_smooth(method ='lm')

plot_Line1<-plot_Line+ 
  xlab('Average Japan Sales (Mil.)') +  #x-axis label
  ylab('Average Global Sales (Mil.)') #y-axis label
plot_Line1 

#creates a scatter plot relationship between avg JP sales and Global Sales in regard to year
plot_F <- ggplot(TS_Year, aes(JP_Sales,Global_Sales-JP_Sales, colour = factor(Year))) +
  geom_point()
print(plot_F)

plot_G<-plot_F +theme(legend.position="bottom")
print(plot_G)

plot_H<-plot_G + 
  xlab('Average Japan Sales (Mil.)') +  #x-axis label
  ylab('Average Global Sales (Mil.)') #y-axis label
plot_H

plot_I<-plot_H +
  ggtitle('Comparison of Sales') #adds title
plot_I

plot_J<-plot_I +
  ggtitle('Comparison of Sales') + #adds title
  theme(plot.title = element_text(hjust = .5))
plot_J

plot_Line2<-ggplot(TS_Year, aes(x = JP_Sales, y = Global_Sales)) + 
  geom_point() +
  geom_smooth(method ='lm') #creates a new plot with line of best fit

plot_Line3<-plot_Line2+ 
  xlab('Average Japan Sales (Mil.)') +  #x-axis label
  ylab('Average Global Sales (Mil.)') #y-axis label
plot_Line3

##ANALYSIS OF SALES BY PLATFORM##
#creates a cluttered text using the data of different platforms
plot_text<-ggplot(CS_Platform, aes(NA_Sales, Global_Sales)) +
  geom_text(aes(label = Platform)) +
  xlim(0,1.5) + ylim(0,3)
plot_text

#removes the overlapping of platforms
plot_text2<-ggplot(CS_Platform, aes(NA_Sales, Global_Sales)) +
  geom_text(aes(label = Platform), check_overlap = TRUE) +
  xlim(0,1.5) + ylim(0,3)
plot_text2

plot_text3 <- plot_text2 + 
  xlab('Average North America Sales (Mil.)') +  #x-axis label
  ylab('Average Global Sales (Mil.)') #y-axis label
plot_text3

CS_Platform[6,1:3] #checks the gameboy sales
CS_Platform[12,1:3] #checks the nes sales


##ANALYSIS OF BY GENRE##
#creates bar chart of global sales dependent on genre
Bar1<-ggplot(data=CS_Genre, aes(x=Genre, y=Global_Sales)) +
  geom_bar(stat="identity") 
Bar1

#adds red to the bars and adjusts width to be smaller
Bar2<-ggplot(data=CS_Genre, aes(x=Genre, y=Global_Sales)) +
  geom_bar(stat="identity", color="Red", fill="Red", width=.5)
Bar2

Bar3<-Bar2 + coord_flip() #flips the bar graph horizontally
Bar3

Bar4 <- Bar3 + 
  xlab('Genre') +  #x-axis label
  ylab('Average Global Sales (Mil.)') #y-axis label
Bar4

#ANALYSIS OF SALES BY GENRE##
#creates piechart of north america sales
pie1<-ggplot(CS_Genre, aes(x="", y=NA_Sales, fill=Genre)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)
pie1

pie_text<- pie1 + 
  ylab('North America Sales') #fixes label
pie_text

#creates piechart for japan sales
pie2<-ggplot(CS_Genre, aes(x="", y=JP_Sales, fill=Genre)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)

pie_text2<- pie2 + 
  ylab('Japan Sales') #fixes label
pie_text2

#creates text for europe sales
pie3<-ggplot(CS_Genre, aes(x="", y=EU_Sales, fill=Genre)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)

pie_text3<- pie3 + 
  ylab('Europe Sales') #fixes label
pie_text3

##TEST FOR NORMALITY IN TS YEAR##
library(tseries) 
install.packages('tseries')

#creates a histogram for global sales in the time series set of year
hist(CS_Genre$Global_Sales, prob = TRUE)

#adds normal density curve to the histogram
curve(dnorm(x, mean = mean(CS_Genre$Global_Sales), sd = sd(CS_Genre$Global_Sales)), col = "darkblue", lwd = 2, add = TRUE)

#conducts a hypothesis test for normality
jarque.bera.test(CS_Genre$Global_Sales) 





