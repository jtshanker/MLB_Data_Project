rm(list = ls())
# Group Project 
#Jason Shanker 

# need to add libary of functions used 
library(dplyr)
library(XML)
library(RCurl)

# 1.Webscraped Spotrac Data
player<-character(0) # Initialize empty vectors to store scraped data
age<-character(0)
position<-character(0)
payroll_salary<-character(0)
teams<-c("boston-red-sox","houston-astros","new-york-yankees","chicago-cubs","los-angeles-dodgers","new-york-mets", "philadelphia-phillies", "st.-louis-cardinals", "colorado-rockies",  "los-angeles-angels", "san-diego-padres", "san-francisco-giants", "atlanta-braves", "cincinnati-reds",  "texas-rangers", "washington-nationals", "oakland-athletics",  "seattle-mariners", "arizona-diamondbacks", "cleveland-indians", "minnesota-twins", "milwaukee-brewers", "detroit-tigers", "chicago-white-sox", "kansas-city-royals", "pittsburgh-pirates", "baltimore-orioles", "tampa-bay-rays", "miami-marlins")
for (i in teams){
  print(i)
  Sys.sleep(10)
  full_url = paste("https://www.spotrac.com/mlb/",i,"/payroll/2019/")
  url<-getURL(full_url)
  page <-htmlParse(url)
  players <-xpathSApply(page,"//table[@class='datatable'][1]/tbody/tr/td//a",xmlValue)
  player<-c(player, players) # Append to vector
  ages <- xpathSApply(page,"//table[@class='datatable'][1]/tbody/tr/td[2]",xmlValue)
  age<-c(age, ages) # Append to vector
  positions<-xpathSApply(page,"//table[@class='datatable'][1]/tbody/tr/td[3]",xmlValue)
  position<-c(position,positions)
  payroll_salarys<-xpathSApply(page,"//table[@class='datatable'][1]/tbody/tr/td[8]",xmlValue)
  payroll_salary<-c(payroll_salary,payroll_salarys)
}
#We now have all MLB players salarys scraped into R studio from Spotrac Website 
# We now create data frame for this scraped data called MLB_df which hold all players salarys for hitters and pitchers
MLB_df<-data.frame(Players=player,Age=age,Position=position,Salary=payroll_salary,stringsAsFactors = FALSE)
MLB_df<-unique(MLB_df)   #gets rid of duplicates from dataframe created

write.csv(MLB_df,"MLB_Spotrac_Data.csv")

# Data Cleaning 
# have to create hitters data frame from MLB_df to seperate pitchers from hitters.
# This dataframe is called MLB_Spotrac_df_hitting and holds all player salarys other than pitchers
MLB_Spotrac_df_hitting<-subset(MLB_df,Position=="1B" | Position=="2B"| Position=="3B"| Position=="C"| Position=="CF"| Position=="DH"| Position=="INF"| Position=="LF" | Position=="OF"| Position=="RF"| Position=="SS")
# We created a dataframe with players that are hitters by selecting all the players that play a certain position except pitchers becuse pichers don't hit and we just want hitters.


# changed salary for hitting dataframe to integer
Salary<-gsub(",","",MLB_Spotrac_df_hitting$Salary)
Salary<-gsub("\\$","",Salary)
MLB_Spotrac_df_hitting$Salary<-as.integer(Salary)

# calculate Average Salary for hitters salary
mean(MLB_Spotrac_df_hitting$Salary)

# The average MLB Player Salary for Hitters is 3638907
# We will create a threshold from this average by saying players who salary is above this average is considered High Salary and players who salary is below this average is low salary.


# We will create a coulmn called Salary Category with threshold deciding category. Two categories High Salary and Low Salary
MLB_Spotrac_df_hitting$Salary_Classification<-ifelse(MLB_Spotrac_df_hitting$Salary >mean(MLB_Spotrac_df_hitting$Salary),"High Salary","Low Salary")
# We then created a binary classification for salary where 0= low salary and 1=High Salary
MLB_Spotrac_df_hitting$Salary_Classification2<-ifelse(MLB_Spotrac_df_hitting$Salary >mean(MLB_Spotrac_df_hitting$Salary),1,0)


# Reading in second data set into dataframe
# We now read in our MLB.com hitting data with performace statics for hitters. 
MLB.com_hitting_df<-read.csv("MLB.com_Data_Hitting.csv",stringsAsFactors = FALSE)

# We want to combined the MLB_Spotrac_df_hitting and MLB.com_hitting_df together to get player statics and salary together.
# We can see that the players name column is the best way to merge these two dataframes together.
# However in the MLB.com data players first name is not spelled out fully like it is in spotrac so we deced the best way to merge data is by players lastname.

# Data Cleaning
# After reading in the MLB.com_hitting_df we can see that the best way to merge the MLB_Spotrac_df_hitting and MLB.com_hitting_df is by lastname
# So We  need to split players name by creating a First Name coulmnn and a Last Name Coulmn Becuse MLB.com hitting Players first name isnt spelled out fully so to make to dataframes merge easily we will split so we can merge by last name later.
firstname<-strsplit(MLB_Spotrac_df_hitting$Players," ")
MLB_Spotrac_df_hitting$Firstname_Player<-sapply(firstname,"[[",1 )
# We split players first name into a First Name Coulmn

lastname<-strsplit(MLB_Spotrac_df_hitting$Players," ")
MLB_Spotrac_df_hitting$Lastname_Player<-sapply(lastname,"[[",2 )
# we have just split the last name of players into a Last name coulmn

# we can now merge the two dataframes MLB_Spotrac_df_hitting and MLB.com_hitting_df by Last names coulmn. But might need to change coulmn name so it matches what that coulmn is called in the other dataframe. 

# Before we merge data by last name we realize there is an extra space in the MLB.com_hitting_df lastname values that isn't in the MLB_Spotrac_hitting_df
# So we get rid of this space below by doing the following
MLB.com_hitting_df$Lastname_Player<-gsub(" ","",MLB.com_hitting_df$Lastname_Player)

# we also found before merging that these players last names were not spelled the exact way in the Spotrac and MLB.com hitting dataframes so we changed spotrac player names so that they would match MLB.com hitting so they can merge correctly.
MLB_Spotrac_df_hitting$Lastname_Player[which(MLB_Spotrac_df_hitting$Lastname_Player=="Bradley")]<-"BradleyJr."
MLB_Spotrac_df_hitting$Lastname_Player[which(MLB_Spotrac_df_hitting$Lastname_Player=="Guerrero")]<-"GuerreroJr."
MLB_Spotrac_df_hitting$Lastname_Player[which(MLB_Spotrac_df_hitting$Lastname_Player=="Acuna")]<-"AcunaJr."


first_initials<-substring(MLB_Spotrac_df_hitting$Firstname_Player, 1,1)
first_initials
MLB_Spotrac_df_hitting$FirstInitial<-first_initials
View(MLB_Spotrac_df_hitting)
View(MLB.com_hitting_df)

write.csv(MLB_Spotrac_df_hitting,"MLB_Spotrac_hitting_Data.csv")


MLB.com_hitting_df$Firstname_Player<-gsub(" ","",MLB.com_hitting_df$Firstname_Player)
names(MLB.com_hitting_df)[which(names(MLB.com_hitting_df)=='Firstname_Player')]<-"FirstInitial"


# We then Merge our two dataframes MLB.com_hitting_df and MLB_Spotrac_df_hitting into a dataframe called MLB_hitting_df that stores all hitting players statics and their salary. 
MLB_hitting_df<-merge(MLB.com_hitting_df,MLB_Spotrac_df_hitting,by.x=c("Lastname_Player","FirstInitial"),by.y =c("Lastname_Player","FirstInitial"), all.x = TRUE )

# Two players salarys from spotrac where not included in our data meaning they didn't have a salary so they showed up NA.
MLB_hitting_df$Salary_Classification[is.na(MLB_hitting_df$Salary_Classification)]<-"Low Salary"
MLB_hitting_df$Salary[is.na(MLB_hitting_df$Salary)]<-0
# I replace slary classification for these two players with low salary becuse I just looked up there salry up online and they fell in this classification.
# I did this so I can generte graphs without any NAs generated.
MLB_hitting_df$G<-NULL
MLB_hitting_df$Position<-NULL
MLB_hitting_df$AB<-NULL
MLB_hitting_df$X2B<-NULL
MLB_hitting_df$X3B<-NULL
MLB_hitting_df$SB<-NULL
MLB_hitting_df$CS<-NULL
MLB_hitting_df$BB<-NULL
MLB_hitting_df$SO<-NULL
# One issue with this new dataframe when we merge the two datasets where that there are some players that have the same last name so that will cause duplicates where it is matching those values.
# So then merged by firstinital and players last name but we still have some duplicates becuse some players have same lastname and first Letter initial.
# Example is  Miguel Cabera and Melky Cabera there values create some duplicates.
# We felt this was one of our biggest problems when integrading that data.
# However, we decided to leave it as is with a few duplicates becuse it wont have a huge impact on our analysis.


#Data Analysis for Hitters
# we will now create a threshold for batting avg like we did for salary by getting the average of batting average.
# We will then create a binary coulmn where players batting averages above average is 1=above average hitter, and below average is 0=below average hitter.
# we will use average from MLB.com data since when we merge we have duplicates so to get more accuarte will use this. 
mean(MLB.com_hitting_df$AVG)
# The average batting average from our data is 0.2725926
# We decided to use the MLB.com Average since this doesn't have any duplicates to get a more accurate Avg Batting average.
MLB_hitting_df$AVG_Classification<-ifelse(MLB_hitting_df$AVG> mean(MLB.com_hitting_df$AVG),1,0)
MLB_hitting_df$AVG_Classification2<-ifelse(MLB_hitting_df$AVG> mean(MLB.com_hitting_df$AVG),'High Average', 'Low Average')

write.csv(MLB_hitting_df,"MLB_hitting_Final_Data.csv")

table2<-table(MLB_hitting_df$Salary_Classification,MLB_hitting_df$AVG_Classification2)
table2
# High salary plaers had 41 people with a high batting averge and low salary players had 35 people with high batting average.

prop.table(table2)
# Chi-Square Test to see if here a relationship between a salary level pay category and wheather they have a higher AVG?
chisq<-chisq.test(table2)
chisq

chisq$p.value
chisq1<-chisq$p.value
# we use p=0.05 as threshold for significance
#The P-value of the Chi_square test is 0.6551, which indicates that we cannot reject the null hypothesis that salary classification and batting average performance are independent.
#Can't say if there is a relationship between salary class and Batting AVG. 


# This will show Total number of high salrys and low salrys along with what the average batting average is for high salarys and low salrays.


                          
#Descriptive Statistics         

df5<-group_by(MLB_hitting_df,Salary_Classification)
salary_Hitting_summary3<-summarize(df5,
                          Total_Respondents=n(),
                          Mean_AVG=mean(AVG,na.rm = TRUE),
                          Mean_H=mean(H,na.rm=TRUE),
                          Mean_HR=mean(HR,na.rm=TRUE),
                          Mean_OPS=mean(OPS,na.rm = TRUE))

salary_Hitting_summary3      
barplot_height2<-salary_Hitting_summary3$Mean_AVG

names(barplot_height2)<-salary_Hitting_summary3$Salary_Classification
barplot(barplot_height2,xlab="Salary Classification",ylab="AVG Batting AVG",col = "Blue")
#This shows that hitting average and salary don't have a huge difference
plot(MLB_hitting_df$Salary,MLB_hitting_df$AVG)
plot(MLB_hitting_df$Salary,MLB_hitting_df$H)
plot(MLB_hitting_df$RK,MLB_hitting_df$Salary,xlab="Hitting Rank",ylab = "Salary" )
#This shows ranking and Salary has no correlation.

# want to know which hitting postion has the highest average salary.
df6<-group_by(MLB_hitting_df,Pos)
Position_salary_summary<-summarize(df6,
                           Total_Respondents=n(),
                           AVG_Salary=mean(Salary,na.rm=TRUE))
                        
Position_salary_summary



Highest_Position_salary<-Position_salary_summary$Pos[which(Position_salary_summary$AVG_Salary==max(Position_salary_summary$AVG_Salary))]
Highest_Position_salary
# The DH position has the highest average salary.


# For Pitching
# We then do the same thing as we did above excpt we are now creating a Pitching dataframe.
# We  create a pitchers data frame from MLB_df from above
# we created a dataframe with players that are just pitchers to seperate them from players other than hitters and relif pitchers becuse pitchers they have different statisics than other types of players. 
MLB_Spotrac_df_pitching<-subset(MLB_df,Position=="SP")
# This dataframe stores all MLB Starting Pitchers Salarys.

# We changed salary for pitching dataframe to integer
Salary2<-gsub(",","",MLB_Spotrac_df_pitching$Salary)
Salary2<-gsub("\\$","",Salary2)
MLB_Spotrac_df_pitching$Salary<-as.integer(Salary2)

# calculate Average Salary for pitchers salary
mean(MLB_Spotrac_df_pitching$Salary)

# The Average MLB Player Salary for Pitchers is 5386657

# We will create a coulmn called Salary Category with threshold deciding category. Two categories High Salary and Low Salary
MLB_Spotrac_df_pitching$Salary_Classification<-ifelse(MLB_Spotrac_df_pitching$Salary >mean(MLB_Spotrac_df_pitching$Salary),"High Salary","Low Salary")

MLB_Spotrac_df_pitching$Salary_Classification2<-ifelse(MLB_Spotrac_df_pitching$Salary >mean(MLB_Spotrac_df_pitching$Salary),1,0)
# We then created a binary classification for salary where 0= low salary and 1=High Salary

# Read in 3rd dataset
# We now read in our MLB.com pitching data with performace statics for pitchers.
MLB.com_pitching_df<-read.csv("MLB.com_Pitching_data.csv",stringsAsFactors = FALSE)

# We want to combined the MLB_Spotrac_df_pitching and MLB.com_pitching_df together to get player statics and salary together.
# We can see that the players name coulmn is the best way to merge these two dataframes together.
# However in the MLB.com data players first name is not spelled out fully like it is in spotrac so we deced the best way to merge data is by players lastname like we did above for merging hitters.
# So We  need to split players name by creating a First Name coulmnn and a Last Name Coulmn Because MLB.com pitching Players first name isnt spelled out fully so to make to dataframes merge easily we will split so we can merge by last name later.
# We seperate the players name in Spotrac Dataframe for pitchers
firstname2<-strsplit(MLB_Spotrac_df_pitching$Player," ")
MLB_Spotrac_df_pitching$Firstname_Player<-sapply(firstname2,"[[",1 )
# We split players first name into a First Name Coulmn

lastname2<-strsplit(MLB_Spotrac_df_pitching$Player," ")
MLB_Spotrac_df_pitching$Lastname_Player<-sapply(lastname2,"[[",2 )
# we have just split the last name of players into a Last name coulmn


names(MLB.com_pitching_df)[which(names(MLB.com_pitching_df)=='Player_Last_Name')]<-"Lastname_Player"

# Before we merge data by last name we realize there is an extra space in the MLB.com_pitching_df lastname values that isn't in the MLB_Spotrac_pitching_df
# So we get rid of this space below by doing the following
MLB.com_pitching_df$Lastname_Player<-gsub(" ","",MLB.com_pitching_df$Lastname_Player)

# we also found before merging that these players last names were not spelled the exact way in the Spotrac and MLB.com pitching dataframes so we changed spotrac player names so that they would match MLB.com pitching so they can merge correctly. 
MLB_Spotrac_df_pitching$Lastname_Player[which(MLB_Spotrac_df_pitching$Lastname_Player=="deGrom")]<-"DeGrom"

# We can get rid of some coulmns that might not be needed


# We then Merge our two dataframes MLB.com_pitching_df and MLB_Spotrac_df_pitching into a dataframe called MLB_pitching_df that stores all starting pitcher players statics and their salary. 
MLB_Pitching_df<-merge(MLB.com_pitching_df,MLB_Spotrac_df_pitching, by.x="Lastname_Player",by.y ="Lastname_Player" ,all.x = TRUE)

MLB_Pitching_df<-unique(MLB_Pitching_df)

# This dataframe will contain the top 61 pitchers according to MLB.com with their stattics and mergered salary from spotac along with age, and ... 

# These pitching statiscs don't apply to starting pitchers so we can remove them from our MLB_pitching_df
MLB_Pitching_df$SV<-NULL
MLB_Pitching_df$SVO<-NULL
MLB_Pitching_df$AVG<-NULL
MLB_Pitching_df$WHIP<-NULL

# One issue with this new dataframe when we merge the two datasets where that there are some players that have the same last name so that will cause duplicates where it is matching those values.
# Example is  3 pitchers have the lastname Lopez so there values for the differnt lopezes cause some duplicates.
# We felt this was one of our biggest problems when integrading that data.
# However, we decided to leave it as is with a few duplicates becuse it wont have a huge impact on comparing.
# Another problem was that some players from spotrac were never webscraped into our data becuse they were under a different table that we couldn't collect their data.
# Example All of our data came from the active player roster but below it was an injured player roster that was in a different table n the website so we couldn't get it to scrape.
# Example Geolite of the White Soxs is in the MLB.com pitching data of top 61 pitchers but we get NA for his salary when integraded becuse of what I explained right above.


write.csv(MLB_Pitching_df,"MLB_pitching_Final_Data2.csv")


# need to seperate first and last name of player to be able to integrade more easily.Like we did for Pitching data.

# WE want to find what the average wins,losses, ERA,Innings pitched,hits allowed,and walks allowed is by Salary category.
df8<-group_by(MLB_Pitching_df,Salary_Classification)
salary_Pitching_summary7<-summarize(df8,
                           Total_Respondents=n(),
                           Mean_W=mean(W,na.rm = TRUE),
                           Mean_L=mean(L,na.rm=TRUE),
                           Mean_ERA=mean(ERA,na.rm=TRUE),
                           Mean_IP=mean(IP,na.rm = TRUE),
                           Mean_H=mean(H,na.rm=TRUE),
                           Mean_BB=mean(BB,na.rm=TRUE))
                           
                           

salary_Pitching_summary7



barplot_height<-salary_Pitching_summary7$Mean_ERA
names(barplot_height)<-salary_Pitching_summary7$Salary_Classification
barplot(barplot_height,xlab="Salary Classification",ylab="AVG ERA",col = "Red")
# This plot shows that pitchers ERA is higher when they are paid less and that pitchers ERA is lower when salary is higher.
Pitcher_salary_comp<-salary_Pitching_summary7
plot(MLB_Pitching_df$RK,MLB_Pitching_df$Salary)
# shows ranking vs salary pay
# can kind of tell from this that the better the ranking number has higher salary numbers than low ranked.
plot(MLB_Pitching_df$Salary,MLB_Pitching_df$ERA,xlab = "Salary",ylab = "ERA",)
abline(lm(MLB_Pitching_df$ERA~MLB_Pitching_df$Salary),col="red")
# This plot shows that a pitchers ERA is higher when the salry is lower and ERA is lower when salary is higher.

mean(MLB_Pitching_df$ERA)
#Avearge Pitcher ERA from data is 
MLB_Pitching_df$ERAClassification<-ifelse(MLB_Pitching_df$ERA >mean(MLB_Pitching_df$ERA),'High ERA','Low ERA')
# Descriptive Analytics
table75<-table(MLB_Pitching_df$Salary_Classification,MLB_Pitching_df$ERAClassification)
table75
salary_ERA<-table75
salary_ERA
# This table shows that 18 high salary players have a low era while 11 low salary players have low ERA.
# This table shows that more pitchers who have a high salry have a lower ERA than low salary players.
# Chi-Square Test
chisq2<-chisq.test(table75)
chisq2

# We cannot 
#The P-value of the Chi_square test is 0.3483, which indicates that we cannot reject the null hypothesis that salary classification and pitchers ERA performance are independent.
#Can't say if there is a relationship between salary class and Pitcher ERA. 

# We created a pie chart
pie(table(MLB_Pitching_df$Salary_Classification2,MLB_Pitching_df$ERAClassification ),label=c("Low ERA and High Salary","High ERA and High Salary","Low ERA and Low Salary","High ERA and Low Salary"))
# This shows that Low ERA with a low salary has the smallest meaning 
# This just shows another way that there are some differences between high and low salary with ERA

# can also see if the older the age of the pitcher leads to a decrease in performance, and why teams don't want to pay older players alot of money compared to younger age pitchers.
df12<-group_by(MLB_Pitching_df,Age)
Age_salary_Pitching_summary<-summarize(df12,
                                    Total_Respondents=n(),
                                    Mean_=mean(W,na.rm = TRUE),
                                    Mean_L=mean(L,na.rm=TRUE),
                                    Mean_ERA=mean(ERA,na.rm=TRUE),
                                    Mean_Salary=mean(Salary,na.rm = TRUE),
                                    Mean_H=mean(H,na.rm=TRUE),
                                    Mean_BB=mean(BB,na.rm=TRUE))



Age_salary_Pitching_summary
# From these statics doesn't look like Age correlates to performace would need more data to get more accurate results.
cor(Age_salary_Pitching_summary$Mean_ERA,Age_salary_Pitching_summary$Mean_Salary)
#-0.4163231 correlation for ages with Average ERA and Average Salary.
# Our Findings we found were that players who get paid a higher salary than others usually perform better than low salary players. But our results weren't suggnificantly differnt.
# Overall performance and salary paid needs more data to signicicantly say if these results are true.                        
                      