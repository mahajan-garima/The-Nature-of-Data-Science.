####################   Question1     ############################
project<-read.csv("project2018S.csv")
str(project)

####Exploring of dataset
View(project)
dim(project)
names(project)
head(project)

City=c("Adelaide", "Brisbane", "Hobart", "Melbourne", "Perth", "Sydney")
Population=c(1313927,2408223,226884,4850740,2050138,5131326)
Propotion=c(0.082, 0.151, 0.014, 0.304, 0.128, 0.321)
OzPopulation=data.frame(City,Population,Propotion)

OzPopulation
#### To look at the sample population propotions
OzSample.count=table(project$City)
OzSample.count

#proportion and %age calculation for given dataset
OzSample.Propotion =OzSample.count/1000
OzSample.Percentage =OzSample.Propotion*100

#proportion and %age calculation for given population
#OzPopulation$Propotion =c(0.082, 0.151, 0.014, 0.304, 0.128, 0.321)
OzPopulation.Percentage=OzPopulation$Propotion*100


#####Ploting the Bar Graphs
plot1=rbind(OzSample.Percentage,OzPopulation.Percentage)
barplot(plot1, xlab="Cities",
        ylab="Percentage of the Population", 
        main="Percentage of the Population staying in Six different cities of Australia",
        beside= TRUE, col=c("grey", "red"),ylim=c(0,45),legend= row.names(plot1))

### Applying The test
g.count=table(project$City)          #given count from the dataset
e.count=OzPopulation$Propotion*1000     #considering the same scale and calulating the no of individuals city-vise

######### Representative Sample ############
chi2=sum((OzSample.count-e.count)^2/e.count)
chi2
####------------   test to see if data matches proportions
#H0= The distribution of cities in the data matches the above population proportions of the cities.
#HA= The distribution of cities in the data does not matches the above population proportions of the cities.

chisq.test(OzSample.count, p=OzPopulation.Percentage, rescale.p=TRUE, simulate.p.value = TRUE, B=1000)
## p value is high, so we can't say that the proportions don't match.

#*****************************************************************************************************************************************************************
#################################   Question 2   #############################
# CHOSEN MOVIE
#It is believed that people from difference cities have different preferences. Is there a dependence between the sample variables City and Chosen Movie?
project<-read.csv("project2018S.csv")
movie_city=table(project$Movie,project$City)
movie_city
city_movie=table(project$City,project$Movie)
chisq.test(movie_city,simulate.p.value = TRUE,B=2000)
#H0: The The two variables are independent
#H1: The The two variables are dependent

chisq.test(movie_city)
chisq.test(movie_city,B=2000)
#p value is high, so we can't say that variables are dependent.

barplot(movie_city,beside = TRUE, col=c("sea green","violet","indian red"),xlab="City", ylab="Movie Choice",
        main="Movie Choice based on City", ylim=c(0,280),legend=T)

barplot(city_movie,beside = TRUE, col=1:6,xlab="Movies------->", ylab="Count------>",
        main="Movie Choices", ylim=c(0,280),legend=T)

#*****************************************************************************************************************************************************************
######################## Question3 ###########################################
##   HARSH RATERS
#As people grow older, it is believed that they expect more from movies and so provide lower ratings. 
#To answer this question, test if there is a difference in the mean rating for people over and including 40 years old, compared to the mean rating from people under 40 years old.
project<-read.csv("project2018S.csv")
Age1= ifelse(project$Age< 40,"Under 40","Above or equal 40")
project1= data.frame(project,Age1)
#project2= data.frame(project1$Rating,project1$Age1)
names(project1)

t.test(Rating~Age1, data= project1)

## H0: no difference in movie rating between ppl below 40 and ppl Above 40
## H1: there is a difference in movie rating.

t.test(Rating~Age1, data=project1, var.equal=TRUE)
#

boxplot(Rating~Age1, data=project1)
#Another way###
t.test(Rate.Above40, Rate.Under40 )
Rate.Above40 = subset(project2, project1.Age1=="Above or equal 40", project1.Rating, drop=TRUE)
Rate.Under40 = subset(project2, project1.Age1=="Under 40", project1.Rating, drop=TRUE)

#
library(lattice)
histogram(~Rating|Age1, data=project1, main="Rating on the Scale of 5-10 based on Age group of Under 40 and Above 40")

#*****************************************************************************************************************************************************************
#####################    Question 4 ###########################
#BEST MOVIE
#Is there evidence that the movies are not equally preferred? Test if all mean ratings are equal for each movie?
#If there is not equal preference, which is preferred over which?
project<-read.csv("project2018S.csv")

movie<-project[,-c(1,3)]
movie_data<-table(movie)
movie_data

chisq.test(movie_data, simulate.p.value = TRUE, B=2000)

movie.r= aov(Rating~Movie, data= project)
summary(movie.r)

TukeyHSD(movie.r)
plot(TukeyHSD(movie.r))

boxplot(Rating~Movie, data= project,xlab="Movies", ylab="Rating",
        main="Rating of the Movies" )
#IQR(project$Rating)
#quantile(project$Rating, 0.25)
#quantile(project$Rating, 0.75)
#quantile(project$Rating, c(0.25,0.5, 0.75))

#dotchart(q4$Rating, group=q4$Movie, xlab="Rating for the Movie",pch=16)

#*****************************************************************************************************************************************************************

###################  Question5###############
# SImple Linear Regression (AGE AND RATING)
#We found that older people rate movies lower. Using a linear model of Rating as a function of Age, is there evidence that the gradient is not zero? 
#Use the model to predict the expected rating for someone of age 32.
MovieR_lm=lm(Rating~Age, data= project)
plot(x= project$Age, y= project$Rating, xlab= "Age",ylab= "Rating", main="Linear dependence of Movie Rating with Age", pch=16)
abline(MovieR_lm, col="red",lwd=2)
#H0= slope is zero
#H1= slope is not equal to zero
x=replicate(1000,{
  Age.Shuff = sample(project$Age)
  MovieR_lm1=lm(Rating~Age.Shuff, data= project)
  coef(MovieR_lm1)[2]
})
hist(x, col="lightblue", main="", xlab="")
MovieR_lm=lm(Rating~Age, data= project)
slope= coef(MovieR_lm)[2]
pVal=mean(x>abs(slope)) + mean(x< -abs(slope))
pVal

predict(MovieR_lm, list(Age=32))
coef(MovieR_lm)
#*****************************************************************************************************************************************************************



