#Programming 1 - Assignment 2
#Your name(s): Genesis Roberto & Steven Broussard 


# Preparation: Load packages used in this session of R

#1) Load the World values survey data "WorldValues3.csv" and save it as WorldData. 
#   Also load the country code data "CountryCodes3.csv" and save it as CountryData.
CountryData=read.csv("CountryCodes3.csv")
WorldData=read.csv("WorldValues3.csv")
install.packages("tidyverse")
require(tidyverse)


#2) Join the country name from the CountryCode data into WorldData. 
#   Note that country code is called *ID* in CountryData and *CountryCode* in WorldData. 
#   Write a code to return the list of first 10 unique countries in WorldData. 

WorldData=left_join(WorldData,CountryData, 
                by= c('CountryCode'= 'ID'))
WorldData

CountryList=unique(WorldData$Country)
CountryList[1:10]

#*Don't use code - practing - revisit later****
Wd=unique(WorldData,by=Country)
wtf=Wd[!duplicated(Wd$Country),]
wtf[1:10,]
Wd= WorldData %>%
  distinct(Country) %>%

unique(WorldData$Country)
WorldData%>%
  filter(Country%in% Wd)

WorldData

dplyr::arrange(WorldData)
WorldData = distinct(Country)


#3) Update WorldData by dropping income, education, and lifeexpect variables.
#   What is the dimension of the updated WorldData?

WorldData=dplyr::select(WorldData,-Income,-Education,-lifeexpect)
dim(WorldData)

#4) Using pipes, calculate the average age and percent immigrant (i.e., average ratio of immigrants) for every country.
#   Display the top 5 countries in terms of average age. 
#   Hint: For displaying the top 5, refer to the help for slice( ) to find the right form of the function to use.

worldData2=
  WorldData%>%
  group_by(Country) %>%
  mutate(Age= SurveyYear-BirthYear)%>%
  summarize(
    AvgAge= mean(Age,na.rm = T),
    ImmigrantPop=sum(Immigrant,na.rm=T),
    TotalPop=n(),
    ImmigrantRatio=(ImmigrantPop/TotalPop)*100)%>%
    slice_max(AvgAge,n=5, with_ties = F)

worldData2

#Genesis Code Pt2
ImmigrantRatio= 
  WorldData%>%
  group_by(Country)%>%
  summarize(
    ImmigrantPop=sum(Immigrant,na.rm=T),
    TotalPop=n(),
    ImmigrantRatio=(ImmigrantPop/TotalPop)*100)
ImmigrantRatio

sum(Immigrant)/nrow(WorldData,
    groups="drop")%>%
      arrange(Country,Immigrant)%>%
      filter(AvgAge,ImmigrantRatio)%>%
      
  )%>%
  filter()
ungroup()%>%
  arrange(desc(ImmigrantRatio))
slice_tail(1:5) #HEL h ij jbjk

worldData2
#Genesis Code Pt2

worldData2=  mutate(WorldData,Age= SurveyYear-BirthYear)
worldData2%>%
  group_by(Country,Immigrant) %>%
  summarize(
    AvgAge= mean(Age,na.rm = T),
    ImmigrantRatio= sum(Immigrant)/nrow(WorldData,
            groups="drop")%>%
    arrange(Country,Immigrant)%>%
    filter(AvgAge,ImmigrantRatio)%>%
    slice_max(AvgAge,n=5, with_ties = F)
  )%>%
    filter()
  ungroup()%>%
  arrange(desc(ImmigrantRatio))
  slice_tail(1:5) #HEL h ij jbjk
  

#Revist to fit into code


#5) Group WorldData by country and marital status and show the percent of people in each category who are immigrants.
#   Show the results for China. (the coding for the *Marital* variable is 1 = Married, 2 = Living together, 3 = Divorced, 
#   4 = Separated, 5 = Widowed, and 6 = Single).

WorldData%>%
  group_by(Country, Marital)%>%
  summarize(Immigrantpercent=sum(Immigrant)/nrow(WorldData)*100)%>%
  filter(Country=="China")

#Genesis Code 
WorldData%>%
  group_by(Country)%>%
  filter(Immigrant==1& Marital!="NA")%>%
  summarize(Married=(sum(Marital==1)),
            Living2=(sum(Marital==2)),
            Divorced=(sum(Marital==3)),
            Separated=(sum(Marital==4)),
            Widowed=(sum(Marital==5)),
            single=(sum(Marital==6)))

#Genesis Code p2
Immigrantmaritalstat=
WorldData%>%
  group_by(Country)%>%
  filter(Immigrant==1& Marital!="NA")%>%
  summarize(Married=(sum(Marital==1))/n()*100,
            Living2=(sum(Marital==2))/n()*100,
            Divorced=(sum(Marital==3))/n()*100,
            Separated=(sum(Marital==4))/n()*100,
            Widowed=(sum(Marital==5))/n()*100,
            single=(sum(Marital==6))/n()*100)
Immigrantmaritalstat[8,]


WorldData%>%
  group_by(Country=="China")%>%
  filter(Immigrant==1& Marital!="NA")%>%
  summarize(Married=(sum(Marital==1))/n()*100,
            Living2=(sum(Marital==2))/n()*100,
            Divorced=(sum(Marital==3))/n()*100,
            Separated=(sum(Marital==4))/n()*100,
            Widowed=(sum(Marital==5))/n()*100,
            single=(sum(Marital==6))/n()*100)
        

#6) Using pipes, create a function that takes a country name and calculates its average life satisfaction ("Satisfied") 
#   broken down by "immigrant". Name this function meanSat. Show the results for China.
  
meanSat= function(country_name){
    result= WorldData%>%
      filter(Country==country_name)%>%
      group_by(Immigrant)%>%
      summarize(AvgLifeSat=mean(Satisfied,na.rm = T))
    
    return(result)
}

country_name="China"
countryavgSat=meanSat(country_name = "China")

print(countryavgSat)




#Steven Code pt2
meanSat= function(country_name){
  result= WorldData%>%
    filter(Country==country_name)%>%
    group_by(Immigrant)%>%
    summarize(AvgLifeSat=mean(Satisfied))
  
  return(result)
}

country_name="China"
countryavgSat=meanSat(country_name = "China")
print(countryavgSat)

#Genesis Code 
meanSat=function(x){
  WorldData%>%
    group_by(Country=x,Immigrant)%>%
    summarize(avgSat=mean(Satisfied,na.rm=T))
}
meanSat("China")

#Steven Edit
meanSat=function(x){
  WorldData%>%
    group_by(Immigrant)%>%
    summarize(avgSat=mean(Satisfied,na.rm=T))%>%
    filter("China")
  
}


#Genesis Code Pt2
meanSat=function(x){
  WorldData%>%
    group_by(Country=x,Immigrant)%>%
    mutate(Immigrant=case_when(
        Immigrant==0~"No",Immigrant==1~"Yes"))%>%
    summarize(avgSat=mean(Satisfied,na.rm=T))
}
meanSat("China")

#Old Code - Please rework This later
WorldData%>%
    meanSat= 
    group_by(Country, Immigrant)%>%
  summarize(meanSat,na.rm = T)%>%
  filter(Country=="China")
    


