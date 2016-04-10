```{r}
library(foreign)
library(ggplot2)
library(Hmisc)

setwd("/home/ab/Documents/MBD/data_mining/assignment3/data")
list.files()
df = spss.get("SPSS_Smart_Cities.sav" ) # Changed form read.spss to spss.get. This gives us the survey topics directly.
df = as.data.frame(df)
head(df)
head(df[,1:200])
levels(df$p1.1) <- c(levels(df$p1.1), "Si, Pero no") 
df$p1.1[df$p1.1 == "Lo he escuchado pero desconozco su significado"] <- "Si, Pero no"  # Change long answer to shorter answer to better fit visualizations. 




####
## Here I identify the questions about demographics
####

plot(table(df$ciudad),)#home city
table(df$ciudadq)#question city

table(df$edad)#age
table(df$sexo)#gender
table(df$ClaseSocial) #social class 

####
## Here we identify the questions about if the respondant knows about smart cities 
####

plot(df$p1.1, main = "Answer to 'Do you know about smart cities' by number of respondents")  ##Do you know about smart cities 
sp <- ggplot(df, aes(x=p1.1)) + geom_bar(fill="white", colour="black") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
sp + facet_grid(. ~ sexo)   # More men than women have heard about smart cities. This is about a 30-40% difference ###############
sp + facet_grid(. ~ClaseSocial)  # More middle and upper middle have heard about smart cities. 
sp + facet_grid(. ~edad) # Seems more evenly distributed by age. 

plot(df$p1.3)   ##How smart is your city? 
sp <- ggplot(df, aes(x=p1.3)) + geom_bar(fill="white", colour="black") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
sp + facet_grid(. ~ sexo)   # Pretty evenly distributed. Men have more fours but women slightly more "muy intellegente" 
sp + facet_grid(. ~ClaseSocial)  # Nothing notable 
sp + facet_grid(. ~edad) # Nothing notable  


####
## Here we identify the questions about the importance for each segment
####

###
# Transport
###

##For transport, we now scale the data. We work out the average score for transport per person.
#We then subtract the score from the average for each question.
#Positive scores now mean that, for transport, the person values this aspect of transport. 
#For example, across transport, a person's average score is 4.
#If they rate intelligent transport as 3, then they may see this segment as less important.

#Create the average
df_trans_sum = (as.numeric(df$p2.2.2.it1.Slice) +
                  as.numeric(df$p2.2.2.it2.Slice) +
                  as.numeric(df$p2.2.2.it3.Slice) +
                  as.numeric(df$p2.2.2.it4.Slice) +
                  as.numeric(df$p2.2.2.it5.Slice) + 
                  as.numeric(df$p2.2.2.it6.Slice))

df_trans_ave = (as.numeric(df$p2.2.2.it1.Slice) +
                  as.numeric(df$p2.2.2.it2.Slice) +
                  as.numeric(df$p2.2.2.it3.Slice) +
                  as.numeric(df$p2.2.2.it4.Slice) +
                  as.numeric(df$p2.2.2.it5.Slice) + 
                  as.numeric(df$p2.2.2.it6.Slice)) / 6

hist(df_trans_ave)

#Scale the scores
df$p2.2.2.it1.Slice_scale = as.numeric(df$p2.2.2.it1.Slice) - df_trans_ave
df$p2.2.2.it2.Slice_scale = as.numeric(df$p2.2.2.it2.Slice) - df_trans_ave
df$p2.2.2.it3.Slice_scale = as.numeric(df$p2.2.2.it3.Slice) - df_trans_ave
df$p2.2.2.it4.Slice_scale = as.numeric(df$p2.2.2.it4.Slice) - df_trans_ave
df$p2.2.2.it5.Slice_scale = as.numeric(df$p2.2.2.it5.Slice) - df_trans_ave
df$p2.2.2.it6.Slice_scale = as.numeric(df$p2.2.2.it6.Slice) - df_trans_ave

#Look at the means
mean(df$p2.2.2.it1.Slice_scale)
mean(df$p2.2.2.it2.Slice_scale)
mean(df$p2.2.2.it3.Slice_scale)
mean(df$p2.2.2.it4.Slice_scale)
mean(df$p2.2.2.it5.Slice_scale)
mean(df$p2.2.2.it6.Slice_scale)

###Here we check if score for all transport is significantly different for gender.
tr_aov = aov(df_trans_sum~sexo,df)
summary(tr_aov) # We see difference
#The P-Value is above .05. This means that, given the null is true, there is evidence to suggest the alternative is true.

#Now we look at the density of the sum of transport scores
tr <- ggplot(df, aes(x=df_trans_sum )) + geom_density(fill="white", colour="black")
tr # Intelligent transport such as integrated fairs and real time planning
tr + facet_grid(. ~ sexo)   # Men appear to rate intelligent transport higher than women 
tr + facet_grid(. ~ClaseSocial)  #
tr + facet_grid(. ~edad) # Younger participants appear to rate intelligent transport lower

#Here we look at thetransport scores by segment
inte_tr <- ggplot(df, aes(x=p2.2.2.it1.Slice)) + geom_bar(fill="white", colour="black") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
inte_tr # Intelligent transport such as integrated fairs and real time planning
inte_tr + facet_grid(. ~ sexo)   # Men appear to rate intelligent transport higher than women 
inte_tr + facet_grid(. ~ClaseSocial)  #
inte_tr + facet_grid(. ~edad) # Younger participants appear to rate intelligent transport lower

inte_tr_sc <- ggplot(df, aes(x=p2.2.2.it1.Slice_scale )) + geom_histogram(fill="white", colour="black") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
inte_tr_sc # Intelligent transport such as integrated fairs and real time planning
inte_tr_sc + facet_grid(. ~ sexo)   # Men appear to rate intelligent transport higher than women 
inte_tr_sc + facet_grid(. ~ClaseSocial)  #
inte_tr_sc + facet_grid(. ~edad) # Younger participants appear to rate intelligent transport lower


###Here we check if score is significantly different for gender.
inte_tr_aov = aov(as.numeric(p2.2.2.it1.Slice)~sexo,df)
summary(inte_tr_aov) # Given that the null is true, there is no evidence to suggest the alternative is true


control_tr <- ggplot(df, aes(x=p2.2.2.it2.Slice)) + geom_bar(fill="white", colour="black") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
control_tr # control measures such as traffic restrictions, tolls etc
control_tr + facet_grid(. ~ sexo)   #  
control_tr + facet_grid(. ~ClaseSocial)  #
control_tr + facet_grid(. ~edad) # 
#In general, control methods appear to be less supported than intelligent measures.

realtime_tr <- ggplot(df, aes(x=p2.2.2.it3.Slice)) + geom_bar(fill="white", colour="black") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
realtime_tr # Real time traffic management
realtime_tr + facet_grid(. ~ sexo)   #  
realtime_tr + facet_grid(. ~ClaseSocial)  #
realtime_tr + facet_grid(. ~edad) # 
#Younger people appear to not value real time traffic management as much. 
#Maybe this has something to do with the idea that fewer younger people are using road vehicles.
#The car is not a status symbol for younger people, where it was for baby boomers


plot(df$p2.2.2.it4.Slice)## Smart Fleet
smrtfleet_tr <- ggplot(df, aes(x=p2.2.2.it4.Slice)) + geom_bar(fill="white", colour="black") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
smrtfleet_tr # Real time traffic management
smrtfleet_tr + facet_grid(. ~ sexo)   #  
smrtfleet_tr + facet_grid(. ~ClaseSocial)  #
smrtfleet_tr + facet_grid(. ~edad) # 
#Smart fleet appears to not be overly important across all facets.

plot(df$p2.2.2.it5.Slice)## Parking Intelligence
parkintel_tr <- ggplot(df, aes(x=p2.2.2.it5.Slice)) + geom_bar(fill="white", colour="black") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
parkintel_tr # Real time parking intelligence. Such as which spaces are free now.
parkintel_tr + facet_grid(. ~ sexo)   #  
parkintel_tr + facet_grid(. ~ClaseSocial)  #
parkintel_tr + facet_grid(. ~edad) # 
#Of interest, there do not seem to be any clear differences across our three facets.

plot(df$p2.2.2.it6.Slice)## Electric Vehicles
elecvehicles_tr <- ggplot(df, aes(x=p2.2.2.it6.Slice)) + geom_bar(fill="white", colour="black") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
elecvehicles_tr # Introduction of these vehicles and necessary infrastructure.
elecvehicles_tr + facet_grid(. ~ sexo)   #  
elecvehicles_tr + facet_grid(. ~ClaseSocial)  #
elecvehicles_tr + facet_grid(. ~edad) # 
#Electric vehicles appear to be important. 
#Would this relate to the fact that air polution is important in many European Cities?


# Security 
plot(df$p2.3.2.it1.Slice)## Video Survallence
plot(df$p2.3.2.it2.Slice)## Cyber Security
plot(df$p2.3.2.it3.Slice)## Servallenc at Transport Locations
plot(df$p2.3.2.it4.Slice)## Smart Em,ergency Control Centres
plot(df$p2.3.2.it5.Slice)## People Tracking
plot(df$p2.3.2.it6.Slice)## Protection of Heritage


# Education: Education/Technology is faceted by sex, age and income.###################
plot(df$p2.4.2.it1.Slice)## Technology in schools
sp <- ggplot(df, aes(x=p2.4.2.it1.Slice)) + geom_bar(fill="white", colour="black") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
sp + facet_grid(. ~ sexo)   # Close to same distribution
 sp + facet_grid(. ~edad) 

plot(df$p2.4.2.it2.Slice)## Training opportunites with technology
sp <- ggplot(df, aes(x=p2.4.2.it2.Slice)) + geom_bar(fill="white", colour="black") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
sp + facet_grid(. ~ sexo)   # men more fours, women more fives
sp + facet_grid(. ~edad)  # importance increases with age

plot(df$p2.4.2.it3.Slice)## Open Education (MOOCS) 
sp <- ggplot(df, aes(x=p2_4_2_it3_Slice)) + geom_bar(fill="white", colour="black") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
sp + facet_grid(. ~ sexo)   # men more fours, women more fives (this pattern repeats?)
 sp + facet_grid(. ~edad)  # 5's clearly increase with age, 4's are about constant. 


plot(df$p2.7.2.it1.slice)## Management of Cultural Heritage
plot(df$p2.7.2.it2.Slice)## Data is open to the people
plot(df$p2.7.2.it3.Slice)## A standard platform for service management
plot(df$p2.7.2.it4.Slice)## Remote services
plot(df$p2.7.2.it5.Slice)## Service communication  

####
## Here I identify the questions about who should lead smart city development
####

plot(df$Gap.F1)###Who should lead, ranked 1st
plot(df$Gap.F2)###Who should lead, ranked 2nd
plot(df$Gap.F3)###Who should lead, ranked 3rd 

####
## Here I identify the questions about how important each improvement is
####

## All of these seem to have high no's and low yes's.
table(df$Gap.A.gap1.slice1) ###is urban management this most important to you
table(df$Gap.A.gap1.slice2)###is mobility and traffic the most important to you
table(df$Gap.A.gap1.slice3)###is Security in the city the most important to you
table(df$Gap.A.gap1.slice4)###is local education this most important to you
table(df$Gap.A.gap1.slice5)###is health this most important to you
table(df$Gap.A.gap1.slice6)###is economy this most important to you
table(df$Gap.A.gap1.slice7)###is municipal govetnment this most important to you

table(df$Gap.A.gap2.slice1) ###is urban management the 2nd most important to you
table(df$Gap.A.gap2.slice2)###is mobility and traffic the 2nd most important to you
table(df$Gap.A.gap2.slice3)###is Security in the city the 2nd most important to you
table(df$Gap.A.gap2.slice4)###is local education the 2nd most important to you
table(df$Gap.A.gap2.slice5)###is health  the 2nd most important to you
table(df$Gap.A.gap2.slice6)###is economy the 2nd most important to you
table(df$Gap.A.gap2.slice7)###is municipal govetnment the 2nd most important to you


####
## Here Carmen and I tried to identify the more complex demographic data
####


table(df$C30) #Who brings the money to your home

plot(df$C31) #Education level
table(df$C31x) #Education level of the money provider

table(df$C32) #Current employment situation
table(df$C32x) #Current employment situation of the money provider

table(df$C32) #Current employment situation of the money provider

table(df$C32a) #Current employment situation
table(df$C32ax) #Current employment situation of the money provider

table(df$c32b) #Current employment situation of the money provider


####
## Now some multivariate plots ### I am going to add the ggplot format and look at each by age and sex. 
#### 

### Demand Management Health Care
plot(df$edad,df$p2.5.2.it1.Slice, main = "Responses per Age - Health Slice 1" ,ylab = "Importance",xlab = "Age Group", col = "Pink")
sp <- ggplot(df, aes(x=p2.5.2.it1.Slice)) + geom_bar(fill="white", colour="black") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
sp + ggtitle("DEMAND MANAGEMENT HEALTH CARE")
sp + facet_grid(. ~ sexo) + ggtitle("DEMAND MANAGEMENT HEALTH CARE BY SEX")   # women much more fives. More important to women. 
sp + facet_grid(. ~edad) + ggtitle("DEMAND MANAGEMENT HEALTH CARE BY AGE") # importance increases with age (as we would expect) 

# TELE-ASSISTANCE 
plot(df$edad,df$p2.5.2.it2.Slice, main = "Responses per Age - Health Slice 2" ,ylab = "Importance",xlab = "Age Group", col = "Pink")
sp <- ggplot(df, aes(x=p2.5.2.it2.Slice)) + geom_bar(fill="white", colour="black") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
sp + ggtitle("TELEASSISTANCE")
sp + facet_grid(. ~ sexo) + ggtitle("TELEASSISTANCE")   # women again feel this is much more important
sp + facet_grid(. ~edad) + ggtitle("TELEASSISTANCE")  # Increases with age 


# HEALTH PROGRAMS 
plot(df$edad,df$p2.5.2.it3.Slice)
sp <- ggplot(df, aes(x=p2.5.2.it3.Slice)) + geom_bar(fill="white", colour="black") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
sp + ggtitle("HEALTH PROGRAMS")
sp + facet_grid(. ~ sexo) + ggtitle("HEALTH PROGRAMS")   # Women more 5's, men more 4's. 
sp + facet_grid(. ~edad) + ggtitle("HEALTH PROGRAMS")   # Of course increases with age. 

# Prevention and Health Alerts 
plot(df$edad,df$p2.5.2.it4.Slice)
sp <- ggplot(df, aes(x=p2.5.2.it4.Slice)) + geom_bar(fill="white", colour="black") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
sp + ggtitle("PREVENTION AND HEALTH ALERTS") 
sp + facet_grid(. ~ sexo) + ggtitle("PREVENTION AND HEALTH ALERTS")   # more 5' and 4's for women. 
sp + facet_grid(. ~edad) + ggtitle("PREVENTION AND HEALTH ALERTS")   # Of course increases with age.

# DIGITAL CLINICAL HISTORY 
plot(df$edad,df$p2.5.2.it5.Slice)
sp <- ggplot(df, aes(x=p2.5.2.it5.Slice)) + geom_bar(fill="white", colour="black") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
sp + ggtitle("DIGITAL CLINICAL HISTORY") 
sp + facet_grid(. ~ sexo) + ggtitle("DIGITAL CLINICAL HISTORY")   # more 5'S for women, 4's for men. difference not as pronounced this time. 
sp + facet_grid(. ~edad) + ggtitle("DIGITAL CLINICAL HISTORY")   # increases with age  


plot(df$edad,df$p2.6.2.it1.Slice)
plot(df$edad,df$p2.6.2.it2.Slice)
plot(df$edad,df$p2.6.2.it3.Slice)
plot(df$edad,df$p2.6.2.it4.Slice)
plot(df$edad,df$p2.6.2.it5.Slice) 




