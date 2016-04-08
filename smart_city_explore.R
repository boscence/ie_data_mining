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
## Here I identify the questions about if the respondant knows about smart cities 
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
## Here I identify the questions about the importance for each 
####

# Transport
plot(df$p2.2.2.it1.Slice)## Intellegent Transport
plot(df$p2.2.2.it2.Slice)## Control Measures
plot(df$p2.2.2.it3.Slice)## Real time traffic management
plot(df$p2.2.2.it4.Slice)## Smart Fleet
plot(df$p2.2.2.it5.Slice)## Parking Intelligence
plot(df$p2.2.2.it6.Slice)## Electric Vehicles

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




