library(foreign)

setwd("/home/ab/Documents/MBD/data_mining/assignment3/data")
list.files()
df = read.spss("SPSS_Smart_Cities.sav" )
df = as.data.frame(df)
head(df)
head(df[,1:200])

# Just a test to play with github

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

plot(df$p1_1)##Do you know about smart cities
plot(df$p1_3)##How smart is your city?

####
## Here I identify the questions about the importance for each 
####

plot(df$p2_2_2_it1_Slice)## Intellegent Transport
plot(df$p2_2_2_it2_Slice)## Control Measures
plot(df$p2_2_2_it3_Slice)## Real time traffic management
plot(df$p2_2_2_it4_Slice)## Smart Fleet
plot(df$p2_2_2_it5_Slice)## Parking Intelligence
plot(df$p2_2_2_it6_Slice)## Electric Vehicles

plot(df$p2_3_2_it1_Slice)## Video Survallence
plot(df$p2_3_2_it2_Slice)## Cyber Security
plot(df$p2_3_2_it3_Slice)## Servallenc at Transport Locations
plot(df$p2_3_2_it4_Slice)## Smart Em,ergency Control Centres
plot(df$p2_3_2_it5_Slice)## People Tracking
plot(df$p2_3_2_it6_Slice)## Protection of Heritage

plot(df$p2_4_2_it1_Slice)## Technology in schools
plot(df$p2_4_2_it2_Slice)## Training opportunites with technology
plot(df$p2_4_2_it3_Slice)## Open Education (MOOCS)

plot(df$p2_7_2_it1_Slice)## Management of Cultural Heritage
plot(df$p2_7_2_it2_Slice)## Data is open to the poeple
plot(df$p2_7_2_it3_Slice)## A standard platform for service management
plot(df$p2_7_2_it4_Slice)## Remote services
plot(df$p2_7_2_it5_Slice)## Service communicatio 

####
## Here I identify the questions about who should lead smart city development
####

plot(df$Gap_F1)###Who should lead, ranked 1st
plot(df$Gap_F2)###Who should lead, ranked 2nd
plot(df$Gap_F3)###Who should lead, ranked 3rd

####
## Here I identify the questions about how important each improvement is
####

table(df$Gap_A_gap1_slice1) ###is urban management this most important to you
table(df$Gap_A_gap1_slice2)###is mobility and traffic the most important to you
table(df$Gap_A_gap1_slice3)###is Security in the city the most important to you
table(df$Gap_A_gap1_slice4)###is local education this most important to you
table(df$Gap_A_gap1_slice5)###is health this most important to you
table(df$Gap_A_gap1_slice6)###is economy this most important to you
table(df$Gap_A_gap1_slice7)###is municipal govetnment this most important to you

table(df$Gap_A_gap2_slice1) ###is urban management the 2nd most important to you
table(df$Gap_A_gap2_slice2)###is mobility and traffic the 2nd most important to you
table(df$Gap_A_gap2_slice3)###is Security in the city the 2nd most important to you
table(df$Gap_A_gap2_slice4)###is local education the 2nd most important to you
table(df$Gap_A_gap2_slice5)###is health  the 2nd most important to you
table(df$Gap_A_gap2_slice6)###is economy the 2nd most important to you
table(df$Gap_A_gap2_slice7)###is municipal govetnment the 2nd most important to you


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
## Now some multivariate plots
####

plot(df$edad,df$p2_2_2_it1_Slice, main = "Responses per Age - Health Slice 1" ,ylab = "Importance",xlab = "Age Group", col = "Pink")
plot(df$edad,df$p2_1_2_it2_Slice, main = "Responses per Age - Health Slice 2" ,ylab = "Importance",xlab = "Age Group", col = "Pink")
plot(df$edad,df$p2_1_2_it3_Slice)
plot(df$edad,df$p2_1_2_it4_Slice)
plot(df$edad,df$p2_1_2_it5_Slice)

plot(df$edad,df$p2_6_2_it1_Slice)
plot(df$edad,df$p2_6_2_it2_Slice)
plot(df$edad,df$p2_6_2_it3_Slice)
plot(df$edad,df$p2_6_2_it4_Slice)
plot(df$edad,df$p2_6_2_it5_Slice)




