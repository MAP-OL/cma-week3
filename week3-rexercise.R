## Load the necessary libraries ################################################

library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(sf)           # to handle spatial vector data
library(terra)        # To handle raster data
library(lubridate)    # To handle dates and times

## Import the downloaded csv ##################################################
Caro<-read_delim("caro60.csv",",")

#Task 1 Segmentation 
Caro <- Caro %>%
  mutate(                                            #Measure distance within this temporal window
    nMinus3 = sqrt((lag(E,3)-E)^2+(lag(N,3)-N)^2),   # distance to pos -3 minutes
    nMinus2 = sqrt((lag(E,2)-E)^2+(lag(N,2)-N)^2),   # distance to pos -2 minutes
    nMinus1 = sqrt((lag(E,1)-E)^2+(lag(N,1)-N)^2),   # distance to pos -1 minute
    nPlus1  = sqrt((E-lead(E,1))^2+(N-lead(N,1))^2), # distance to pos +1 mintues
    nPlus2  = sqrt((E-lead(E,2))^2+(N-lead(N,2))^2), # distance to pos +2 minutes
    nPlus3 =  sqrt((E-lead(E,3))^2)+(N-lead(N,3))^2)# distance to pos +3 minutes

Caro <- Caro %>%                                     #Calculating Mean
  rowwise() %>%
  mutate(
    stepMean = mean(c(nMinus3,nMinus2, nMinus1,nPlus1,nPlus2,nPlus3))
  ) %>%
  ungroup()

Caro

#Task 2 Specify and apply threshold d

ggplot(Caro,aes(x= stepMean)) +                     
  geom_histogram(binwidth = 5)

ggplot(Caro, aes(y=stepMean)) +                     
  geom_boxplot(binwidth=5)

summary(Caro)

Caro <- Caro %>%                                    #specifying a threshold distance on stepMean
  ungroup() %>%
  mutate(static = stepMean < mean(stepMean, na.rm = TRUE))

Caro_filter <- Caro %>%                             #Removing static points
  filter(!static)

# Task 3    #The trajectory of Caro, filtered to the positions where the animal was not static

ggplot(data=Caro,aes(E, N))  +
  geom_path() +
  geom_point(aes(color=static)) +
  coord_equal() +
  theme(legend.position = "bottom") +
  theme_classic()
 
# Task 4 Segment-based analysis

rle_id <- function(vec){
  x <- rle(vec)$lengths
  as.factor(rep(seq_along(x), times=x))
}

Caro <- Caro %>%
  mutate(segment_id = rle_id(static))               #assigning unique IDs to subtrajectories

ggplot(Caro,aes(E, N))  +                           #creating ggplot with uncleaned moving segments
  geom_path(aes(colour=segment_id)) +
  geom_point(aes(color=segment_id)) +
  coord_equal() +
  theme(legend.position="none")+
  labs(color="Segments", titel="Moving segments (uncleaned)")

# remove short segments with duration < 5 Minutes

Caro_5<- Caro%>%
  group_by(segment_id)%>%
  mutate(n=n())%>%
  filter(n>=5)

ggplot(Caro_5,aes(E, N))  +                           #creating ggplot with uncleaned moving segments
  geom_path(aes(colour=segment_id)) +
  geom_point(aes(color=segment_id)) +
  coord_equal() +
  theme(legend.position = "none")
  labs(color="Segments", titel="Moving segments (removed segments <5)") #Not sure why my filter does not work? GGplot looks uncleaned and cleaned nearly the same? What did I do wrong?
  
# Task 5 Similarity measures
  
Pedestrian<-read_delim("pedestrian.csv",",")
str(Pedestrian)

Pedestrian<- Pedestrian%>%
  mutate(
    trajID_fac=as.factor(TrajID),
DateTime_integ=as.integer(DatetimeUTC))
str(Pedestrian)
  
ggplot(Pedestrian,aes(E, N, colour=TrajID))  +                           #creating ggplot for the 6 trajectories
  geom_path() +
  geom_point() +
  coord_equal() +
  facet_wrap(~TrajID)+
  theme_classic()+
  theme(legend.position = "none")+
labs(titel="Visual Comparision of the 6 trajectories")


