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
    nPlus3 =  sqrt((E-lead(E,3))^2)+(N-lead(N,3))^2) # distance to pos +3 minutes

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
Pedestrian
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

#Task 6 Calculate similarity

install.packages("SimilarityMeasures")
library(SimilarityMeasures)
help(package = "SimilarityMeasures")


Tr1 <- Pedestrian  [1:47,]
Tr2 <- Pedestrian  [48:95,]
Tr3 <- Pedestrian  [96:141,]
Tr4 <- Pedestrian  [142:190,]
Tr5 <- Pedestrian  [191:242,]
Tr6 <- Pedestrian  [143:289,]

Tr1 <- dplyr::select(Tr1, E,N)
Tr2 <- dplyr::select(Tr2, E,N)
Tr3 <- dplyr::select(Tr3, E,N)
Tr4 <- dplyr::select(Tr4, E,N)
Tr5 <- dplyr::select(Tr5, E,N)
Tr6 <- dplyr::select(Tr6, E,N)

Tr1 <- as.matrix(Tr1)                 #Creating a matrix for each trajectory
Tr2 <- as.matrix(Tr2)
Tr3 <- as.matrix(Tr3)
Tr4 <- as.matrix(Tr4)
Tr5 <- as.matrix(Tr5)
Tr6 <- as.matrix(Tr6)


# DTW

DTW2 <- DTW(Tr1,Tr2)
DTW3 <- DTW(Tr1,Tr3)
DTW4 <- DTW(Tr1,Tr4)
DTW5 <- DTW(Tr1,Tr5)
DTW6 <- DTW(Tr1,Tr6)

DTW<- c(NA,DTW2,DTW3,DTW4,DTW5,DTW6)

#Edit Distance 

ED2<-EditDist(Tr1,Tr2)
ED3<-EditDist(Tr1,Tr3)
ED4<-EditDist(Tr1,Tr4)
ED5<-EditDist(Tr1,Tr5)
ED6<-EditDist(Tr1,Tr6)

ED<- c(NA,ED2,ED3,ED4,ED5,ED6)

#Frechet Distance

FD2<-Frechet(Tr1,Tr2)
FD3<-Frechet(Tr1,Tr3)
FD4<-Frechet(Tr1,Tr4)
FD5<-Frechet(Tr1,Tr5)
FD6<-Frechet(Tr1,Tr6)

FD<-c(NA,FD2,FD3,FD4,FD5,FD6)

LCSS2<-LCSS(Tr1,Tr2,8,4,0.5)
LCSS3<-LCSS(Tr1,Tr3,8,4,0.5)
LCSS4<-LCSS(Tr1,Tr4,8,4,0.5)
LCSS5<-LCSS(Tr1,Tr5,8,4,0.5)
LCSS6<-LCSS(Tr1,Tr6,8,4,0.5)

LCSS<-c(NA,LCSS2,LCSS3,LCSS4,LCSS5,LCSS6)

sim.mea<-data.frame(DTW,ED,FD,LCSS)
sim.mea$traj<-c(1,2,3,4,5,6)
sim.mea

#Result Plots
#DTW
DTW_plot<-ggplot(data=sim.mea,aes(x=traj,y=DTW,fill=traj))+
  geom_bar(stat="identity",)+
  theme_light()+
  theme(legend.position = "none")+
  labs(title = "DTW")
DTW_plot
#ED
ED_plot<-ggplot(data=sim.mea,aes(x=traj,y=ED,fill=traj))+
  geom_bar(stat="identity",)+
  theme_light()+
  theme(legend.position = "none")+
  labs(title = "Edit Distance")
ED_plot

#FD
FD_plot<-ggplot(data=sim.mea,aes(x=traj,y=FD,fill=traj))+
  geom_bar(stat="identity",)+
  theme_light()+
  theme(legend.position = "none")+
  labs(title = "Frechet Distance")
FD_plot

#LCSS
LCSS_plot<-ggplot(data=sim.mea,aes(x=traj,y=LCSS,fill=traj))+
  geom_bar(stat="identity",)+
  theme_light()+
  theme(legend.position = "none")+
  labs(title = "LCSS")
LCSS_plot
library(cowplot)

Final_plot <- plot_grid(DTW_plot, ED_plot, FD_plot, LCSS_plot) #Final Plot to compare different results of the methods
Final_plot



