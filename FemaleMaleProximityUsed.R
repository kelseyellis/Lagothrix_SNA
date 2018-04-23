library(dplyr)
library(igraph)
library(xts)

####Set WD here when working on laptop####
setwd("C:/Users/Kelsey/Box Sync/Cleaned data")
getwd()

####Set WD here when working at school#####
setwd("C:/Users/kme479/Box Sync/Cleaned data/AAPA2017")
getwd()


#pull in focal data with FV removed
focaldata<-read.csv(file.choose(), header = TRUE, sep = ",")
head(focaldata)
names(focaldata)

#pull in focal samples
focalsamples<-read.csv(file.choose(), header = TRUE, sep = ",")
head(focalsamples)
names(focalsamples)

#pull in femaleInfant file
females<-read.csv(file.choose(), header = TRUE, sep = ",")
head(females)
names(females)

#pull in male file
males<-read.csv(file.choose(), header = TRUE, sep = ",")
head(males)

filter_focalsamples<-subset(focalsamples, Focal.Animal %in% females$ID | Focal.Animal %in% males$ID)
filter_focalsamples

names(filter_focalsamples)
 
merged_focals<-merge(focaldata, filter_focalsamples[, c("Focal.Sample.ID", "Focal.Animal", "Date")], by="Focal.Sample.ID")
merged_focals

names(merged_focals)
proximitydf<-merged_focals[c(31,30,2,5,6,8,19:23)]

head(proximitydf)
names(proximitydf)
proximitydf<-filter(proximitydf, Time.In.Sample != "")
  
###Only do if you need to rewrite the Data Frame  
write.table(proximitydf, file="C:/Users/Kelsey/Box Sync/Cleaned data/AAPA2017/ProxDF_ALL.csv",row.names=TRUE,col.names=TRUE,sep=",")
getwd()

###Need to make sure that R is reading date column as dates
class(proximitydf$Date)
proximitydf$Date<-as.Date(proximitydf$Date, "%m/%d/%Y")
proximitydf$Date

#class(females$InfantStart)
#females$InfantStart<-as.Date(females$InfantStart, "%m/%d/%Y")
#females$InfantStart
#females$InfantEnd<-as.Date(females$InfantEnd,"%m/%d/%Y")
#class(females$InfantEnd)
#females$InfantEnd

#Creating DF for each month
proximitydf_Jun2015<-filter(proximitydf[proximitydf$Date >= "2015-06-01" & proximitydf$Date <= "2015-06-30",])
head(proximitydf_Jun2015)

#before creating matrix need to rerun code for each month, and instead of editing the function, just use this generic df
proximitydf_date<-proximitydf_Jun2015
head(proximitydf_date)


###Counting number of times seen within 10 m of focal animal####
###Create dataframe for animal 1 and count number of times animal 2 within 10 m
###Then create dataframe for animal2 and count number of times animal 1 within 10 m

Proximity2<-function (animal1, animal2){
  
  totalscansasfocal1<-nrow(proximitydf_date[proximitydf_date$Focal.Animal== animal1,])
  
  focal1<-filter(proximitydf_date[proximitydf_date$Focal.Animal== animal1,])
  
  matches2a<-grepl(animal2, focal1[,9])
  two_a<-length(matches2a[matches2a==TRUE])
  
  matches1a<-grepl(animal2, focal1[,8])
  one_a<-length(matches1a[matches1a==TRUE])
  
  matchescona<-grepl(animal2, focal1[,7])
  contact_a<-length(matchescona[matchescona==TRUE])
  
  scanswithin2a<-sum(two_a, one_a, contact_a)
  scanswithin2a
  
  totalscansasfocal2<-nrow(proximitydf_date[proximitydf_date$Focal.Animal== animal2,])
  
  focal2<-filter(proximitydf_date[proximitydf_date$Focal.Animal== animal2,])
  
  matches2b<-grepl(animal1, focal2[,9])
  two_b<-length(matches2b[matches2b==TRUE])
  
  matches1b<-grepl(animal1, focal2[,8])
  one_b<-length(matches1b[matches1b==TRUE])
  
  matchesconb<-grepl(animal1, focal2[,7])
  contact_b<-length(matchesconb[matchesconb==TRUE])
  
  scanswithin2b<-sum(two_b, one_b, contact_b)
  scanswithin2b
  
  out<-(scanswithin2a + scanswithin2b)/(totalscansasfocal1 + totalscansasfocal2)
  return(out)
}

Proximity2("PTU/", "PRC/")


individual.females<-as.vector(unique(females$ID))
individual.males<-as.vector(unique(males$ID))
individuals<-c(individual.females, individual.males)
individuals

#matrix of AIs using list of individuals from females file
m <- NULL
for (i in 1:length(individuals)){
  r <- NULL
  for (j in 1:length(individuals)){
    f1 <-paste(individuals[i])
    f2 <-paste(individuals[j])
    AI <- Proximity2(f1,f2)
    r <- cbind(r,AI)
  }
  m <- rbind(m,r)
}
colnames(m) <- individuals
rownames(m) <- individuals


write.table(m, file="C:/Users/Kelsey/Box Sync/Cleaned data/AAPA2017/MOnthlyMatrices/Prox2Matrix_Jun2015.csv",row.names=TRUE,col.names=TRUE,sep=",")


#totalscansforeachfocal<-function (a){
#  scansout<-nrow(proximitydf[proximitydf$Focal.Animal== a,])
#  return(scansout)
#}

#individuals
#totalscansforeachfocal("COR/")

#####Can also write a for loop to count total scans for each individual###

l<-NULL
for (k in 1:length(individuals)){
  name<-paste(individuals[k])
  scansout<-nrow(proximitydf_date[proximitydf_date$Focal.Animal==name,])
  l<-rbind(l,scansout)
}
l
as.vector(l)
totscans<-cbind(individuals, l)
totscans

write.table(totscans, file="C:/Users/Kelsey/Box Sync/Cleaned data/AAPA2017/MonthlyMatrices/FocalTotalScans_Jun2015.csv",row.names=TRUE,col.names=TRUE,sep=",")

Proximity2<-function (animal1, animal2){
  totalscansasfocal1<-nrow(proximitydf_date[proximitydf_date$Focal.Animal== animal1,])
  
  focal1<-filter(proximitydf[proximitydf_date$Focal.Animal== animal1,])
  
  matches2a<-grepl(animal2, focal1[,9])
  two_a<-length(matches2a[matches2a==TRUE])
  
  matches1a<-grepl(animal2, focal1[,8])
  one_a<-length(matches1a[matches1a==TRUE])
  
  matchescona<-grepl(animal2, focal1[,7])
  contact_a<-length(matchescona[matchescona==TRUE])
  
  scanswithin2a<-sum(two_a, one_a, contact_a)
  scanswithin2a
  
  totalscansasfocal2<-nrow(proximitydf_date[proximitydf_date$Focal.Animal== animal2,])
  
  focal2<-filter(proximitydf_date[proximitydf_date$Focal.Animal== animal2,])
  
  matches2b<-grepl(animal1, focal2[,9])
  two_b<-length(matches2b[matches2b==TRUE])
  
  matches1b<-grepl(animal1, focal2[,8])
  one_b<-length(matches1b[matches1b==TRUE])
  
  matchesconb<-grepl(animal1, focal2[,7])
  contact_b<-length(matchesconb[matchesconb==TRUE])
  
  scanswithin2b<-sum(two_b, one_b, contact_b)
  scanswithin2b
  
  out<-(scanswithin2a + scanswithin2b)/(totalscansasfocal1 + totalscansasfocal2)
  return(out)
}

Proximity2("CAL/", "CLA/")


#matrix of AIs using list of individuals from specific time-period
individuals
m <- NULL
for (i in 1:length(individuals)){
  r <- NULL
  for (j in 1:length(individuals)){
    f1 <-paste(individuals[i])
    f2 <-paste(individuals[j])
    AI <- Proximity2(f1,f2)
    r <- cbind(r,AI)
  }
  m <- rbind(m,r)
}
colnames(m) <- individuals
rownames(m) <- individuals

write.table(m, file="C:/Users/Kelsey/Box Sync/Cleaned data/AAPA2017/MonthlyMatrices/Prox2Matrix_Jun2015.csv",row.names=TRUE,col.names=TRUE,sep=",")


