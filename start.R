##############################
##MODELING MOSQUITOES##
#######################

# NOTES:
# ALL NOTES GO IN THIS AREA
# 1. do we want to use lagged weather variables because yesterday's weather affects today's hatch?
# 2. need to merge with nearest spray or perhaps merge all sprays and then see the effect area of the spray
#  3. need to include month
#  4. need to include past weather - mosquitos live 10 days
#       need to really get past wetness and past temperature (former being good for mosq. latter being bad for cold)
# 5. prolonged rains might be terrible. sprinkles might be better
# 6. oversample wnvpresent==1
# 7. where is the wind blowing in relation to lattitude/longitude and see if number of mosquitos in one area are going to move toward the other part of the city
#       -so if houston has 1000 mosquitos on january 1st and the direction is northwest austin might have a lot of mosquitos on jan 5th 
#       -need to get the speed of the wind and the result direction to get the exact lat long and the distance from 1000
#       -resultdir is possibly the 360 direction/10 off of true north
#       -potentially sample different percentages of the average speed of wind
#8. my task - to deal with wind and lagged variables


# working with train data -------------------------------------------------------
jason_dir <- "C:/Users/jbest/Box Sync/Personal (Private) - jbest/R/kaggle/westNile"

#Brian's Directory
brian_dir <- "C:/Users/user1/Documents/Kaggle"

if (file.exists(brian_dir)){
    data_dir<-brian_dir
} else if (file.exists(jason_dir)) {
    
    data_dir<-jason_dir
} else {
    
    stop('directory doesnt exist')
}

source(file.path(data_dir,'scripts/helperFunctions.R')) #pulling in helper functions
source(file.path(data_dir,'scripts/dependencies.R')) #reading in library dependencies


train<-fread(file.path(data_dir,'train.csv')) #i'd rather use data.tables


train[,Date:=as.Date(Date)]
train[,Species:=as.factor(Species)]
train[,Trap:=as.factor(Trap)]

weather<-fread(file.path(data_dir,'weather.csv'))

# working with weather ----------------------------------------------------
#there are 2 stations and they report different numbers. sometimes station 2 has missing observations as well
#this part could be made more sophisticated but doing it this way for now.
weather[,Date:=as.Date(Date)]

#extracting weather conditions from Codesum
conditions<-c('FC','TS','RA','GR','RA','DZ','SN','SG','PL','IC','FG','BR',
              'UP','HZ','FU','VA','DU','DS','PO','SA','SS','PY','SQ','DR','SH',
              'FZ','MI','PR','BC','VC','BL','+','-')

conditioning<-function(con){
    
    weather[grep(con,weather$CodeSum),eval(as.name(con)):=1] #finding each instance of different conditions
    
    if(con %in% names(weather)) weather[is.na(weather[,eval(as.name(con))]),eval(as.name(con)):=0] #replacing NAs with 0s
    
}

for(i in conditions){ #running the function for each potential condition
    conditioning(i)
}


#collapsing the 2 stations into 1 by means/maxes and excluding missing values

key.vars1<-c('Tmax','Tmin','Tavg','Depart','DewPoint','WetBulb','Heat','Cool','Sunrise','Sunset','PrecipTotal','StnPressure',
             'SeaLevel','ResultSpeed','ResultDir','AvgSpeed')
key.vars2<-c('TS','RA','GR','DZ','SN','FG','BR','HZ','FU','SQ','MI','BC','VC')

#first making key.vars1 numeric

weather[,(key.vars1):=lapply(.SD, as.numeric), .SDcols=key.vars1]

#next getting mean of key.vars1 and max of key.vars2
weather1 <- weather[, c(lapply(.SD,mean,na.rm=TRUE)), by=Date, .SDcols=key.vars1]

weather2 <- weather[, c(lapply(.SD,max,na.rm=TRUE)), by=Date, .SDcols=key.vars2]
#merging back together



weather<-merge(weather1,weather2,by='Date')

for(i in names(weather)[names(weather) %ni% 'Date']){
    print(i)
    a<-median(weather[,eval(as.name(i))],na.rm=TRUE)
    weather[!is.finite(weather[,eval(as.name(i))]),eval(as.name(i)):=a]
    weather[is.na(weather[,eval(as.name(i))]),eval(as.name(i)):=a]
    
    
}


    


train<-merge(train,weather,by='Date',all.x=TRUE)



# spray info --------------------------------------------------------------

#notes - need to merge based on location. only for 2011-2013
spray<-fread(file.path(data_dir,'spray.csv'))
spray[,Date:=as.Date(Date)]
spray[,combTime:=paste(Date,Time)]
spray[,combTime:=as.POSIXct(combTime,format='%Y-%m-%d %X')]

#Trying to build a 4-column dataframe that calculates distance from each trap from the spray locations
#Probably need to just define a function and run an lapply or something, but my mind always 
#starts with for loop logic
#Unfortunately this runs endlessly on my machine, but it should be pretty quick.  Not sure why.

#Maybe simplify even further with a WHILE Loop?  i.e. 
#Populate three new columns in the train data table with zeros
#While tran$Date==spray$date perform the distance function
#Otherwise leave it at zero for now?

# JB NOTES:
# just merge with location and date - i can see if all of the locations are in our train dataset
# going to use haversine method for distance in geosphere package
# for each date i'm going to grab the nearest latitude and longitude for that area by minimizing haversine
# and create a new variable called nearlong nearlat and merge based on date and those
# # two variables
# ISSUES: 1. I WANT TO HAVE THE OTHER SPRAY DATES DISTANCE REPLACED WHEN THE NEW SPRAY DATE COMES ABOUT
# 2. I WANT TO EVENTUALLY CAPTURE WIND
# "2011-08-29" "2011-09-07" "2013-07-17" "2013-07-25" "2013-08-08" "2013-08-15" "2013-08-16" "2013-08-22" "2013-08-29" "2013-09-05"

   
#we could go through and create columns for each date and the minimum distance associated with that date
#need the lattitude and longitude associated with that date as well
#3 cols - date1.dist, date1.lat,date1.long

min.distance<-function(x,y){
    
    dhav=distHaversine(c(x,y),init.spraydat) #getting distance from that observation
    ind.min<-which(dhav==min(dhav)) #getting the index of the minimum distance
    min.dist<-dhav[ind.min] #minimum distance
    return(min.dist)
    
}
min.latitude<-function(x,y){
    
    dhav=distHaversine(c(x,y),init.spraydat) #getting distance from that observation    ind.min<-which(dhav==min(dhav)) #getting the index of the minimum distance    
    ind.min<-which(dhav==min(dhav)) #getting the index of the minimum distance
    min.lat<-init.spraydat[ind.min,Latitude]    
    return(min.lat)
 
}

min.longitude<-function(x,y){
    
    dhav=distHaversine(c(x,y),init.spraydat) #getting distance from that observation    ind.min<-which(dhav==min(dhav)) #getting the index of the minimum distance
    ind.min<-which(dhav==min(dhav)) #getting the index of the minimum distance
    min.long<-init.spraydat[ind.min,Longitude] #longitude with the minimum distance
    return(min.long)

    
}

j=1
for(i in unique(spray$Date)){
    
    print(i)
    init.spraydat<-subset(spray,subset=Date==i,select=c('Longitude','Latitude'))
    train[,eval(as.name(paste0('min.dist',j))):=min.distance(Longitude,Latitude),by=1:nrow(train)] #running on each individual row. takes forever. need to vectorize
    train[,eval(as.name(paste0('min.lat',j))):=min.latitude(Longitude,Latitude),by=1:nrow(train)]
    train[,eval(as.name(paste0('min.long',j))):=min.longitude(Longitude,Latitude),by=1:nrow(train)]
   j=j+1
}



########################################################################
# 
# train_dt<-select(train, c(Date, Longitude, Latitude))
# 
# dist_spray<-function(train_dt, spray){
#     minmedmax<-as.data.table(matrix(rep(rep(0, times = 4), 
#                                         length(train_dt$Latitude)), ncol = 4))
#     setnames(minmedmax,names(minmedmax),c("Date", "dist_min", "dist_ave", "dist_max"))
#     
#     for(i in 1:length(unique(spray$Date))){ 
#         
#         for(j in 1:length(train_dt$Date)){
#             if(train_dt$Date[j] == spray$Date[i]){
#                 trap_loc<-matrix(c(train_dt$Longitude[j], train_dt$Latitude[j]), ncol = 2)
#                 
#                 spray_loc<-matrix(spray[which(spray$Date == as.Date(unique(spray$Date)[i])),
#                                         c("Longitude", "Latitude")], ncol = 2)
#                 distances<-spDistsN1(spray_loc, trap_loc, longlat = TRUE)
#                 
#                 minmedmax$Date[j]<-train_dt$Date[j]
#                 minmedmax$dist_min[j]<-min(distances)
#                 minmedmax$dist_ave[j]<-mean(distances)
#                 minmedmax$dist_max[j]<-max(distances)
#             }
#             else{
#                 minmedmax$Date[j]<-train_dt$Date[j]
#                 minmedmax$dist_min[j]<-0
#                 minmedmax$dist_ave[j]<-0
#                 minmedmax$dist_max[j]<-0
#             }
#             
#             
#         }
#     }
#     
# }


#Do we know if there are sprays during the test set? 
#It's almost like we have to figure out the effect of the sprays 
#  so we can remove that effect from the learning algoritm... 
#The rarity of sprays creates another unbalanced situation in which the vast majority of dates will have
#   no sprays at all...

#Idea for calculating locations via wind speed. (Not Spray related, just brainstorming)
#Pick a central lat/long over the area of interest (Chicago) for a point.  Populate a matrix incrementally
# covering the entire area
#use the distance calculator in reverse to supply the coordinates of a calculated distance - this may not work...
#Need to figure out the cumulative effect of mosquitos going by a trap...i.e. 50% trapped from original 
#bunch, 25% 5 hours later from arriving bunch, 10% 15 hours later from final bunch blowing through...  
#i.e. 24 new variables guesstimating the locations of bugs throughout the day given weather/wind

#Make new variables related to the traps that includes time and distance away from sprays





# basic info --------------------------------------------------------------

tabling<-function(dat,var){
    print(var)
    table(dat[,eval(as.name(var))])
    
}

#why are there more addresses than traps?
#delete<-train[,list(address=length(unique(Address))),by=Trap]
#T009, T035
#delete<-subset(train,subset=Trap %in% c('T009','T035'))
#delete<-delete[!duplicated(delete[,c('Trap','Address'),with=FALSE]),]
#within 4 kilometers of each other
vars<-c('Species','NumMosquitos','WnvPresent','Trap','AddressAccuracy')

lapply(vars,tabling,dat=train)
#notes:
# lot of different traps
# not a lot of west nile virus
# looks like data may be top coded at 50 for numMosq
# some species are very rare
# 3 5 8 and 9 for accuracy of address

# feature engineering -----------------------------------------------------

#doing lags on weather variables

#getting lagged dates so i can merge#
for(i in 1:7){
    
    train[,eval(as.name(paste0('Date',i))):=Date-i]
    
}

#changing names of weather dataset
init.names<-copy(colnames(weather))
for(i in 2:7){
    
    print(i)
    setnames(weather,colnames(weather),paste0(init.names,i)) #going to change all of the names so i can reference them as lags
    train<-merge(train,weather,by=paste0('Date',i),all.x=TRUE)
    
}
setnames(weather,colnames(weather),init.names) #returning names back


# modeling ----------------------------------------------------------------
dont.use<-c(names(train)[grep('Date',names(train))], 'Address','Street','AddressNumberAndStreet','Lattitude','Longitude','AddressAccuracy','WnvPresent','Trap')
vars<-names(train)[names(train) %ni% dont.use]

validation.dat<-x.validate(data=train,vtu=vars,outVar='WnvPresent',FUN=Random,err='auc',dat.return=TRUE)

x.validate(data=train,vtu=vars,outVar='WnvPresent',FUN=Random,err='auc')