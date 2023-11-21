########## Formatting data for MARK program #################

require(reshape)
require(lubridate)
require(RMark)

#load data
input.data<-read.csv("Sightings/field.data.csv")#col = date, animal_id

#Transform data & add a group covariate (e.g. "stage/sex")--

  input.data<-read.csv("Sightings/field.data.csv")
  input.data$date <- as.Date(input.data$date,format = "%d/%m/%Y")
  input.data$date = floor_date(input.data$date, "year")#rounds date down to year level
  
  #load covariate file (Covariate folder)
  groups = read.csv("Covariates/individuals_groups.csv",header = T)#load groups
  #filter
  groups = subset(groups, stage !="#N/A" & stage != "" & stage != "UNK") #remove unknown stages
  
  
  #creating an indicator to show a capture occurred
  input.data$sighting = 1
  
  #reshaping functions to pivot the data
  output=cast(input.data,animal_id ~date)
  
  #fill in all the days when the animal wasn't seen with zeros
  output[is.na(output)]=0
  
  #convert entries >1 to 1 (needed if animals can be caught >1x per occasion)
  k=dim(output)[2]
  output[,2:k]<-(output[,2:k]>0)*1
  
  #function to read the matrix and create the capture history strings
  concat<-function(x)
  {
    k<-ncol(x)
    n<-nrow(x)
    out<-array(dim=n)
    for (i in 1:n)
    {
      y<-(x[i,]>0)*1
      out[i]<-paste(y,sep="",collapse="")
    }
    return(out)
  }
  
  #capture history data frame for RMark  
  capt.hist<-data.frame(animal_id=output$animal_id, ch=concat(output[,2:k]))


#OUTPUT
output= merge(x=output,y=groups, by.x=c("animal_id"), by.y=c("animal_id"))#add groups to life histories
write.csv(output, "Encounter history format.csv", row.names = F)


#----------------------------------------------------------------------------------------------
