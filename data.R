####################################

#Abhishek Pramod Choure 1132200437
#Ajinkya Anand Kuchake 1132200438
#Ayeshwarya Suresh Gotkhindi 1132200511

####################################





######################################
          #Libraries
######################################
library(readxl)
library(plyr)
library(dplyr)
library(ggmap)


######################################
        #Original Dataset
######################################

#read the excel file
fileLocation <- "C:\\abhishek\\Projects\\DMW\\Citizen Complaints Data.xlsx"
citizensComplaintDataSet <- read_excel(fileLocation)
View(citizensComplaintDataSet)

FinalDataSet <- read_excel("C:\\abhishek\\Projects\\DMW\\FinalComplaintsData .xlsx")


nrow(citizensComplaintDataSet)
cleanDataSet1 <- (na.omit(citizensComplaintDataSet))
nrow(cleanDataSet1)

cleanDataSet1 <- FinalDataSet
cleanDataSet1 <- cleanDataSet1 %>% mutate(Category = replace(Category,Category == "Garbage (SWM)","Garbage"))
cleanDataSet1 <- cleanDataSet1 %>% mutate(Category = replace(Category,Category == "Garbage (Soild Waste Management)","Garbage"))
cleanDataSet1 <- cleanDataSet1 %>% mutate(Category = replace(Category,Category == "Garbage (Solid Waste Management)","Garbage"))
######################################
        #Data Cleaning
######################################

categoriesCol <- cleanDataSet1$Category
categoriesFactor <- as.factor(categoriesCol) 
categoriesFactor # Category Column as a Factor (60 Levels)

subcategoriesCol <- cleanDataSet1$Subcategory
subcategoriesFactor <- as.factor(subcategoriesCol)
subcategoriesFactor # Sub-Category Column as a Factor (299 Levels)

mediaCol <- cleanDataSet1$Media
mediaFactor <- as.factor(mediaCol)
mediaFactor # Media Column as a Factor (18 Levels)

wardOfficeCol <- cleanDataSet1$Ward.Office
wardOfficeFactor <- as.factor(wardOfficeCol)
wardOfficeFactor # Media Column as a Factor (18 Levels)

areaCol <- cleanDataSet1$Area
areaColFactor <- as.factor(areaCol)
summary(areaColFactor)

mediaCol <- cleanDataSet1$Media
mediaFactor <- as.factor(mediaCol)
mediaFactor

statusCol <- cleanDataSet1$Status
statusColFactor <- as.factor(statusCol)
statusColFactor

######################################
#Adding Media Type Column
######################################

mediaType <- c()
for(i in 1:length(mediaFactor)){
  if(mediaFactor[i] == "Facebook" || mediaFactor[i] == "Whatsapp"
     || mediaFactor[i] == "Swachhata App" ||  mediaFactor[i] == "Twitter" | 
     mediaFactor[i] == "E-mail" || mediaFactor[i] ==  "Aaple Sarkar" ||
     mediaFactor[i] == "Prime Minster-Complaint Portal"){
        mediaType[i] = "Online"
  }else{
    mediaType[i] = "Offline"
  }
}


mediaType <- as.factor(mediaType)
cleanDataSet1 <- cbind(cleanDataSet1 , mediaType)

onlineComplaints <- filter(cleanDataSet1, mediaType == "Online")
offlineComplaints <- filter(cleanDataSet1, mediaType == "Offline")



######################################
    #Count of each Category Level
######################################

categoryCountDF <- plyr::count(categoriesFactor) #noise (W46666)
subCategoryCountDF <- plyr::count(subcategoriesFactor)  #noise
mediaCategoryCountDF <- plyr::count(mediaFactor) #noise (CLOSED)
wardOfficeCategoryCountDF <- plyr::count(wardOfficeFactor) #noise present (dates)
areaCountDF <- plyr::count(areaColFactor)
mediaCountDF <- plyr::count(mediaFactor)
statusCountDF <- plyr::count(statusColFactor)


#bar charts
barplot(categoryCountDF$freq , names.arg=categoryCountDF$x)
barplot(subCategoryCountDF$freq , names.arg=subCategoryCountDF$x)
barplot(mediaCategoryCountDF$freq , names.arg=mediaCategoryCountDF$x)
barplot(wardOfficeCategoryCountDF$freq , names.arg=wardOfficeCategoryCountDF$x)

barplot(categoryCountDF$freq[1],names.arg=categoryCountDF$x[1])

#result <- filter(citizensComplaintDataSet, citizensComplaintDataSet$Category == "Garbage (Solid Waste Management)")
#pieData<- (as.factor(na.omit(result$Subcategory)))
#pieData
#pieDataDF <- plyr::count(pieData)




######################################
            #Functions
######################################

getSubCategoriesNames = function(category){
  key <- category
  result <- filter(cleanDataSet1, cleanDataSet1$Category == key)
  #as data cleaning not complete
  resultSub <- unique(result$Subcategory)
  return(resultSub)
}


getSubCategoriesDF = function(category){
  key <- category
  result <- filter(cleanDataSet1, cleanDataSet1$Category == key)
  resultSC <- as.factor(result$Subcategory)
  resultSCDF <- plyr::count(resultSC)
  return(resultSCDF)
}

getAreaNames = function(category,subcategory){
  key <- category
  result <- filter(cleanDataSet1,
                   cleanDataSet1$Category == key,
                   cleanDataSet1$Subcategory == subcategory)
  resultArea <- as.factor(result$Area)
  #resultAreaDF <- plyr::count(resultArea)
  return(resultArea)
}

getComplaintsByArea = function(category,subcategory,area){
  result <- filter(cleanDataSet1,
                   cleanDataSet1$Category == category,
                   cleanDataSet1$Subcategory == subcategory,
                   cleanDataSet1$Area == area)
  #resultDF <- plyr::count(result)
  return(result)
}

getMediaByCategory = function(category){
  result <- filter(cleanDataSet1,
                   cleanDataSet1$Category == category)
  resultDF <- plyr::count(as.factor(result$Media))
  return(resultDF)
}

getTopFiveCategory = function(){
  result <- arrange(categoryCountDF , desc(freq))
  return(head(result,5))
}

getWardOfficeDF = function(cat){
  result <- filter(cleanDataSet1 , cleanDataSet1$Category == cat)
  wardOfficeDF <- plyr::count(as.factor(result$Ward.Office))
  wardOffices <- unique(result$Ward.Office)
  data <- filter(result , result$Ward.Office == wardOffices)
  return(data.frame(wardOfficesDF,data))
}


getMapData = function(cat){
  result <- filter(cleanDataSet1 , cleanDataSet1$Category == cat)
  wardOfficeDF <- plyr::count(as.factor(result$Ward.Office))
  df <- data.frame(lat=0,long=0)
  for(i in 1:nrow(wardOfficeDF)){
    coords <- result %>% filter(result$Ward.Office == wardOfficeDF$x[i]) %>% head(1) %>% select(lat,long)
    df <- rbind(df,coords)
  }
  resultDF <- data.frame(wardOfficeDF,tail(df,nrow(df)-1))
  return(resultDF)
}


getMapDataSC = function(cat,subcat){
  result <- filter(cleanDataSet1 , cleanDataSet1$Category == cat, cleanDataSet1$Subcategory == subcat)
  wardOfficeDF <- plyr::count(as.factor(result$Ward.Office))
  df <- data.frame(lat=0,long=0)
  for(i in 1:nrow(wardOfficeDF)){
    coords <- result %>% filter(result$Ward.Office == wardOfficeDF$x[i]) %>% head(1) %>% select(lat,long)
    df <- rbind(df,coords)
  }
  resultDF <- data.frame(wardOfficeDF,tail(df,nrow(df)-1))
  return(resultDF)
}

getMissingAreas = function(){
  result <- filter(cleanDataSet1 , cleanDataSet1$Area == "I dont know" | cleanDataSet1$Area == "I don't know" )
  return(result)
}

getMissingOffices = function(){
  result <- filter(cleanDataSet1 , cleanDataSet1$Ward.Office == "Head Office")
  return(result)
}


getMediaTypes = function(cat){
  result <- filter(cleanDataSet1,cleanDataSet1$Category == cat)
  return(plyr::count(result$mediaType))
}

