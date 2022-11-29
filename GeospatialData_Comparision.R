library(ggplot2)
library(ggthemes)

library(reshape2)
library(dplyr)

library(RColorBrewer)

library(stringr)
library(scales)

library(viridis)

## loading data
setwd("D:/GFZ/OpenEM/SurveyReport")
df.raw = read.csv2("./Data/SurveyResp_20221117.csv")


### 9) Comparing type of geospatial data used and produced ###################
#loading data
df.usr = df.raw[df.raw$You.are.primarily...=="a user of geospatial data",c(1,14)]
df.prd = df.raw[df.raw$You.are.primarily...=="a producer / provider of geospatial data",c(1,53)]
#renaming for clarity
names(df.usr)
names(df.usr)<- c("RowID","GeospatialData")
names(df.prd)<- c("RowID","GeospatialData")
#adding group field
df.usr$Group = "User"
df.prd$Group = "Producer / provider"

#combining
df.comb = rbind(df.usr,df.prd)

#there are multiple answers which we will decompose to get an overview
df.new = data.frame(Group=character(),
                    TypeOfDataUsed=character())

for (i in 1:nrow(df.comb)){
  print(i)
  
  temp_split = strsplit(df.comb[i,]$GeospatialData,split=";")[[1]]
  for (j in 1:length(temp_split)){
    
    #print(j)
    #in some cases, the first letter of the string was a space, so this needs to be corrected
    tmp_str = temp_split[j]
    if (substr(tmp_str,1,1)== " "){
      tmp_str = substr(tmp_str,2,nchar(tmp_str))
    }
    
    
    df.tmp = data.frame(Group=df.comb$Group[i],TypeOfDataUsed=tmp_str)
    df.new = rbind(df.new,df.tmp)
  }
}

names(df.new)

#counting the various occurrences based on the data so it can be used for plotting later
df.new <- df.new %>% 
  group_by(Group,TypeOfDataUsed) %>% summarize(count=n()) 

#doing the lazy way
total_usr = sum(df.new$count[df.new$Group=="User" ])
total_prd = sum(df.new$count[df.new$Group=="Producer / provider" ])
#actual number of participants
total_usr_participants = 82
total_prd_participants = 32

#percentages calculation
df.new$percent <-NA
df.new$percent2 <-NA

for (i in 1:nrow(df.new)){
  
  if (df.new$Group[i]=="User" ){
    df.new$percent[i]=df.new$count[i]/total_usr_participants
    df.new$percent2[i] = paste0( percent(df.new$percent[i],1))
  } else {
    df.new$percent[i]=df.new$count[i]/total_prd_participants
    df.new$percent2[i] = paste0( percent(df.new$percent[i],1))
  }
}

df.new$TypeOfDataUsed_Wrapped = str_wrap(df.new$TypeOfDataUsed,25)


names(df.new)

gplot <- ggplot(df.new,aes(x=TypeOfDataUsed_Wrapped,y=percent,fill=Group))+
  geom_bar(stat = "identity", position = 'dodge')+
  geom_text(aes(label = percent2), vjust =-0.1,
            position = position_dodge(.9))+
  #sets y axis to be a percentage format
  scale_y_continuous(labels = scales::percent)+
  #sets overall theme
  theme_hc()+
  #removes axis titles
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())+
  #rotates axis text
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        text=element_text(size=15))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="What type of geospatial data do you produce / provide?")

gplot

write.csv2(df.new,"./Plotsdata/Q09_Comparison_GeoSpatialDataUsed.csv")


# Saving plot
png(file="./Plots/Q09_Comparison_GeoSpatialDataUsed.png", units="in", width=12, height=6, res=600)

print(gplot)
dev.off()





### 10)Comparing the scale between users and producers ####

#loading data
df.usr = df.raw[df.raw$You.are.primarily...=="a user of geospatial data",c(1,17)]
df.prd = df.raw[df.raw$You.are.primarily...=="a producer / provider of geospatial data",c(1,56)]
#renaming for clarity
names(df.usr)
names(df.usr)<- c("RowID","GeographicScale")
names(df.prd)<- c("RowID","GeographicScale")
#adding group field
df.usr$Group = "User"
df.prd$Group = "Producer / provider"

#combining
df.comb = rbind(df.usr,df.prd)

#there are multiple answers which we will decompose to get an overview
df.new = data.frame(Group=character(),
                    GeoScale=character())

for (i in 1:nrow(df.comb)){
  print(i)
  
  temp_split = strsplit(df.comb[i,]$GeographicScale,split=";")[[1]]
  for (j in 1:length(temp_split)){
    
    #print(j)
    #in some cases, the first letter of the string was a space, so this needs to be corrected
    tmp_str = temp_split[j]
    if (substr(tmp_str,1,1)== " "){
      tmp_str = substr(tmp_str,2,nchar(tmp_str))
    }
    
    
    df.tmp = data.frame(Group=df.comb$Group[i],GeoScale=tmp_str)
    df.new = rbind(df.new,df.tmp)
  }
}


names(df.new)

#counting the various occurrences based on the data so it can be used for plotting later
df.new <- df.new %>% 
  group_by(Group,GeoScale) %>% summarize(count=n()) 

#doing the lazy way
total_usr = sum(df.new$count[df.new$Group=="User" ])
total_prd = sum(df.new$count[df.new$Group=="Producer / provider" ])
#actual number of participants
total_usr_participants = 82
total_prd_participants = 32

#percentages calculation
df.new$percent <-NA
df.new$percent2 <-NA

for (i in 1:nrow(df.new)){
  
  if (df.new$Group[i]=="User" ){
    df.new$percent[i]=df.new$count[i]/total_usr_participants
    df.new$percent2[i] = paste0( percent(df.new$percent[i],1))
  } else {
    df.new$percent[i]=df.new$count[i]/total_prd_participants
    df.new$percent2[i] = paste0( percent(df.new$percent[i],1))
  }
}


names(df.new)

gplot <- ggplot(df.new,aes(x=GeoScale,y=percent,fill=Group))+
  geom_bar(stat = "identity", position = 'dodge')+
  geom_text(aes(label = percent2), vjust =-0.1,
            position = position_dodge(.9))+
  #sets y axis to be a percentage format
  scale_y_continuous(labels = scales::percent)+
  #sets overall theme
  theme_hc()+
  #removes axis titles
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())+
  #rotates axis text
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        text=element_text(size=15),
        plot.title = element_text(hjust = 0.5))+
  labs(title="At what level of scale do you primarily produce / provide (or use) geospatial data?")
  
gplot


#saving data structure to file
write.csv2(df.new,"./Plotsdata/Q10_Comparison_GeoScale.csv")


# Saving plot
png(file="./Plots/Q10_Comparison_GeoScale.png", units="in", width=12, height=6, res=600)

print(gplot)
dev.off()



### 11) Comparing the important features of geospatial data ####

#loading data
df.usr = df.raw[df.raw$You.are.primarily...=="a user of geospatial data",c(1,19:28)]
df.prd = df.raw[df.raw$You.are.primarily...=="a producer / provider of geospatial data",c(1,58:67)]

#Cleaning the column names
names(df.usr) <- gsub("How.important.are.the.following.features.of.geospatial.data.to.you...","",names(df.usr))
names(df.prd) <- gsub("From.your.point.of.view..what.are.important.features.of.geospatial.data.for.the.users...","",names(df.prd))

#melting to adapt the data structure to be used for ggplot and analysis
df.usr.melt = melt(df.usr,id=c("RowID"))
df.prd.melt = melt(df.prd,id=c("RowID"))

#creating a clean variable name
df.usr.melt$variable2 = str_replace_all(df.usr.melt$variable,"."," ")
df.usr.melt$variable2 = gsub("."," ",as.character(df.usr.melt$variable),fixed=TRUE)

df.prd.melt$variable2 = str_replace_all(df.prd.melt$variable,"."," ")
df.prd.melt$variable2 = gsub("."," ",as.character(df.prd.melt$variable),fixed=TRUE)

#re-ordering factor list to improve visualization
#re-ordering the list of responses
df.usr.melt$value2 = factor(df.usr.melt$value,                 # Relevel group factor
                        levels = c("Extremely important",
                                   "Very important",
                                   "Important",
                                   "Slightly important",
                                   "Not important",
                                   "No opinion"))

df.prd.melt$value2 = factor(df.prd.melt$value,                 # Relevel group factor
                            levels = c("Extremely important",
                                       "Very important",
                                       "Important",
                                       "Slightly important",
                                       "Not important",
                                       "No opinion"))

#creating a grouping column so we can have one single dataframe with all the answers
df.usr.melt$Group = "User"
df.prd.melt$Group = "Producer / Provider"


df.comb = rbind(df.usr.melt,df.prd.melt)

#in this case we have also to summarize per group each of the questions
#counting the various occurrences based on the data so it can be used for plotting later
names(df.comb)
df.new <- df.comb %>% 
  group_by(Group,variable2,value2) %>% summarize(count=n())

#actual number of participants
total_usr_participants = 82
total_prd_participants = 32

#percentages calculation
df.new$percent <-NA
df.new$percent2 <-NA

for (i in 1:nrow(df.new)){
  
  if (df.new$Group[i]=="User" ){
    df.new$percent[i]=df.new$count[i]/total_usr_participants
    df.new$percent2[i] = paste0( percent(df.new$percent[i],1))
  } else {
    df.new$percent[i]=df.new$count[i]/total_prd_participants
    df.new$percent2[i] = paste0( percent(df.new$percent[i],1))
  }
}

#adding a wrap feature to the x axis labels
df.new$variable2_wrap = str_wrap(df.new$variable2, width = 20) 

as.character(unique(df.new$value2))

#removing labels for very small values or less important to improve visualization
df.new$percent2_sel = df.new$percent2
df.new$percent2_sel[df.new$value2=="No opinion" ]             <- NA
df.new$percent2_sel[df.new$value2=="Not important"  ]         <- NA
df.new$percent2_sel[df.new$value2=="Slightly important" ]     <- NA

head(df.new)

gplot <- ggplot(df.new,aes(x=Group,y=percent,fill=value2))+
  geom_bar(position = 'stack',stat="identity")+
  
  geom_text(aes(label = percent2_sel),position = position_stack(vjust = 0.5))+
  
  #sets y axis as a percentage scale
  scale_y_continuous(labels = scales::percent)+
  
  #wraps into two columns based on the question
  facet_wrap(. ~ variable2_wrap, ncol=5)+
  #sets the colors based on a given pallet

  scale_fill_brewer(palette="RdYlGn")+
  #sets overall theme
  theme_minimal()+
  #removes axis titles
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title=element_blank(),
        legend.position = "bottom",
        text=element_text(size=15),
        plot.title = element_text(hjust = 0.5))+
  
  guides(fill=guide_legend(nrow=1,byrow=TRUE))+
  labs(title="What are the important features of geospatial data?")

gplot

#saving data structure to file
write.csv2(df.new,"./Plotsdata/Q11_Comparison_FeaturesOfGeospatialData.csv")


# Saving plot
png(file="./Plots/Q11_Comparison_FeaturesOfGeospatialData.png", units="in", width=12, height=6, res=600)

print(gplot)
dev.off()



### 12) Comparing the how problematic do users find the following features of geospatial data? ####

#loading data
df.usr = df.raw[df.raw$You.are.primarily...=="a user of geospatial data",c(1,30:34)]
df.prd = df.raw[df.raw$You.are.primarily...=="a producer / provider of geospatial data",c(1,69:73)]

#Cleaning the column names
names(df.usr) <- gsub("How.problematic.do.you.find.the.following.features.of.geospatial.data...","",names(df.usr))
names(df.prd) <- gsub("From.your.point.of.view..how.problematic.do.users.find.the.following.features.of.geospatial.data...","",names(df.prd))

#melting to adapt the data structure to be used for ggplot and analysis
df.usr.melt = melt(df.usr,id=c("RowID"))
df.prd.melt = melt(df.prd,id=c("RowID"))

#creating a clean variable name
df.usr.melt$variable2 = str_replace_all(df.usr.melt$variable,"."," ")
df.usr.melt$variable2 = gsub("."," ",as.character(df.usr.melt$variable),fixed=TRUE)

df.prd.melt$variable2 = str_replace_all(df.prd.melt$variable,"."," ")
df.prd.melt$variable2 = gsub("."," ",as.character(df.prd.melt$variable),fixed=TRUE)


unique(df.prd.melt$value)

#re-ordering factor list to improve visualization
#re-ordering the list of responses
df.usr.melt$value2 = factor(df.usr.melt$value,                 # Relevel group factor
                            levels = c("Extremely problematic",
                                       "Very problematic",
                                       "Problematic",
                                       "Slightly problematic",
                                       "Not problematic",
                                       "No opinion"))

df.prd.melt$value2 = factor(df.prd.melt$value,                 # Relevel group factor
                            levels = c("Extremely problematic",
                                       "Very problematic",
                                       "Problematic",
                                       "Slightly problematic",
                                       "Not problematic",
                                       "No opinion"))

#creating a grouping column so we can have one single dataframe with all the answers
df.usr.melt$Group = "User"
df.prd.melt$Group = "Producer / Provider"


df.comb = rbind(df.usr.melt,df.prd.melt)

#in this case we have also to summarize per group each of the questions
#counting the various occurrences based on the data so it can be used for plotting later
names(df.comb)
df.new <- df.comb %>% 
  group_by(Group,variable2,value2) %>% summarize(count=n())

#actual number of participants
total_usr_participants = 82
total_prd_participants = 32


#percentages calculation
df.new$percent <-NA
df.new$percent2 <-NA

for (i in 1:nrow(df.new)){
  
  if (df.new$Group[i]=="User" ){
    df.new$percent[i]=df.new$count[i]/total_usr_participants
    df.new$percent2[i] = paste0( percent(df.new$percent[i],1))
  } else {
    df.new$percent[i]=df.new$count[i]/total_prd_participants
    df.new$percent2[i] = paste0( percent(df.new$percent[i],1))
  }
}

#adding a wrap feature to the x axis labels
df.new$variable2_wrap = str_wrap(df.new$variable2, width = 20) 

as.character(unique(df.new$value2))

#removing labels for very small values or less important to improve visualization
df.new$percent2_sel = df.new$percent2
df.new$percent2_sel[df.new$value2=="No opinion" ]             <- NA
#df.new$percent2_sel[df.new$value2=="Not problematic"   ]         <- NA
#df.new$percent2_sel[df.new$value2=="Slightly problematic" ]     <- NA



gplot <- ggplot(df.new,aes(x=Group,y=percent,fill=value2))+
  geom_bar(position = 'stack',stat="identity")+
  
  geom_text(aes(label = percent2_sel),position = position_stack(vjust = 0.5))+
  
  #sets y axis as a percentage scale
  scale_y_continuous(labels = scales::percent)+
  
  #wraps into two columns based on the question
  facet_wrap(. ~ variable2_wrap, ncol=5)+
  #sets the colors based on a given pallet
  
  scale_fill_brewer(palette="RdYlGn")+
  #sets overall theme
  theme_minimal()+
  #removes axis titles
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title=element_blank(),
        legend.position = "bottom",
        text=element_text(size=15),
        plot.title = element_text(hjust = 0.5))+

  labs(title="How problematic you find the following features of geospatial data?")+
  
  guides(fill=guide_legend(nrow=1,byrow=TRUE))

gplot


#saving data structure to file
write.csv2(df.new,"./Plotsdata/Q12_Comparison_ProblematicOfGeospatialData.csv")


# Saving plot
png(file="./Plots/Q12_Comparison_ProblematicOfGeospatialData.png", units="in", width=12, height=6, res=600)

print(gplot)
dev.off()







### 13) Comparing the how user users find the geospatial data you produce / provide? ################



#loading data
df.usr = df.raw[df.raw$You.are.primarily...=="a user of geospatial data",c(1,36)]
df.prd = df.raw[df.raw$You.are.primarily...=="a producer / provider of geospatial data",c(1,75)]

names(df.usr)
names(df.prd)

#melting to adapt the data structure to be used for ggplot and analysis
df.usr.melt = melt(df.usr,id=c("RowID"))
df.prd.melt = melt(df.prd,id=c("RowID"))

#creating a clean variable name
df.usr.melt$variable2 = "What is your favorite approach to finding geospatial data?"
df.prd.melt$variable2 = "How can users find the geospatial data you produce / provide?"

#equivalent variable name
df.usr.melt$Group = "User"
df.prd.melt$Group = "Producer / provider"

#combinging
df.comb = rbind(df.usr.melt,df.prd.melt)


#there are multiple answers which we will decompose to get an overview
df.new = data.frame(Group=character(),
                    PreferredMethod=character())

for (i in 1:nrow(df.comb)){
  print(i)
  
  temp_split = strsplit(df.comb[i,]$value,split=";")[[1]]
  for (j in 1:length(temp_split)){
    
    #print(j)
    #in some cases, the first letter of the string was a space, so this needs to be corrected
    tmp_str = temp_split[j]
    if (substr(tmp_str,1,1)== " "){
      tmp_str = substr(tmp_str,2,nchar(tmp_str))
    }
    
    
    df.tmp = data.frame(Group=df.comb$Group[i],PreferredMethod=tmp_str)
    df.new = rbind(df.new,df.tmp)
  }
}



#checking for errors
unique(df.new$PreferredMethod)

#and nnow we count each event and summarise
df.new.sum <- df.new %>% group_by(Group,PreferredMethod) %>% summarize(count=n())


#actual number of participants
total_usr_participants = 82
total_prd_participants = 32

#percentages calculation
df.new.sum$percent <-NA
df.new.sum$percent2 <-NA

for (i in 1:nrow(df.new.sum)){
  
  if (df.new.sum$Group[i]=="User" ){
    df.new.sum$percent[i]=df.new.sum$count[i]/total_usr_participants
    df.new.sum$percent2[i] = paste0( percent(df.new.sum$percent[i],1))
  } else {
    df.new.sum$percent[i]=df.new.sum$count[i]/total_prd_participants
    df.new.sum$percent2[i] = paste0( percent(df.new.sum$percent[i],1))
  }
}


#re-organizing the factor levels
unique(df.new.sum$PreferredMethod)

#minor correction 
df.new.sum$PreferredMethod[df.new.sum$PreferredMethod=="Through commercial data provider"] <- "Through a commercial data provider"
unique(df.new.sum$PreferredMethod)


df.new.sum$PreferredMethod_sort <- factor(df.new.sum$PreferredMethod,
                                          levels=c("Through web search (e.g. via Google, Bing, Ecosia)",
                                                   "On a website",
                                                   "Through geospatial catalogs (e.g. geoportal)",
                                                   "Through open repositories (e.g. zenodo)",
                                                   "Through data hubs (e.g. Copernicus Open Access Hub, Sentinel Hub)",
                                                   "Through a commercial data provider",
                                                   "I do not search for data myself",
                                                   "Other"))

df.new.sum$PreferredMethod_sort_wrap <- str_wrap(df.new.sum$PreferredMethod_sort,20)

#saving data structure to file
write.csv2(df.new.sum,"./Plotsdata/Q13_FindingData.csv")


#the plots work better if separated


plt.fieldapp <- 
  ggplot(df.new.sum, aes(x="", y=percent, fill=PreferredMethod_sort_wrap)) +
  geom_bar(stat="identity",width=1,color="white")+
  facet_grid(.~Group)+
  coord_polar("y", start=0,direction=-1)+
  #sets y axis as a percentage scale
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(label = percent2,x=1.3),
            position = position_stack(vjust = 0.5)) +
  
  scale_fill_brewer(palette="Spectral")+
  theme_void()+
  theme(legend.position="right",
        legend.title=element_blank(),
        text=element_text(size=15)
  )+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="What is your field of application?")

plt.fieldapp


