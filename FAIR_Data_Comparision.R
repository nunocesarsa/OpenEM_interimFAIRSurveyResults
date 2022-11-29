library(ggplot2)
library(ggthemes)
library(gridExtra)

library(reshape2)
library(dplyr)

library(RColorBrewer)

library(stringr)
library(scales)

library(viridis)

## loading data
setwd("D:/GFZ/OpenEM/SurveyReport")
df.raw = read.csv2("./Data/SurveyResp_20221117.csv")


### 14) How familiar are you with the FAIR data principles? ############

#loading data
df.usr = df.raw[df.raw$You.are.primarily...=="a user of geospatial data",c(1,38)]
df.prd = df.raw[df.raw$You.are.primarily...=="a producer / provider of geospatial data",c(1,77)]

#melting to adapt the data structure to be used for ggplot and analysis
df.usr.melt = melt(df.usr,id=c("RowID"))
df.prd.melt = melt(df.prd,id=c("RowID"))

#creating a clean variable name
df.usr.melt$variable2 = str_replace_all(df.usr.melt$variable,"."," ")
df.usr.melt$variable2 = gsub("."," ",as.character(df.usr.melt$variable),fixed=TRUE)

df.prd.melt$variable2 = unique(df.usr.melt$variable2)

unique(df.prd.melt$value)
unique(df.usr.melt$value)

#correcting error in responses
df.usr.melt$value[df.usr.melt$value  == 'Extremly familar'] <- 'Extremely familiar'
df.prd.melt$value[df.prd.melt$value  == "Very" ] <- "Very familiar"

#re-ordering factor list to improve visualization
#re-ordering the list of responses
df.usr.melt$value2 = factor(df.usr.melt$value,                 # Relevel group factor
                            levels = c("Extremely familiar",
                                       "Very familiar",
                                       "Familiar",
                                       "Slightly familiar",
                                       "Not familiar",
                                       "No opinion"))

df.prd.melt$value2 = factor(df.prd.melt$value,                 # Relevel group factor
                            levels = c("Extremely familiar",
                                       "Very familiar",
                                       "Familiar",
                                       "Slightly familiar",
                                       "Not familiar",
                                       "No opinion"))

#creating a grouping column so we can have one single dataframe with all the answers
df.usr.melt$Group = "User"
df.prd.melt$Group = "Producer / Provider"


df.comb = rbind(df.usr.melt,df.prd.melt)

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

#inverting the colors
colours= brewer.pal(name="RdYlGn", n=nlevels(df.new$value2))
names(colours)= rev(levels(df.new$value2))


#plotting
gplot <- ggplot(df.new,aes(x=Group,y=percent,fill=value2))+
  geom_bar(position = 'stack',stat="identity")+
  
  geom_text(aes(label=percent2), position = position_stack(vjust = 0.5)) +
  
  #sets y axis as a percentage scale
  scale_y_continuous(labels = scales::percent)+
  
  #wraps into two columns based on the question
 # facet_wrap(. ~ variable2, ncol=5)+
  #sets the colors based on a given pallet
  
  #scale_fill_brewer(palette="RdYlGn")+
  scale_fill_manual(values=colours)+

  #sets overall theme
  theme_minimal()+
  #removes axis titles
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title=element_blank(),
        legend.position = "bottom",
        text=element_text(size=15))+
  
  guides(fill=guide_legend(nrow=1,byrow=TRUE))+
  
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="How familiar are you with the FAIR data principles?")

gplot


#saving data structure to file
write.csv2(df.new,"./Plotsdata/Q14_Comparison_FairDataFamiliarity.csv")


# Saving plot
png(file="./Plots/Q14_Comparison_FairDataFamiliarity.png", units="in", width=12, height=6, res=600)

print(gplot)
dev.off()



### 15) How important are the following FAIR principles for you? ############

#loading data
df.usr = df.raw[df.raw$You.are.primarily...=="a user of geospatial data",c(1,39:44)]
df.prd = df.raw[df.raw$You.are.primarily...=="a producer / provider of geospatial data",c(1,78:83)]

#Cleaning the column names
names(df.usr) <- gsub("How.important.are.the.following.FAIR.principles.for.you...","",names(df.usr))
names(df.prd) <- gsub("How.important.are.the.following.FAIR.principles.to.you...","",names(df.prd))

#melting to adapt the data structure to be used for ggplot and analysis
df.usr.melt = melt(df.usr,id=c("RowID"))
df.prd.melt = melt(df.prd,id=c("RowID"))

unique(df.usr.melt$variable)
unique(df.prd.melt$variable)

#creating a clean variable name
df.usr.melt$variable2 = str_replace_all(df.usr.melt$variable,"."," ")
df.usr.melt$variable2 = gsub("."," ",as.character(df.usr.melt$variable),fixed=TRUE)

df.prd.melt$variable2 = str_replace_all(df.prd.melt$variable,"."," ")
df.prd.melt$variable2 = gsub("."," ",as.character(df.prd.melt$variable),fixed=TRUE)

unique(df.prd.melt$value)
unique(df.usr.melt$value)

unique(df.prd.melt$variable2)
unique(df.usr.melt$variable2)

#corrections of some errors in the data
df.usr.melt$value[df.usr.melt$value  == 'Extremly important'] <- 'Extremely important'
df.usr.melt$variable2[df.usr.melt$variable2  == "Data must be interoperable with other data sets "] <- "Data must be interoperable with other data" 
df.usr.melt$variable2[df.usr.melt$variable2  == "Data must be reusable " ] <- "Data must be reusable"
df.prd.melt$variable2[df.prd.melt$variable2  == "Dat must be reusable"   ] <- "Data must be reusable"

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


gplot <- ggplot(df.new,aes(x=Group,y=percent,fill=value2))+
  geom_bar(position = 'stack',stat="identity")+
  
  geom_text(aes(label = percent2_sel),position = position_stack(vjust = 0.5))+
  
  #sets y axis as a percentage scale
  scale_y_continuous(labels = scales::percent)+
  
  #wraps into two columns based on the question
  facet_wrap(. ~ variable2_wrap, ncol=6)+
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
  labs(title="How important are the following FAIR principles to you?")

gplot

#saving data structure to file
write.csv2(df.new,"./Plotsdata/Q15_Comparison_FairDataPrinciplesImportance.csv")


# Saving plot
png(file="./Plots/Q15_Comparison_FairDataPrinciplesImportance.png", units="in", width=14, height=6, res=600)

print(gplot)
dev.off()





### 15) Are you providing FAIR data? / Have you already worked with FAIR data?

#loading data
df.usr = df.raw[df.raw$You.are.primarily...=="a user of geospatial data",c(1,48)]
df.prd = df.raw[df.raw$You.are.primarily...=="a producer / provider of geospatial data",c(1,87)]


### 16)  Are you providing/using fair data?  ############

#loading data
df.usr = df.raw[df.raw$You.are.primarily...=="a user of geospatial data",c(1,45)]
df.prd = df.raw[df.raw$You.are.primarily...=="a producer / provider of geospatial data",c(1,84)]

#melting
df.usr.melt = melt(df.usr,id=c("RowID"))
df.prd.melt = melt(df.prd,id=c("RowID"))


#creating a clean variable name
df.prd.melt$variable2 = "Are you providing FAIR data?"
df.usr.melt$variable2 = "Have you already worked with FAIR data?"

unique(df.prd.melt$value)
unique(df.usr.melt$value)

#correcting error in responses
df.usr.melt$value[df.usr.melt$value  == "I don’t kn"] <- "I don't know" 


#re-ordering factor list to improve visualization
#re-ordering the list of responses
df.usr.melt$value2 = factor(df.usr.melt$value,                 # Relevel group factor
                            levels = c("Yes",
                                       "No" ,
                                       "I don't know"))

df.prd.melt$value2 = factor(df.prd.melt$value,                 # Relevel group factor
                            levels = c("Yes",
                                       "No" ,
                                       "Partially",
                                       "I don't know"))


#creating a grouping column so we can have one single dataframe with all the answers
df.usr.melt$Group = "User"
df.prd.melt$Group = "Producer / Provider"


df.comb = rbind(df.usr.melt,df.prd.melt)

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



#in this case we have to divide the plot into two and combine with grid_extra


prd.df = df.new[df.new$Group== "Producer / Provider",]
prd.df$value2 = factor(prd.df$value2,                 # Relevel group factor
                       levels = c("Yes",
                                  "Partially",
                                  "No" ,
                                  "I don't know"))

usr.df = df.new[df.new$Group!= "Producer / Provider",]
usr.df$value2 = factor(usr.df$value2,                 # Relevel group factor
                       levels = c("Yes",
                                  "No" ,
                                  "I don't know"))



prod_plt = ggplot(prd.df,aes(x=Group,y=percent,fill=value2))+
  geom_bar(position = 'stack',stat="identity")+
  
  geom_text(aes(label = percent2),position = position_stack(vjust = 0.5))+
  
  #sets y axis as a percentage scale
  scale_y_continuous(labels = scales::percent)+
  
  #wraps into two columns based on the question
  facet_wrap(. ~ variable2, ncol=5)+
  #sets the colors based on a given pallet
  
  #scale_fill_brewer(palette="RdYlGn")+
  scale_fill_manual(values=c("#008000",
                             "#FFD700",
                             "#FF0000",
                             "#D3D3D3"
                             ))+
  #sets overall theme
  theme_minimal()+
  #removes axis titles
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title=element_blank(),
        legend.position = "bottom",
        text=element_text(size=15),
        plot.title = element_text(hjust = 0.5))+
  
  guides(fill=guide_legend(nrow=1,byrow=TRUE))
  #labs(title="Have you already worked with or are you providing FAIR data?")

prod_plt

user_plt = ggplot(usr.df,aes(x=Group,y=percent,fill=value2))+
  geom_bar(position = 'stack',stat="identity")+
  
  geom_text(aes(label = percent2),position = position_stack(vjust = 0.5))+
  
  #sets y axis as a percentage scale
  scale_y_continuous(labels = scales::percent)+
  
  #wraps into two columns based on the question
  facet_wrap(. ~ variable2, ncol=5)+
  #sets the colors based on a given pallet
  
  #scale_fill_brewer(palette="RdYlGn")+
  scale_fill_manual(values=c("#008000",
                             "#FF0000",
                             "#D3D3D3"
  ))+
  #sets overall theme
  theme_minimal()+
  #removes axis titles
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title=element_blank(),
        legend.position = "bottom",
        text=element_text(size=15),
        plot.title = element_text(hjust = 0.5))+
  
  guides(fill=guide_legend(nrow=1,byrow=TRUE))
#labs(title="Have you already worked with or are you providing FAIR data?")

user_plt


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(prod_plt)


fnl.plt = grid.arrange(arrangeGrob(prod_plt + theme(legend.position="none"),
                       user_plt + theme(legend.position="none"),
                       ncol=2, nrow=1),
                       top=grid::textGrob("Have you already worked with or are you providing FAIR data?"),
                       mylegend, nrow=2,heights=c(10, 1))



#saving data structure to file
write.csv2(df.new,"./Plotsdata/Q16_Comparison_UsedFAIRdata.csv")


# Saving plot
png(file="./Plots/Q16_Comparison_UsedFAIRdata.png", units="in", width=12, height=6, res=600)

print(grid.arrange(arrangeGrob(prod_plt + theme(legend.position="none"),
                               user_plt + theme(legend.position="none"),
                               ncol=2, nrow=1),
                   top=grid::textGrob("Have you already worked with or are you providing FAIR data?"),
                   mylegend, nrow=2,heights=c(10, 1)))
dev.off()



### 17)  From your point of view, is there a difference between FAIR data and open data? ############

#loading data
df.usr = df.raw[df.raw$You.are.primarily...=="a user of geospatial data",c(1,48)]
df.prd = df.raw[df.raw$You.are.primarily...=="a producer / provider of geospatial data",c(1,87)]

#melting
df.usr.melt = melt(df.usr,id=c("RowID"))
df.prd.melt = melt(df.prd,id=c("RowID"))

unique(df.prd.melt$variable)

#creating a clean variable name
df.prd.melt$variable2 = "From your point of view  is there a difference between FAIR data and open data?"
df.usr.melt$variable2 = "From your point of view  is there a difference between FAIR data and open data?"

unique(df.prd.melt$value)
unique(df.usr.melt$value)

#correcting error in responses
df.usr.melt$value[df.usr.melt$value  == "I don’t kn"] <- "I don't know" 

unique(d)

#re-ordering factor list to improve visualization
#re-ordering the list of responses
df.usr.melt$value2 = factor(df.usr.melt$value,                 # Relevel group factor
                            levels = c("Yes, FAIR data is not necessarily open",
                                       "No, FAIR data is always open" ,
                                       "I don't know"))

df.prd.melt$value2 = factor(df.prd.melt$value,                 # Relevel group factor
                            levels = c("Yes, FAIR data is not necessarily open",
                                       "No, FAIR data is always open" ,
                                       "I don't know"))

#creating a grouping column so we can have one single dataframe with all the answers
df.usr.melt$Group = "User"
df.prd.melt$Group = "Producer / Provider"


df.comb = rbind(df.usr.melt,df.prd.melt)

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

#plotting
gplot <- ggplot(df.new,aes(x=Group,y=percent,fill=value2))+
  geom_bar(position = 'stack',stat="identity")+
  
  geom_text(aes(label = percent2),position = position_stack(vjust = 0.5))+
  
  #sets y axis as a percentage scale
  scale_y_continuous(labels = scales::percent)+
  
  #wraps into two columns based on the question
  #facet_wrap(. ~ variable2, ncol=5)+
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
  labs(title="From your point of view, is there a difference between FAIR data and open data?")

gplot


#saving data structure to file
write.csv2(df.new,"./Plotsdata/Q17_Comparison_FAIRvsOpenData.csv")


# Saving plot
png(file="./Plots/Q17_Comparison_FAIRvsOpenData.png", units="in", width=12, height=6, res=600)

print(gplot)
dev.off()

### 18)  What do you think are the biggest barriers to use FAIR data? Select your top 3 ############

#loading data
df.usr = df.raw[df.raw$You.are.primarily...=="a user of geospatial data",c(1,49)]
df.prd = df.raw[df.raw$You.are.primarily...=="a producer / provider of geospatial data",c(1,88)]

#in this case we need to disagregate the answers given that the survey asked for multiple response per submissi

#renaming columns for simplicity
names(df.usr) <- c("RowID","BarrierFair")
names(df.prd) <- c("RowID","BarrierFair")

#disagregating answers

df.usr.new = data.frame(BarriersFAIRdata=character())

for (i in 1:nrow(df.usr)){
  #print(i)
  
  temp_split = strsplit(df.usr$BarrierFair[i],split=";")[[1]]
  for (j in 1:length(temp_split)){
    
    #print(j)
    #in some cases, the first letter of the string was a space, so this needs to be corrected
    tmp_str = temp_split[j]
    if (substr(tmp_str,1,1)== " "){
      tmp_str = substr(tmp_str,2,nchar(tmp_str))
    }
    
    
    df.tmp = data.frame(BarriersFAIRdata=tmp_str)
    df.usr.new = rbind(df.usr.new,df.tmp)
  }
}

df.prd.new = data.frame(BarriersFAIRdata=character())

for (i in 1:nrow(df.prd)){
  #print(i)
  
  temp_split = strsplit(df.prd$BarrierFair[i],split=";")[[1]]
  for (j in 1:length(temp_split)){
    
    #print(j)
    #in some cases, the first letter of the string was a space, so this needs to be corrected
    tmp_str = temp_split[j]
    if (substr(tmp_str,1,1)== " "){
      tmp_str = substr(tmp_str,2,nchar(tmp_str))
    }
    
    
    df.tmp = data.frame(BarriersFAIRdata=tmp_str)
    df.prd.new = rbind(df.prd.new,df.tmp)
  }
}

df.usr.new$Group <- "User"
df.prd.new$Group <- "Producer / provider"

unique(df.usr.new$BarriersFAIRdata)
unique(df.prd.new$BarriersFAIRdata)

#correcting errors
df.usr.new$BarriersFAIRdata[df.usr.new$BarriersFAIRdata  == "I don’t kn"] <- "I don't know" 


#and nnow we count each event and summarise
df.usr.new.sel <- df.usr.new %>% group_by(BarriersFAIRdata,Group) %>% summarize(count=n())
df.prd.new.sel <- df.prd.new %>% group_by(BarriersFAIRdata,Group) %>% summarize(count=n())

df.new = rbind(df.usr.new.sel,df.prd.new.sel)


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

#placeholder for text size on both graphics
overall_text = 15


#wrapping to cretea a smaller legend
df.new$BarriersFAIRdata_wrap = str_wrap(df.new$BarriersFAIRdata,30)


#user only group
df.new.usr = df.new[df.new$Group=="User",]


#re-ordering responses according to biggest pie first

unique(df.new.usr$BarriersFAIRdata_wrap)
df.new.usr$BarriersFAIRdata_wrap_ord <- factor(df.new.usr$BarriersFAIRdata_wrap,  
                                               levels = c("Lack of awareness of which\ndata sets are FAIR (e.g.\nmissing label or identifier).",
                                                          "Lack of knowledge about the\nbenefits of FAIR data.",
                                                          "I don't know",
                                                          "FAIR data is not necessarily\nopen.",
                                                          "Lack of guidelines how to use\nFAIR data.",
                                                          "Concerns about licensing.",
                                                          "Missing technical solutions\n(e.g. missing tools to read\ndata).") )

#creating plot object
gplot_user = ggplot( df.new.usr,aes(x="" ,y=percent,fill=BarriersFAIRdata_wrap_ord))+
  geom_bar(stat="identity",width=1,color="white")+
  coord_polar("y", start=0,direction=-1)+
  #sets y axis as a percentage scale
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(label = percent2,x=1.3),
            position = position_stack(vjust = 0.5)) +
  
  #scale_fill_brewer(palette="Oranges")+
  scale_fill_manual(values=rev(brewer.pal(7,"Blues")))+
  theme_void()+
  theme(legend.position="right",
        legend.title=element_blank(),
        text=element_text(size=overall_text)
        )+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="a user perspective")
  


gplot_user

#producer only group
df.new.prd = df.new[df.new$Group!="User",]


unique(df.new.prd$BarriersFAIRdata_wrap)
df.new.prd$BarriersFAIRdata_wrap_ord <- factor(df.new.prd$BarriersFAIRdata_wrap,  
                                               levels = c("Lack of resources (e.g.\npersonnel, time)" ,
                                                          "Missing incentives (e.g.\nuncertain benefit, lack of\nrecognition)",
                                                          "Lack of guidelines (e.g.\nguidelines are missing,\nincomplete or unclear)",
                                                          "Disagreement on data use (e.g.\npotential misuse)",
                                                          "Restrictive policies (e.g.\nrestrict data sharing\nguidelines)",
                                                          "Competitive disadvantage\n(e.g. missing business\nopportunities)",
                                                          "Missing technical solutions\n(e.g. missing tools)",
                                                          "I don't know"
                                                           ) )


#creating plot object
gplot_prod = ggplot( df.new.prd,aes(x="" ,y=percent,fill=BarriersFAIRdata_wrap_ord))+
  geom_bar(stat="identity",width=1,color="white")+
  coord_polar("y", start=0,direction=-1)+
  #sets y axis as a percentage scale
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(label = percent2,x=1.3),
            position = position_stack(vjust = 0.5)) +
  
  #scale_fill_brewer(palette="OrRd")+
  scale_fill_manual(values=rev(brewer.pal(8,"OrRd")))+
  theme_void()+
  theme(legend.position="right",
        legend.title=element_blank(),
        text=element_text(size=overall_text)
        )+
  theme(plot.title = element_text(hjust = 0.5))+
  #theme(legend.spacing.y = unit(1, 'cm'))
  labs(title="a producers / providers perspective")

gplot_prod


#savingdata 
#saving data structure to file
write.csv2(df.new,"./Plotsdata/Q18_Comparison_FAIR_Barriers.csv")


# Saving plot
png(file="./Plots/Q18_Comparison_FAIR_Barriers.png", units="in", width=14, height=6, res=600)

print(
  grid.arrange(gplot_prod,gplot_user,nrow=1,
               top=grid::textGrob("Barriers to the use of FAIR data from:",
                                  gp=grid::gpar(fontsize=overall_text+5)))
)
dev.off()



### 19)  What do you think are the biggest barriers to use OPEN data? Select your top 3 ############

#loading data
df.usr = df.raw[df.raw$You.are.primarily...=="a user of geospatial data",c(1,51)]
df.prd = df.raw[df.raw$You.are.primarily...=="a producer / provider of geospatial data",c(1,90)]


#in this case we need to disagregate the answers given that the survey asked for multiple response per submissi

#renaming columns for simplicity
names(df.usr) <- c("RowID","BarrierOPEN")
names(df.prd) <- c("RowID","BarrierOPEN")



#disagregating answers
df.usr.new = data.frame(BarriersOPENdata=character())

for (i in 1:nrow(df.usr)){
  #print(i)
  
  temp_split = strsplit(df.usr$BarrierOPEN[i],split=";")[[1]]
  for (j in 1:length(temp_split)){
    
    #print(j)
    #in some cases, the first letter of the string was a space, so this needs to be corrected
    tmp_str = temp_split[j]
    if (substr(tmp_str,1,1)== " "){
      tmp_str = substr(tmp_str,2,nchar(tmp_str))
    }
    
    
    df.tmp = data.frame(BarriersOPENdata=tmp_str)
    df.usr.new = rbind(df.usr.new,df.tmp)
  }
}

df.prd.new = data.frame(BarriersOPENdata=character())

for (i in 1:nrow(df.prd)){
  #print(i)
  
  temp_split = strsplit(df.prd$BarrierOPEN[i],split=";")[[1]]
  for (j in 1:length(temp_split)){
    
    #print(j)
    #in some cases, the first letter of the string was a space, so this needs to be corrected
    tmp_str = temp_split[j]
    if (substr(tmp_str,1,1)== " "){
      tmp_str = substr(tmp_str,2,nchar(tmp_str))
    }
    
    
    df.tmp = data.frame(BarriersOPENdata=tmp_str)
    df.prd.new = rbind(df.prd.new,df.tmp)
  }
}

df.usr.new$Group <- "User"
df.prd.new$Group <- "Producer / provider"

#checking for errors 
unique(df.usr.new$BarriersOPENdata)
unique(df.prd.new$BarriersOPENdata)

#correcting errors
df.usr.new$BarriersOPENdata[df.usr.new$BarriersOPENdata  == "I don’t kn"] <- "I don't know" 


#and nnow we count each event and summarise
df.usr.new.sel <- df.usr.new %>% group_by(BarriersOPENdata,Group) %>% summarize(count=n())
df.prd.new.sel <- df.prd.new %>% group_by(BarriersOPENdata,Group) %>% summarize(count=n())

df.new = rbind(df.usr.new.sel,df.prd.new.sel)

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


#placeholder for text size on both graphics
overall_text = 15


#wrapping to cretea a smaller legend
df.new$BarriersOPENdata_wrap = str_wrap(df.new$BarriersOPENdata,30)


#user only group
df.new.usr = df.new[df.new$Group=="User",]


#re-ordering responses according to biggest pie first
unique(df.new.usr$BarriersOPENdata_wrap)
df.new.usr$BarriersOPENdata_wrap_ord <- factor(df.new.usr$BarriersOPENdata_wrap,  
                                               levels = c("Lack of continuity (e.g. no\nguaranteed continuation).",
                                                          "Missing support (e.g. lack of\ncontact person for questions).",
                                                          "Lack of standards (e.g.\nunknown quality).",
                                                          "Concerns on metadata (e.g.\nuncomplete, too complex)." ,
                                                          "Legal concerns regarding\nlicensing (e.g. ownership &\ncopyrights are unclear).",
                                                          "Missing technical solutions\n(e.g. lack of tools to process\ndata).",
                                                          "I don't know" ) )

#creating plot object
gplot_user = ggplot( df.new.usr,aes(x="" ,y=percent,fill=BarriersOPENdata_wrap_ord))+
  geom_bar(stat="identity",width=1,color="white")+
  coord_polar("y", start=0,direction=-1)+
  #sets y axis as a percentage scale
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(label = percent2,x=1.3),
            position = position_stack(vjust = 0.5)) +
  
  #scale_fill_brewer(palette="OrRd")+
  scale_fill_manual(values=rev(brewer.pal(7,"Blues")))+
  theme_void()+
  theme(legend.position="right",
        legend.title=element_blank(),
        text=element_text(size=overall_text)
  )+
  theme(plot.title = element_text(hjust = 0.5))+
  #theme(legend.spacing.y = unit(1, 'cm'))
  labs(title="a producers / providers perspective")



gplot_user

#producer only group
df.new.prd = df.new[df.new$Group!="User",]


unique(df.new.prd$BarriersOPENdata_wrap)
df.new.prd$BarriersOPENdata_wrap_ord <- factor(df.new.prd$BarriersOPENdata_wrap,  
                                               levels = c("Competitive disadvantage (e.g.\nmissing business opportunities",
                                                          "Economic disadvantage (e.g.\ncosts to produce data are not\ncovered)" ,
                                                          "Concerns regarding sensible\ndata (e.g. potential misuse)" ,
                                                          "Missing infrastructure (e.g.\nlarge data traffic overloads\nsystem)",
                                                          "Legal concerns regarding\nlicensing (e.g. disagreement\non ownership & copyrights)",
                                                          "Restrictive policies (e.g.\ne.g. restrict data sharing\nguidelines)",
                                                          "Disagreement on data use (e.g.\nuse case is against ethical\nand moral principles of\nproducer)",
                                                          "Legal concerns regarding\nprivacy protection (e.g.\npotential violation of\npersonal rights)" ,
                                                          "I don't know"  
                                               ) )


#creating plot object
gplot_prod = ggplot( df.new.prd,aes(x="" ,y=percent,fill=BarriersOPENdata_wrap_ord))+
  geom_bar(stat="identity",width=1,color="white")+
  coord_polar("y", start=0,direction=-1)+
  #sets y axis as a percentage scale
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(label = percent2,x=1.3),
            position = position_stack(vjust = 0.5)) +
  
  #scale_fill_brewer(palette="OrRd")+
  scale_fill_manual(values=rev(brewer.pal(9,"OrRd")))+
  theme_void()+
  theme(legend.position="right",
        legend.title=element_blank(),
        text=element_text(size=overall_text)
  )+
  theme(plot.title = element_text(hjust = 0.5))+
  #theme(legend.spacing.y = unit(1, 'cm'))
  labs(title="a producers / providers perspective")

gplot_prod


#savingdata 
#saving data structure to file
write.csv2(df.new,"./Plotsdata/Q19_Comparison_OPEN_Barriers.csv")


# Saving plot
png(file="./Plots/Q19_Comparison_OPEN_Barriers.png", units="in", width=14, height=6, res=600)

print(
  grid.arrange(gplot_prod,gplot_user,nrow=1,
               top=grid::textGrob("Barriers to the use of Open data from:",
                                  gp=grid::gpar(fontsize=overall_text+5)))
)
dev.off()
