library(ggplot2)
library(ggthemes)
library(ggrepel)

#multiple plots
library(patchwork)
library(gridExtra)

library(reshape2)
library(dplyr)

library(RColorBrewer)

library(stringr)

library(scales)

## loading data
setwd("D:/GFZ/OpenEM/SurveyReport")
df.raw = read.csv2("./Data/SurveyResp_20221117.csv")

##################################
######## General questions ######
################################

#more options for pie charts here: 
## https://r-charts.com/part-whole/pie-chart-labels-outside-ggplot2/



### 1) In which country do you work? ############
  head(df.raw)
  
  #reducing the data to something simpler
  df.sel = df.raw[,c(1,2)]
  df.sel <- df.sel %>% group_by(In.which.country.do.you.work.) %>% summarize(count=n())
  
  sum(df.sel$count)
  df.sel$Percent = df.sel$count/sum(df.sel$count)*100
  
  
  #combining countries with low number of participation into groups
  df.sel$Group <- df.sel$In.which.country.do.you.work.

  #df.sel$Group[df.sel$count==1] <- "Canada, Croatia, Greece, Luxembourg, Portugal, Slovenia, Sweden"
  #df.sel$Group[df.sel$count==2] <- "Austria, France, Spain, Switzerland"
  
  df.sel$Group[df.sel$count<=2] <- "Two or less participants"
  
  
  #creating a new group
  df.sel2 <- df.sel %>% group_by(Group) %>% summarize(Sum=sum(count))
  df.sel2$Percent = df.sel2$Sum/sum(df.sel2$Sum)*100
  
  
  
  #Sorting alphabetically + 2 or less participants
  sort(unique(df.sel2$Group))
  df.sel2$Group_Sorted <- factor(df.sel2$Group,
                                 levels=c(
                                   "Germany",
                                   "Italy",
                                   "Netherlands",
                                   "Romania",
                                   "United Kingdom",
                                   "United States of America",
                                   "Two or less participants"
                                   ))
  

  head(df.sel2)
  #% labels within pie chart
  df.sel2 <- df.sel2 %>% 
    mutate(csum = rev(cumsum(rev(Sum))), 
           pos = Sum/2 + lead(csum, 1),
           pos = if_else(is.na(pos), Sum/2, pos)) %>% 
    mutate(legend_labs = paste0(Group_Sorted, " (", percent(df.sel2$Sum/sum(df.sel2$Sum)), ")"))%>% 
    mutate(percent2 = paste0( percent(df.sel2$Sum/sum(df.sel2$Sum),1)))
  
      
  
  # adapted from: https://r-graph-gallery.com/piechart-ggplot2.html

  Q01_plot <- ggplot(df.sel2, aes(x="", y=Percent, fill=Group_Sorted)) +
    geom_bar(stat="identity", width=1,color="white") +
    #coord_polar("y", start=0)+
    coord_polar("y", start=0,direction = -1) + 
    scale_fill_brewer(palette="Set3",name = "Country:")+
    geom_text(aes(label = percent2,x=1.3),
              position = position_stack(vjust = 0.5)) +
    
    #scale_fill_manual(values = spectral)+
    theme_void()+
    theme(legend.position="right",
          text=element_text(size=12),
          plot.title = element_text(hjust = 0.5)) + 
    ggtitle("In which country do you work?")
  
  Q01_plot 
  

  #saving data structure to file
  write.csv2(df.sel,"./Plotsdata/Q01_Country.csv")
  write.csv2(df.sel2,"./Plotsdata/Q01_Country_PLOTONLY.csv")
  
  

  # Saving plot
  png(file="./Plots/Q01_Country.png", units="in", width=8, height=5, res=600)

  print(Q01_plot)
  
  dev.off()
  

  

### 2) What type of organization do you work for? ############
  
  
  #reducing the data to something simpler
  df.sel = df.raw[,c(1,3)]
  head(df.sel)
  
  df.sel <- df.sel %>% group_by(What.type.of.organization.do.you.work.for.) %>% summarize(count=n())
  
  sum(df.sel$count)
  df.sel$Percent = df.sel$count/sum(df.sel$count)*100
  
  # adding extra colors to the pallete
  nb.cols <- 7
  mycolors <- colorRampPalette(brewer.pal(7, "Spectral"))(nb.cols)
  
  names(df.sel)
  #% labels within pie chart
  df.sel <- df.sel %>% 
    mutate(csum = rev(cumsum(rev(count))), 
           pos = count/2 + lead(csum, 1),
           pos = if_else(is.na(pos), count/2, pos)) %>% 
    mutate(legend_labs = paste0(What.type.of.organization.do.you.work.for., 
                                " (", percent(df.sel$count/sum(df.sel$count)), ")"))%>% 
    mutate(percent2 = paste0( percent(df.sel$count/sum(df.sel$count),1)))
  
  
  # adapted from: https://r-graph-gallery.com/piechart-ggplot2.html
  
  df.sel$Organization = factor(df.sel$What.type.of.organization.do.you.work.for.,levels = unique(df.sel$What.type.of.organization.do.you.work.for.))

  
  #saving data structure to file
  write.csv2(df.sel,"./Plotsdata/Q02_TypeOrganization.csv")
  
  
  #ploting 
  ggplot(df.sel, aes(x="", y=Percent, fill=legend_labs)) +
    geom_bar(stat="identity", width=1,color="white") +
    coord_polar("y") + 
    scale_fill_brewer(palette="Spectral",name = "What type of organisation you work for?:")+
    #geom_text(aes(label = percent2),
    #          position = position_stack(vjust = 0.5)) +
    #geom_label_repel(data = df.sel,
    #                 aes(x="",y = Percent, label = percent2),
    #                 size = 4.5, nudge_x = 1, show.legend = FALSE) +
    theme_void()
  
  
### 4) Which role applies primarily to you? ############
  
  #reducing the data to something simpler
  df.sel = df.raw[,c(1,5)]
  head(df.sel)
  
  df.sel <- df.sel %>% group_by(Which.role.applies.primarily.to.you.) %>% summarize(count=n())
  
  sum(df.sel$count)
  df.sel$Percent = df.sel$count/sum(df.sel$count)*100
  
  #% labels within pie chart
  df.sel <- df.sel %>% 
    mutate(csum = rev(cumsum(rev(count))), 
           pos = count/2 + lead(csum, 1),
           pos = if_else(is.na(pos), count/2, pos)) %>% 
    mutate(legend_labs = paste0(Which.role.applies.primarily.to.you., 
                                " (", percent(df.sel$count/sum(df.sel$count)), ")"))%>% 
    mutate(percent2 = paste0( percent(df.sel$count/sum(df.sel$count),1)))
  
  # adding extra colors to the pallete
  nb.cols <- 7
  mycolors <- colorRampPalette(brewer.pal(7, "Spectral"))(nb.cols)
  
  #saving data structure to file
  write.csv2(df.sel,"./Plotsdata/Q04_Roles.csv")
  
  
  ggplot(df.sel, aes(x="", y=Percent, fill=legend_labs)) +
    geom_bar(stat="identity", width=1,color="white") +
    coord_polar("y", start=0)+
    #scale_fill_brewer(palette="Spectral")+
    scale_fill_brewer(palette="Spectral",name = "Which role applies to you?:")+
    theme_void()
  
  
### 5) What is your gender identity ############
  
  #reducing the data to something simpler
  df.sel = df.raw[,c(1,9)]
  head(df.sel)
  
  #removing a not acceptable answer - user tagged all options
  unique(df.sel$What.is.your.gender.identity.)
  df.sel <- df.sel[-(102),]
  
  #grouping
  df.sel <- df.sel %>% group_by(What.is.your.gender.identity.) %>% summarize(count=n())
  
  df.sel$Percent = df.sel$count/sum(df.sel$count)*100
  df.sel <- df.sel %>% 
    mutate(csum = rev(cumsum(rev(count))), 
           pos = count/2 + lead(csum, 1),
           pos = if_else(is.na(pos), count/2, pos)) %>% 
    mutate(legend_labs = paste0(What.is.your.gender.identity., 
                                " (", percent(df.sel$count/sum(df.sel$count)), ")"))%>% 
    mutate(percent2 = paste0( percent(df.sel$count/sum(df.sel$count),1)))
  
  #saving data structure to file
  write.csv2(df.sel,"./Plotsdata/Q05_GenderIdentity.csv")
  
  # plotting
  png(file="./Plots/Q05_GenderIdentity.png", units="in", width=8, height=5, res=600)
  
  
  print(
  ggplot(df.sel, aes(x="", y=Percent, fill=legend_labs)) +
    geom_bar(stat="identity", width=1,color="white") +
    coord_polar("y", start=0)+
    scale_fill_brewer(#name = ,
                      palette="Spectral")+
    theme_void()+
    theme(legend.position="bottom",
          legend.title=element_blank(),
          text=element_text(size=8))+
    labs(title="What is your gender identity?")
  
  )
  dev.off()

### 6) What are your main fields of application? ############
  
  #reducing the data to something simpler
  df.sel = df.raw[,c(1,10,13)]
  head(df.sel)
  
  df.sel <- df.sel %>% group_by(What.are.your.main.fields.of.application.) %>% summarize(count=n())
  
  
  #in this case we need to disagregate the answers given that the survey asked for multiple response per submission
  head(df.sel)
  
  df.new = data.frame(FieldApplication=character())
  
  for (i in 1:nrow(df.sel)){
    #print(i)
    
    temp_split = strsplit(df.sel[i,1]$What.are.your.main.fields.of.application.,split=";")[[1]]
    for (j in 1:length(temp_split)){
      
      #print(j)
      #in some cases, the first letter of the string was a space, so this needs to be corrected
      tmp_str = temp_split[j]
      if (substr(tmp_str,1,1)== " "){
        tmp_str = substr(tmp_str,2,nchar(tmp_str))
      }
      
      
      df.tmp = data.frame(FieldApplication=tmp_str)
      df.new = rbind(df.new,df.tmp)
    }
  }
  
  
  #actual number of participants
  total_participants = 82 +32

  #and now we need to group them again
  df.new.sel <- df.new %>% group_by(FieldApplication) %>% summarize(count=n())
  df.new.sel$Percent = df.new.sel$count/total_participants*100
  

  
  #grouping
  df.new.sel <- df.new.sel %>% 
    mutate(csum = rev(cumsum(rev(count))), 
           pos = count/2 + lead(csum, 1),
           pos = if_else(is.na(pos), count/2, pos)) %>% 
    mutate(legend_labs = paste0(FieldApplication, 
                                " (", percent(df.new.sel$count/total_participants), ")"))%>% 
    mutate(percent2 = paste0( percent(df.new.sel$count/total_participants,1)))

  sort(df.new.sel$FieldApplication)
  
  df.new.sel$FieldApplication_sort <- factor(df.new.sel$FieldApplication,  
                                        levels = c("Agriculture / land degradation",
                                                   "Atmosphere / weather" ,
                                                   "Climate / carbon" ,
                                                   "Coastal marine areas",
                                                   "Energy / mineral resources",
                                                   "Forestry",
                                                   "Infrastructure / transportation",
                                                   "Nature conservation / biodiversity",
                                                   "Risk / hazards",
                                                   "Water resources",
                                                   "Other") )
  
  
  
  #saving data structure to file
  write.csv2(df.new.sel,"./Plotsdata/Q06_FieldsOfApplication.csv")
  
  
  
  
  plt.fieldapp <- 
  ggplot(df.new.sel, aes(x="", y=Percent, fill=FieldApplication_sort)) +
    geom_bar(stat="identity",width=1,color="white")+
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
  
  # plotting
  png(file="./Plots/Q06_FieldOfApplication.png", units="in", width=8, height=4, res=600)
  
  
  print(plt.fieldapp)
  
  dev.off()
  

  
  
### 7) What is your age ############
  
  
  #reducing the data to something simpler
  df.sel = df.raw[,c(1,12)]
  head(df.sel)
  
  df.sel <- df.sel %>% group_by(What.is.your.age.) %>% summarize(count=n())
  
  sum(df.sel$count)
  df.sel$Percent = df.sel$count/sum(df.sel$count)*100
  
  # adding extra colors to the pallete
  nb.cols <- 7
  mycolors <- colorRampPalette(brewer.pal(7, "Spectral"))(nb.cols)
  
  #saving data structure to file
  write.csv2(df.sel,"./Plotsdata/Q07_Age.csv")
  
  ggplot(df.sel, aes(x="", y=Percent, fill=What.is.your.age.)) +
    geom_bar(stat="identity", width=1,color="white") +
    coord_polar("y", start=0)+
    scale_fill_brewer(palette="Spectral")+
    theme_void()
  
### 8) You are primarily... user / producer of spatial data ############
  
  #reducing the data to something simpler
  df.sel = df.raw[,c(1,13)]
  head(df.sel)
  
  df.sel <- df.sel %>% group_by(You.are.primarily...) %>% summarize(count=n())
  
  sum(df.sel$count)
  df.sel$Percent = df.sel$count/sum(df.sel$count)*100
  
  #% labels within pie chart
  df.sel <- df.sel %>% 
    mutate(csum = rev(cumsum(rev(count))), 
           pos = count/2 + lead(csum, 1),
           pos = if_else(is.na(pos), count/2, pos)) %>% 
    mutate(legend_labs = paste0(You.are.primarily..., 
                                " (", percent(df.sel$count/sum(df.sel$count)), ")"))%>% 
    mutate(percent2 = paste0( percent(df.sel$count/sum(df.sel$count),1)))
  
  # adding extra colors to the pallete
  nb.cols <- 7
  mycolors <- colorRampPalette(brewer.pal(7, "Spectral"))(nb.cols)
  
  
  plt.usertype <- 
  ggplot(df.sel, aes(x="", y=Percent, fill=You.are.primarily...)) +
    geom_bar(stat="identity",width=1,color="white")+
    coord_polar("y", start=0,direction=-1)+
    #sets y axis as a percentage scale
    scale_y_continuous(labels = scales::percent)+
    geom_text(aes(label = percent2,x=1.3),
              position = position_stack(vjust = 0.5)) +
    
    #scale_fill_manual(values=c("#0072B2","#D55E00"))+
    theme_void()+
    theme(legend.position="right",
          legend.title=element_blank(),
          text=element_text(size=15)
    )+
    theme(plot.title = element_text(hjust = 0.5))+
    labs(title="You are primarily a ...")
    
  plt.usertype
  

  #saving data structure to file
  write.csv2(df.sel,"./Plotsdata/Q08_UserType.csv")
  
  # plotting
  png(file="./Plots/Q08_UserType.png", units="in", width=8, height=4, res=600)
  

  print(plt.usertype)

  dev.off()
