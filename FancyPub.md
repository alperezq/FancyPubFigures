Fancy Publication Summary
================
Alvaro L Perez-Quintero
2/26/2021

As seen on twitter
(<https://twitter.com/alperezqui/status/1365042068435918855>), this is
the R code and short tutorial to generate a figures ummarizing
publication statistics from pubmed and google scholar, you’ll need to
have a google scholar profile <https://scholar.google.com/>

## Requirements

``` r
install.packages("gcite")
install.packages("scholar")
install.packages("stringr")
install.packages("ggplot2")
install.packages("lemon")
install.packages("cowplot")
install.packages("ggrepel")
```

## 1\. Pubilcation history plot

Version 1. Impact factor vs publication year, highlighting articles with
text

``` r
#load libraries
library(gcite) #to get dataframe of publications
library(scholar) #to get impach factors
library(stringr) #to modify text
#for plotting
library(cowplot) 
library(ggplot2)
library(ggrepel)
library(lemon)

#Set variabels
Scholar_ID <- "Sc3CfZsAAAAJ" 
# this is your author scholar Id, it'll be fiound in the adress of your scholar profile after user=and before &, https://scholar.google.com/citations?user=Sc3CfZsAAAAJ&hl=en
Author_fullname<- c("Alvaro L Perez-Quintero", "Pérez-Quintero") #full name as it appears ina rticles in pubmed
Author_lastname<-c("Perez-Quintero","Pérez-Quintero") #last anme and alternative spellings

#Get publication record from scholar
SC<-gcite(user=Scholar_ID,plot_wordcloud = FALSE)
df<-SC$paper_df
df<-df[!is.na(df$journal),] #flter out entries with unknown journal
df<-df[!is.na(df$`publication date`),] #flter out entries with unknown date
df$year<-as.numeric(str_split_fixed(df$`publication date`,"/",2)[,1])#get only year of publication  


#get impact factor
IF<-get_impactfactor(df$journal) #get impact factor for journals, some might not be accurate and may require to be changed manually
df<-cbind(df,IF)#add impact factors to data frame

#highlight first author paper
fchar<-str_split_fixed(df$authors,",",2)[,1]#split author list by comma
df$first<-ifelse(grepl(paste(Author_lastname,collapse = "|"),fchar),1,0)#find matches to your name, I have to use alternative spellings with and without accent
#This does not identify equal contibution first authors, those would have to be entered manually, example
df$first[df$journal=="Nature biotechnology"]<-1 #a paper where I know I'm co-first author
df<-df[order(df$year),]

#highlight custom papers
Highlight<-c("Nature biotechnology") #Create a vector For those articles you'd like to show the name or logo of the journal
df$labels<-ifelse(df$journal %in% Highlight, df$journal, NA)


p1v1<-ggplot(df,aes(y=ImpactFactor,x=year))+
  geom_point(aes(fill=ImpactFactor,stroke=first),size=4,shape=21,na.rm = TRUE)+ #points filled according to impact factor, with border according to first authos
  geom_text_repel(aes(label=labels), xlim = c(max(df$year)+2, Inf), ylim = c(-Inf, Inf),min.segment.length = 0,na.rm = TRUE)+ #add labels starting 2 years after the xlimit
  theme_cowplot()+
  coord_cartesian(clip = "off") + #
  coord_capped_cart(bottom='both')+
  scale_fill_gradient2(low="grey70",mid="khaki3",high="deepskyblue3", guide=FALSE)+ #create color scale
  scale_x_continuous(breaks=c(min(df$year),max(df$year)),limits = c(min(df$year),max(df$year)+15))+ #modify x axis to expand to the right, but only label years with dara
  coord_capped_cart(bottom='both')+ #limit the line on the x axis to the limits
  continuous_scale("stroke", "stroke",  palette = function(x){scales::rescale(x, c(0, 1.5))}, breaks = c(1),labels = c("First author"),name = NULL)+ # Modify scale for stroke size
  ggtitle("Journal Impact Factor")+
  #modify axes to clean up the plot
  xlab("Year of publication")+
  theme(
    axis.title.x =element_text(angle=0,color="black",hjust = 0.05,size=11),axis.text.x = element_text(size=9,color="grey30"),
    axis.line.y = element_blank(),panel.grid.major.y = element_line(color="grey",linetype = 3),axis.title.y.left = element_blank(), axis.text.y = element_text(size=9,color="grey30"),#y axis
    plot.title = element_text(color="grey40",face = "bold",size=12), #tittle
    legend.position = c(0.6,0), legend.text= element_text(size=9,color="grey30"),#legend
    )
p1v1
```

![](FancyPub_files/figure-gfm/Publication%20history%20v1%20-1.png)<!-- -->

Version 2. Add images to highlight articles

``` r
library(ggtext) #to allow to insert figures

#Add images to sopecific entries in the dataframe, can be added from urls or local files
df$images[df$journal=="Nature biotechnology"]<-"<img src='http://blogs.nature.com/tradesecrets/files/2016/07/NBT-logo-red-1024x280.jpg' width='90'/>"


p1v2<-ggplot(df,aes(y=ImpactFactor,x=year))+
  geom_point(aes(fill=ImpactFactor,stroke=first),size=4,shape=21,na.rm = TRUE)+ #points filled according to impact factor, with border according to first authos
  geom_richtext(aes(label=images,x=max(year)),na.rm = TRUE,fill = NA, label.color = NA,hjust=0)+ #plot laimages as labels  
  geom_segment(data=df[!is.na(df$images),],aes(x=year,xend=(max(year)+2),y=ImpactFactor,yend=ImpactFactor),color="black",na.rm = TRUE,size=0.5)+ #add line between image and point
  theme_cowplot()+
  coord_cartesian(clip = "off") + #
  coord_capped_cart(bottom='both')+
  scale_fill_gradient2(low="grey70",mid="khaki3",high="deepskyblue3", guide=FALSE)+ #create color scale
  scale_x_continuous(breaks=c(min(df$year),max(df$year)),limits = c(min(df$year),max(df$year)+15))+ #modify x axis to expand to the right, but only label years with dara
  coord_capped_cart(bottom='both')+ #limit the line on the x axis to the limits
  ggtitle("Journal Impact Factor")+
  #modify axes to clean up the plot
  xlab("Year of publication")+
  theme(
    axis.title.x =element_text(angle=0,color="black",hjust = 0.05,size=11),axis.text.x = element_text(size=9,color="grey30"),
    axis.line.y = element_blank(),panel.grid.major.y = element_line(color="grey",linetype = 3),axis.title.y.left = element_blank(),axis.text.y = element_text(size=9,color="grey30"),#y axis
    plot.title = element_text(color="grey40",face = "bold",size=12) #tittle
  )
  

p1v2
```

![](FancyPub_files/figure-gfm/Publication%20history%20v2%20-1.png)<!-- -->

## Including Plots

You can also embed plots, for example:

![](FancyPub_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
