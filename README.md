Fancy Publication Summary
================
Alvaro L Perez-Quintero
2/26/2021

As seen on twitter
(<https://twitter.com/alperezqui/status/1365042068435918855>), this is
the R code and short tutorial to generate a figures summarizing
publication statistics from pubmed and google scholar, you’ll need to
have a google scholar profile <https://scholar.google.com/>

## Requirements

``` r
#For biblio analysis
install.packages("scholar")
install.packages("easyPubMed")
install.packages("tm")

#For maps
install.packages("maps")
install.packages("rgeos")
install.packages("rworldmap")

#for plotting
install.packages("ggplot2")
install.packages("lemon")
install.packages("cowplot")
install.packages("ggrepel")
install.packages("ggtext")
install.packages("ggwordcloud")

#Miscellaneous
install.packages("stringr")
install.packages("dplyr")
```

## 1\. Publication History Plot

Version 1. Impact factor vs publication year, highlighting articles with
text

``` r
#load libraries
library(scholar) #to get publications and impact factors
library(stringr) #to modify text
#for plotting
library(cowplot) 
library(ggplot2)
library(ggrepel)
library(lemon)
library(dplyr)

#Set variabels
Scholar_ID <- "Sc3CfZsAAAAJ" 
# this is your author scholar Id, it'll be found in the adress of your scholar profile after user=and before &, https://scholar.google.com/citations?user=Sc3CfZsAAAAJ&hl=en
Author_fullname<- c("Alvaro L Perez-Quintero","Alvaro Perez-Quintero") #full name as it appears in articles in pubmed
Author_lastname<-c("Perez-Quintero","Pérez-Quintero","Perez‐Quintero") #last name and alternative spellings

#Get publication record from scholar
df<-get_publications(Scholar_ID)
colnames(df)<-c("title","authors","journal","number","total.citations","publication.date","cid","pubid")

################## ALTERNATIVE ###########
#if for some reason getting publications with scholar is not working use these commands for gcite
#install.packages("gcite")
#library(gcite)
#SC<-gcite(user=Scholar_ID,plot_wordcloud = FALSE)
#df<-SC$paper_df
##########################################

df<-df[!is.na(df$journal),] #filter out entries with unknown journal
df<-df[!is.na(df$publication.date),] #flter out entries with unknown date
df$year<-as.numeric(str_split_fixed(df$publication.date,"/",2)[,1])#get only year of publication  

#get impact factor
IF<-get_impactfactor(df$journal) #get impact factor for journals, some might not be accurate and may require to be changed manually
df<-cbind(df,IF)#add impact factors to data frame

#IMPORTANT#
#the get_impactfactor fucntion is prone to error, so verify and correct, some journal names are not found or get replaced by others

#highlight first author paper
fchar<-str_split_fixed(df$authors,",",2)[,1]#split author list by comma
df$first<-ifelse(grepl(paste(Author_lastname,collapse = "|"),fchar),1,0)#find matches to your name, I have to use alternative spellings with and without accent
#This does not identify equal contibution first authors, those would have to be entered manually, example
df$first[df$journal=="Nature biotechnology"]<-1 #a paper where I know I'm co-first author
df<-df[order(df$year),]

#highlight custom papers
Highlight<-c("Nature biotechnology") #Create a vector For those articles you'd like to show the name or logo of the journal
df$labels<-ifelse(df$journal %in% Highlight, as.character(df$journal), NA)

#Optional: limit to publications after a given year, I have only tested the code using ~10years
df<-df[df$year>=2009,]
#Plot
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

#Add images to specific entries in the dataframe, can be added from urls or local files
df$images[df$journal=="Nature biotechnology"]<-"<img src='http://blogs.nature.com/tradesecrets/files/2016/07/NBT-logo-red-1024x280.jpg' width='90'/>"
#repeat for as many figures as you'd like to include

#Plot
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

Version 3. If you’d rather not have impact fator, an alternative is
citations per paper

``` r
df$total.citations<-as.numeric(df$total.citations)

#Plot
p1v3<-ggplot(df,aes(y=total.citations,x=year))+
  geom_point(aes(fill=total.citations,stroke=first),size=4,shape=21,na.rm = TRUE)+ #points filled according to impact factor, with border according to first authos
  geom_text_repel(aes(label=labels), xlim = c(max(df$year)+2, Inf), ylim = c(-Inf, Inf),min.segment.length = 0,na.rm = TRUE)+ #add labels starting 2 years after the xlimit
  theme_cowplot()+
  coord_cartesian(clip = "off") + #
  coord_capped_cart(bottom='both')+
  scale_fill_gradient2(low="grey70",mid="khaki3",high="deepskyblue3", guide=FALSE)+ #create color scale
  scale_x_continuous(breaks=c(min(df$year),max(df$year)),limits = c(min(df$year),max(df$year)+15))+ #modify x axis to expand to the right, but only label years with dara
  coord_capped_cart(bottom='both')+ #limit the line on the x axis to the limits
  continuous_scale("stroke", "stroke",  palette = function(x){scales::rescale(x, c(0, 1.5))}, breaks = c(1),labels = c("First author"),name = NULL)+ # Modify scale for stroke size
  ggtitle("Citations per Article")+
  #modify axes to clean up the plot
  xlab("Year of publication")+
  theme(
    axis.title.x =element_text(angle=0,color="black",hjust = 0.05,size=11),axis.text.x = element_text(size=9,color="grey30"),
    axis.line.y = element_blank(),panel.grid.major.y = element_line(color="grey",linetype = 3),axis.title.y.left = element_blank(), axis.text.y = element_text(size=9,color="grey30"),#y axis
    plot.title = element_text(color="grey40",face = "bold",size=12), #tittle
    legend.position = c(0.6,0), legend.text= element_text(size=9,color="grey30"),#legend
    )

p1v3
```

![](FancyPub_files/figure-gfm/Publication%20history%20v3%20-1.png)<!-- -->

Version 4. Highlight first and last author

``` r
#highlight last author paper
lauthor<-sapply(str_split(df$authors,","), tail, 1)#get last author for each paper
df$first_last<-ifelse(grepl(paste(Author_lastname,collapse = "|"),lauthor),2,df$first) #assign value 2 to last author papers, first author gets value 1

df$either<-df$first_last #create colum to give bold stroke to either firt and last
df$either[df$either>1]<-1
#df$first_last[df$first_last==0]<-NA


#Plot
#lines with changes added to highlight last auhtor are indicated with (last author) # this is the version with Journal impact factor vs publication history
p1v4<-ggplot(df,aes(y=ImpactFactor,x=year))+
  geom_point(aes(fill=ImpactFactor,stroke=either,shape=as.character(first_last)),size=4,na.rm = TRUE)+ #points filled according to impact factor, with border according to first author (last author)
  geom_text_repel(aes(label=labels), xlim = c(max(df$year)+2, Inf), ylim = c(-Inf, Inf),min.segment.length = 0,na.rm = TRUE)+ #add labels starting 2 years after the xlimit
  theme_cowplot()+
  coord_cartesian(clip = "off") + #
  coord_capped_cart(bottom='both')+
  scale_fill_gradient2(low="grey70",mid="khaki3",high="deepskyblue3", guide=FALSE)+ #create color scale
  scale_x_continuous(breaks=c(min(df$year),max(df$year)),limits = c(min(df$year),max(df$year)+15))+ #modify x axis to expand to the right, but only label years with dara
  scale_shape_manual(values=c(21,21,23),breaks=c("1","2"),name=NULL,labels=c("First Author","Last Author"))+ #scale fo shapes for first and last author (last author)
  coord_capped_cart(bottom='both')+ #limit the line on the x axis to the limits
  continuous_scale("stroke", "stroke",  palette = function(x){scales::rescale(x, c(0, 1.5))}, breaks = c(1),name = NULL,guide=FALSE)+ # Modify scale for stroke size (last author)
  ggtitle("Journal Impact Factor")+
  #modify axes to clean up the plot
  xlab("Year of publication")+
  theme(
    axis.title.x =element_text(angle=0,color="black",hjust = 0.05,size=11),axis.text.x = element_text(size=9,color="grey30"),
    axis.line.y = element_blank(),panel.grid.major.y = element_line(color="grey",linetype = 3),axis.title.y.left = element_blank(), axis.text.y = element_text(size=9,color="grey30"),#y axis
    plot.title = element_text(color="grey40",face = "bold",size=12), #tittle
    legend.position = c(0.6,0), legend.text= element_text(size=9,color="grey30"),#legend
    )+
  guides(shape = guide_legend(override.aes = list(stroke=1.5)))#get same size for stroke in the legend (last author)

#Plot
#lines with changes added to highlight last author are indicated with (last author) # this is the version with citations vs publication history
p1v4<-ggplot(df,aes(y=total.citations,x=year))+
  geom_point(aes(fill=total.citations,stroke=either,shape=as.character(first_last)),size=4,na.rm = TRUE)+ #points filled according to impact factor, with border according to first author (last author)
  geom_text_repel(aes(label=labels), xlim = c(max(df$year)+2, Inf), ylim = c(-Inf, Inf),min.segment.length = 0,na.rm = TRUE)+ #add labels starting 2 years after the xlimit
  theme_cowplot()+
  coord_cartesian(clip = "off") + #
  coord_capped_cart(bottom='both')+
  scale_fill_gradient2(low="grey70",mid="khaki3",high="deepskyblue3", guide=FALSE)+ #create color scale for points
  scale_shape_manual(values=c(21,21,23),breaks=c("1","2"),name=NULL,labels=c("First Author","Last Author"))+ #scale fo shapes for first and last author (last author)
  scale_x_continuous(breaks=c(min(df$year),max(df$year)),limits = c(min(df$year),max(df$year)+15))+ #modify x axis to expand to the right, but only label years with dara
  coord_capped_cart(bottom='both')+ #limit the line on the x axis to the limits
  continuous_scale("stroke", "stroke",  palette = function(x){scales::rescale(x, c(0, 1.5))}, breaks = c(1),name = NULL,guide=FALSE)+ # Modify scale for stroke size (last author)
  ggtitle("Citations per Article")+
  #modify axes to clean up the plot
  xlab("Year of publication")+
  theme(
    axis.title.x =element_text(angle=0,color="black",hjust = 0.05,size=11),axis.text.x = element_text(size=9,color="grey30"),
    axis.line.y = element_blank(),panel.grid.major.y = element_line(color="grey",linetype = 3),axis.title.y.left = element_blank(), axis.text.y = element_text(size=9,color="grey30"),#y axis
    plot.title = element_text(color="grey40",face = "bold",size=12), #tittle
    legend.position = c(0.6,0), legend.text= element_text(size=9,color="grey30"),#legend
    )+
  guides(shape = guide_legend(override.aes = list(stroke=1.5)))#get same size for stroke in the legend (last author)

p1v4
```

![](FancyPub_files/figure-gfm/Publication%20history%20v4%20-1.png)<!-- -->

## 2\. Coauthor affiliation Map

``` r
library(easyPubMed)
library(maps)
library(rgeos)
library(rworldmap)

#Get detailed author and abstract information for each article from pubmed
my_query <- paste(Author_fullname,"[AU]",sep="",collapse = " OR ")
my_entrez_id <- get_pubmed_ids(my_query)
my_abstracts_xml <- fetch_pubmed_data(pubmed_id_list = my_entrez_id) # get data in xml format

#transfrom pubmed data to data frame
my_PM_list <- articles_to_list(pubmed_data = my_abstracts_xml)
#NOTE= the next function adds the affiliations to a data frame, for some journals only the affiliation of the first author is shown, the parameter 'autofill=TRUE' means in those instances all authors get assigned the same affiliation. I recommend to manually check this data frame and correct affiliations where needed
xx <- lapply(my_PM_list, article_to_df, autofill = TRUE, max_chars = 1000)
full_df <- do.call(rbind, xx)


#extract country from affiliation info from dataframe
raw <- gsub("[[:punct:]\n]","",full_df$address)
raw2 <- strsplit(raw, " ") # Split data at word boundaries
data(world.cities)
CountryList_raw <- (lapply(raw2, function(x)x[which(toupper(x) %in% toupper(world.cities$country.etc))])) #match any city in the author address to any city name in the world.cities data
full_df$country<-NA
for(i in 1:nrow(full_df)){ #keep only first country amtched for addresses with multiple matches
  full_df$country[i]<-CountryList_raw[[i]][1]
}

#Remove self from author data frame, 
co_df<-full_df[!grepl(paste(Author_lastname,collapse = "|"),as.character(full_df$lastname), useBytes = TRUE),]
co_df<-co_df[!duplicated(paste(co_df$lastname,co_df$firstname,co_df$country)),] # remove duplicated coauthors

#Get country counts
countries <- as.data.frame(table(na.omit(co_df$country)))
colnames(countries)<-c("region","count")

#might need to correct some names so they match country names rworldmap
countries$region <- gsub("USA","United States of America",countries$region)
countries$region <- gsub("UK","United Kingdom",countries$region)

#We need to get coordinates for each country to add to a map
#I decided to use geographical centroids as calculated from latitude and longitude data

wmap <- getMap(resolution="high")
centroids <- gCentroid(wmap, byid=TRUE)
centr<-as.data.frame(centroids)
centr<-cbind(rownames(centr),centr[,1:2])
colnames(centr)<-c("region","long","lat")

MyCounts<-left_join(countries,centr) 
World <- map_data("world") #lat and long of every country to have as background map

#Plot
p2<-ggplot() +
  geom_polygon(data = World, aes(x=long, y = lat, group = group), fill="khaki3", alpha=0.8)+ #background map
  geom_point(data= MyCounts, aes(x=long, y=lat,size=count,fill=count),color="black",pch=21,stroke=0.6) + #points centered in each country
  geom_text(data= MyCounts, aes(x=long, y=lat,label=count),size=3,na.rm = TRUE,color="white")+ #add numbers to each point
  scale_fill_gradient2(low=alpha("gold",0.8),mid=alpha("deepskyblue3",0.8),high=alpha("dodgerblue4",0.8),midpoint = 2,guide=FALSE)+ #color scale for points
  scale_size(range = c(3,10),guide = FALSE)+ #size scale for points
  ggtitle("Coauthors by Affiliation")+
  theme_cowplot()+
  #remove axis
  theme(axis.title.y = element_blank(),axis.ticks.y = element_blank(),axis.text.y = element_blank(),axis.line.y = element_blank(),
        axis.title.x = element_blank(),axis.ticks.x = element_blank(),axis.text.x = element_blank(),axis.line.x = element_blank(),legend.position = "bottom",
        plot.title = element_text(color="grey40",face = "bold",size=12))


p2
```

![](FancyPub_files/figure-gfm/Affiliation%20Map%20-1.png)<!-- -->

## 3\. Citation History

``` r
#Get citation history
Cithist<-get_citation_history(Scholar_ID)
colnames(Cithist)<-c("year","n_citations")
Cithist<-Cithist[Cithist$year>2009,]

#ALTERNATIVE, if using "gcite":
#Cithist<-SC$overall_citations

#Calculate h-index using the df object from step 1
citvec<-df$total.citations[order(df$total.citations,decreasing = TRUE)]#vector of ordered citation totals
hind<-tail(which(citvec >= seq_along(citvec)), 1)

p3<-ggplot(Cithist) +
  geom_bar(aes(x=year,y=n_citations,fill=n_citations),stat="identity",position = "dodge",width = 0.95)+ #bar plot with citations
  geom_text(aes(x=year,y=n_citations+10,label=n_citations),size=3,nudge_y = 10)+ #add citation numbers above bars, might need to modify according to scale
  ggtitle(paste("Citations per year \n(h-index=",hind,")",sep=""))+
  scale_fill_gradient2(low="khaki2",mid="deepskyblue3",high="dodgerblue4",guide=FALSE,midpoint = quantile(Cithist$n_citations,probs = 0.85))+ #color scale, strong blues for the upper 0.15 quantile
  scale_x_continuous(expand = c(0,0),limits = c(min(Cithist$year)-1,max(Cithist$year)+1))+ #expand x axis
  theme_cowplot()+
  coord_capped_cart(bottom='both')+
  theme(axis.title.y = element_blank(),axis.ticks.y = element_blank(),axis.text.y = element_blank(),axis.line.y = element_blank(),
        axis.title.x =element_blank(),axis.text.x = element_text(size=9,color="grey30"),axis.line.x = element_line(color="gray30"),
        plot.title = element_text(color="grey40",face = "bold",size=12))
p3
```

![](FancyPub_files/figure-gfm/Citation%20history%20-1.png)<!-- -->

## 4\. Abstract world cloud

``` r
library(tm)
library(ggwordcloud)

#For text analyses I'm largel followng th eintructions shown in http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know

#we will use the full_df data frame obtained for the coauthor map, alternatively you can get abstracts by doing:
#my_abstracts_xml <- fetch_pubmed_data(pubmed_id_list = my_entrez_id)
#my_Text <- custom_grep(my_abstracts_xml, "AbstractText", "char")

#get abstracts
my_Text <- unique(full_df$abstract)

#process text
docs <- Corpus(VectorSource(my_Text))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
#delete special chracters
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("will", "can","also","including","talb","showed","activatorlike","may","one"))
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

#Here I recommend checking the object d, there will be words that are redundant (synonyms or abbreviations) that one might want to consolidate into one word, for this, create as many vectors with the synonyms to collapse as needed, the first word listed will be the one used for the world cloud

s1<-c("micrornas","mirna","mirnas*","mirnas")
s2<-c("genes","genes")
s3<-c("bacteria","bacterial")

for (j in 1:3){ #here do for as many synonim vectors as created
  i<-get(paste("s",j,sep=""))
  b<-data.frame(word=i[1],freq=sum(d$freq[d$word %in% i]))
  d<-d[!d$word %in% i,]
  d<-rbind(d,b)
}


CL <- d
CL<-CL[order(CL$freq,decreasing = TRUE),]
CL<- CL[1:50,] #limit the plot to the 50 most common words or as desired 

set.seed(40) # change as needed to randomize the words in the word cloud
p4<-ggplot(CL,  aes(label = word, size = freq, color = freq)) +
  geom_text_wordcloud_area(area_corr_power = 1,fontface="bold",family="Helvetica",rm_outside = TRUE,shape="circle") +
  scale_size_area(max_size = 11) +
  theme_minimal() +
  scale_size_continuous(range=c(1,8))+ #minimum and maximum size for words
  ggtitle("Abstract wordcloud")+
  scale_color_gradientn(colors = c("grey60","gold3","cornflowerblue","deepskyblue3","deepskyblue3","dodgerblue4"))+ #color scale
  theme(plot.title = element_text(color="grey40",face = "bold",size=12))

p4
```

![](FancyPub_files/figure-gfm/World%20cloud-1.png)<!-- -->

## 4\. Getting it all together

``` r
#use cowplot's plot grid to arrange plots as needed

#all plots same size
plot_grid(p1v2,p2,p3,p4,ncol=2,nrow=2)
```

![](FancyPub_files/figure-gfm/Big%20plot-1.png)<!-- -->

``` r
#or a more complex grid with a tittle

panel1<-plot_grid(p3,p4,rel_widths = c(1,1.5),ncol=2)
panel2<-plot_grid(p2,panel1,ncol=1, rel_heights = c(1.5,1))
allpanels<-plot_grid(p1v2,panel2,ncol=2,rel_widths = c(1.5,2))
title <- ggdraw() + draw_label(paste("Publication summary\n",Author_fullname[1],sep=""),fontface = 'bold',x = 0,  hjust = 0 ,color="deepskyblue4") +
  theme(plot.margin = margin(0, 0, 0, 0))
plot_grid( title, allpanels,  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)
```

![](FancyPub_files/figure-gfm/Big%20plot-2.png)<!-- -->
