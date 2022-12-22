pacman::p_load(
  #data wrangling
  tidyverse,vtable,tidyr, 
  #visualization
  ggplot2, ggpubr,ggsci,plotly,
  RcolorBrewer,mice,VIM,ggcorrplot
)


#import csv----
world_population <- read_csv("world_population.csv")
glimpse(world_population)
summary(world_population)
#Arranging the data----
Arrange_data <- world_population %>% select(1, 6:17) %>% st(out="csv")%>%
  as.data.frame() 
#create a folder where excel files will be saved ----
dir.create("Population folder")
#write a dataframe as an XLSX excel file----
writexl::write_xlsx(Arrange_data, path= "Population folder/Arrangement.xlsx")
#2022 Population----
split <- world_population %>% group_by(CCA3)%>% summarise(total= sum(`2022 Population`))%>% mutate(label_text=paste(CCA3,total))%>% arrange(desc(total))
# give state boundaries a white border
l <- list(color = toRGB("Red"), width = 2, "Set2")
# specify some map projection/options
g <- list(
  scope = 'World',
  projection = list(type = 'Country'),
  showlakes = TRUE,
  lakecolor = toRGB('Green')
)
fig <- split %>% plot_geo(locationmode = "Continent") %>% 
  add_trace(z = ~total,
            locations = ~CCA3,
            color = ~total,
            text = ~ label_text,
            color= "Red")
fig <- fig %>% colorbar(title="2022 Population")
fig <- fig %>% layout(
  title = "2022 Population",
  geo = g
)
fig

#2022 by continent----

Continent <- world_population %>% group_by(Continent)%>% summarise(total= sum(`2022 Population`))%>% mutate(label_text=paste(Continent,total))
#create a folder where excel files will be saved 
dir.create("Continent")
#write a datframe as an XLSX excel file
B <- writexl::write_xlsx(Continent, path= "Population folder/Continent.xlsx")
#read an XLSX excel file
df <- readxl::read_excel(path="Continent/Continent.xlsx")


# No of countries by continent----
No_of_continent <- world_population %>% group_by(Continent)%>% summarise(n=n())
count <- world_population %>% group_by(Continent)%>% summarise(n=n())
writexl::write_xlsx(No_of_continent, path= "Population folder/No_of_continent.xlsx")
##Barchart of no of countries by continent----
Countries <- count %>% ggplot(aes(x=reorder(Continent,-n), y=n,fill=Continent))+
  geom_bar(stat="identity")+
  geom_text(aes(label=n),vjust=1.5)+
  labs(title="Number of countries by continent",
         x="Continents",y="No of countries")+
  theme_classic()+scale_fill_brewer(palette="Greens")

#pie chart of continent (2020)----
Continents <- world_population %>% group_by(Continent)%>% summarise(total= sum(`2022 Population`))%>% mutate(label_text=paste(Continent,total))
writexl::write_xlsx(Continents, path= "Population folder/Continents.xlsx")
#pie chart
Fig1 <-Continents %>% plot_ly(labels=~Continent, values=~total,type="pie",legendgroup=~Continent,
                              textposition="outside", textinfo="label+percent")%>%
  layout(title ="Population by Continent 2022",showlegend=T, xaxis=list(showgrid=FALSE,zeroline=FALSE,showticklabels=FALSE),
         yaxis=list(showgrid=FALSE,zeroline=FALSE,showticklabels=FALSE))
#Scatter plot for Area and population density 2022
fun_color_range <- colorRampPalette(c("Blue","Purple","Green","Red","Yellow"))
my_colors <- fun_color_range(173)
my_colors
plot1 <- world_population %>% plot_ly(x= ~`2022 Population`, y= ~`Growth Rate`,
                                      text=~Continent, type="scatter",mode="markers",
                                      size=~`2022 Population`, color = ~ Country, colors=my_colors)%>%
  layout(title="Area and density Population of Growth Rate",
         xaxis=list(title="2022 Population"),
         yaxis=list(title="Growth Rate"))

#Scatter plot for Area and population density----
fun_color_range <- colorRampPalette(c("Green","Red","Purple","Blue"))
my_colors <- fun_color_range(173)
my_colors
plot1 <- world_population %>% plot_ly(x= ~`Area (km²)`, y= ~`Density (per km²)`,
                                      text=~Continent, type="scatter",mode="markers",
                                      size=~`Density (per km²)`, color = ~ Country, colors=my_colors)%>%
  layout(title="Area and density Population",
         xaxis=list(title="Area (km²)"),
         yaxis=list(title="Density (per km²)"))

#Growth rate----
Country_scatter_plot <- world_population %>% group_by(Country,CCA3)%>% summarise(Growth_Rate= sum(`Growth Rate`))%>% 
  mutate(label_text2=paste(Country,Growth_Rate))
l <- list(color = toRGB("Green"), width = 2, "Set2")
# specify some map projection/options
T <- list(
  scope = 'Continent',
  projection = list(type = "Country"),
  showlakes = TRUE,
  lakecolor = toRGB('Blue')
)
fig2 <- Country_scatter_plot %>% plot_geo(locationmode = "Country") %>% 
  add_trace(z = ~Growth_Rate,
            locations = ~CCA3,
            color = ~Growth_Rate,
            text = ~label_text2,
            color= "Green")
fig3 <- fig2 %>% colorbar(title="Growth Rate")
fig4 <- fig3 %>% layout(
  title = "Growth Rate",
  geo = T
)
fig4

# Top 10 countries with Most Population----
Top_Countries <- world_population %>% group_by(Country,`2022 Population`) %>%
  summarize(Pop2022=sum(`2022 Population`)) %>% arrange(desc(`2022 Population`))%>% head(10)
writexl::write_xlsx(Top_Countries, path= "Population folder/Top_Countries.xlsx")
#Bar Plot
Countries1 <- Top_Countries %>% ggplot(aes(reorder(Country,-`2022 Population`),y=`2022 Population`,fill=Country))+
  geom_bar(stat="identity")+
  geom_text(aes(label=`2022 Population`),cex=2.5,vjust=0.05)+
  labs(title="Top 10 countries with the most population (2022)",
       x="Countries",y="Population")+ 
       theme_classic()+scale_fill_brewer(palette="BrBG")+
       theme(axis.text.x=element_text(angle=45,vjust=0.5))

#Least 10 countries with the least population----
Least_Countries <- world_population %>% group_by(Country,`2022 Population`) %>%
  summarize(Pop2022=sum(`2022 Population`)) %>% arrange(desc(`2022 Population`))%>% tail(10)
writexl::write_xlsx(Least_Countries, path= "Population folder/Least_Countires.xlsx")

Countries2 <- Least_Countries %>% ggplot(aes(reorder(Country,-`2022 Population`),y=`2022 Population`,fill=Country))+
  geom_bar(stat="identity")+
  geom_text(aes(label=`2022 Population`),cex=2.5,vjust=-0.5)+
  labs(title="Top 10 countries with the least population (2022)",
       x="Countries",y="Population")+ 
  theme_classic()+scale_fill_brewer(palette="YlOrRd")+
  theme(axis.text.x=element_text(angle=45,vjust=0.5))

#2020 Population by CCA3----

Population_2020 <- world_population %>% group_by(CCA3)%>% summarise(total= sum(`2020 Population`))%>% mutate(label_text=paste(CCA3,total))%>% arrange(desc(total))
writexl::write_xlsx(Population_2020, path= "Population folder/Population_2020.xlsx")
# give state boundaries a white border
l <- list(color = toRGB("Red"), width = 2, "Set2")
# specify some map projection/options
g <- list(
  scope = 'World',
  projection = list(type = 'Country'),
  showlakes = TRUE,
  lakecolor = toRGB('Green')
)
fig <- Population_2020 %>% plot_geo(locationmode = "Continent") %>% 
  add_trace(z = ~total,
            locations = ~CCA3,
            color = ~total,
            text = ~ label_text,
            color= "Red")
fig <- fig %>% colorbar(title="2020 Population")
fig <- fig %>% layout(
  title = "2020 Population",
  geo = g
)
fig

#2020 by continent----
Continent_2020 <- world_population %>% group_by(Continent)%>% summarise(total= sum(`2020 Population`))%>% mutate(label_text=paste(Continent,total))
# No of countries by continent
count <- world_population %>% group_by(Continent)%>% summarise(n=n())
Countries_2020 <- count %>% ggplot(aes(x=reorder(Continent,-n), y=n,fill=Continent))+
  geom_bar(stat="identity")+
  geom_text(aes(label=n),vjust=1.5)+
  labs(title="Number of countries by continent(2020)",
       x="Continents",y="No of countries")+
  theme_classic()+scale_fill_brewer(palette="Greens")

#population of continent (2020)----
Fig_2020 <-Continent_2020 %>% plot_ly(labels=~Continent, values=~total,type="pie",legendgroup=~Continent,
                              textposition="outside", textinfo="label+percent")%>%
  layout(title ="Population by Continent 2020",showlegend=T, xaxis=list(showgrid=FALSE,zeroline=FALSE,showticklabels=FALSE),
         yaxis=list(showgrid=FALSE,zeroline=FALSE,showticklabels=FALSE))

# COUNTRIES WITH THE MOST POPULATION 2020
Top_Countries_2020 <- world_population %>% group_by(Country,`2020 Population`) %>%
  summarize(Pop2020=sum(`2020 Population`)) %>% arrange(desc(`2020 Population`))%>% head(10)

#Bar Plot of countries with the most population----
Countries <- Top_Countries_2020 %>% ggplot(aes(reorder(Country,-`2020 Population`),y=`2020 Population`,fill=Country))+
  geom_bar(stat="identity")+
  geom_text(aes(label=`2020 Population`),cex=2.5,vjust=0.05)+
  labs(title="Top 10 countries with the most population (2020)",
       x="Countries",y="Population")+ 
  theme_classic()+scale_fill_brewer(palette="BrBG")+
  theme(axis.text.x=element_text(angle=45,vjust=0.5))
#COUNTRIES WITH LEAST POPULATION(2020) ----
Least_Countries2020 <- world_population %>% group_by(Country,`2020 Population`) %>%
  summarize(Pop2020=sum(`2020 Population`)) %>% arrange(desc(`2020 Population`))%>% tail(10)
Countries2 <- Least_Countries2020 %>% ggplot(aes(reorder(Country,-`2020 Population`),y=`2020 Population`,fill=Country))+
  geom_bar(stat="identity")+
  geom_text(aes(label=`2020 Population`),cex=2.5,vjust=-0.5)+
  labs(title="Least 10 countries with the least population (2020)",
       x="Countries",y="Population")+ 
  theme_classic()+scale_fill_brewer(palette="YlOrRd")+
  theme(axis.text.x=element_text(angle=45,vjust=0.5))

#2015 population by CCA3----
split_2015 <- world_population %>% group_by(CCA3)%>% summarise(total= sum(`2015 Population`))%>% mutate(label_text=paste(CCA3,total))%>% arrange(desc(total))
# give state boundaries a white border
l <- list(color = toRGB("Red"), width = 2, "Set2")
# specify some map projection/options
g <- list(
  scope = 'World',
  projection = list(type = 'Country'),
  showlakes = TRUE,
  lakecolor = toRGB('Green')
)
fig <- split_2015 %>% plot_geo(locationmode = "Continent") %>% 
  add_trace(z = ~total,
            locations = ~CCA3,
            color = ~total,
            text = ~ label_text,
            color= "Red")
fig <- fig %>% colorbar(title="2015 Population")
fig <- fig %>% layout(
  title = "2015 Population",
  geo = g
)
fig

#2015 population by continent----
Continent_2015 <- world_population %>% group_by(Continent)%>% summarise(total= sum(`2015 Population`))%>% mutate(label_text=paste(Continent,total))
# No of countries by continent
count <- world_population %>% group_by(Continent)%>% summarise(n=n())
Countries_2015 <- count %>% ggplot(aes(x=reorder(Continent,-n), y=n,fill=Continent))+
  geom_bar(stat="identity")+
  geom_text(aes(label=n),vjust=1.5)+
  labs(title="Number of countries by continent(2015)",
       x="Continents",y="No of countries")+
  theme_classic()+scale_fill_brewer(palette="Greens")

#pie chart of the continent 2015 ----
Fig_2015 <-Continent_2015 %>% plot_ly(labels=~Continent, values=~total,type="pie",legendgroup=~Continent,
                              textposition="outside", textinfo="label+percent")%>%
  layout(title ="Population by Continent 2015",showlegend=T, xaxis=list(showgrid=FALSE,zeroline=FALSE,showticklabels=FALSE),
         yaxis=list(showgrid=FALSE,zeroline=FALSE,showticklabels=FALSE))
# Top 10 countries with Most Population (2015)----
Top_Countries_2015 <- world_population %>% group_by(Country,`2015 Population`) %>%
  summarize(Pop2015=sum(`2015 Population`)) %>% arrange(desc(`2015 Population`))%>% head(10)
#Bar Plot
Countries <- Top_Countries_2015 %>% ggplot(aes(reorder(Country,-`2015 Population`),y=`2015 Population`,fill=Country))+
  geom_bar(stat="identity")+
  geom_text(aes(label=`2015 Population`),cex=2.5,vjust=0.05)+
  labs(title="Top 10 countries with the most population (2015)",
       x="Countries",y="Population")+ 
  theme_classic()+scale_fill_brewer(palette="BrBG")+
  theme(axis.text.x=element_text(angle=45,vjust=0.5))
#Least 10 countries with the least population(2015)----
Least_Countries2015 <- world_population %>% group_by(Country,`2015 Population`) %>%
  summarize(Pop2015=sum(`2015 Population`)) %>% arrange(desc(`2015 Population`))%>% tail(10)
Countries2 <- Least_Countries2015 %>% ggplot(aes(reorder(Country,-`2015 Population`),y=`2015 Population`,fill=Country))+
  geom_bar(stat="identity")+
  geom_text(aes(label=`2015 Population`),cex=2.5,vjust=-0.5)+
  labs(title="10 countries with the least population (2015)",
       x="Countries",y="Population")+ theme_classic()+scale_fill_brewer(palette="YlOrRd")+
  theme(axis.text.x=element_text(angle=45,vjust=0.5))

  