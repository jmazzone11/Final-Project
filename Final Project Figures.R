library(tidyverse)
library(dplyr)
library(ggplot2)


#catvsdog
#https://www.kaggle.com/code/valchovalev/pets-ownership-cats-vs-dog-popularity-in-usa/data
cat_vs_dog<-read.csv("catsvdogs.csv")


data<-cat_vs_dog%>%
  select(c(Location, Location_Area, Percentage.of.households.with.pets, Percentage.of.Dog.Owners, Percentage.of.Cat.Owners))%>%
  rename("Pet Owners" = "Percentage.of.households.with.pets",
         "Dog Owners" = "Percentage.of.Dog.Owners",
         "Cat Owners" = "Percentage.of.Cat.Owners")

long_data<-data%>%
  pivot_longer(3:5, names_to = "Owner_Type", values_to = "Percentage")%>%
  na.omit()

plot1<-long_data%>%
  ggplot(aes(Location, Percentage, fill = Owner_Type))+
  geom_bar(stat= "identity") + 
  coord_flip()


plot1
ggsave('plot1.jpeg', width = 5, height = 6)

plot2<-long_data%>%
  ggplot(aes(fct_rev(Location_Area), Percentage, fill = Owner_Type))+#fct_rev reverses the Location order
  geom_bar(stat= "identity") + 
  coord_flip()+
  xlab("Location")+#changes the x axis title (remember it was flipped so it is on the y)
  guides(fill=guide_legend(title="Owner Type"))+ #changes the legend guide title
  theme_bw() #makes the theme simpler (white background and thin grey grid lines)
plot2

ggsave('plot2.jpeg')


#plot3 change color

library(RColorBrewer)

plot3<-long_data%>%
  ggplot(aes(fct_rev(Location_Area), Percentage))+
  geom_bar(aes(fill = Owner_Type), stat= "identity") + 
  coord_flip()+
  xlab("Location")+
  guides(fill=guide_legend(title="Owner Type"))+
  theme_bw()+
  scale_fill_brewer(palette = "Paired")

ggsave('plot3.jpeg')

plot4<-long_data%>%
  ggplot(aes(Percentage, fct_rev(Location_Area)))+
  geom_bar(aes(fill = Owner_Type), stat= "identity", position = "dodge") + #places bars next to each other
  ylab("Location")+
  guides(fill=guide_legend(title="Owner Type"))+
  theme_bw()+
  scale_fill_brewer(palette = "Paired") +
  scale_x_continuous(limits = c(0,100), breaks = c(0,20,40,60,80,100)) #adds the beginning and end limit to the x axis

plot4

ggsave('plot4.jpeg')

plot5<-long_data%>%
  ggplot(aes(fct_rev(Location_Area), Percentage))+
  geom_bar(aes(fill = Owner_Type), stat= "identity", position = "dodge", show.legend = FALSE) + #places bars next to each other
  xlab("Location")+
  theme_bw()+
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(limits = c(0,100), breaks = c(0,20,40,60,80,100))+
  guides(x = guide_axis(angle = 35))+ #change x axis word angle
  facet_wrap(~Owner_Type, ncol = 1) #separate chart by Owner Type

plot5

ggsave('plot5.jpeg', width = 8, height = 4.5)

#cat plot

install.packages('remotes')
remotes::install_github("R-CoderDotCom/ggcats@main")

library(ggcats)


View(long_data)

catplot<-long_data%>%
  filter(Owner_Type == "Cat Owners", Location_Area == "Mountain")%>%
  ggplot(aes(Location, Percentage, fill = Location))+
  geom_bar(stat = "identity", show.legend = FALSE)+
  geom_cat(cat = c("nyancat", "bongo", "colonel", "grumpy", "hipster", "lil_bub", "maru", "mouth"),size = 3)+
  scale_y_continuous(limits = c(0,100), breaks = c(0,20,40,60,80,100))+
  xlab("Location")+
  theme_light()

catplot

ggsave('catplot.jpeg')

