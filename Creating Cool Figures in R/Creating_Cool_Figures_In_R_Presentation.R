library(tidyverse)
library(ggalluvial)
data("mtcars")
data("EuStockMarkets")
data("Titanic")
data("airquality")

# Base R scatter plot
plot(x=mtcars$mpg,
     y=mtcars$wt,
     ylab="wt",
     xlab="mpg",
     main="Car weight vs MPG")

# GGplot scatter plot
ggplot(mtcars,aes(x=mpg,y=wt))+
  geom_point()+
  labs(title= "Car weight vs MPG")

# Base R scatter plot with coloring
plot(x=mtcars$mpg,
     y=mtcars$wt,
     ylab="wt",
     xlab="mpg",
     col=1:length(unique(mtcars$gear)),
     main="Car weight vs MPG, by gears",
     pch=19)
legend("topright",
       legend=unique(mtcars$gear),
       col=1:length(unique(mtcars$gear)),
       pch=19,
       title="Gear Number")

# GGplot scatter plot with colors and sizes
ggplot(mtcars,
       aes(x=mpg,y=wt,col=factor(gear)),size=factor(carb))+
  geom_point(aes(size=factor(carb)))+
  labs(title="Car weight vs MPG, by gears and carburetor",
       col="Gear Number",
       size="Carburetor")

## GGplot histogram 
ggplot(mtcars,aes(x=mpg))+
  geom_histogram(bins=10,
                 fill="light blue",
                 col="black")+
  labs(title="MPG Distribution")

## GGplot density plot
ggplot(mtcars,aes(x=mpg))+
  geom_density(fill="light blue",
               alpha=0.5)+
  labs(title="MPG Distribution")

## Facetted histograms
ggplot(mtcars,aes(x=mpg))+
  geom_histogram(bins = 15,
                 col="black",
                 fill="light blue")+
  facet_grid(.~cyl)+
  labs(title="MPG, by Cylinder #")

## Facetted histograms with a filling variable
ggplot(mtcars,aes(x=mpg,fill=factor(carb)))+
  geom_histogram(bins = 5,
                 col="black",
                 position = "dodge")+
  facet_grid(.~cyl)+
  labs(fill="Carburetor",
       title="MPG, by Cylinder and Carburetor #'s")
  
## Bar plot with frequencies
ggplot(mtcars,aes(x=gear))+
  geom_bar(aes(y=..prop..),
           fill=c("red","orange","yellow"),
           col="black",
           stat="count")+
  geom_text(aes(label=round(..prop..,2),y=..prop..),
            stat="count",
            vjust=-0.5)+
  scale_y_continuous(limits = c(0,0.6))+
  labs(title="Gear # Proportions")

## Bar plot split by normed distance from average
mtcars%>%
  mutate(`car name`=rownames(mtcars), 
         mpg_z = round(scale(mpg), 2), 
         mpg_type = ifelse(mpg_z < 0, "below", "above"),
         `car name` = factor(`car name`, 
                             levels = mtcars$`car name`))%>%
  arrange(mpg_z)%>%
  ggplot(., aes(x=`car name`, y=mpg_z, label=mpg_z)) + 
  geom_bar(stat='identity', aes(fill=mpg_type), width=.5)  +
  scale_fill_manual(name="Mileage", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(subtitle="Normalised mileage from 'mtcars'", 
       title= "Diverging Bars") + 
  theme(axis.text.y=element_text(size=7))+
  coord_flip()

## Density plot with new facetting and labels
ggplot(mtcars,aes(x=mpg,fill=factor(carb)))+
  geom_density(alpha=0.5)+
  facet_grid(.~cyl>6,
             labeller=as_labeller(c(`TRUE`="4 and 6 Cylinder",
                                    `FALSE`="8 Cylinder")))+
  labs(fill="Carburetor",
       title = "MPG, by Cylinder and Carburetor #'s")

## Violin plot
ggplot(mtcars,
       aes(y=disp,x=factor(gear),fill=factor(gear)))+
  geom_violin()+
  labs(fill="Gear #",
       title="Displacement by # of Gears")+
  xlab(label="Gear #")


EuStockMarketsLong<-EuStockMarkets%>%
  data.frame()%>%
  mutate(Time=row_number())%>%
  pivot_longer(.,cols=1:4,
               names_to = "Indices",values_to = "Closing Price")


## Longitudinal line plot
ggplot(EuStockMarketsLong,
       aes(x=Time,y=`Closing Price`,col=Indices))+
  geom_line()+
  labs(title="Trends in European Stock Closing Prices")

## Longitudinal area plot 
ggplot(EuStockMarketsLong,
       aes(x=Time,y=`Closing Price`,fill=Indices))+
  geom_area()+
  labs(title="Trends in European Stock Closing Prices")

Titanic<-Titanic%>%as.data.frame()

## Alluvial plot
ggplot(Titanic,aes(y=Freq,axis1=Survived,axis2=Sex,axis3=Class)) +
  geom_alluvium(aes(fill = Class),
                width = 0, knot.pos = 0, reverse = FALSE) +
  geom_stratum(width = 1/4, reverse = FALSE) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
            reverse = FALSE) +
  scale_x_continuous(breaks = 1:3, labels = c("Survived", "Sex","Class")) +
  coord_flip() +
  ggtitle("Titanic survival by class and sex")

## Heat map
ggplot(airquality,
       aes(x=Month,y=Day,fill=Temp))+
  geom_tile()+
  labs(title="Literal Heat Map")


## Convoluted box plot
mtcars%>%
  group_by(cyl,carb)%>%
  count(mpg<20)%>%
  ungroup()%>%
  group_by(`mpg < 20`)%>%
  mutate(Percent=100*n/sum(n))%>%
  ggplot(.,
         aes(y=Percent,
             x=factor(cyl),
             fill=factor(carb)))+
  geom_col(position = "dodge",
           col="black")+
  geom_text(aes(label=round(Percent,1)),
            size=3,
            vjust=-0.5,
            position = position_dodge(width = 1))+
  scale_y_continuous(limits = c(0,50))+
  labs(fill="Carburetor",
       title="Percent of total, for cyclinder and carburetor combinations")+
  xlab(label="Cylinder")+
  facet_grid(`mpg < 20`~.,
             labeller=as_labeller(c(`FALSE`="More than 20 MPG",
                                    `TRUE`="Less than 20 MPG")))


  
