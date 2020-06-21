x=rnorm(10)
y=rnorm(10)
t.test(x,y)
t.test(x,y,var.equal = TRUE)
t.test(x,mu=5)
var(x,y=NULL)
cor(x,y)

set.seed(5)
x <- rnorm(30,sd=runif(30,2,50))

###################



install.packages("gapminder")
library(gapminder)           
summary(gapminder)
data("gapminder")
x<-mean(gapminder$gdpPercap)
x
attach(gapminder)
median(pop)
#detach(gapminder)

hist(lifeExp)
hist(log(pop))
boxplot(lifeExp ~ continent)
plot(lifeExp ~ log(gdpPercap))
gapminder %>% select(country,lifeExp) %>% 
  filter(country=="South Africa" | country=="Ireland") %>% 
  group_by(country) %>% 
  summarise(Average_life=mean(lifeExp))
  

df1<-gapminder %>% select(country,lifeExp) %>% 
  filter(country=="South Africa" | country=="Ireland")

t.test(data=df1,lifeExp ~country)

gapminder %>% filter(gdpPercap < 50000) %>% ggplot %>%
  ggplot(aes(x=gdpPercap, y=lifeExp)) 


+
  geom_point()


##########################

#practice tidyverse

library(tidyverse)
data()
view(starwars)
starwars %>% 
  select(gender,mass,height,species) %>% filter(species == "Human") %>% na.omit() %>% 
  mutate(height=height/100) %>% mutate(BMI=mass/height ^ 2) %>% 
  group_by(gender) %>% 
  summarise(Average_BMI=mean(BMI))


#########

sw<-starwars
sw %>% select(name,height,mass,gender)  %>% rename(weight=mass)

sw<-starwars %>% select(name,height,mass,gender) %>% rename(weight=mass) %>% na.omit() %>% 
  mutate(height=height/100) %>% filter(gender %in% c("male","female")) %>% 
  mutate(gender=recode(gender,male="m",female="f")) %>% 
  mutate(size= height >1 & weight >75,size=if_else(size==TRUE,"BIG","small"))

view(msleep) 
mydata<-msleep %>% select(name,order,sleep_total,bodywt) %>% 
  filter(order=="Primates",bodywt>20)

mydata<-msleep %>% select(name,order,sleep_total,bodywt) %>% 
  filter(order=="Primates" | bodywt>20) 

mydata<-msleep %>% select(name,order,sleep_total,bodywt) %>% 
  filter(order=="Primates"& bodywt>20)

mydata<-msleep %>% select(name,order,sleep_total,bodywt) %>% 
  filter(name=="Cow" |
           name== "Horse"|
           name=="Goat" |
           name=="Girafe")


mydata<-msleep %>% select(name,sleep_total) %>% 
  filter(name %in% c("Cow","Dog","Horse","Goat"))

mydata<-msleep %>% select(name,sleep_total) %>% 
  filter(between(sleep_total,16,18))
         

mydata<-msleep %>% select(name,sleep_total) %>% 
  filter(near(sleep_total,15,tol=0.5))

mydata<-msleep %>% select(name,sleep_total,conservation) %>% 
  filter(!is.na(conservation))

