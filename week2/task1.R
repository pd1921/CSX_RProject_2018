library("ggplot2")

cars
ggplot(data = cars, aes(speed, dist))+
  geom_point()

ChickWeight
ggplot(data = ChickWeight, aes(x=Diet))+
  geom_bar(fill="lightblue", colour = "black")

trees
ggplot(data = trees, aes(Girth, Height))+
  geom_point()


BOD
ggplot(data=BOD, aes(x=Time, y=demand, group=1))+
  geom_line(linetype = "dashed")+
  geom_point()


