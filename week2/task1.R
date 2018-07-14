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


rock
ggplot(data = rock, aes(area, perm))+
  geom_point()


