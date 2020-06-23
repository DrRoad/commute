library(readr)
library(ggplot2)
library(dplyr)
library(igraph)
education_travel <- read_csv("travel-education.csv")
work_travel <- read_csv("travel-work.csv")
length(unique(education_travel$SA2_name_usual_residence_address))
length(unique(education_travel$SA2_name_educational_address))
length(unique(work_travel$SA2_name_usual_residence_address))
length(unique(work_travel$SA2_name_workplace_address))

ggplot(work_travel, aes(x = SA2_code_usual_residence_address, 
                        y = SA2_code_workplace_address,
                        fill = Total)) +
  geom_tile()

g2 <- graph( edges=c(4,9, 9,6, 6, 4, 1,2, 5,6, 9,5, 1,4, 1,5, 2,6, 3,3, 6,6), n=10 )
plot(g2)

tg <- make_empty_graph()

work_travel %>% filter(Total > 100) %>% select(SA2_name_usual_residence_address, SA2_name_workplace_address) %>% 
  as.matrix %>% t %>% as.vector -> elist

locgraph <- graph(edges = elist)

# plot(locgraph, label = NA)

sg <- decompose(locgraph, mode="weak")
