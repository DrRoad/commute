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


work_travel %>% rename(
  res_code = SA2_code_usual_residence_address,
  res_name = SA2_name_usual_residence_address,
  res_east = SA2_usual_residence_easting,
  res_north = SA2_usual_residence_northing,
  work_code = SA2_code_workplace_address,
  work_name = SA2_name_workplace_address,
  work_east = SA2_workplace_easting,
  work_north = SA2_workplace_northing,
  home = Work_at_home,
  private = Drive_a_private_car_truck_or_van,
  company = Drive_a_company_car_truck_or_van,
  passenger = Passenger_in_a_car_truck_van_or_company_bus,
  bus = Public_bus,
  train = Train,
  bicycle = Bicycle,
  walk = Walk_or_jog,
  ferry = Ferry,
  other = Other,
  total = Total
) -> work_simp

work_simp %>% 
  group_by(res_code,
                         res_name,
                         res_east,
                         res_north) %>%
  summarise(
    home = sum(ifelse(home < 0, 0, home)),
    private = sum(ifelse(private < 0, 0, private)),
    company = sum(ifelse(company < 0, 0, company)),
    passenger = sum(ifelse(passenger < 0, 0, passenger)),
    bus = sum(ifelse(bus < 0, 0, bus)),
    train = sum(ifelse(train < 0, 0, train)),
    bicycle = sum(ifelse(bicycle < 0, 0, bicycle)),
    walk = sum(ifelse(walk < 0, 0, walk)),
    ferry = sum(ifelse(ferry < 0, 0, ferry)),
    other = sum(ifelse(other < 0, 0, other)),
    total = sum(ifelse(total < 0, 0, total)), .groups="drop"
  ) -> work_from
    

work_simp %>% 
  group_by(work_code,
                         work_name,
                         work_east,
                         work_north) %>%
  summarise(
    home = sum(ifelse(home < 0, 0, home)),
    private = sum(ifelse(private < 0, 0, private)),
    company = sum(ifelse(company < 0, 0, company)),
    passenger = sum(ifelse(passenger < 0, 0, passenger)),
    bus = sum(ifelse(bus < 0, 0, bus)),
    train = sum(ifelse(train < 0, 0, train)),
    bicycle = sum(ifelse(bicycle < 0, 0, bicycle)),
    walk = sum(ifelse(walk < 0, 0, walk)),
    ferry = sum(ifelse(ferry < 0, 0, ferry)),
    other = sum(ifelse(other < 0, 0, other)),
    total = sum(ifelse(total < 0, 0, total)), .groups="drop"
  ) -> work_to

tencols <-  c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", 
              "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a")
tencols[which.max(work_from[1, 5:14])]


work_from$MAX <- work_from %>% select(home:other) %>% as.matrix() %>% 
  apply(1, function(x) {
    ifelse(max(x) <= 0, NA, which.max(x))
    })
work_to$MAX <- work_to %>% select(home:other) %>% as.matrix() %>% 
  apply(1, function(x) {
    ifelse(max(x) <= 0, NA, which.max(x))
    })
work_simp$MAX <- work_simp %>% select(home:other) %>% as.matrix() %>% 
  apply(1, function(x) {
    ifelse(max(x) <= 0, NA, which.max(x))
    })

save(work_simp, work_to, work_from, tencols, file="viz/datasets.RData")

