#########################################################
### Load basic Packages
#########################################################
if (!require("tidyverse")) {
  install.packages("tidyverse", dependencies = TRUE)
  library(tidyverse)
}
if (!require("lubridate")) {
  install.packages("lubridate", dependencies = TRUE)
  library(lubridate)
}
if (!require("ggplot2")) {
  install.packages("ggplot2", dependencies = TRUE)
  library(ggplot2)
}
if (!require("fiftystater")) {
  install.packages("fiftystater", dependencies = TRUE)
  library(fiftystater)
}
if (!require("maps")) {
  install.packages("maps", dependencies = TRUE)
  library(maps)
}

Project_Dir <- "/Users/amkhosla/Desktop/Statistics/R/Projects/Iraq casualties/"
setwd(Project_Dir)

#########################################################
# Read the Iraq casualties database
#########################################################
casualties_db <- read.csv("/Users/amkhosla/Desktop/Statistics/datasets/iraq_casualties.csv")
#casualties_db <- read.csv(file.choose())

#########################################################
# Wrangle the Iraq casualties database
#########################################################
#We dont' do non-hostiles because sometimes direct cause is missing for non-hostiles
is_hostile_death <- function(x) {
  return (!(str_detect(x, "Non-hostile")))
}

direct_cause_of_death <- function(x) {
  if (is_hostile_death(x)) {
    the.result <- (str_replace(x, fixed("Hostile - "), ""))
    return(the.result)
  } 
}

wrangled_casualties <- casualties_db %>%
  as_tibble() %>%
  mutate(date_of_death = mdy(DATE_OF_DEATH)) %>%
  mutate(year_of_death = year(date_of_death)) %>%
  mutate(month = month(date_of_death)) %>%
  mutate(week_of_death = (100.00*(week(date_of_death)/52.0))) %>%
  mutate(death_day = year_of_death*100 + week_of_death) %>%
  mutate(julian_death = julian(date_of_death)) %>%
  mutate(bin_of_death = year_of_death*100 + week_of_death) %>%
  mutate(hostile_death = is_hostile_death(Cause)) %>%
  mutate(direct_cause = as.factor(direct_cause_of_death(Cause))) 
  
#str(wrangled_casualties)
# What is the average casualties per month
death_bins <- as.tibble(wrangled_casualties$year_of_death*100 + round(100*(wrangled_casualties$month_of_death/12)))
counts_by_month <- as.tibble(table(death_bins))
(average_casualties_per_month <- round(mean(counts_by_month$n)))

#########################################################
# GG Plot themes for big text (for publication)
#########################################################
small_text.theme <- theme_get()
big_text.theme <-  theme(legend.title=element_text(size=18),
                         legend.text=element_text(size=18),
                         axis.text.x = element_text(size=rel(3)),
                         axis.text.y = element_text(size=rel(3)), 
                         axis.title.x = element_text(size=24),
                         axis.title.y = element_text(size=24),
                         plot.title = element_text(size=rel(3), 
                         color = "blue"))
publication.theme <- big_text.theme #or small_text.theme


#########################################################
# Histograms of the data
#########################################################
#Histograms of data
setwd(Project_Dir)

#Table of direct causes
direct_causes <- as.tibble(table(wrangled_casualties$direct_cause))
direct_causes <- direct_causes[order(direct_causes$n),]
jpeg("Deaths_by_Cause.jpg", width=1500,height= 3000) 
par(las=2) # make label text perpendicular to axis
par(mar=c(10,25,4,2)) # increase y-axis margin.
direct_causes.plot <- barplot(direct_causes$n,names.arg = direct_causes$Var1, horiz=TRUE, cex.names=1.0) 
direct_causes.plot
dev.off() 
#direct_causes.plot

#Causes of hostile death
hostile_deaths <- wrangled_casualties %>% filter(hostile_death==TRUE) 
hostile_causes <- as.tibble(table(hostile_deaths$direct_cause))
hostile_causes <- hostile_causes[order(hostile_causes$n),]
jpeg("Hostile_Causes.jpg",  width=1500,height= 3000) 
par(las=2) # make label text perpendicular to axis
par(mar=c(10,25,4,2)) # increase y-axis margin.
hostile_causes.plot <- barplot(hostile_causes$n,names.arg = hostile_causes$Var1, horiz=TRUE, cex.names=1.0)
dev.off() 
#hostile_causes.plot
#non_hostile_deaths <-  wrangled_casualties %>% filter(hostile_death==FALSE)  

#Top 10 causes of hostile death
hostile_causes <- hostile_causes[order(hostile_causes$n, decreasing = TRUE),]
top10_causes <- as.tibble(hostile_causes[seq(10),])
top10_causes <- top10_causes[order(top10_causes$n),]
top10_causes <- top10_causes %>%
  mutate(short_cause = (str_replace(Var1, fixed("hostile "), "")))

jpeg("Top_10_Hostile_Death_Causes.jpg", width=1500,height= 3000) 
top10.plot <- barplot(top10_causes$n,names.arg = top10_causes$short_cause, horiz=TRUE, cex.names=1.0) 
top10.plot
dev.off() 
#top10.plot

#Country data
country.data <- as.tibble(table(wrangled_casualties$Country))
country.data <- country.data[order(country.data$n),]
jpeg("Casualties_by_Country.jpg", width=1500,height= 3000) 
par(las=2) # make label text perpendicular to axis
par(mar=c(10,25,4,2)) # increase y-axis margin.
country.plot <- barplot(country.data$n,names.arg = country.data$Var1, horiz=TRUE, cex.names=1.0) 
country.plot
dev.off() 
#country.plot

#Table of states
data("fifty_states") 
fifty_state_names <- unique(fifty_states$id)
us_casualties <- wrangled_casualties %>%
  filter(Country=="US") %>%
  mutate(map_state = ifelse((tolower(State.Region) %in% fifty_state_names), tolower(State.Region), "NA")) %>%
  filter(map_state != "NA")

state.data <- as_tibble(table(us_casualties$map_state), n="casualties")
state.data <- rename(state.data, state=Var1)  
state.data <- state.data[order(state.data$casualties),]

jpeg("Casulties_by_State.jpg", width=1500,height= 3000) 
par(las=2) # make label text perpendicular to axis
par(mar=c(10,25,4,2)) # increase y-axis margin.
state.plot <- barplot(state.data$casualties,names.arg = state.data$state, horiz=TRUE, cex.names=1.0)
state.plot
dev.off() 

#Death location data
iraq.provinces <- c("Anbar",  "Muthanna",  "Qadisiyyah", "Babil", "Baghdad", "Basra", "Dhi Qar", 
                    "Diyala","Karbala", "Kirkuk", "Maysan", "Najaf", "Nineveh", "Saladin", "Wasit", 
                    "Dohuk", "Erbil", "Sulaymaniyah", "Halabja", "At-Tamim")
province.data <- as.tibble(table(wrangled_casualties$Province))
province.data <- province.data[order(province.data$n),]
province.data <- province.data %>% filter(Var1 %in% iraq.provinces)

jpeg("Casualties_by_Location.jpg", width=1500,height= 3000) 
par(las=2) # make label text perpendicular to axis
par(mar=c(10,25,4,2)) # increase y-axis margin.
province.plot <- barplot(province.data$n,names.arg = province.data$Var1, horiz=TRUE, cex.names=2.0) 
province.plot
dev.off() 
province.plot

#########################################################
# MAP Data
#########################################################
jpeg("Where_did_they_come_from.jpg", width=4000,height= 1500) 
state.map <- ggplot(data= state.data, aes(map_id = state)) + 
    geom_map(aes(fill = casualties),  color= "black", map = fifty_states) + 
    expand_limits(x = fifty_states$long, y = fifty_states$lat) +
    coord_map() +
    # Disable grid and axes
    scale_x_continuous(breaks = NULL) + scale_y_continuous(breaks = NULL) +
    labs(x = "", y = "") + 
    # Increase Legend Size
    guides(fill = guide_legend(keywidth = 5, keyheight = 4,
                               label.theme = element_text(size = 25, face = "italic", angle = 0),
                               title.theme = element_text(size = 30, face = "bold", angle = 0))) +
    theme(legend.position = "left", panel.background = element_blank()) +
    publication.theme
state.map
dev.off() 


#########################################################
# Timeline Data
#########################################################

#"Macro" functions that act like annotate() - inherit from layer(), ggprot()
note_line <- function (geom="segment", x = NULL, y = 0, xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL, 
                          xend = NULL, yend = NULL, ..., na.rm = FALSE) {
    yend<-y;y<-0
    position <- compact(list(x = x, xmin = xmin, xmax = xmax, xend = x, y = y, ymin = ymin, ymax = ymax, yend = yend))
    aesthetics <- c(position, list(...))
    lengths <- vapply(aesthetics, length, integer(1))
    unequal <- length(unique(setdiff(lengths, 1L))) > 1L
    if (unequal) {
        bad <- lengths != 1L
        details <- paste(names(aesthetics)[bad], " (", lengths[bad], 
                         ")", sep = "", collapse = ", ")
        stop("Unequal parameter lengths: ", details, call. = FALSE)
    }
    data <- data.frame(position)
    layer(geom, params = list(na.rm = na.rm, colour="black",...), stat = StatIdentity, 
          position = PositionIdentity, data = data, mapping = aes_all(names(data)), 
          inherit.aes = FALSE, show.legend = FALSE)
}

note_label <- function (geom="text", x = NULL, y = 0, xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL, 
                       ..., na.rm = FALSE) {
    position <- compact(list(x = x, xmin = xmin, xmax = xmax, y = y, ymin = ymin, ymax = ymax))
    aesthetics <- c(position, list(...))
    lengths <- vapply(aesthetics, length, integer(1))
    unequal <- length(unique(setdiff(lengths, 1L))) > 1L
    if (unequal) {
        bad <- lengths != 1L
        details <- paste(names(aesthetics)[bad], " (", lengths[bad], 
                         ")", sep = "", collapse = ", ")
        stop("Unequal parameter lengths: ", details, call. = FALSE)
    }
    data <- data.frame(position)
    layer(geom, params = list(na.rm = na.rm, colour="black", size=9,...), stat = StatIdentity, 
          position = PositionIdentity, data = data, mapping = aes_all(names(data)), 
          inherit.aes = FALSE, show.legend = FALSE)
}

#-----------
# CASUALTIES
#-----------
jpeg("Iraq_Casualties.jpg", width=2700,height= 1500) 
casualties.plot <- ggplot(wrangled_casualties, aes(x=death_day, fill=hostile_death)) + 
    geom_histogram(position="dodge", binwidth=10) +
    note_line(x = 200325, y = 150) + note_label(x = 200358, y = 150, label = "Start of War") +
    note_line(x = 200333, y = 130) + note_label(x = 200373, y = 130, label = "Baghdad Falls") +
    note_line(x = 200430, y = 160) + note_label(x = 200462, y = 157, label = "Abu Ghraib\nPhotos") +
    note_line(x = 200450, y = 150) + note_label(x = 200490, y = 150, label = "Iraq Sovereignty") +
    note_line(x = 200615, y = 160) + note_label(x = 200657, y = 157, label = "Askariya Mosque\nBlownup")  +
    note_line(x = 200691, y = 155) + note_label(x = 200735, y = 155, label = "Rumsfeld Resigns") + 
    note_line(x = 200707, y = 147) + note_label(x = 200737, y = 147, label = "Troop Surge\nAnnounced") + 
    note_line(x = 200767, y = 135) + note_label(x = 200805, y = 135, label = "Moktada al-Sadr\nCeasefire") + 
    note_line(x = 200807, y = 125) + note_label(x = 200855, y = 125, label = "Obama Presidency")  +
    note_line(x = 200867, y = 115) + note_label(x = 200912, y = 115, label = "Anbar Handover")  +
    note_line(x = 201100, y = 150) + note_label(x = 201060, y = 150, label = "Combat Ends")  +
    note_line(x = 201195, y = 150) + note_label(x = 201165, y = 150, label = "War Ends")  +
    scale_x_continuous(limits=c(200300,201200), 
                       breaks=seq(200300,201200,100), 
                       labels = seq(2003,2012)) +
    scale_fill_discrete(name="Cause of Death",  
                        breaks=c(TRUE, FALSE, NA),  
                        labels=c("Hostile", "Non-hostile", "Other")) +

    labs(x = "Year of death", y = "Number of casualties")  +
    labs(title = "Iraq War: Casualties from hostile vs non-hostile attack - 2003-2012") +
    publication.theme

casualties.plot
dev.off() 
#casualties.plot

#---------------
# KILLING ZONES
#---------------
top_killing_zones <- c("Baghdad", "Anbar", "Saladin", "Diyala", 
                       "Babil", "Basra", "At-Tamim") 
provincial_casualties <- wrangled_casualties %>%
    filter(Province %in% top_killing_zones)

jpeg("Top_Casualties_by_Location.jpg", width=2700,height= 1500) 
locations.plot <- ggplot(provincial_casualties, aes(x=death_day, fill=Province)) + 
    geom_histogram(position="dodge", binwidth=30) + 
    note_line(x = 200325, y = 150) + note_label(x = 200355, y = 150, label = "Start of War") +
    note_line(x = 200333, y = 140) + note_label(x = 200370, y = 140, label = "Baghdad Falls") +
    note_line(x = 200430, y = 160) + note_label(x = 200405, y = 160, label = "Abu Ghraib\nPhotos") +
    note_line(x = 200450, y = 175) + note_label(x = 200490, y = 175, label = "Iraq Sovereignty") +
    note_line(x = 200615, y = 160) + note_label(x = 200660, y = 160, label = "Askariya Mosque\nBlownup")  +
    note_line(x = 200691, y = 155) + note_label(x = 200735, y = 155, label = "Rumsfeld Resigns") + 
    note_line(x = 200707, y = 145) + note_label(x = 200740, y = 145, label = "Troop Surge\nAnnounced") + 
    note_line(x = 200767, y = 135) + note_label(x = 200805, y = 135, label = "Moktada al-Sadr\nCeasefire") + 
    note_line(x = 200807, y = 125) + note_label(x = 200855, y = 125, label = "Obama Presidency")  +
    note_line(x = 200867, y = 115) + note_label(x = 200910, y = 115, label = "Anbar Handover")  +
    note_line(x = 201100, y = 150) + note_label(x = 201060, y = 150, label = "Combat Ends")  +
    note_line(x = 201195, y = 150) + note_label(x = 201170, y = 150, label = "War Ends")  +
    scale_x_continuous(limits=c(200300,201200), 
                       breaks=seq(200300,201200,100), 
                       labels = seq(2003,2012)) +
    labs(x = "Year of death", y = "Number of casualties")  +
    labs(title = "Casualties in highest 7 provinces - 2003-2012") +
    publication.theme

locations.plot
dev.off() 
#locations.plot

#---------------
# TOP 5 Causes of death
#---------------
top_causes_of_death <- c("hostile fire - IED attack", "hostile fire", "hostile fire - small arms fire", 
                         "Non-hostile - vehicle accident", "Non-hostile", "Non-hostile - helicopter crash", 
                         "hostile fire - mortar attack", "hostile fire - RPG attack", "hostile fire - car bomb", 
                         "helicopter crash" )
top_causes_of_casualties <- wrangled_casualties %>%
    filter(direct_cause %in% top_causes_of_death)

jpeg("Top_Casualties_by_Cause.jpg", width=2700,height= 1600) 
top_causes.plot <- ggplot(top_causes_of_casualties, aes(x=death_day, fill=direct_cause)) + 
    geom_histogram(position="dodge", binwidth=30) + 
    note_line(x = 200325, y = 225) + note_label(x = 200358, y = 225, label = "Start of War") +
    note_line(x = 200333, y = 180) + note_label(x = 200373, y = 180, label = "Baghdad Falls") +
    note_line(x = 200430, y = 210) + note_label(x = 200462, y = 205, label = "Abu Ghraib\nPhotos") +
    note_line(x = 200450, y = 180) + note_label(x = 200495, y = 180, label = "Iraq Sovereignty") +
    note_line(x = 200615, y = 225) + note_label(x = 200660, y = 225, label = "Askariya Mosque\nBlownup")  +
    note_line(x = 200691, y = 205) + note_label(x = 200740, y = 205, label = "Rumsfeld Resigns") + 
    note_line(x = 200707, y = 175) + note_label(x = 200740, y = 175, label = "Troop Surge\nAnnounced") + 
    note_line(x = 200767, y = 155) + note_label(x = 200805, y = 155, label = "Moktada al-Sadr\nCeasefire") + 
    note_line(x = 200807, y = 125) + note_label(x = 200855, y = 128, label = "Obama Presidency")  +
    note_line(x = 200867, y = 115) + note_label(x = 200912, y = 113, label = "Anbar Handover")  +
    note_line(x = 201100, y = 225) + note_label(x = 201060, y = 225, label = "Combat Ends")  +
    note_line(x = 201195, y = 225) + note_label(x = 201165, y = 225, label = "War Ends")  +
    scale_x_continuous(limits=c(200300,201200),
                   breaks=seq(200300,201200,100),
                   labels = seq(2003,2012)) +
    labs(x = "Year of death", y = "Number of casualties")  +
    labs(title = "Top Casualties by cause - 2003-2012") +
    publication.theme
top_causes.plot
dev.off()
