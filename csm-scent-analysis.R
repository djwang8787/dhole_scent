pacman::p_load(dplyr, ggplot2, lme4, scales)

data = read.csv("CSV Files/Dhole CSV.csv") %>%
  rename(Behaviour = Interval.Channel.1.Value) %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%d/%m/%y %H:%M"),
         Conditions = as.factor(Conditions),
         Behaviour = as.factor(Behaviour))

#### Behaviour data ####
behaviour.data = data %>%
  select(SessionID, Frame.Number, Observer, DateTime, Group.Count, Behaviour, Conditions) %>%
  filter(DateTime >= "2022-06-06 14:56:00") %>%
  na.omit()

bd.table = behaviour.data %>%
  group_by(Conditions, Behaviour) %>%
  summarise(counts = n()) %>%
  group_by(Conditions) %>%
  mutate(total = sum(counts)) %>%
  group_by(Conditions, Behaviour) %>%
  summarise(prop = (counts/total)*100)

##### Round 2 ####

tt1 = behaviour.data %>%
  group_by(Conditions, SessionID) %>%
  summarise(Scent = mean(Behaviour == "Investigation of scent"),
         Investigate = mean(Behaviour == "Investigative Behaviour"),
         Locomotion = mean(Behaviour == "Locomotion"),
         OOS = mean(Behaviour == "OOS"),
         Others = mean(Behaviour == "Others"),
         Positive.Int = mean(Behaviour == "Positive interactions"),
         Vigilance = mean(Behaviour == "Resting/Vigilance"),
         Sleep = mean(Behaviour == "Sleep"))


model1 = glm(OOS~Conditions,
            family = binomial(),
            data = tt1)

model.null = glm(OOS~1,
                family = binomial(),
                data = tt1)

MuMIn::model.sel(model1, model.null) # null model selected


model1 = lme4::glmer(OOS~Conditions + (1|SessionID),
             family = binomial(),
             data = tt1)

model.null = lme4::glmer(OOS~1 + (1|SessionID),
                 family = binomial(),
                 data = tt1)

MuMIn::model.sel(model1, model.null) # scent model selected

model1 = glm(Locomotion~Conditions,
             family = binomial(),
             data = tt1)

model.null = glm(Locomotion~1,
                 family = binomial(),
                 data = tt1)

MuMIn::model.sel(model1, model.null) # null model selected

model1 = glm(Vigilance~Conditions,
             family = binomial(),
             data = tt1)

model.null = glm(Vigilance~1,
                 family = binomial(),
                 data = tt1)

MuMIn::model.sel(model1, model.null) # null model selected

model1 = glm(Sleep~Conditions,
             family = binomial(),
             data = tt1)

model.null = glm(Sleep~1,
                 family = binomial(),
                 data = tt1)

MuMIn::model.sel(model1, model.null) # null model selected

# Table 1: Activity budget across treatments(conditions)
tt1 %>%
  group_by(Conditions) %>%
  summarise(Scent = mean(Scent)*100,
            Investigate = mean(Investigate)*100,
            Locomotion = mean(Locomotion)*100,
            OOS = mean(OOS)*100,
            Others = mean(Others)*100,
            Positive.Int = mean(Positive.Int)*100,
            Vigilance = mean(Vigilance)*100,
            Sleep = mean(Sleep)*100)

#### Spatial data ####
EucDist <- function(vect1, vect2) sqrt(sum((vect1 - vect2)^2))

areas.df = data.frame(
  Area = c("A", "B", "C", "D", "E", "F", "G", "H"),
  X = c(78, 120, 193, 261, 282, 280, 378, 527),
  Y = c(229,153,122,181,86,115,126,131)
)

spatial.data = data %>%
  select(SessionID, Frame.Number, Observer, DateTime, Group.Count, Space.Use.Coordinate.X, Space.Use.Coordinate.Y,
         Conditions, Cond.Locations1, Cond.Locations2) %>%
  rename(X = Space.Use.Coordinate.X,
         Y = Space.Use.Coordinate.Y) %>%
  filter(DateTime >= "2022-06-06 14:56:00") %>%
  select(!Group.Count) %>%
  filter_at(vars(X, Y), all_vars(!is.na(.))) %>%
  mutate(Treatment = ifelse(Conditions == "Baseline", 0, 1))

spatial.data$Y = spatial.data$Y*-1

# for all distances to each point
for (i in 1:nrow(spatial.data)) {
  spatial.data$distA[i] = EucDist(spatial.data[i, 5:6], areas.df[1, 2:3]) #see function; euc dist to area A
  spatial.data$distB[i] = EucDist(spatial.data[i, 5:6], areas.df[2, 2:3]) #see function; euc dist to area B
  spatial.data$distC[i] = EucDist(spatial.data[i, 5:6], areas.df[3, 2:3]) #see function; euc dist to area C
  spatial.data$distD[i] = EucDist(spatial.data[i, 5:6], areas.df[4, 2:3]) #see function; euc dist to area D
  spatial.data$distE[i] = EucDist(spatial.data[i, 5:6], areas.df[5, 2:3]) #see function; euc dist to area E
  spatial.data$distF[i] = EucDist(spatial.data[i, 5:6], areas.df[6, 2:3]) #see function; euc dist to area F
  spatial.data$distG[i] = EucDist(spatial.data[i, 5:6], areas.df[7, 2:3]) #see function; euc dist to area G
  spatial.data$distH[i] = EucDist(spatial.data[i, 5:6], areas.df[8, 2:3]) #see function; euc dist to area H
}

# Overview of points
ggplot(spatial.data) +
  geom_point(aes(x = X, y = Y, colour = Treatment)) +
  facet_grid(.~Treatment)

# Resource selection functions - test
# Areas A, F, H are selected for with regards to treatment'ed days.

model0 = spatial.data %>%
  filter(Cond.Locations1 == "A") %>%
  lm(distA~1,
     data = .)

model1 = spatial.data %>%
  filter(Cond.Locations1 == "A") %>%
  lm(distA~Conditions,
     data = .)

MuMIn::model.sel(model0, model1)


model0 = lm(distA ~ 1,
            data = spatial.data)

model1 = lm(distA ~ Conditions,
            data = spatial.data)

MuMIn::model.sel(model0, model1) #model1
confint(model1) #Cinnamon, Coffee, Perfume

model0 = lm(distB ~ 1,
            data = spatial.data)

model1 = lm(distB ~ Conditions,
            data = spatial.data)

MuMIn::model.sel(model0, model1) #model1
confint(model1) #Cinnamon, Coffee, Perfume


model0 = lm(distC ~ 1,
            data = spatial.data)

model1 = lm(distC ~ Conditions,
            data = spatial.data)

MuMIn::model.sel(model0, model1) #model1
confint(model1) #Cinnamon, Coffee, Perfume

model0 = lm(distD ~ 1,
            data = spatial.data)

model1 = lm(distD ~ Conditions,
            data = spatial.data)

MuMIn::model.sel(model0, model1) #model1
confint(model1) #Cinnamon, Coffee, Perfume

fig.plot = spatial.data %>%
  group_by(Conditions) %>%
  summarise(dist.A = mean(distA),
            dist.B = mean(distB),
            dist.C = mean(distC),
            dist.D = mean(distD),
            dist.E = mean(distE),
            dist.F = mean(distF),
            dist.G = mean(distG),
            dist.H = mean(distH))

fig.plot = fig.plot %>%
  reshape2::melt(id = c("Conditions"))

fig.plot1 = fig.plot %>%
  group_by(variable) %>%
  mutate(scaled.dist = scales::rescale(value))

ggplot(fig.plot1) +
  geom_point(aes(x = variable, y = scaled.dist, colour = Conditions))

fig.plot2 = fig.plot %>%
  group_by(Conditions) %>%
  mutate(scaled.dist = scales::rescale(value))



conditions1.plot = spatial.data %>%
  group_by(Conditions, Cond.Locations1) %>%
  summarise(dist.A = mean(distA),
            dist.B = mean(distB),
            dist.C = mean(distC),
            dist.D = mean(distD),
            dist.E = mean(distE),
            dist.F = mean(distF),
            dist.G = mean(distG),
            dist.H = mean(distH))

conditions2.plot = spatial.data %>%
  group_by(Conditions, Cond.Locations2) %>%
  summarise(dist.A = mean(distA),
            dist.B = mean(distB),
            dist.C = mean(distC),
            dist.D = mean(distD),
            dist.E = mean(distE),
            dist.F = mean(distF),
            dist.G = mean(distG),
            dist.H = mean(distH))

conditions2.plot = conditions2.plot %>%
  reshape2::melt(id = c("Conditions", "Cond.Locations2"))

ggplot(conditions2.plot) +
  geom_point(aes(x = variable, y = value, colour = Cond.Locations2)) +
  facet_grid(.~Conditions)


# # Just to those 'treatment' datasets
# euc.dist.df = spatial.data %>%
#   filter(!Conditions %in% c("Baseline"))
#
# for (i in 1:nrow(euc.dist.df)) {
#   c = euc.dist.df[i, 5:6]
#   d = euc.dist.df[i, 10:11]
#   e = euc.dist.df[i, 12:13]
#   euc.dist.df$cond1.dist[i] = EucDist(c, d) #see function; euclidean distance from individual to condition 1
#   euc.dist.df$cond2.dist[i]  = EucDist(c, e) #to condition 2
# }


