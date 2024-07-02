library(ggplot2)
library(deeptime)

#setwd()

#Read in Data
data <- read.csv("Stammberger_BodyMass_2024.csv", header = TRUE, sep = ";")

summary(data)

#Cleaning up Data
data <- data[,-c(3,5,6)]

headers <- c("Taxa", "Clade", "Body_mass_Kg", "Early_Interval", "Late_Interval", "Max_ma", "Min_ma")

colnames(data) <- headers


str(data)

data$Clade <- as.factor(data$Clade)


data$Mid_ma <- (data$Min_ma + data$Max_ma)/2

data$Body_mass_log <- log(data$Body_mass_Kg)

#########################
########Plotting#########
#########################

###Plotting Data (kg)
ggplot(data = data, aes(x = Mid_ma, y= Body_mass_Kg, col = Clade)) +
  xlab("Age") + ylab("Body mass (kg)") +
    geom_point() +
    geom_vline(xintercept=201.4) +
    scale_x_reverse() +
    coord_geo(
      dat = list("stages", "periods"), xlim = c(233, 174.1),
      pos = list("b", "b"), abbrv = list(TRUE, FALSE), center_end_labels = TRUE, height = unit(1.5, "line"), expand = TRUE,
    )


####Building models

#plotting models kg
plotkg <- ggplot(data = data, aes(x = Mid_ma, y= Body_mass_Kg, col = Clade)) +
  xlab("Age") + ylab("Body mass (kg)") +
  geom_point() +
  geom_vline(xintercept=201.4) +
  geom_smooth(method = "lm") +
  scale_x_reverse() +
  coord_geo(
    dat = list("stages", "periods"), xlim = c(233, 174.1),
    pos = list("b", "b"), abbrv = list(TRUE, FALSE), center_end_labels = TRUE, height = unit(1.5, "line"), expand = TRUE,
  )

###Slopes using lm model
##dat subsetting to the 3 clades
data_sauropodomorpha <- data[data$Clade == "Sauropodomorpha", ]
data_Theropoda <- data[data$Clade == "Theropoda", ]
data_Ornithischia <- data[data$Clade == "Ornithischia", ]
##Slope values
#Sauropodomorpha
lm_sauropodomorpha = lm(Body_mass_Kg ~ Mid_ma, data_sauropodomorpha)
summary(lm_sauropodomorpha) # 82.38
#Theropoda
lm_Theropoda = lm(Body_mass_Kg ~ Mid_ma, data_Theropoda)
summary(lm_Theropoda) # 3.09
#Ornithischia
lm_Ornitischia = lm(Body_mass_Kg ~ Mid_ma, data_Ornithischia)
summary(lm_Ornitischia) # 2.35

###Mean values
##use subsets
mean(data_sauropodomorpha$Body_mass_Kg) #2307.014
mean(data_Theropoda$Body_mass_Kg) #106.8632
mean(data_Ornithischia$Body_mass_Kg) #109.5143

######################
#plotting models log(kg)
plotLog <- ggplot(data = data, aes(x = Mid_ma, y= Body_mass_log, col = Clade)) + geom_point() + 
  xlab("Age") + ylab("Body mass (log)") +
  geom_vline(xintercept=201.4) +
  geom_smooth(method = "lm", se=FALSE) +
  scale_x_reverse() +
  theme_minimal()  +
  coord_geo(
    dat = list("stages", "periods"), xlim = c(233, 174.1),
    pos = list("b", "b"), abbrv = list(TRUE, FALSE), center_end_labels = TRUE, height = unit(1.5, "line"), expand = TRUE,
  )

###Slopes using lm model
##Slope values
#Sauropodomorpha
lm_sauropodomorpha_Log = lm(Body_mass_log ~ Mid_ma, data_sauropodomorpha)
summary(lm_sauropodomorpha_Log) # 0.09
#Theropoda
lm_Theropoda_Log = lm(Body_mass_log ~ Mid_ma, data_Theropoda)
summary(lm_Theropoda_Log) # 0.03
#Ornithischia
lm_Ornitischia_Log = lm(Body_mass_log ~ Mid_ma, data_Ornithischia)
summary(lm_Ornitischia_Log) # -0.01

###Mean values
##use subsets
mean(data_sauropodomorpha$Body_mass_log) #6.637625
mean(data_Theropoda$Body_mass_log) #3.281556
mean(data_Ornithischia$Body_mass_log) #2.997283


######################
####building models Sauropodomorph vs Non-Sauropodomorphs
data_sauropodomorpha <- data[data$Clade == "Sauropodomorpha", ]

data_nonsauropodomorph <- data[data$Clade != "Sauropodomorpha", ]
data_nonsauropodomorph$Clade <- "Non-Sauropodomorpha"
###combine Non-Sauropodomorpha from Theropoda and Ornitischia have very low r values

data_two <- rbind(data_sauropodomorpha, data_nonsauropodomorph)
######################

######################
## plotting Non-Sauropodomorpha

plotNonSauropod <- ggplot(data = data_two, aes(x = Mid_ma, y= Body_mass_log, col = Clade)) +
  xlab("Age") + ylab("Body mass (log)") +
  geom_point() +
  geom_vline(xintercept=201.4)+
  geom_smooth(method = "lm") +
  scale_x_reverse()  +
  coord_geo(
    dat = list("stages", "periods"), xlim = c(233, 174.1),
    pos = list("b", "b"), abbrv = list(TRUE, FALSE), center_end_labels = TRUE, height = unit(1.5, "line"), expand = TRUE,
  )

###Slopes using lm model
##Slope values
#NonSauropodomorpha
lm_NonSauropodmorpha = lm(Body_mass_log ~ Mid_ma, data_nonsauropodomorph)
summary(lm_NonSauropodmorpha) # 0.02

###Mean values
##use subsets
mean(data_sauropodomorpha$Body_mass_log) #6.637625
mean(data_nonsauropodomorph$Body_mass_log) #3.205021

###########################
####Triassic Data
###########################
dataTriassic <- data[data$Min_ma >=201.3,]

######################
## plotting data
plotTrias <- ggplot(data = dataTriassic, aes(x = Mid_ma, y= Body_mass_Kg, col = Clade)) +
  xlab("Age") + ylab("Body mass (kg)") +
  geom_point() +
  geom_vline(xintercept=201.4) +
  scale_x_reverse()  +
  coord_geo(
    dat = list("stages", "periods"), xlim = c(234, 203),
    pos = list("b", "b"), abbrv = list(TRUE, FALSE), center_end_labels = TRUE, height = unit(1.5, "line"), expand = TRUE,
  )

######################
##plotting models kg
plotkgTrias <- ggplot(data = dataTriassic, aes(x = Mid_ma, y= Body_mass_Kg, col = Clade)) +
  xlab("Age") + ylab("Body mass (kg)") +
  geom_point() +
  geom_vline(xintercept=201.4) +
  geom_smooth(method = "lm") +
  scale_x_reverse()  +
  coord_geo(
    dat = list("stages", "periods"), xlim = c(234, 203),
    pos = list("b", "b"), abbrv = list(TRUE, FALSE), center_end_labels = TRUE, height = unit(1.5, "line"), expand = TRUE,
  )

##data subsetting to the 2 clades
data_sauropodomorpha_Triassic <- dataTriassic[dataTriassic$Clade == "Sauropodomorpha", ]
data_Theropoda_Triassic <- dataTriassic[dataTriassic$Clade == "Theropoda", ]
##Slope values
#Sauropodomorpha
lm_sauropodomorpha_Triassic = lm(Body_mass_Kg ~ Mid_ma, data_sauropodomorpha_Triassic)
summary(lm_sauropodomorpha_Triassic) # 92.29
#Theropoda
lm_Theropoda_Triassic = lm(Body_mass_Kg ~ Mid_ma, data_Theropoda_Triassic)
summary(lm_Theropoda_Triassic) # -0.78

###Mean values
##use subsets
mean(data_sauropodomorpha_Triassic$Body_mass_Kg) #1378.133
mean(data_Theropoda_Triassic$Body_mass_Kg) #54.25
#Ornithischians = 35.5

######################
##plotting models log(kg)
plotLogTrias <- ggplot(data = dataTriassic, aes(x = Mid_ma, y= Body_mass_log, col = Clade)) + geom_point() +
  xlab("Age") + ylab("Body mass (log)") +
  geom_vline(xintercept=201.4) +
  geom_smooth(method = "lm", se=FALSE) +
  scale_x_reverse() +
  theme_minimal()  +
  coord_geo(
    dat = list("stages", "periods"), xlim = c(234, 203),
    pos = list("b", "b"), abbrv = list(TRUE, FALSE), center_end_labels = TRUE, height = unit(1.5, "line"), expand = TRUE,
  )

##Slope values
#Sauropodomorpha
lm_sauropodomorpha_Triassic_Log = lm(Body_mass_log ~ Mid_ma, data_sauropodomorpha_Triassic)
summary(lm_sauropodomorpha_Triassic_Log) # 0.2
#Theropoda
lm_Theropoda_Triassic_Log = lm(Body_mass_log ~ Mid_ma, data_Theropoda_Triassic)
summary(lm_Theropoda_Triassic_Log) # -0.02

###Mean values
##use subsets
mean(data_sauropodomorpha_Triassic$Body_mass_log) #5.947717
mean(data_Theropoda_Triassic$Body_mass_log) #2.682488
#Ornithischians = 3.57


######################
####plotting Sauropodomoprha/Non-Sauropodomorpha
data_sauropodomorpha_Trias <- dataTriassic[dataTriassic$Clade == "Sauropodomorpha", ]

data_nonsauropodomorph_Trias <- dataTriassic[dataTriassic$Clade != "Sauropodomorpha", ]
data_nonsauropodomorph_Trias$Clade <- "Non-Sauropodomorpha"

data_two_Triassic <- rbind(data_sauropodomorpha_Trias, data_nonsauropodomorph_Trias)
######################

######################
##plotting models log(kg)
plotNonSauropod_Trias <- ggplot(data = data_two_Triassic, aes(x = Mid_ma, y= Body_mass_log, col = Clade)) +
  xlab("Age") + ylab("Body mass (log)") +
  geom_point() +
  geom_vline(xintercept=201.4)+
  geom_smooth(method = "lm") +
  scale_x_reverse()  +
  coord_geo(
    dat = list("stages", "periods"), xlim = c(234, 203),
    pos = list("b", "b"), abbrv = list(TRUE, FALSE), center_end_labels = TRUE, height = unit(1.5, "line"), expand = TRUE,
  )

##Slope values
#NonSauropodomorpha
lm_NonSauropodomorpha_Triassic_Log = lm(Body_mass_log ~ Mid_ma, data_nonsauropodomorph_Trias)
summary(lm_NonSauropodomorpha_Triassic_Log) # -0.02

###Mean values
##use subsets
mean(data_sauropodomorpha_Triassic$Body_mass_log) #5.947717
mean(data_nonsauropodomorph_Trias$Body_mass_log) #2.750722


###########################
####Jurassic Data
###########################
dataJurassic <- data[data$Max_ma <= 201.4,]

######################
## plotting data
plotJura <- ggplot(data = dataJurassic, aes(x = Mid_ma, y= Body_mass_Kg, col = Clade)) +
  xlab("Age") + ylab("Body mass (kg)") +
  geom_point() +
  geom_vline(xintercept=201.4) +
  scale_x_reverse()  +
  coord_geo(
    dat = list("stages", "periods"), xlim = c(200, 174.1),
    pos = list("b", "b"), abbrv = list(TRUE, FALSE), center_end_labels = TRUE, height = unit(1.5, "line"), expand = TRUE,
  )

######################
##plotting models kg
plotkgJura <- ggplot(data = dataJurassic, aes(x = Mid_ma, y= Body_mass_Kg, col = Clade)) +
  xlab("Age") + ylab("Body mass (kg)") +
  geom_point() +
  geom_vline(xintercept=201.4) +
  geom_smooth(method = "lm") +
  scale_x_reverse()  +
  coord_geo(
    dat = list("stages", "periods"), xlim = c(200, 174.1),
    pos = list("b", "b"), abbrv = list(TRUE, FALSE), center_end_labels = TRUE, height = unit(1.5, "line"), expand = TRUE,
  )

##data subsetting to the 3 clades
data_sauropodomorpha_Jurassic <- dataJurassic[dataJurassic$Clade == "Sauropodomorpha", ]
data_Theropoda_Jurassic <- dataJurassic[dataJurassic$Clade == "Theropoda", ]
data_Ornithischia_Jurassic <- dataJurassic[dataJurassic$Clade == "Ornithischia", ]
##Slope values
#Sauropodomorpha
lm_sauropodomorpha_Jurassic = lm(Body_mass_Kg ~ Mid_ma, data_sauropodomorpha_Jurassic)
summary(lm_sauropodomorpha_Jurassic) # 135.8
#Theropoda
lm_Theropoda_Jurassic = lm(Body_mass_Kg ~ Mid_ma, data_Theropoda_Jurassic)
summary(lm_Theropoda_Jurassic) # -10.75
#Ornithischia
lm_Ornithischia_Jurassic = lm(Body_mass_Kg ~ Mid_ma, data_Ornithischia_Jurassic)
summary(lm_Ornithischia_Jurassic) # -3.36e-01

###Mean values
##use subsets
mean(data_sauropodomorpha_Jurassic$Body_mass_Kg) #3187.005
mean(data_Theropoda_Jurassic$Body_mass_Kg) #197.0571
mean(data_Ornithischia_Jurassic$Body_mass_Kg) #121.85

######################
##plotting models log(kg)
plotLogJura <- ggplot(data = dataJurassic, aes(x = Mid_ma, y= Body_mass_log, col = Clade)) + geom_point() +
  xlab("Age") + ylab("Body mass (log)") +
  geom_vline(xintercept=201.4) +
  geom_smooth(method = "lm", se=FALSE) +
  scale_x_reverse() +
  theme_minimal()  +
  coord_geo(
    dat = list("stages", "periods"), xlim = c(200, 174.1),
    pos = list("b", "b"), abbrv = list(TRUE, FALSE), center_end_labels = TRUE, height = unit(1.5, "line"), expand = TRUE,
  )

##Slope values
#Sauropodomorpha
lm_sauropodomorpha_Jurassic_log = lm(Body_mass_log ~ Mid_ma, data_sauropodomorpha_Jurassic)
summary(lm_sauropodomorpha_Jurassic_log) # 0.08
#Theropoda
lm_Theropoda_Jurassic_log = lm(Body_mass_log ~ Mid_ma, data_Theropoda_Jurassic)
summary(lm_Theropoda_Jurassic_log) # -0.16
#Ornithischia
lm_Ornithischia_Jurassic_log = lm(Body_mass_log ~ Mid_ma, data_Ornithischia_Jurassic)
summary(lm_Ornithischia_Jurassic_log) # -0.24

###Mean values
##use subsets
mean(data_sauropodomorpha_Jurassic$Body_mass_log) #7.291221
mean(data_Theropoda_Jurassic$Body_mass_log) #4.30853
mean(data_Ornithischia_Jurassic$Body_mass_log) #2.901908


######################
###plotting Sauropodomoprha/Non-Sauropodomorpha
data_sauropodomorpha_Jura <- dataJurassic[dataJurassic$Clade == "Sauropodomorpha", ]

data_nonsauropodomorph_Jura <- dataJurassic[dataJurassic$Clade != "Sauropodomorpha", ]
data_nonsauropodomorph_Jura$Clade <- "Non-Sauropodomorpha"

data_two_Jurassic <- rbind(data_sauropodomorpha_Jura, data_nonsauropodomorph_Jura)
######################

######################
#plotting models log(kg)
plotNonSauropod_Jura <- ggplot(data = data_two_Jurassic, aes(x = Mid_ma, y= Body_mass_log, col = Clade)) +
  xlab("Age") + ylab("Body mass (log)") +
  geom_point() +
  geom_vline(xintercept=201.4)+
  geom_smooth(method = "lm") +
  scale_x_reverse() +
  coord_geo(
    dat = list("stages", "periods"), xlim = c(200, 174.1),
    pos = list("b", "b"), abbrv = list(TRUE, FALSE), center_end_labels = TRUE, height = unit(1.5, "line"), expand = TRUE,
  )

##Slope values
#NonSauropodomorpha
lm_NonSauropodomorpha_Jurassic_Log = lm(Body_mass_log ~ Mid_ma, data_nonsauropodomorph_Jura)
summary(lm_NonSauropodomorpha_Jurassic_Log) # -0.04

###Mean values
##use subsets
mean(data_sauropodomorpha_Jurassic$Body_mass_log) #7.291221
mean(data_nonsauropodomorph_Jura$Body_mass_log) #3.65932


##########################################
#Boxplots
data_sauropodomorpha_Jura$Time <- "Jurassic"
data_sauropodomorpha_Trias$Time <- "Triassic"
dataTriasJura_Sauropod <- rbind(data_sauropodomorpha_Trias, data_sauropodomorpha_Jura)
Box <- ggplot(dataTriasJura_Sauropod, aes(x = Time, y = Body_mass_log)) +
    geom_boxplot() + 
    geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5) +
    ylab("Body Mass (log)") +
    scale_x_discrete(name = "System", limits=c("Triassic", "Jurassic"))

###Test of significant difference
t.test(data_nonsauropodomorph_Jura$Body_mass_log, data_sauropodomorpha_Trias$Body_mass_log, var.equal = FALSE, alternative="two.sided")

##p value
#p-value for alternative="two.sided" = 0.0074

