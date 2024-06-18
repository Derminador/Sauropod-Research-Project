library(ggplot2)

#setwd("C:/Studium/Master/2_Semester/Research_Project")
setwd("D:/Studium/Master/2.Semster/Research_project/Data")

data <- read.csv("Stammberger_BodyMass_2024.csv", header = TRUE, sep = ";")

summary(data)

data <- data[,-c(3,5,6)]

headers <- c("Taxa", "Clade", "Body_mass_Kg", "Early_Interval", "Late_Interval", "Max_ma", "Min_ma")

colnames(data) <- headers


str(data)

data$Clade <- as.factor(data$Clade)


data$Mid_ma <- (data$Min_ma + data$Max_ma)/2

data$Body_mass_log <- log(data$Body_mass_Kg)


###Plotting Data (kg)
ggplot(data = data, aes(x = Mid_ma, y= Body_mass_Kg, col = Clade)) +
    geom_point() +
    geom_vline(xintercept=201.4) +
    scale_x_reverse()


###Building models (kg)


## sauropodomorpha linear model
#data_sauropodomorpha <- data[data$Clade == "Sauropodomorpha", ]
#lm_sauropodomorpha = lm(Body_mass_Kg ~ Mid_ma, data_sauropodomorpha)
#summary(lm_sauropodomorpha)

#
plotkg <- ggplot(data = data, aes(x = Mid_ma, y= Body_mass_Kg, col = Clade)) +
  geom_point() +
  geom_vline(xintercept=201.4) +
  geom_smooth(method = "lm") +
  scale_x_reverse()


#  geom_abline(intercept = 19063.54, slope = -81.03, color = "#00BA38") +
 # geom_abline(intercept = 851.77, slope = -3.5 , color = "#619CFF") +
  #geom_abline(intercept = 582,65, slope = -2,35, color = "#F8766D")

#building models log(kg)

# plotting all regression lines

plotLog <- ggplot(data = data, aes(x = Mid_ma, y= Body_mass_log, col = Clade)) + geom_point() +
  geom_vline(xintercept=201.4) +
  geom_smooth(method = "lm", se=FALSE) +
  scale_x_reverse() +
  theme_minimal()

#  geom_abline(intercept = 24.35, slope = -0.086, color = "#00BA38") +
 # geom_abline(intercept = 10,287, slope = -0.033 , color = "#619CFF") +
  #geom_abline(intercept = 0.29, slope = 0.013, color = "#F8766D") 


#building models sauropodomorph vs non sauropodomorphs
data_sauropodomorpha <- data[data$Clade == "Sauropodomorpha", ]

data_nonsauropodomorph <- data[data$Clade != "Sauropodomorpha", ]
data_nonsauropodomorph$Clade <- "Non-Sauropodomorpha"
###combine non sauropodomorpha as theropoda and orni have very low r values

data_two <- rbind(data_sauropodomorpha, data_nonsauropodomorph)

## non sauropodomorpha linear model

lm_nonsauropodomorpha = lm(Body_mass_log ~ Mid_ma, data_nonsauropodomorph)
summary(lm_nonsauropodomorpha)

plotNonSauropod <- ggplot(data = data_two, aes(x = Mid_ma, y= Body_mass_log, col = Clade)) +
  geom_point() +
  geom_vline(xintercept=201.4)+
  geom_smooth(method = "lm") +
  scale_x_reverse()
 
#   geom_abline(intercept = 24.35, slope = -0.086, color = "#00BA38") +
 # geom_abline(intercept = 7.52, slope = -0.02, color = "purple")

###Triassic Data
dataTriassic <- data[data$Min_ma >=201.3,]
## plotting data
plotTrias <- ggplot(data = dataTriassic, aes(x = Mid_ma, y= Body_mass_Kg, col = Clade)) +
  geom_point() +
  geom_vline(xintercept=201.4) +
  scale_x_reverse()
##plotting kg
plotkgTrias <- ggplot(data = dataTriassic, aes(x = Mid_ma, y= Body_mass_Kg, col = Clade)) +
  geom_point() +
  geom_vline(xintercept=201.4) +
  geom_smooth(method = "lm") +
  scale_x_reverse()

##plotting log
plotLogTrias <- ggplot(data = dataTriassic, aes(x = Mid_ma, y= Body_mass_log, col = Clade)) + geom_point() +
  geom_vline(xintercept=201.4) +
  geom_smooth(method = "lm", se=FALSE) +
  scale_x_reverse() +
  theme_minimal()

##plotting Sauropodomoprha/Non-Sauropodomorpha
data_sauropodomorpha_Trias <- dataTriassic[dataTriassic$Clade == "Sauropodomorpha", ]

data_nonsauropodomorph_Trias <- dataTriassic[dataTriassic$Clade != "Sauropodomorpha", ]
data_nonsauropodomorph_Trias$Clade <- "Non-Sauropodomorpha"

data_two_Triassic <- rbind(data_sauropodomorpha_Trias, data_nonsauropodomorph_Trias)
#plot
plotNonSauropod_Trias <- ggplot(data = data_two_Triassic, aes(x = Mid_ma, y= Body_mass_log, col = Clade)) +
  geom_point() +
  geom_vline(xintercept=201.4)+
  geom_smooth(method = "lm") +
  scale_x_reverse()

################
###Jurassic Data
dataJurassic <- data[data$Max_ma <= 201.4,]
plotJura <- ggplot(data = dataJurassic, aes(x = Mid_ma, y= Body_mass_Kg, col = Clade)) +
  geom_point() +
  geom_vline(xintercept=201.4) +
  scale_x_reverse()
##plotting kg
plotkgJura <- ggplot(data = dataJurassic, aes(x = Mid_ma, y= Body_mass_Kg, col = Clade)) +
  geom_point() +
  geom_vline(xintercept=201.4) +
  geom_smooth(method = "lm") +
  scale_x_reverse()

##plotting log
plotLogJura <- ggplot(data = dataJurassic, aes(x = Mid_ma, y= Body_mass_log, col = Clade)) + geom_point() +
  geom_vline(xintercept=201.4) +
  geom_smooth(method = "lm", se=FALSE) +
  scale_x_reverse() +
  theme_minimal()

##plotting Sauropodomoprha/Non-Sauropodomorpha
data_sauropodomorpha_Jura <- dataJurassic[dataJurassic$Clade == "Sauropodomorpha", ]

data_nonsauropodomorph_Jura <- dataJurassic[dataJurassic$Clade != "Sauropodomorpha", ]
data_nonsauropodomorph_Jura$Clade <- "Non-Sauropodomorpha"

data_two_Jurassic <- rbind(data_sauropodomorpha_Jura, data_nonsauropodomorph_Jura)
#plot
plotNonSauropod_Jura <- ggplot(data = data_two_Jurassic, aes(x = Mid_ma, y= Body_mass_log, col = Clade)) +
  geom_point() +
  geom_vline(xintercept=201.4)+
  geom_smooth(method = "lm") +
  scale_x_reverse()

##########################################
#Boxplots
data_sauropodomorpha_Jura$Time <- "Jurassic"
data_sauropodomorpha_Trias$Time <- "Triassic"
dataTriasJura_Sauropod <- rbind(data_sauropodomorpha_Trias, data_sauropodomorpha_Jura)
Box <- ggplot(dataTriasJura_Sauropod, aes(x = Time, y = Body_mass_log)) +
    geom_boxplot() + 
    geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5) +
    ylab("Body Mass (log)") +
    ggtitle("Sauropodomorpha Mass Difference") +
    scale_x_discrete(name = "System", limits=c("Triassic", "Jurassic"))

###Test of significant difference
t.test(data_nonsauropodomorph_Jura$Body_mass_log, data_sauropodomorpha_Trias$Body_mass_log, var.equal = FALSE, alternative="two.sided")
#p value

# p-value for alternative="two.sided" = 0.0074

