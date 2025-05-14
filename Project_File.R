library(GGally)
library(corrplot)
library(ggplot2)
library(psych)
setwd("~/DataVisualization")

responses <- read.csv(file="LocalCrimeOneYearofData2012.csv", header=TRUE, sep=",")

names(responses)

crime_data <- cor(responses[, c("Larceny_theft", "Murder_and_Manslaughter", "Forcible_rape", 
                                "Robbery", "Aggravated_assault", "Motor_vehicle_theft", "Burglary")])

colnames(crime_data) <- c("Larceny Theft", "Murder and Manslaughter", "Forcible Rape", "Robbery", 
                          "Aggravated Assault", "Motor Vehicle Theft", "Burglary")
rownames(crime_data) <- colnames(crime_data)
avg <- mean(rowMeans(crime_data))

red_color <- rgb(220, 50, 0, maxColorValue = 255)


par(mar = c(5, 4, 4, 2) + 0.1)  


names(responses)
# Filter relevant columns
crime_data <- responses[, c("State", "Murder_and_Manslaughter", "Forcible_rape", "Robbery", "Aggravated_assault",
                            "Motor_vehicle_theft", "Burglary", "Motor_vehicle_theft", "Property_crime_total")]

# Reshape data for ggplot
crime_data_long <- reshape2::melt(crime_data, id.vars = "State")

# Plot stacked bar chart
ggplot(crime_data_long, aes(x = State, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  labs(title = "Crime Composition Across States",
       x = "State",
       y = "Count") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# we can vicualize among all states total property crimes are the highest, than burglary





names(responses)
describe(responses)
summary(responses)
res <- responses [, 3:15]
summary(res)
str(res)

# Visualization of different state population
ggplot(responses, aes(x = State, y = Population)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "State Populations", x = "State", y = "Population") +
  theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust=1,
                                                     , vjust = 1))



names(responses)

crime_data <- cor(responses[, c("Larceny_theft", "Murder_and_Manslaughter", "Forcible_rape", 
                                "Robbery", "Aggravated_assault", "Motor_vehicle_theft", "Burglary")])

colnames(crime_data) <- c("Larceny Theft", "Murder and Manslaughter", "Forcible Rape", "Robbery", 
                          "Aggravated Assault", "Motor Vehicle Theft", "Burglary")
rownames(crime_data) <- colnames(crime_data)
avg <- mean(crime_data[lower.tri(crime_data)])

red_color <- rgb(255, 160, 170, maxColorValue = 255)

par(mar = c(5, 4, 4, 2) + 0.1)  

corrplot(crime_data, method = "color", col = colorRampPalette(c("white", red_color))(200), 
         type = "lower", order = "hclust", addCoef.col = "black", tl.col = "black",
         is.corr = FALSE,  tl.cex = 0.9,
         addgrid.col = "black", clim = c(0.32, avg, 1))+ title("Associations of Crime Data", cex.main = 1.6)


