## Generate sample data set showing 8th-grade math achievement,
## ACT composite, and postscondary enrollment

library(dplyr)

## Sample data set bears general statistical similarity to recent
## TN high school cohorts

set1 <- round(rnorm(8500, mean = 14, sd = 2.3),0)
set2 <- round(rnorm(21400, mean = 15.7, sd = 2.8),0)
set3 <- round(rnorm(34200, mean = 18.4, sd = 3.3),0)
set4 <- round(rnorm(36400, mean = 22.6, sd = 4.1),0)

sampleData1 <- data.frame(set1) %>%
  mutate(Math8 = "Below Basic")
names(sampleData1)[1] <- "ACT_composite"

sampleData2 <- data.frame(set2) %>%
  mutate(Math8 = "Basic")
names(sampleData2)[1] <- "ACT_composite"

sampleData3 <- data.frame(set3) %>%
  mutate(Math8 = "Proficient")
names(sampleData3)[1] <- "ACT_composite"

sampleData4 <- data.frame(set4) %>%
  mutate(Math8 = "Advanced")
names(sampleData4)[1] <- "ACT_composite"

sampleData <- rbind(sampleData1, sampleData2, sampleData3, sampleData4) %>%
  filter(ACT_composite <=36) %>%
  mutate(PSenrollment = "0")
sampleData$Math8 <- as.factor(sampleData$Math8)
sampleData$Math8 <- factor(sampleData$Math8,levels(sampleData$Math8)[c(1,4,2,3)])

sampleData$PSenrollment[sampleData$ACT_composite %in% c(4:7)] <- 1
sampleData$PSenrollment[sampleData$ACT_composite %in% c(8:10)] <- 
  sample(c(1:2), nrow(sampleData[sampleData$ACT_composite %in% c(8:9),]),
         prob = c(.88, .12), replace = TRUE)
sampleData$PSenrollment[sampleData$ACT_composite == 11] <- 
  sample(c(1:2), nrow(sampleData[sampleData$ACT_composite == 11,]),
         prob = c(.83, .17), replace = TRUE)
sampleData$PSenrollment[sampleData$ACT_composite == 12] <- 
  sample(c(1:2), nrow(sampleData[sampleData$ACT_composite == 12,]),
         prob = c(.77, .23), replace = TRUE)
sampleData$PSenrollment[sampleData$ACT_composite == 13] <- 
  sample(c(1:2), nrow(sampleData[sampleData$ACT_composite == 13,]),
         prob = c(.71, .29), replace = TRUE)
sampleData$PSenrollment[sampleData$ACT_composite == 14] <- 
  sample(c(1:2), nrow(sampleData[sampleData$ACT_composite == 15,]),
         prob = c(.62, .38), replace = TRUE)
sampleData$PSenrollment[sampleData$ACT_composite == 15] <- 
  sample(c(1:2), nrow(sampleData[sampleData$ACT_composite == 15,]),
         prob = c(.55, .45), replace = TRUE)
sampleData$PSenrollment[sampleData$ACT_composite == 16] <- 
  sample(c(1:2), nrow(sampleData[sampleData$ACT_composite == 16,]),
         prob = c(.48, .52), replace = TRUE)
sampleData$PSenrollment[sampleData$ACT_composite == 17] <- 
  sample(c(1:2), nrow(sampleData[sampleData$ACT_composite == 17,]),
         prob = c(.42, .58), replace = TRUE)
sampleData$PSenrollment[sampleData$ACT_composite == 18] <- 
  sample(c(1:2), nrow(sampleData[sampleData$ACT_composite == 18,]),
         prob = c(.34, .66), replace = TRUE)
sampleData$PSenrollment[sampleData$ACT_composite == 19] <- 
  sample(c(1:2), nrow(sampleData[sampleData$ACT_composite == 19,]),
         prob = c(.28, .72), replace = TRUE)
sampleData$PSenrollment[sampleData$ACT_composite == 20] <- 
  sample(c(1:2), nrow(sampleData[sampleData$ACT_composite == 20,]),
         prob = c(.26, .74), replace = TRUE)
sampleData$PSenrollment[sampleData$ACT_composite == 21] <- 
  sample(c(1:2), nrow(sampleData[sampleData$ACT_composite == 21,]),
         prob = c(.18, .82), replace = TRUE)
sampleData$PSenrollment[sampleData$ACT_composite == 22] <- 
  sample(c(1:2), nrow(sampleData[sampleData$ACT_composite == 22,]),
         prob = c(.16, .84), replace = TRUE)
sampleData$PSenrollment[sampleData$ACT_composite == 23] <- 
  sample(c(1:2), nrow(sampleData[sampleData$ACT_composite == 23,]),
         prob = c(.14, .86), replace = TRUE)
sampleData$PSenrollment[sampleData$ACT_composite == 24] <- 
  sample(c(1:2), nrow(sampleData[sampleData$ACT_composite == 24,]),
         prob = c(.12, .88), replace = TRUE)
sampleData$PSenrollment[sampleData$ACT_composite == 25] <- 
  sample(c(1:2), nrow(sampleData[sampleData$ACT_composite == 25,]),
         prob = c(.09, .91), replace = TRUE)
sampleData$PSenrollment[sampleData$ACT_composite %in% c(26:27)] <- 
  sample(c(1:2), nrow(sampleData[sampleData$ACT_composite %in% c(26:27),]),
         prob = c(.08, .92), replace = TRUE)
sampleData$PSenrollment[sampleData$ACT_composite == 28] <- 
  sample(c(1:2), nrow(sampleData[sampleData$ACT_composite == 28,]),
         prob = c(.07, .93), replace = TRUE)
sampleData$PSenrollment[sampleData$ACT_composite == 29] <- 
  sample(c(1:2), nrow(sampleData[sampleData$ACT_composite == 29,]),
         prob = c(.06, .94), replace = TRUE)
sampleData$PSenrollment[sampleData$ACT_composite %in% c(30:32)] <- 
  sample(c(1:2), nrow(sampleData[sampleData$ACT_composite%in% c(30:32),]),
         prob = c(.05, .95), replace = TRUE)
sampleData$PSenrollment[sampleData$ACT_composite %in% c(33:36)] <- 
  sample(c(1:2), nrow(sampleData[sampleData$ACT_composite %in% c(33:36),]),
         prob = c(.04, .96), replace = TRUE)

sampleData <- filter(sampleData, ACT_composite %in% c(5:36))

sampleData$PSenrollment <- as.factor(sampleData$PSenrollment)
levels(sampleData$PSenrollment) <- c("Did not enroll", "Enrolled")

write.csv(sampleData, "sampleData.csv", row.names = FALSE)