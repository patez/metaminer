library(tidyverse)

setwd("C:/Users/pauli_000/OneDrive - Oulun yliopisto/gradu/csv")

data <- readRDS("data1.Rda")


data2 <- data %>%
  separate(robot_content, character("no-robot", "user-agent","disallow","sitemap"), sep = "\\r\\n")

head(data2)


print(data$robot_content[2])
splitted <- (str_split(data$robot_content[2], "\r\n"))
print(splitted)
