##pielar ve barlar

data <- read.csv("C://Users//USER//Desktop//new365.csv",check.names = F)
colnames(data) <- c("id","Efficiency","Gender","Education_Status","Faculty","CGPA","Books","Breakfast","Lunch",
                    "Dinner","Night", "Sports","Societies","Ig","X","YouTube","Tiktok","Twitch",
                    "WhatsApp","Pinterest","Reddit","Others","Negative_Impact","Attantion_Time","S_Motivation","Focus",
                    "Lecture_Knowledge","Language_Proficiency","Material_Usage","Clarity","Voice",
                    "Answering","Reachability","Motivation","Bedtime","Waking_time","Sleeping_hours")
data[75,c("Breakfast","Lunch","Dinner","Night")] <- c(3,3,4,2)
str(data)
kategori_sutunlari <- c("Lecture_Knowledge","Language_Proficiency","Material_Usage","Clarity","Voice","Answering","Reachability","Motivation")
for (sutun in kategori_sutunlari) {
  data[[sutun]] <- as.numeric(factor(data[[sutun]], levels = c("Very Poor", "Poor", "Fair", "Good", "Very Good", "Excellent")))
}
str(data)

pie(table(data$Gender), main="Gender Dağılımı", xlab="Gender", ylab="")
pie(table(data$Education_Status), main="eğitim Dağılımı", xlab="education", ylab="Frequency")
pie(table(data$Faculty), main="fakülte Dağılımı", xlab="fakülte", ylab="Frequency")
pie(table(data$Gender), main="Cinsiyet Dağılımı", xlab="Gender", ylab="Frequency")

# Load necessary libraries
library(ggplot2)

# Function to create a customized pie chart
create_pie_chart <- function(data, variable, title) {
  df <- as.data.frame(table(data[[variable]]))
  
  ggplot(df, aes(x = " ", y = Freq)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    theme_minimal() +
    scale_fill_brewer(palette = "Set3")  # You can choose a different color palette
}

# Create pie charts
pie_chart_gender <- create_pie_chart(data, "Gender", "Gender ")
pie_chart_education <- create_pie_chart(data, "Education_Status", "Education Status ")
pie_chart_faculty <- create_pie_chart(data, "Faculty", "Faculty ")

# Display the pie charts
print(pie_chart_gender)
print(pie_chart_education)
print(pie_chart_faculty)

str(data)

# Load necessary libraries
library(ggplot2)

# Bar graph for Books
ggplot(data, aes(x = Books)) +
  geom_bar(fill = "skyblue", color = "black", alpha = 0.7) +
  ggtitle("Books Distribution") +
  xlab("Books") +
  ylab("Frequency") +
  theme_minimal()

# Bar graph for Breakfast
ggplot(data, aes(x = factor(Breakfast))) +
  geom_bar(fill = "lightcoral", color = "black", alpha = 0.7) +
  ggtitle("Breakfast Distribution") +
  xlab("Breakfast") +
  ylab("Frequency") +
  theme_minimal()

# Bar graph for Lunch
ggplot(data, aes(x = factor(Lunch))) +
  geom_bar(fill = "lightgreen", color = "black", alpha = 0.7) +
  ggtitle("Lunch Distribution") +
  xlab("Lunch") +
  ylab("Frequency") +
  theme_minimal()

# Bar graph for Dinner
ggplot(data, aes(x = factor(Dinner))) +
  geom_bar(fill = "lightcoral", color = "black", alpha = 0.7) +
  ggtitle("Dinner Distribution") +
  xlab("Dinner") +
  ylab("Frequency") +
  theme_minimal()

# Bar graph for Night
ggplot(data, aes(x = factor(Night))) +
  geom_bar(fill = "lightsalmon", color = "black", alpha = 0.7) +
  ggtitle("Night Distribution") +
  xlab("Night") +
  ylab("Frequency") +
  theme_minimal()

# Bar graph for Sports
ggplot(data, aes(x = Sports)) +
  geom_bar(fill = "lightblue", color = "black", alpha = 0.7) +
  ggtitle("Sports Distribution") +
  xlab("Sports") +
  ylab("Frequency") +
  theme_minimal()

# Bar graph for Societies
ggplot(data, aes(x = Societies)) +
  geom_bar(fill = "lightgreen", color = "black", alpha = 0.7) +
  ggtitle("Societies Distribution") +
  xlab("Societies") +
  ylab("Frequency") +
  theme_minimal()

# Load necessary libraries
library(ggplot2)

# Histogram for Bedtime

-------------------------


# Histogram for Bedtime


# Load necessary libraries
library(ggplot2)

# Convert Bedtime to a factor with desired levels
data$Bedtime <- factor(data$Bedtime, levels = c(22, 23, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))

# Bar plot for Bedtime
ggplot(data, aes(x = Bedtime)) +
  geom_bar(fill = "skyblue", color = "black", alpha = 0.7, stat = "count") +
  ggtitle("Bedtime Distribution") +
  xlab("Bedtime") +
  ylab("Frequency") +
  theme_minimal() +
  scale_x_discrete(limits = c("22", "23", "0", "1", "2", "3", "4", "5", "6", "7"))


# Assuming 'data' is your data frame
# Assuming 'Bedtime' and 'Gender' are columns in your data

ggplot(data, aes(x = Bedtime, fill = Gender)) +
  geom_bar(color = "black", alpha = 0.7, stat = "count", position = "dodge") +
  ggtitle("Bedtime Distribution by Gender") +
  xlab("Bedtime") +
  ylab("Frequency") +
  theme_minimal() +
  scale_x_discrete(limits = c("22", "23", "0", "1", "2", "3", "4", "5", "6", "7"))


-----------------------------------------

# Histogram for Waking_time
ggplot(data, aes(x = Waking_time)) +
  geom_histogram(binwidth = 1, fill = "lightcoral", color = "black", alpha = 0.7) +
  ggtitle("Waking Time Distribution") +
  xlab("Waking_time") +
  ylab("Frequency") +
  theme_minimal()

# Histogram for Sleeping_hours
ggplot(data, aes(x = Sleeping_hours)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black", alpha = 0.7) +
  ggtitle("Sleeping Hours Distribution") +
  xlab("Sleeping Hours") +
  ylab("Frequency") +
  theme_minimal()



ggplot(data, aes(x = Faculty)) +
  geom_bar(fill = "lightcoral", color = "black", alpha = 0.7) +
  labs(title = "Faculty Distribution",
       x = "Faculty",
       y = "Count") +
  theme_minimal()

ggplot(data, 
       aes(x = Faculty, 
           fill = Gender)) + 
  geom_bar(position = position_dodge(preserve = "single"))


ggplot(data, 
       aes(x = Books, 
           fill = Gender)) + 
  geom_bar(position = position_dodge(preserve = "single"))

ggplot(data, 
       aes(x = Societies, 
           fill = Gender)) + 
  geom_bar(position = position_dodge(preserve = "single"))

ggplot(data, 
       aes(x = Sports, 
           fill = Gender)) + 
  geom_bar(position = position_dodge(preserve = "single"))


ggplot(data, 
       aes(x = Faculty, 
           fill = Gender)) + 
  geom_bar(position = "stack")






str(data)
library(ggplot2)

# Assuming your gender variable is named 'Gender' in the data frame
gender_counts <- table(data$Gender)

# Create a data frame for the pie chart
gender_data <- data.frame(Gender = names(gender_counts), Count = as.numeric(gender_counts))

# Create a pie chart
ggplot(gender_data, aes(x = "", y = Count, fill = Gender)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  labs(title = "Gender Distribution",
       fill = "Gender") +
  theme_minimal() +
  theme(legend.position = "bottom")


library(ggplot2)

# Assuming 'data' is your data frame
gender_counts <- table(data$Gender)

# Create a data frame for the pie chart
gender_df <- data.frame(Gender = names(gender_counts), Count = as.numeric(gender_counts))

# Create a pie chart using ggplot
ggplot(gender_df, aes(x = "", y = Count, fill = Gender)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y") +  # Convert to polar coordinates for a pie chart
  theme_void() +  # Remove unnecessary elements (axis, labels, etc.)
  scale_fill_manual(values = c("pink", "skyblue","yellow")) +  # Customize colors if needed
  labs(title = "Distribution of Gender in the Dataset")















kategori_sutunlari_2<- c("Ig","X","YouTube","Tiktok","Twitch",
                         "WhatsApp","Pinterest","Reddit","Others")

sayisal_degerler_2 <- c("0"=0,"1 - 29"=15,"30 - 59"=45,"60 - 89"=75,"90 - 119"=105,
                        "120 - 149"=135,"150 - 179"=165,"180 +"=195)
for (sutun in kategori_sutunlari_2) {
  data[[sutun]] <- as.numeric(factor(data[[sutun]], levels = names(sayisal_degerler_2)))
  
}
correlation_matrix <- cor(data[, c("Efficiency", "Ig","X","YouTube","Tiktok","Twitch",
                                   "WhatsApp","Pinterest","Reddit","Others")])
library(corrplot)
corrplot(correlation_matrix, method="color")










# Load required libraries
library(corrplot)

# Define categorical columns and their corresponding numeric values
kategori_sutunlari_2 <- c("Ig", "X", "YouTube", "Tiktok", "Twitch",
                          "WhatsApp", "Pinterest", "Reddit", "Others")

sayisal_degerler_2 <- c("0" = 0, "1 - 29" = 15, "30 - 59" = 45, "60 - 89" = 75, "90 - 119" = 105,
                        "120 - 149" = 135, "150 - 179" = 165, "180 +" = 195)

# Convert categorical columns to numeric using the defined mapping
for (sutun in kategori_sutunlari_2) {
  data[[sutun]] <- as.numeric(factor(data[[sutun]], levels = names(sayisal_degerler_2)))
}

# Replace NA and empty cells with 0 in specified columns
data[, kategori_sutunlari_2] <- lapply(data[, kategori_sutunlari_2], function(x) ifelse(is.na(x) | x == "", 0, x))

# Convert any non-numeric values to 0
data[, kategori_sutunlari_2] <- lapply(data[, kategori_sutunlari_2], as.numeric)

# Calculate correlation matrix
correlation_matrix <- cor(data[, c("Efficiency", kategori_sutunlari_2)])

# Plot the correlation matrix using corrplot
corrplot(correlation_matrix, method = "color")





# Load required libraries
library(corrplot)

# Define categorical columns and their corresponding numeric values
kategori_sutunlari_2 <- c("Ig", "X", "YouTube", "Tiktok", "Twitch",
                          "WhatsApp", "Pinterest", "Reddit", "Others")

sayisal_degerler_2 <- c("0" = 0, "1 - 29" = 15, "30 - 59" = 45, "60 - 89" = 75, "90 - 119" = 105,
                        "120 - 149" = 135, "150 - 179" = 165, "180 +" = 195)

# Convert categorical columns to numeric using the defined mapping
for (sutun in kategori_sutunlari_2) {
  data[[sutun]] <- as.numeric(factor(data[[sutun]], levels = names(sayisal_degerler_2)))
}

# Replace NA and empty cells with 0 in specified columns
data[, kategori_sutunlari_2] <- lapply(data[, kategori_sutunlari_2], function(x) ifelse(is.na(x) | x == "", 0, x))

# Convert any non-numeric values to 0
data[, kategori_sutunlari_2] <- lapply(data[, kategori_sutunlari_2], as.numeric)

# Calculate correlation matrix
correlation_matrix <- cor(data[, c("Efficiency", kategori_sutunlari_2)])

# Add a small value to the diagonal to prevent undefined correlations
diag(correlation_matrix) <- diag(correlation_matrix) + 1e-6

# Plot the correlation matrix using corrplot
corrplot(correlation_matrix, method = "color")

