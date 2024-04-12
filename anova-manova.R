data <- read.csv("C://Users//USER//Desktop//new365.csv",check.names = F)
colnames(data) <- c("id","Efficiency","Gender","Education_Status","Faculty","CGPA","Books","Breakfast","Lunch",
                    "Dinner","Night", "Sports","Societies","Ig","X","YouTube","Tiktok","Twitch",
                    "WhatsApp","Pinterest","Reddit","Others","Negative_Impact","Attantion_Time","S_Motivation","Focus",
                    "Lecture_Knowledge","Language_Proficiency","Material_Usage","Clarity","Voice",
                    "Answering","Reachablety","Motivation","Bedtime","Waking_time","Sleeping_hours")
data[75,c("Breakfast","Lunch","Dinner","Night")] <- c(3,3,4,2)
head(data)

data$Bedtime <- as.integer(data$Bedtime)
data$Sleeping_hours <- as.integer(data$Sleeping_hours)
str(data)
summary(data)

--------------------------------------------------------------
  ###sleep effciency
  
# Check the structure of your data to ensure the new columns are numeric
str(data)

# Calculate correlation matrix
cor_matrix <- cor(data[c("Bedtime", "Waking_time", "Sleeping_hours", "Efficiency")])

# Print correlation matrix
print(cor_matrix)


------------------------------------------------------------------
  
# Assuming 'Education_Status' is a categorical variable and 'Efficiency' is the dependent variable

  
# Convert 'Education_Status' to factor if it's not already
data$Education_Status <- factor(data$Education_Status)

# Perform one-way ANOVA
anova_result <- aov(Efficiency ~ Education_Status, data = data)

# Display ANOVA table
print(anova_result)

# Summary of the ANOVA
summary(anova_result)








# Load necessary libraries
library(ggplot2)

# Boxplot for Efficiency and Education Status
ggplot(data, aes(x = Education_Status, y = Efficiency)) +
  geom_boxplot(fill = "skyblue", color = "black", alpha = 0.7) +
  ggtitle("Boxplot of Efficiency by Education Status") +
  xlab("Education Status") +
  ylab("Efficiency") +
  theme_minimal()

data$Education_Status[44] <- "Graduate"



kruskal_result <- kruskal.test(Efficiency ~ Education_Status, data = data)

# Display the Kruskal-Wallis test summary
print(kruskal_result)



new_matrix <- cbind(data$Efficiency, data$Education_status)
print(new_matrix)

new_matrix$Grouped_Education <- ifelse(new_matrix$Education %in% c("Preparatory School"), "Undergraduate" <- "", new_matrix$Education)


kruskal_result <- kruskal.test(data$Efficiency ~ new_matrix$Education_Status)
print(kruskal_result)


data$Grouped_Education <- ifelse(data$Education %in% c("Preparatory School", "Undergraduate"), "Prep_Undergrad", data$Education)

# Display the modified data
print(data)

boxplot(Efficiency ~ Education_Status, data = data, 
        main = "Boxplot of Efficiency by Education Group",
        xlab = "Education Group", ylab = "Efficiency")

boxplot(Efficiency ~ Education_Status, data = data, 
        main = "Boxplot of Efficiency by Education Group",
        xlab = "Education Group", ylab = "Efficiency",
        col = "darkorchid",  # Set boxplot color to purple
        names = c("Grad", "Undergrad", "Prep"),  # Set custom axis labels
        border = "black"  # Set border color of the boxes
)




kruskal_result <- kruskal.test(Efficiency ~ Education_Status, data = data)

# Display the Kruskal-Wallis test summary
print(kruskal_result)

kruskal_result <- kruskal.test(Efficiency ~ Grouped_Education, data = data)

# Display the Kruskal-Wallis test summary
print(kruskal_result)



********************************************
-----------------------------------------

data$Gender <- factor(data$Gender)

# Perform one-way ANOVA
anova_result <- aov(Efficiency ~ Gender, data = data)

# Display ANOVA table
print(anova_result)

# Summary of the ANOVA
summary(anova_result)   # 0.204

kruskal_result <- kruskal.test(Efficiency ~ Gender, data = data)

# Display the Kruskal-Wallis test summary
print(kruskal_result)
-----------------------------------------
  
  data$Faculty <- factor(data$Faculty)

# Perform one-way ANOVA
anova_result <- aov(Efficiency ~ Faculty, data = data)

# Display ANOVA table
print(anova_result)

# Summary of the ANOVA
summary(anova_result)   #0.386
-----------------------------------------
  
data$CGPA <- factor(data$CGPA)

# Perform one-way ANOVA
anova_result <- aov(Efficiency ~ CGPA, data = data)

# Display ANOVA table
print(anova_result)

# Summary of the ANOVA
summary(anova_result)       #0.0000628

-----------------------------------------
  
  data$Books     <- factor(data$Books)

# Perform one-way ANOVA
anova_result <- aov(Efficiency ~ Books     , data = data)

# Display ANOVA table
print(anova_result)

# Summary of the ANOVA
summary(anova_result)            #0.0673

-----------------------------------------
  
  data$Lecture_Knowledge     <- factor(data$Lecture_Knowledge     )

# Perform one-way ANOVA
anova_result <- aov(Efficiency ~ Lecture_Knowledge     , data = data)

# Display ANOVA table
print(anova_result)

# Summary of the ANOVA          
summary(anova_result)           # 0.00000014
---------------------------------------------
  
# Assuming 'Efficiency', 'sports', and 'societies' are factors
data$Sports <- factor(data$Sports)
data$Societies <- factor(data$Societies)

# Two-way ANOVA
anova_result <- aov(Efficiency ~ Sports * Societies, data = data)

# Display ANOVA table
print(anova_result)

# Summary of the ANOVA
summary(anova_result)

corr(data$Sports,data$Societies)

# Create a contingency table
contingency_table <- table(data$Sports, data$Societies)

# Perform chi-squared test
chi_squared_result <- chisq.test(contingency_table)

# Print the result
print(chi_squared_result)


# Create a contingency table
contingency_table <- table(data$Attantion_Time, data$Motivation)

# Perform the chi-squared test
chi_square_test <- chisq.test(contingency_table)

# Print the result
print(chi_square_test)


# Create a contingency table
contingency_table <- table(data$Efficiency, data$Motivation)

# Perform the chi-squared test
chi_square_test <- chisq.test(contingency_table)

# Print the result
print(chi_square_test)







---------------------------------
  # Assuming 'Efficiency' is the dependent variable and the specified variables are factors
data$Lecture_Knowledge <- factor(data$Lecture_Knowledge)
data$Language_Proficiency <- factor(data$Language_Proficiency)
data$Material_Usage <- factor(data$Material_Usage)
data$Clarity <- factor(data$Clarity)
data$Voice <- factor(data$Voice)
data$Answering <- factor(data$Answering)
data$Reachablety <- factor(data$Reachablety)


# Calculate correlation matrix for the entire dataset
correlation_matrix <- cor(data)

# Print the correlation matrix
print(correlation_matrix)



# Fit a multiple linear regression model
lm_model <- lm(Efficiency ~ Lecture_Knowledge + Language_Proficiency + Material_Usage +
                 Clarity + Voice + Answering + Reachablety, data = data)

# Display the results
summary(lm_model)
---------------------------------------------------------
  # Assuming 'Negative_Impact' and 'Efficiency' are numeric variables in your dataset
  
  # Scatter plot
plot(data$Negative_Impact, data$Efficiency, main = "Negative Impact vs Efficiency", 
       xlab = "Negative Impact", ylab = "Efficiency", pch = 19, col = "blue")

# Calculate correlation coefficient
correlation_coefficient <- cor(data$Negative_Impact, data$Efficiency)
cat("Correlation Coefficient:", correlation_coefficient, "\n")

# Fit a linear regression model
lm_model <- lm(Efficiency ~ Negative_Impact, data = data)

# Display the regression results
summary(lm_model)
#------------------------------------------------
  
str(data)

# Select numeric variables for the regression model
numeric_vars <- c("Efficiency", "Breakfast", "Lunch", "Dinner", "Night")

# Create a subset of the data with only numeric variables
numeric_data <- data[, numeric_vars]

# Check for missing values and replace if needed
numeric_data[is.na(numeric_data)] <- 0  # Replace missing values with 0, you might want to handle missing values differently based on your data

# Build a linear regression model
lm_model <- lm(Efficiency ~ ., data = numeric_data)

# Display the summary
summary(lm_model)


-------------------------------------------
kategori_sutunlari <- c("Lecture_Knowledge","Language_Proficiency","Material_Usage","Clarity","Voice","Answering","Reachability","Motivation")
for (sutun in kategori_sutunlari) {
  data[[sutun]] <- as.numeric(factor(data[[sutun]], levels = c("Very Poor", "Poor", "Fair", "Good", "Very Good", "Excellent")))
}



# Load necessary libraries
library(corrplot)

# Select numerical columns for correlation analysis
numeric_data <- data[, sapply(data, is.numeric)]

# Calculate correlation matrix
correlation_matrix <- cor(numeric_data, use = "pairwise.complete.obs")

# Create a heatmap of the correlation matrix
corrplot(correlation_matrix, method = "color", type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

cor(data$Waking_time,data$Sleeping_hours)

# Load necessary libraries
library(ggplot2)

# Scatter plot for Waking_time and Sleeping_hours

ggplot(data, aes(x = Waking_time, y = Sleeping_hours)) +
  geom_point(color = "darkblue") +
  ggtitle("Scatter Plot of Waking Time vs. Sleeping Hours") +
  xlab("Waking Time") +
  ylab("Sleeping Hours") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_text(aes(label = sprintf("Correlation: %.2f", cor(data$Waking_time, data$Sleeping_hours))),
            x = Inf, y = -Inf, hjust = 1, vjust = 0, color = "black", size = 4)

# Load necessary libraries
library(ggplot2)

# Scatter plot for Waking_time and Sleeping_hours
ggplot(data, aes(x = Waking_time, y = Sleeping_hours)) +
  geom_point(color = "skyblue", size = 3) +
  ggtitle("Scatter Plot of Waking Time vs. Sleeping Hours") +
  xlab("Waking Time") +
  ylab("Sleeping Hours") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = "none") +
  geom_text(aes(label = sprintf("Correlation: %.2f", cor(data$Waking_time, data$Sleeping_hours))),
            x = Inf, y = -Inf, hjust = 1, vjust = 0, color = "black", size = 4)



#*****************************************************************
  
  
set.seed(32)
data$CGPA_new<-""
data$CGPA_new[which(data$CGPA == "0.00 - 1.99")] <- round(runif(41, min=0.00, max=1.99),2)
data$CGPA_new[which(data$CGPA == "2.00 - 2.49")] <- round(runif(50,min=2.00,max=2.49),2)
data$CGPA_new[which(data$CGPA == "2.50 - 2.99")] <- round(runif(56, min=2.50,max=2.99),2)
data$CGPA_new[which(data$CGPA == "3.00 - 3.49")] <- round(runif(66, min=3.00, max=3.49),2)
data$CGPA_new[which(data$CGPA == "3.50 - 4.00")] <- round(runif(42, min=3.50,max=4.00),2)
data$CGPA_new <- as.numeric(data$CGPA)
data$CGPA_new <- round(data$CGPA,2)




library(ggplot2)
ggplot(data, 
         aes(x = Efficiency, 
             y = CGPA_new, 
             color = Efficiency)) +
  geom_boxplot(size = 1,
               outlier.shape = 1,
               outlier.color = "black",
               outlier.size = 3) +
  geom_jitter(alpha = 0.5, width = 0.2) + 
  labs(title = "CGPA by Efficiency", 
       subtitle = "",
       x = "Efficiency",
       y = "CGPA") +
  theme_minimal() +
  scale_x_discrete(breaks = c(1:5),  
                   labels = c("Very Low", "Low", "Medium", "High", "Very High")) +
  scale_y_continuous(expand = c(0, 0)) +  # Ensure y-axis starts from 0
  theme(legend.position = "none") +
  coord_flip()


contingency_table <- table(data$Attantion_Time, data$Motivation)

# Perform the chi-squared test
chi_square_test <- chisq.test(contingency_table)

# Print the result
print(chi_square_test)
str(data)


contingency_table <- table(data$Books, data$Focus)
> # Perform chi-squared test
chi_squared_result <- chisq.test(contingency_table)

# Assuming 'data' is your data frame

# Convert CGPA to a factor
data$CGPA <- as.factor(data$CGPA)

# Fit ANOVA model
anova_result <- aov(Sleeping_hours ~ CGPA, data = data)

# Display ANOVA table
print(anova_result)

# Summary of the ANOVA
summary(anova_result)

linear_model <- lm(Efficiency ~ Sleeping_hours, data = data)

# Display summary of the regression
summary(linear_model)

# Assuming 'data' is your data frame

# Load the ggplot2 library
library(ggplot2)

# Scatter plot with regression line
ggplot(data, aes(x = Sleeping_hours, y = Efficiency)) +
  geom_point() +  # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add linear regression line
  labs(title = "Scatter Plot of Sleeping Hours and Efficiency",
       x = "Sleeping Hours",
       y = "Efficiency") +
  theme_minimal()


ggplot(data, 
       aes(x = Sleeping_hours, 
           fill = Efficiency)) + 
  geom_bar(position = position_dodge(preserve = "single"))
# Grouped bar plot with individual data points
ggplot(data, aes(x = Efficiency, y = Sleeping_hours, fill = Efficiency)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_jitter(position = position_dodge(width = 0.8), size = 2, alpha = 0.7) +
  labs(title = "Grouped Bar Plot with Jitter of Sleeping Hours by Efficiency",
       x = "Efficiency",
       y = "Sleeping Hours") +
  theme_minimal()

# Assuming 'data' is your data frame

# Load the ggplot2 library
library(ggplot2)

# Grouped bar plot
ggplot(data, aes(x = Efficiency, y = Sleeping_hours, fill = Efficiency)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_gradient(low = "lightpink", high = "darkred") +
  labs(title = "Grouped Bar Plot of Sleeping Hours by Efficiency",
       x = "Efficiency",
       y = "Sleeping Hours") +
  theme_minimal()

ggplot(data, aes(x = CGPA, y = Sleeping_hours, fill = Efficiency)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_gradient(low = "Red", high = "darkred") +
  labs(title = "Grouped Bar Plot of Sleeping Hours by Efficiency",
       x = "Efficiency",
       y = "Sleeping Hours") +
  theme_minimal()





set.seed(32)
data$CGPA_new <- ""

data$CGPA_new[which(data$CGPA == "0.00 - 1.99")] <- round(runif(sum(data$CGPA == "0.00 - 1.99"), min = 0.00, max = 1.99), 2)
data$CGPA_new[which(data$CGPA == "2.00 - 2.49")] <- round(runif(sum(data$CGPA == "2.00 - 2.49"), min = 2.00, max = 2.49), 2)
data$CGPA_new[which(data$CGPA == "2.50 - 2.99")] <- round(runif(sum(data$CGPA == "2.50 - 2.99"), min = 2.50, max = 2.99), 2)
data$CGPA_new[which(data$CGPA == "3.00 - 3.49")] <- round(runif(sum(data$CGPA == "3.00 - 3.49"), min = 3.00, max = 3.49), 2)
data$CGPA_new[which(data$CGPA == "3.50 - 4.00")] <- round(runif(sum(data$CGPA == "3.50 - 4.00"), min = 3.50, max = 4.00), 2)

data$CGPA_new <- as.numeric(data$CGPA_new)

Q1 <- quantile(data$Sleeping_hours, 0.25)
Q3 <- quantile(data$Sleeping_hours, 0.75)
IQR <- Q3 - Q1

# Define the lower and upper bounds for outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Drop outliers from the dataset
data <- data[data$Sleeping_hours >= lower_bound & data$Sleeping_hours <= upper_bound, ]

ggplot(data, aes(x = CGPA_new, y = Sleeping_hours)) +
  geom_point(shape = 21, position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.75), fill = "black") +
  labs(title = "Scatter Plot of Sleeping Hours by CGPA",
       x = "CGPA",
       y = "Sleeping Hours") +
  theme_minimal()

ggplot(data, aes(x = CGPA_new, y = Sleeping_hours)) +
  geom_point(shape = 21, position = position_jitter(width = 0.3), fill = "black") +
  labs(title = "Scatter Plot of Sleeping Hours by CGPA",
       x = "CGPA",
       y = "Sleeping Hours") +
  geom_smooth(method = "lm", se = FALSE, color = "purple")+
  theme_minimal()+
  annotate("text", x = max(x), y = max(y), label = paste("Corr =", round(correlation_coefficient, 2)), hjust = 1) +


corr(data$Sleeping_hours,data$CGPA_new)





library(ggplot2)

# Define a threshold for outliers (you can adjust this based on your data)
outlier_threshold <- quantile(data$Sleeping_hours, 0.95)

# Filter data to exclude outliers
filtered_data <- data[data$Sleeping_hours <= outlier_threshold, ]

# Create a ggplot with better aesthetics using the filtered data
ggplot(filtered_data, aes(x = CGPA_new, y = Sleeping_hours)) +
  geom_point(shape = 21, position = position_jitter(width = 0.3), fill = "black") +
  labs(title = "Scatter Plot of Sleeping Hours by CGPA",
       x = "CGPA",
       y = "Sleeping Hours") +
  geom_smooth(method = "lm", se = FALSE, color = "purple")+
  theme_minimal()



anova_result <- aov(CGPA_new ~ Efficiency, data = your_data)

# Display the ANOVA summary
summary(anova_result)

# Assuming your data has variables named CGPA and Efficiency
# Replace 'your_data' with your actual data frame name
anova_result <- aov(CGPA_new ~ Efficiency, data = data)

# Display the ANOVA summary
summary(anova_result)


anova_result <- aov(CGPA_new ~ Sleeping_hours, data = data)

# Display the ANOVA summary
summary(anova_result)

library(ggplot2)
scatter_plot <-ggplot(data, aes(x =CGPA_new , y = Sleeping_hours)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "purple") +  # Add a linear regression line without confidence interval
  labs(title = "Relationship between Sleeping Hours and CGPA",
       y = "Sleeping Hours",
       x = "CGPA_new")

correlation_coefficient <- cor(data$Sleeping_hours, data$CGPA_new)

final_plot <- scatter_plot +
  annotate("text", x = 4, 
           y = min(data$CGPA_new),
           label = paste("Correlation: ", round(correlation_coefficient, 2)),
           hjust = 1, vjust = 0,
           color = "purple")  # Color of the correlation text

# Print the final plot
print(final_plot)


ggplot(data, aes(x = Efficiency, y = Sleeping_hours)) +
  geom_boxplot(fill = "skyblue", color = "darkblue") +
  labs(title = "Boxplot of Sleeping Hours by Efficiency",
       x = "Efficiency",
       y = "Sleeping Hours")


boxplot(Sleeping_hours ~ Efficiency, data = data, 
        main = "Boxplot of Efficiency by Sleeping Hours",
        xlab = "Efficiency", ylab = "Average Sleeping Hours",
       # ylim = c(0:15),  # Set y-axis limits
        names = c(1:5),  # Set custom axis labels
        col = c("coral"),  # Set custom colors
        border = "black")  # Set border color of the boxes

Efficiency_by_Sleeping_hours <- table(data$Efficiency, data$Sleeping_hours)

# Chi-squared test
chi_square_test <- chisq.test(Efficiency_by_Sleeping_hours)

# Display the test result

print(chi_square_test)

str(data)

books_focus <- table(data$Focus, data$Books)

# Chi-squared test
chi_square_test <- chisq.test(books_focus)

# Display the test result

print(chi_square_test)


efficiency_sports <- table(data$Sports, data$Efficiency)

# Chi-squared test
chi_square_test <- chisq.test(efficiency_sports)

# Display the test result

print(chi_square_test)

efficiency_societies <- table(data$Societies, data$Efficiency)

# Chi-squared test
chi_square_ <- chisq.test(efficiency_societies)

# Display the test result

print(chi_square_)


anova_result <- aov(CGPA_new ~ Societies+Sports, data = data)

# Display the ANOVA summary
summary(anova_result)

# Assuming you've already run the ANOVA
anova_result <- aov(CGPA_new ~ Societies + Sports + Societies:Sports, data = data)

# Display the ANOVA summary
summary(anova_result)


focus_sports<- table(data$Focus, data$Sports)

# Chi-squared test
chi_square_ <- chisq.test(focus_sports)

# Display the test result

print(chi_square_)


data <- read.csv("C://Users//USER//Desktop//new365_wodays.xlsx",check.names = F)
colnames(data) <- c("id","Efficiency","Gender","Education_Status","Faculty","CGPA","Books","Breakfast","Lunch",
                    "Dinner","Night", "Sports","Societies","Ig","X","YouTube","Tiktok","Twitch",
                    "WhatsApp","Pinterest","Reddit","Others","Negative_Impact","Attantion_Time","S_Motivation","Focus",
                    "Lecture_Knowledge","Language_Proficiency","Material_Usage","Clarity","Voice",
                    "Answering","Reachablety","Motivation","Bedtime","Waking_time","Sleeping_hours")
str(data)

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
library(ggplot2)
library(dplyr)


# Assuming your data is named 'heatmap_data'
# Load required libraries
library(dplyr)
library(tidyr)


# Load required libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Assuming your data is named 'heatmap_data'
heatmap_data <- data %>%
  select(Efficiency, Ig,X,YouTube,Tiktok,Twitch,
         WhatsApp,Pinterest,Reddit,Others)

# Replace "Jan-29" with "01-03" in all columns
heatmap_data <- heatmap_data %>%
  mutate_all(~ ifelse(. == "Jan-29", "01-03", .)) %>%
  replace(is.na(.), 0)  # Replace NA values with 0

# Convert character columns to factors if needed
heatmap_data[, 2:ncol(heatmap_data)] <- lapply(heatmap_data[, 2:ncol(heatmap_data)], as.factor)

# Reshape the data for ggplot
heatmap_data_long <- heatmap_data %>%
  pivot_longer(-Efficiency, names_to = "Platform", values_to = "Frequency")

# Create a heatmap
ggplot(heatmap_data_long, aes(x = Efficiency, y = Platform, fill = Frequency)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("0" = "white", "01-03" = "lightblue", "30 - 59" = "skyblue",
                               "60 - 89" = "blue", "90 - 119" = "darkblue", "120 - 149" = "navy")) +
  labs(title = "Efficiency vs. Social Media Platforms",
       x = "Efficiency",
       y = "Social Media Platforms") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



Efficiency_by_Sleeping_hours <- table(data$Reachablety, data$CGPA)

# Chi-squared test
chi_square_test <- chisq.test(Efficiency_by_Sleeping_hours)

# Display the test result

print(chi_square_test)

anova_result <- aov(CGPA_new ~ Reachablety , data = data)

# Display the ANOVA summary
summary(anova_result)

# Assuming 'data' is your dataframe
# Load required library
library(ggplot2)

# Create a boxplot for Efficiency and Reachability
ggplot(data, aes(x = c("Efficiency", "Reachablety"), y = c(Efficiency, Reachablety))) +
  geom_boxplot(fill = c("Efficiency" = "lightblue", "Reachablety" = "lightgreen")) +
  labs(title = "Boxplot of Efficiency and Reachablety",
       x = "",
       y = "Values") +
  theme_minimal()

ggplot(data, aes(x = Reachablety, y = CGPA_new)) +
  geom_boxplot(fill = "skyblue", color = "darkblue") 

