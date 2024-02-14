# Create a data frame
df <- data.frame(
  Opinion = c("High", "Decreasing", "No Opinion", "Stable"),
  Number_of_Respondents = c(66, 51, 32, 7)
)
install.packages("ggplot2")
library(ggplot2)

# Create a bar plot
ggplot(df, aes(x = Opinion, y = Number_of_Respondents, fill = Opinion)) +
  geom_line(stat = "identity"),
  #labs(title = "Opinions According to Number of Respondents",
       x = "Opinion",
       y = "Number of Respondents") +
  theme_minimal()
# line plot
# 
# # Create a line plot
ggplot(df, aes(x = Opinion, y = Number_of_Respondents, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "Opinions According to Number of Respondents",
       x = "Opinion",
       y = "Number of Respondents") +
  theme_minimal()


#----------odered 
#
# Order the data frame by the number of respondents
df <- df[order(df$Number_of_Respondents, decreasing = TRUE), ]

# Create a line plot with red line
ggplot(df, aes(x = Opinion, y = Number_of_Respondents, group = 1)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  labs(x = "Respondents perception", y = "Number of Respondents") +
  theme_minimal()

#---------------
ggplot(df, aes(x = Opinion, y = Number_of_Respondents, group = 1)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  labs(x = "Respondents perception", y = "Number of Respondents") +
  theme_minimal() +
  theme(axis.line = element_line(color = "black"))


#----------------
ggplot(df, aes(x = Opinion, y = Number_of_Respondents, group = 1)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  labs(x = "Respondents perception", y = "Number of Respondents") +
  theme_minimal() +
  theme(axis.line = element_line(color = "black"),
        axis.text = element_text(color = "black"))
#--------------------------------------------
# Create the data frame
df2 <- data.frame(
  Gender = c("Male", "Female", "Male", "Female", "Male", "Female", "Male", "Female"),
  Education = c("No education", "No education", "Primary education", "Primary education", 
                "Secondary education", "Secondary education", "Higher education", "Higher education"),
  Initiative = c("Bad initiative", "Good initiative", "Yes", "No", "Yes", "No", "Yes", "No")
)

# Print the data frame
print(df)

# Install and load the knitr package
install.packages("knitr")
library(knitr)

# Display the table in a nice format
kable(df2, caption = "Table: Gender, Education, and Initiative")

ggplot(df2, aes(x = Education, fill = Initiative)) +
  geom_bar(position = "dodge", color = "black", stat = "count") +
  facet_wrap(~Gender) +
  labs(title = "Variation of Education and Initiative According to Gender",
       x = "Education",
       y = "Count",
       fill = "Initiative") +
  theme_minimal()

ggplot(df2, aes(x = Education, fill = Gender)) +
  geom_bar(position = "dodge", color = "black", stat = "count") +
  facet_wrap(~Initiative) +
  labs(title = "Variation of Education According to Gender and Initiative",
       x = "Education",
       y = "Count",
       fill = "Gender") +
  theme_minimal()


data<-Opinion
# Create a plot
# Define colors for male and female
# Define colors for male and female
gender_colors <- c("Female" = "red", "Male" = "blue")

# Create a scatter plot with points
ggplot(data, aes(x = Opinion, y = Education, color = Gender)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), size = 3) +
  scale_color_manual(values = gender_colors) +  # Use custom colors
  labs(title = "Variation of Opinion and Education According to Gender",
       x = "Opinion",
       y = "Education",
       color = "Gender") +
  theme_minimal()

######################################
ggplot(data, aes(x = Opinion, y = Education, color = Gender)) +
  geom_point(position = "dodge", size = 3) +
  scale_color_manual(values = gender_colors)+  # Use custom colors
 labs(title = "Variation of Opinion and Education According to Gender",
       x = "Opinion",
       y = "Education",
       color = "Gender") +
  theme_minimal()

#plot without title
ggplot(data, aes(x = Opinion, y = Education, color = Gender)) +
  geom_point(position = "dodge", size = 3) +
  scale_color_manual(values = gender_colors) +  # Use custom colors
  labs(x = "Opinion of respondents", y = "Education", color = "Gender") +
  theme_minimal() +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title.x = element_text(color = "black"),
        axis.title.y = element_text(color = "black"))
