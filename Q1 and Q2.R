print("hello world")

df= read.csv("C:/Users/Emmanuel/Documents/Data Science/ITBDA4-14/project/northwest_highschool_grades.csv",header =TRUE,sep=';' ) 
#head(df,5)

#density function
dens_df = density(df$G3)
plot(dens_df,xlab="final grade of students", "density function")

#box plot Q1.1. b
boxplot(df$G3 ~ df$address, data = df, main = "Box Plot of final grade  by student's home address type", xlab = "home address type", ylab = "final grade")

#box plot Q1.1. c
df$parentsEdu <- rowMeans(df[, c("Medu", "Fedu")], na.rm = TRUE)
boxplot(df$G3 ~ df$parentsEdu, data = df, main = "Box Plot of final grade  by  parents’ education", xlab = "parents’ education", ylab = "final grade")

#print(df$parentsEdu)

#box plot Q1.1. d
boxplot(df$G3 ~ df$studytime, data = df, main = "Box Plot of final grade  by student's study time", xlab = "study time", ylab = "final grade")

#box plot Q1.1. e
boxplot(df$G3 ~ df$romantic, data = df, main = "Box Plot of final grade  by romantic status", xlab = "romantic status", ylab = "final grade")

#box plot Q1.1. f
boxplot(df$G3 ~ df$famrel, data = df, main = "Box Plot of final grade  by quality of family relationships", xlab = "quality of family relationships", ylab = "final grade")

############################
#Question 2
#a.
df_Q2 <-df#copy
print(df_Q2)

df_Q2[df_Q2$studytime == '1', 'studytime'] <- '<2 hours'
df_Q2[df_Q2$studytime == '2', 'studytime'] <- '2 to 5 hours'
df_Q2[df_Q2$studytime == '3', 'studytime'] <- '5 to 10 hours'
df_Q2[df_Q2$studytime == '4', 'studytime'] <- '>10 hours'

print(df_Q2)
df_Q2$studytime<- as.factor(df_Q2$studytime)
aovmodel <- aov(G3 ~ studytime, data = df_Q2)
# Display the summary of the ANOVA result
summary(aovmodel)

TukeyHSDResult <- TukeyHSD(aovmodel)
print(TukeyHSDResult)

#b.
df_Q2$address<- as.factor(df_Q2$address)
aovmodel <- aov(G3 ~ address, data = df_Q2)
# Display the summary of the ANOVA result
summary(aovmodel)

TukeyHSDResult <- TukeyHSD(aovmodel)
print(TukeyHSDResult)

#c.
df_Q2$romantic<- as.factor(df_Q2$romantic)
aovmodel <- aov(G3 ~ romantic, data = df_Q2)
# Display the summary of the ANOVA result
summary(aovmodel)

TukeyHSDResult <- TukeyHSD(aovmodel)
print(TukeyHSDResult)

#d.
df_Q2$famrel<- as.factor(df_Q2$famrel)
aovmodel <- aov(G3 ~ famrel, data = df_Q2)
# Display the summary of the ANOVA result
summary(aovmodel)

TukeyHSDResult <- TukeyHSD(aovmodel)
print(TukeyHSDResult)

#e.
df_Q2[df_Q2$Medu == '0', 'Medu'] <- 'none'
df_Q2[df_Q2$Medu == '1', 'Medu'] <- 'primary education (4th grade)'
df_Q2[df_Q2$Medu == '2', 'Medu'] <- '5th to 9th grade'
df_Q2[df_Q2$Medu == '3', 'Medu'] <- 'secondary education'
df_Q2[df_Q2$Medu == '4', 'Medu'] <- 'higher education'

df_Q2[df_Q2$Fedu == '0', 'Fedu'] <- 'none'
df_Q2[df_Q2$Fedu == '1', 'Fedu'] <- 'primary education (4th grade)'
df_Q2[df_Q2$Fedu == '2', 'Fedu'] <- '5th to 9th grade'
df_Q2[df_Q2$Fedu == '3', 'Fedu'] <- 'secondary education'
df_Q2[df_Q2$Fedu == '4', 'Fedu'] <- 'higher education'

df_Q2$Medu<- as.factor(df_Q2$Medu)
df_Q2$Fedu<- as.factor(df_Q2$Fedu)
aovmodel <- aov(G3 ~ Medu*Fedu, data = df_Q2)
# Display the summary of the ANOVA result
summary(aovmodel)

TukeyHSDResult <- TukeyHSD(aovmodel)
print(TukeyHSDResult)
