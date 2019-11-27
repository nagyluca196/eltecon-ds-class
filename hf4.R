#1. Either use your project data or find Something on [Kaggle](https://www.kaggle.com/)
#2. Find a good research/business question that involves prediction (either ) and write it down
#3. Answer your question using what we've learned previously and today
   # a. Use at least one graph to see variable interdependence (use `geom_smooth`)
    #a. Look at variable correlations
    #b. See what do residuals look like. Is it random? Why? Why not?
    #c. Run a regression
    #d. Add/Remove variables based on the result, rerun the regression
  #  e. Try adding an interaction
   # f. Describe how well the model fits your data. Try to reason why is it weak if so?
    #g. Try answering your question with your words
    #h. finish @ Home!

library(data.table)
library(ggplot2)
library(readxl)
data <- read_excel("data.xls", 
                  col_types = c("numeric", "text", "text", 
                                "text", "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric"))
as.data.frame(data)

#várható élettartam előrejelzése országoknál GDP függvényében

ggplot(data, aes(lifeexp_total, gdppercap))+geom_point()+theme_classic()

ggplot(data, aes(gdppercap, lifeexp_total))+geom_point()+theme_classic()+geom_smooth(method="lm")

ggplot(data, aes(lifeexp_total, gdppercap))+geom_point()+theme_classic()+geom_smooth(method="lm")
#Az első ábra jobb lm-t illeszt

cor(data[sapply(data, is.numeric)], use = "complete.obs")

#várható élettartam pozitívan korrelál: gpdpercap, urbanpop, kis poz. korr. current_health_expenditure változóval

#residuals: az ábrán elég nagy eltérések mutatkoznak, a várható élettartam kérdése komplexebb mint ezek a változók

slm <- lm(formula=lifeexp_total~gdppercap+urbanpop+current_health_exp, data=data)
summary(slm)

slm2 <- lm(formula=lifeexp_total~gdppercap+urbanpop, data=data)

summary(slm2)
 #slm2 a jobb modell

plot(slm2)

slm3 <- lm(formula=lifeexp_total~gdppercap+urbanpop+current_health_exp*gdpgrowth, data=data)
summary(slm3)

#interakcióval kicsit jobb modellt kapunk

plot(slm3)

#még így sem igazán illeszkedik (residuals vs. fitted), slm2 jobban illeszkedik. a várható élettartamhoz több változó is szükséges lehet, illetve meg nem mérhető változók is közrejátszanak (genetika, életstílus stb.)

residuals <- data.table(residuals=slm3$residuals, fitted=slm3$fitted.values)
ggplot(residuals, aes(fitted, residuals))+geom_point()+theme_classic()

