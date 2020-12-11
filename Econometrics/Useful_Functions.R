F- Test for Joint Significance 
# heteroskedasticity-robust F-test
linearHypothesis(model, c("size=0", "expenditure=0"), white.adjust = "hc1")

#Not robust to heteroskedacity 
summary(model)$fstatistic

#olsrr is the package 
# Conducts Breusch Pagan Test
model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
ols_test_breusch_pagan(model)

------------------------------------------------------------------------------------------

# compute robust standard errors
rob_se <- diag(vcovHC(model, type = "HC1"))^0.5

# compute robust 95% confidence intervals
rbind("lower" = coef(model) - qnorm(0.975) * rob_se,
      "upper" = coef(model) + qnorm(0.975) * rob_se)
-------------------------------------------------------------------------------------------------     
# draw the robust 95% confidence set for coefficients on size and expenditure 
confidenceEllipse(model, 
                  fill = T,
                  lwd = 0,
                  which.coef = c("size", "expenditure"),
                  main = "95% Confidence Sets",
                  vcov. = vcovHC(model, type = "HC1"),
                  col = "red")
------------------------------------------------------------------------------------------------------                  
 # fit the quadratic Model
quadratic_model <- lm(score ~ income + I(income^2), data = CASchools)
----------------------------------------------------------------------------------
# obtain the model summary
coeftest(quadratic_model, vcov. = vcovHC, type = "HC1")

# estimate a cubic model
cubic_model <- lm(score ~ poly(income, degree = 3, raw = TRUE), data = CASchools)
-----------------------------------------------------------------------------------






------------------------------------------------------------------------------------------------------
quadriatic_model <- lm(score ~ income + I(income^2), data = CASchools)

# set up data for prediction
new_data <- data.frame(income = c(10, 11))

# do the prediction
Y_hat <- predict(quadriatic_model, newdata = new_data)

# compute the difference
diff(Y_hat)

-------------------------------------------------------------------------------------------------------------

new_data <- data.frame(income = c(10, 11, 40, 41))

# predict the outcomes 
Y_hat <- predict(LinearLog_model, newdata = new_data)

# compute the expected difference
Y_hat_matrix <- matrix(Y_hat, nrow = 2, byrow = TRUE)
Y_hat_matrix[, 2] - Y_hat_matrix[, 1]


-------------------------------------------------------------------------------------------------------------------------

bi_model <- lm(score ~ HiSTR * HiEL, data = CASchools)

# print a robust summary of the coefficients
coeftest(bi_model, vcov. = vcovHC, type = "HC1")

-----------------------------------------------------------------------------------------------------------------------


# estimate the fixed effects regression with plm()
fatal_fe_mod <- plm(fatal_rate ~ beertax, 
                    data = Fatalities,
                    index = c("state", "year"), 
                    model = "within")

# print summary using robust standard errors
coeftest(fatal_fe_mod, vcov. = vcovHC, type = "HC1")

------------------------------------------------------------------------------------------------------------------------------

fatal_tefe_mod <- plm(fatal_rate ~ beertax, 
                      data = Fatalities,
                      index = c("state", "year"), 
                      model = "within", 
                      effect = "twoways")

coeftest(fatal_tefe_mod, vcov = vcovHC, type = "HC1")

--------------------------------------------------------------------------------------------------------------------------------

class(fatal_tefe_lm_mod)
coeftest(fatal_tefe_lm_mod, vcov = vcovHC, type = "HC1")[1, ]
class(fatal_tefe_mod)
coeftest(fatal_tefe_mod, vcov = vcovHC, type = "HC1")
--------------------------------------------------------------------------------------------------------------------------------------

Fatalities$drinkagec <- cut(Fatalities$drinkage,
                            breaks = 18:22, 
                            include.lowest = TRUE, 
                            right = FALSE)

# set minimum drinking age [21, 22] to be the baseline level
Fatalities$drinkagec <- relevel(Fatalities$drinkagec, "[21,22]")

# mandadory jail or community service?
Fatalities$punish <- with(Fatalities, factor(jail == "yes" | service == "yes", 
                                             labels = c("no", "yes")))

# the set of observations on all variables for 1982 and 1988
Fatalities_1982_1988 <- Fatalities[with(Fatalities, year == 1982 | year == 1988), ]

--------------------------------------------------------------------------------------------------------------------------------------------
# compute the null model
denyprobit_null <- glm(formula = deny ~ 1, 
                       family = binomial(link = "probit"), 
                       data = HMDA)

# compute the pseudo-R2 using 'logLik'
1 - logLik(denyprobit2)[1]/logLik(denyprobit_null)[1]
#> [1] 0.08594259
