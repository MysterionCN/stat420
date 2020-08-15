crit = qt(0.95,8)
crit
-qt(0.1/2,8)
sim_slr = function(x,
                   beta_0 = 5,
                   beta_1 = 4,
                   sigma = 2) {
  n = 20
  epsilon = rnorm(n, mean = 0, sd = sigma)
  y = beta_0 + beta_1 * x + epsilon
  data.frame(predictor = x, response = y)
}
pnorm(4.2,4,1.5,lower.tail = FALSE)
?faithful
model = lm(formula = faithful$eruptions~faithful$waiting)
summary(model)$coef[1,3]
model$coefficients
4/1.5
pnorm(4.2,4,1.5,lower.tail = FALSE)
pnorm(4.2,4,sqrt(4/1.5),lower.tail = FALSE)
confint(model,level = 0.9)[1,2]
confint(model,level = 0.95)[2,2]-confint(model,level = 0.95)[2,1]
model2 = lm(eruptions~waiting,data = faithful)
model2
summary(model2)
predict(model2,newdata = data.frame(waiting = 72),interval = c("predict"),level = 0.99)
