#Test 1

mc <- mtcars
str(mc)
summary(mc)

plot(mc$disp, mc$mpg)

cor(mc$disp, mc$mpg)

cor.test(mc$disp, mc$mpg)

cor.test(mc$disp, mc$mpg, method = "spearman")
