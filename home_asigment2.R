#načteme soubor data.txt
data = read.table("data.txt", header = TRUE, sep = "")

# máme zadanou regresní fci ale tu musíme zlogarytmovat
# po úpravě vznikne: log(Output) = B1*log(LPrice) + B2*log(KPrice) + B3*log(FPrice) + (B0 + epsilon)*log(u) 

# přiřadíme proměnné k datovému souboru
LPrice = data$LPrice
KPrice = data$KPrice
FPrice = data$FPrice
Output = data$Output

# nadefinujeme si proměnné
log.output = log(Output)
log.lprice = log(LPrice)
log.kprice = log(KPrice)
log.fprice = log(FPrice)

regrese = lm(log.output ~ log.lprice + log.kprice + log.fprice, data)
summary(regrese)
# na 5% hladině významnosti jsou statisticky významné  pouze B0 a B3

# grafický test hetetroskedasticity
par(mfrow=c(2,2)) 
plot(regrese)

#Breusch-Pagan test na heteroskedasticitu
library(lmtest)
bptest(regrese)
# p-hodnota dostatečně vysoká -> zamítám H0 -> heteroskedasticita

# test heteroskedasticity
rezidua = regrese$residuals
bptest = lm(rezidua^2 ~ log.lprice + log.kprice + log.fprice, data)
summary(bptest)
# p-hodnota opět vysoká -> zamítám H0 -> heteroskedasticita

# hypotéza H0: B1 = 1,5
ttestL = (1.5975-1.5)/1.084206
# hodnota je vyšší než 0,05 -> nezamítáme H0

# hypotéza H0: B0=B1=0
regreseOmez = lm(log.output ~ 0 + log.kprice + log.fprice, data )
anova(regreseOmez, regrese)
# p-hodnota je příliž nízká (<0,05) -> nezamítáme H0