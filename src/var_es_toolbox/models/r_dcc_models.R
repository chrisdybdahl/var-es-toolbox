library(rugarch)

xspec = ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE))
uspec = multispec(replicate(2, xspec))
spec = dccspec(uspec = uspec, dccOrder = c(1, 1), distribution = 'mvnorm')
res = dccfit(spec, data = y)
H=res@mfit$H
DCCrho=vector(length=dim(y)[1])
for(i in 1:dim(y)[1]){
    DCCrho[i] =  H[1,2,i]/sqrt(H[1,1,i]*H[2,2,i])
}
