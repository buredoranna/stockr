actual.port.purchase.price <- 6854.50
start.dollars <- 27350
purchase.multiplier <- start.dollars / actual.port.purchase.price

current.val.voo <- 189.76
current.val.uso <- 18.50

purchase.val.voo <- 188.44
purchase.val.uso <- 19.11

weight.voo <- 0.49
weight.uso <- 0.51


nshares.voo <- start.dollars * weight.voo / purchase.val.voo
nshares.uso <- start.dollars * weight.uso / purchase.val.uso

purchase.port.val <- nshares.voo * purchase.val.voo + nshares.uso * purchase.val.uso

current.val.port  <- nshares.voo * current.val.voo + nshares.uso * current.val.uso

delta.port.val <- current.val.port - purchase.port.val
print(delta.port.val)
print(purchase.multiplier)


