garch.sigma <- function(x) {
    spec <- ugarchspec(distribution.model = "std")
    fit <- ugarchfit(spec, x, solver = 'hybrid') %>% ugarchforecast(n.ahead = 1)
    fit@forecast$sigmaFor[1, 1]
}

system.time({
roll.dopar <- hs300[1:180, {
    n <- 120
    garch.sigma <- function(x) {
    spec <- ugarchspec(distribution.model = "std")
    fit <- ugarchfit(spec, x, solver = 'hybrid') %>% ugarchforecast(n.ahead = 1)
    fit@forecast$sigmaFor[1, 1]
    }

    foreach(t = (n + 1):.N, .final = rbindlist) %dopar% { 
        skew <- skewness(dret[(t - n):t])
        rv <- sum((dret[(t - n):t]) ^ 2)
        gv <- garch.sigma(dret[(t - n):t])
        list(date = date[t], skew = skew, rv = rv, gv = gv)
    }
    }
]
})




identical(roll.lapply, roll.for)