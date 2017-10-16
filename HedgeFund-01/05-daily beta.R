# make reg dataset ----
ld(rv)
ld(drf)
ld(short)

ld(dret.idx)
ld(wret.idx)

ld(d4f)
ld(w4f)

ld(index.name.select)
ld(index.name.noselect)

dreg <- d4f[, .(trddt, riskpremium1, smb1, hml1, mom)][
    rv[, .(trddt, dret, drv.ab)], on = "trddt", nomatch = 0][
    drf, on = "trddt", nomatch = 0][
    short, on = "trddt", nomatch = 0]

demean <- function(x) {
    r <- x - mean(x, na.rm = T)
}
dreg <- dret.idx[, .(index_name, trddt, ret.idx.rf, ret.idx)][dreg, on = "trddt", nomatch = 0][, ":="(dret.rf = dret - rf)][order(index_name, trddt)][
    , ":="(rf = demean(rf), securityshort = demean(securityshort), dret.demean = demean(dret))]


# run regression ----
index.name.stock <- c("股票多头", "股票多空", "股票市场中性")

cond.i <- quote((index_name == i))
#cond.i <- quote(trddt <= "2014-11-30" & index_name == i)
#cond.i <- quote(trddt > "2014-12-01" & trddt <= "2015-06-30" & index_name == i)
#cond.i <- quote(trddt > "2015-07-01" & trddt <= "2015-09-10" & index_name == i)
#cond.i <- quote(trddt > "2015-09-10" & index_name == i)

lm.list <- list()
for (i in index.name.stock) {
    lm.list[[i]] <- lm(ret.idx ~ I(dret * c(NA, rf[-length(rf)]) * 100) + I(dret * c(NA, dret.demean[-length(dret.demean)])) + I(dret * c(NA, drv.ab[-length(drv.ab)]) * 10) + dret.rf + smb1 + hml1 + mom, dreg[eval(cond.i)])
}

# render to html
stars <- c(0.01, 0.05, 0.1)
file <- file.path(getwd(), "results", "daily.beta.html")
#var.names <- c("Alpha", "Market", "Gamma1", "Gamma2", "Lambda1", "Lambda2", "SMB", "HML", "MOMENTUM")

htmlreg(lm.list, file = file, stars = stars, digits = 3, caption = "")


# plot daily beta
index.name.stock <- c("股票多头", "股票多空", "股票市场中性")

coef <- dreg[(index_name == index.name.stock), as.list(coef(lm(ret.idx ~ I(dret * c(NA, rf[-length(rf)]) * 100) + I(dret * c(NA, dret.demean[-length(dret.demean)])) + I(dret * c(NA, drv.ab[-length(drv.ab)]) * 10) + dret.rf + smb1 + hml1 + mom, .SD))), by = index_name]
setnames(coef, names(coef), c("index_name", "alpha", "gama.rf", "gama.csi300", "gama.rv", "beta.csi300", "beta.smb1", "beta.hml1", "beta.mom"))

dbeta <- dreg[(index_name %in% index.name.stock), .(index_name, trddt, dret.demean, drv.ab, rf)][(coef), on = "index_name", nomatch = 0][, ":="(dbeta = beta.csi300 + gama.rv * c(NA, drv.ab[-length(drv.ab)]) + gama.csi300 * c(NA, dret.demean[-length(dret.demean)]) + gama.rf * c(NA, rf[-length(rf)]))]

abline <- as.numeric(as.Date(c("2015-07-08", "2015-08-25", "2015-08-31", "2015-09-02", "2015-09-07", "2016-01-01")))
beta <- ggplot(dbeta[index_name %in% index.name.stock & trddt %between% c("2012-01-01", "2017-01-01")], aes(x = trddt, y = dbeta, linetype = index_name)) +
    geom_line(size = 0.5) +
    geom_vline(xintercept = abline, colour = "grey", size = 1, linetype = "dashed") +
    theme_bw() +
    theme(legend.position = "bottom") +
    scale_linetype_discrete(name = '', labels = c("Long & Short", "Long", "Market Neutral")) +
    scale_x_date(date_labels = "%Y-%m") +
    ylab("Daily Beta") +
    xlab("")

ld(r.hs300.daily)
mkt <- ggplot(r.hs300.daily[trddt %between% c("2012-01-01", "2017-01-01")], aes(x = trddt, y = close)) +
    geom_line(size = 1) +
    xlab("") +
    ylab("Index Value") +
    geom_vline(xintercept = abline, colour = "grey", size = 1, linetype = "dashed") +
    theme_bw() +
    scale_linetype_discrete(name = '', labels = c("Long & Short", "Long", "Market Neutral")) +
    scale_x_date(date_labels = "%Y-%m")

multiplot(beta, mkt)


# time series correlation
d <- dbeta[order(index_name, trddt)][index_name %in% index.name.stock, .(index_name, trddt, dbeta = dbeta)] %>% dcast(trddt ~ index_name)
setnames(d, names(d), c("trddt", "long", "longshort", "neutral"))
d <- d[complete.cases(d)]
d[, cor(long, neutral)]

d <- dret.idx.noselect[order(index_name, trddt)][index_name %in% index.name.stock] %>% dcast(trddt ~ index_name, value.var = "ret.idx")
setnames(d, names(d), c("trddt", "longshort", "long", "neutral"))
d[, .(cor(longshort, long))]



