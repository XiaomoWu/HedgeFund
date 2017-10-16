# 事件研究法：使用index数据 （日度）----
# 生成event study的数据集，设定事件日：2016-01-01
ld(f.dreg.idx)
es <- f.dreg.idx[!(date %between% c(as.Date("2016-01-04"), as.Date("2016-01-07")))][date == as.Date("2016-01-08"), event.flg := 1]

c1 <- 31
c2 <- 230
m1 <- 120
m2 <- 30

# do car
do_car <- function(n, r, rm_rf, smb, hml, umd, date)
{
    stopifnot(m1 > m2)
    if (n - m1 < 0)
    {
        cat("n =", n, "is too small \n")
    } else if (n + c2 > length(r))
    {
        cat("n =", n, "is too large \n")
    } else
    {
        i1 <- max(1, n - m1)
        i2 <- n - m2
        i3 <- n - c1
        i4 <- n + c2
        r.model <- r[i1:i2]
        rm_rf.model <- rm_rf[i1:i2]
        smb.model <- smb[i1:i2]
        hml.model <- hml[i1:i2]
        umd.model <- umd[i1:i2]

        r.car <- r[i3:i4]
        rm_rf.car <- rm_rf[i3:i4]
        smb.car <- smb[i3:i4]
        hml.car <- hml[i3:i4]
        umd.car <- umd[i3:i4]

        model <- lm(I(r.model * 100) ~ I(rm_rf.model * 100) + I(smb.model * 100) + I(hml.model * 100) + I(umd.model * 100))
        coef <- coef(model)
        ars <- r.car - predict(model, list(r.model = r.car, rm_rf.model = rm_rf.car, smb.model = smb.car, hml.model = hml.car, umd.model = umd.car))
        list(date = list(date[i3:i4]), coef = list(coef), ars = list(ars))
    }
}

car <- es[,
    {
        ns <- which(event.flg == 1);
        lapply(ns, partial(do_car, r = ret.idx, rm_rf = rm_rf, smb = smb, hml = hml, umd = umd, date = date)) %>% rbindlist()
    },
    by = index.name]

car <- car[, .(ars = unlist(ars), date = as.Date(unlist(date))), keyby = index.name][order(index.name, date)][, car := cumsum(ars) , keyby = .(index.name)]

# 把CAR画出来
index.name.select <- c("股票多空", "股票多头", "股票市场中性", "多策略", "套利策略", "定向增发", "新三板")

car[(index.name %in% index.name.select) & date <= "2016-06-01"] %>%
    ggplot(aes(x = date, y = car, color = index.name)) +
    geom_line()

# 事件研究法：使用个股数据（周度）----
# 生成event study的数据集，设定事件日：2016-01-01
ld(f.wreg.fd)
ld(f.hfid)
es <- f.wreg.fd[date %between% c(as.Date("2015-09-01"), as.Date("2016-12-31"))][!(date %between% c(as.Date("2016-01-04"), as.Date("2016-01-07")))][date == as.Date("2016-01-08"), event.flg := 1]

c1 <- 4
c2 <- 12
m1 <-16
m2 <- 5

# do car
do_car <- function(n, r, rm_rf, smb, hml, umd, date)
{
    stopifnot(m1 > m2)
    if (n - m1 < 0)
    {
        cat("n =", n, "is too small \n")
    } else if (n + c2 > length(r))
    {
        cat("n =", n, "is too large \n")
    } else
    {
        i1 <- max(1, n - m1)
        i2 <- n - m2
        i3 <- n - c1
        i4 <- n + c2
        r.model <- r[i1:i2]
        rm_rf.model <- rm_rf[i1:i2]
        smb.model <- smb[i1:i2]
        hml.model <- hml[i1:i2]
        umd.model <- umd[i1:i2]

        r.car <- r[i3:i4]
        rm_rf.car <- rm_rf[i3:i4]
        smb.car <- smb[i3:i4]
        hml.car <- hml[i3:i4]
        umd.car <- umd[i3:i4]

        model <- lm(I(r.model * 100) ~ I(rm_rf.model * 100) + I(smb.model * 100) + I(hml.model * 100) + I(umd.model * 100))
        coef <- coef(model)
        ars <- r.car - predict(model, list(r.model = r.car, rm_rf.model = rm_rf.car, smb.model = smb.car, hml.model = hml.car, umd.model = umd.car))
        list(date = list(date[i3:i4]), coef = list(coef), ars = list(ars))
    }
}

car <- es[,
{
    ns <- which(event.flg == 1);
    lapply(ns, partial(do_car, r = wret.fd, rm_rf = rm_rf, smb = smb, hml = hml, umd = umd, date = date)) %>% rbindlist()
},
    by = fund.id]

car <- car[, .(ars = unlist(ars), date = as.Date(unlist(date))), keyby = fund.id][order(fund.id, date)][, car := cumsum(ars), keyby = .(fund.id)]
car <- car[f.hfid[, .(fund.id, type)], on = .(fund.id), nomatch = 0]


# 把CAR画出来
abline <- data.frame(start = as.Date("2016-01-01"), end = as.Date("2016-01-08"))
plot <- car[date %between% c(as.Date("2015-12-01"), as.Date("2016-04-01")), .(car = median(car)), keyby = .(type, date)]

ggplot() +
    theme_bw() +
    geom_rect(data = abline, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.35) +
    geom_smooth(data = plot, aes(x = date, y = car, color = type), size = 0.75, se = F, span = 0.15) +
    xlab('') +
    ylab('CAR (%)') +
    theme(legend.position = "bottom")

# C.B. , 2016-01-01 to 2016-02-01, CB = 1 ----
ld(f.wreg.idx)
reg <- f.wreg.idx[date >= "2015-09-01"][, ":="(is.cb = ifelse(date %between% as.Date(c("2016-01-01", "2016-02-01")), 1, 0)), keyby = index.name][order(index.name, date)] %>% na.omit()
# 用来进行 four factor回归的函数，注意里面添加了dummy！
run4f <- function(cond.i, data, index.name = c("股票多空", "股票多头", "股票市场中性", "多策略", "套利策略", "定向增发"))
{
    lmlist <- list()
    for (i in index.name)
    {
        lmlist[[i]] <- lm(I(ret.idx * 100) ~ I(rm_rf * 100) + I(smb * 100) + I(hml * 100) + I(umd * 100) + is.cb, data = data[eval(cond.i)])
    }
    lmlist
}
# 将结果用htmlreg输出
file <- file.path(getwd(), "results", "4factor_index.html")
lmlist <- run4f(quote(index.name == i), reg)
lmlist <- run4f(quote(index.name == index.name), reg)

stars <- c(0.01, 0.05, 0.1)
htmlreg(lmlist, file = file, digits = 3, caption = '', stars = stars)

# C.B. , 2015-09-08 to 2016-01-01, CB = 1 ----
ld(f.wreg.idx)
reg <- f.wreg.idx[date >= "2015-09-01"][, ":="(is.cb = ifelse(date <= "2016-01-01", 0, ifelse(date >= "2016-01-07", 1, NA))), keyby = index.name][order(index.name, date)] %>% na.omit()
# 用来进行 four factor回归的函数，注意里面添加了dummy！
run4f <- function(cond.i, data, index.name = c("股票多空", "股票多头", "股票市场中性", "多策略", "套利策略", "定向增发"))
{
    lmlist <- list()
    for (i in index.name)
    {
        lmlist[[i]] <- lm(I(ret.idx * 100) ~ I(rm_rf * 100) + I(smb * 100) + I(hml * 100) + I(umd * 100) + is.cb, data = data[eval(cond.i)])
    }
    lmlist
}
# 将结果用htmlreg输出
file <- file.path(getwd(), "results", "4factor_index.html")
lmlist <- run4f(quote(index.name == i), reg)
lmlist <- run4f(quote(index.name == index.name), reg)

htmlreg(lmlist, file = file, digits = 3, caption = '')

# 在C.B.之后计算period alpha (基于 fund)，1m, 1-3m, 1-6m, 1-12m ----
ld(f.wreg.fd)
ld(f.hfid)

lag_alpha_fd <- function(dt, cond.i.idx, cond.i.hfid, cond.i.date)
{
    alpha <- f.wreg.fd[i.hfid, on = .(fund.id), nomatch = 0][eval(i.date)][,
    {
        model <- lm(wret.fd ~ rm_rf + smb + hml + umd, data = .SD);
        alpha <- summary(model)$coefficients[1, c(1, 4)];
        names(alpha) <- c("coef", "p");
        as.list(alpha)
    },
        keyby = fund.id]
    alpha <- alpha[i.hfid, on = .(fund.id), nomatch = 0]
    alpha[, .(alpha = mean(coef, na.rm = T), p = mean(p, na.rm = T)), keyby = type.name]
}

i.idx <- c("股票多头", "股票多空", "股票市场中性", "多策略", "套利策略")
i.hfid <- f.hfid[type.name %in% i.idx | type.name == "套利策略", .(fund.id, type.name)]
i.date <- quote(date %between% as.Date(c("2016-01-08", "2016-07-08")))
lag_alpha_fd(dt = f.wreg.fd, cond.i.idx = i.idx, cond.i.date = i.date, cond.i.hfid = i.hfid)[]
# 在C.B.之后计算period alpha（基于 index），1m, 1-3m, 1-6m, 1-12m ----
ld(f.wreg.idx)
ld(f.dreg.idx)

lag_alpha <- function(dt, cond.i.idx, cond.i.date)
{
    dt[index.name %in% i.idx | index.name == "套利策略"][eval(i.date)][,
    {
        model <- lm(lm(I(ret.idx * 100) ~ I(rm_rf * 100) + I(smb * 100) + I(hml * 100) + I(umd * 100), data = .SD));
        alpha <- summary(model)$coefficients[1, c(1, 4)];
        names(alpha) <- c("coef", "p");
        as.list(alpha)
    },
    keyby = index.name]
}

i.idx <- c("股票多头", "股票多空", "股票市场中性", "多策略", "套利策略")
i.date <- quote(date %between% as.Date(c("2016-01-08", "2016-12-30")))

la <- lag_alpha(dt = f.wreg.idx, cond.i.date = i.date, cond.i.idx = i.idx)
la

# ---------------------------------------------
ld(f.wreg.idx)
ld(alpha.12w.idx)
draw.index <- c("股票多头", "股票多空", "股票市场中性")

abline <- data.frame(start = c(as.Date("2016-01-01"), as.Date("2015-07-08")), end = c(as.Date("2016-01-08"), as.Date("2015-09-08")))
plot <- alpha.12w.idx[date %between% c(as.Date("2015-07-01"), as.Date("2016-04-01"))]
ggplot() +
    theme_bw() +
    geom_rect(data = abline, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.35) + geom_line(data = plot[index.name %in% draw.index], aes(x = date, y = alpha, color = index.name, linetype = index.name), size = 1) +
    #geom_smooth(span = 0.05, se = F) +
    xlab('') +
    ylab('Weekly Alpha') +
    #scale_linetype_discrete(name = '', labels = c("Long", "Long & Short", "Market Neutral")) +
    #scale_color_discrete(name = "", labels = c("Long", "Long & Short", "Market Neutral")) +
    scale_linetype_discrete(name = '', labels = c("Mutiple", "Arbitrage", "SEO")) +
    scale_color_discrete(name = "", labels = c("Mutiple", "Arbitrage", "SEO")) +
    theme_bw() +
    theme(legend.position = "bottom")
