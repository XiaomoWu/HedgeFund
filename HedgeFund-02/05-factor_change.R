# 对周/日指数收益进行回归，no dummy ----
#ld(f.dreg.idx)
ld(f.wreg.idx)

# run4f 函数用来计算four factor model
#i.idx <- c("股票多空", "股票多头", "股票市场中性", "债券基金", "多策略", "套利策略", "宏观策略", "定向增发")
i.idx <- c("股票多空", "股票多头", "股票市场中性", "多策略", "套利策略")

run4f <- function(cond.i, data, index.name = i.idx) {
    lmlist <- list()
    for (i in index.name) {
        lmlist[[i]] <- lm(I(ret.idx * 100) ~ I(rm_rf * 100) + I(smb * 100) + I(hml * 100) + I(umd * 100), data = data[eval(cond.i)])
    }
    lmlist
}

# 用周收益
# 每个stategy分别回归
lmlist <- run4f(quote(date <= "2015-07-01" & index.name == i), f.wreg.idx)
lmlist <- run4f(quote(date >= "2015-09-01" & index.name == i), f.wreg.idx)
# 不区分strategy
lmlist <- run4f(quote(date <= "2015-07-01" & index.name %in% i.idx), f.wreg.idx)
lmlist <- run4f(quote(date >= "2015-09-01" & index.name %in% i.idx), f.wreg.idx)

# 用日收益（robustness）
# 每个stategy分别回归
lmlist <- run4f(quote(date <= "2015-07-01" & index.name == i), f.dreg.idx)
lmlist <- run4f(quote(date >= "2015-09-01" & index.name == i), f.dreg.idx)
# 不区分strategy
lmlist <- run4f(quote(date <= "2015-07-01"), f.dreg.idx)
lmlist <- run4f(quote(date >= "2015-09-01"), f.dreg.idx)

# 输出htmlreg
file <- file.path(getwd(), "results", "4factor_index.html")
stars <- c(0.01, 0.05, 0.1)
htmlreg(lmlist, file = file, digits = 3, caption = '', stars = stars)

rm(lmlist, file)

# 对周/日指数收益进行回归， with dummy ----
ld(f.dret.idx)
ld(f.wreg.idx)

# 用来进行 four factor回归的函数，注意里面添加了dummy！
i.idx <- c("股票多空", "股票多头", "股票市场中性", "多策略", "套利策略")
#i.idx <- c("多策略", "套利策略", "定向增发")

# run4f 表示区分strategy
run4f <- function(cond.i, data, index.name = i.idx) {
    # 为数据集标记出is.ban
    data <- copy(data)[, ":="(is.ban = ifelse(date <= "2015-07-01", 0, ifelse(date >= "2015-09-01", 1, NA))), keyby = index.name][order(index.name, date)] %>% na.omit()
    lmlist <- list()
    for (i in index.name) {
        lmlist[[i]] <- lm(I(ret.idx * 100) ~ I(rm_rf * 100) + I(smb * 100) + I(hml * 100) + I(umd * 100) + is.ban, data = data[eval(cond.i)])
    }
    lmlist
}

# 使用周收益
# 区分strategy
lmlist <- run4f(quote(index.name == i), f.wreg.idx)
# 不区分strategy
lmlist <- run4f(quote(index.name %in% i.idx), f.wreg.idx)

# 使用日收益
# 区分strategy
lmlist <- run4f(quote(index.name == i), f.dreg.idx)
# 不区分strategy
lmlist <- run4f.all(quote(index.name %in% i.idx), f.dreg.idx)

# 输出htmlreg
file <- file.path(getwd(), "results", "4factor_index.html")
stars <- c(0.01, 0.05, 0.1)
htmlreg(lmlist, file = file, digits = 3, caption = '', stars = stars)

# Conditional model， no dummy ----
ld(f.wreg.cond)
ld(liq.wk.ps.4d)
ld(liq.wk.ps.stk.4d)
ld(liq.wk.ps.stk.5d)

# 用于回归的方程
#i.idx <- c("股票多头", "多策略", "套利策略", "定向增发")
i.idx <- c("股票多空", "股票多头", "股票市场中性", "多策略", "套利策略")

library(forecast)
reg <- f.wreg.cond[liq.wk.ps.4d[order(year, week), .(year, week, liq, liq.rs, liq.rs.dm = liq.rs - fitted(ets(liq.rs, model = "ANN")) %>% as.vector())], on = .(year, week), nomatch = 0]

#[, ":="(liq.rs.dm = liq.rs - mean(liq.rs), liq.rs.dmD = liq.rs - median(liq.rs))]
runcond <- function(cond.i) {
    lm.list <- list()
    for (i in i.idx) {
        lm.list[[i]] <- lm(I(ret.idx * 100) ~ I(rm_rf * 100) + I(smb * 100) + I(hml * 100) + I(umd * 100) + I(shift(liq.rs.dm)) + I(whs300ret * c(NA, liq.rs.dm[-length(liq.rs.dm)]) * 100) + I(whs300ret * c(NA, whs300ret[-length(whs300ret)]) * 100) + I(whs300ret * c(NA, wrv[-length(wrv)]) * 10000), reg[eval(cond.i)])
    }
    lm.list
}

# 区分不同的strategy
lmlist <- runcond(quote(date <= "2015-07-01" & index.name == i))
lmlist <- runcond(quote(date >= "2015-09-01" & index.name == i))
# 不区分strategy
lmlist <- runcond(quote(date <= "2015-07-01" & index.name %in% i.idx))
lmlist <- runcond(quote(date >= "2015-09-01" & index.name %in% i.idx))

# 将结果用htmlreg输出
file <- file.path(getwd(), "results", "conditional_model.html")
stars <- c(0.01, 0.05, 0.1)
htmlreg(lmlist, file = file, digits = 3, caption = '', stars = stars)
#rm(file, lmlist, runcond)

# conditional model, with dummy ----
ld(f.wreg.cond)
ld(liq.wk.ps.4d)

reg <- copy(f.wreg.cond)[, ":="(is.ban = ifelse(date <= "2015-07-01", 0, ifelse(date >= "2015-09-01", 1, NA))), keyby = index.name][order(index.name, date)] %>% na.omit()
reg <- reg[liq.wk.ps.4d[order(year, week), .(year, week, liq, liq.rs, liq.rs.dm = liq.rs - fitted(ets(liq.rs, model = "ANN")) %>% as.vector())], on = .(year, week), nomatch = 0]

# 用于回归的方程
i.idx <- c("股票多空", "股票多头", "股票市场中性", "多策略", "套利策略")

runcond <- function(cond.i) {
    lm.list <- list()
    for (i in i.idx) {
        lm.list[[i]] <- lm(I(ret.idx * 100) ~ I(rm_rf * 100) + I(smb * 100) + I(hml * 100) + I(umd * 100) + I(whs300ret * c(NA, liq.rs.dm[-length(liq.rs.dm)]) * 100) + I(whs300ret * c(NA, whs300ret[-length(whs300ret)]) * 100) + I(whs300ret * c(NA, wrv[-length(wrv)]) * 10000) + is.ban, reg[eval(cond.i)])
    }
    lm.list
}

# 区分不同的strategy
lmlist <- runcond(quote(index.name == i))
# 不区分strategy
lmlist <- runcond(quote(index.name %in% i.idx))

# 将结果用htmlreg输出
file <- file.path(getwd(), "results", "conditional_model.html")
stars <- c(0.01, 0.05, 0.1)
htmlreg(lmlist, file = file, digits = 3, caption = '', stars = stars)
