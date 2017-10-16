# ����/��ָ��������лع飬no dummy ----
#ld(f.dreg.idx)
ld(f.wreg.idx)

# run4f ������������four factor model
#i.idx <- c("��Ʊ���", "��Ʊ��ͷ", "��Ʊ�г�����", "ծȯ����", "�����", "��������", "��۲���", "��������")
i.idx <- c("��Ʊ���", "��Ʊ��ͷ", "��Ʊ�г�����", "�����", "��������")

run4f <- function(cond.i, data, index.name = i.idx) {
    lmlist <- list()
    for (i in index.name) {
        lmlist[[i]] <- lm(I(ret.idx * 100) ~ I(rm_rf * 100) + I(smb * 100) + I(hml * 100) + I(umd * 100), data = data[eval(cond.i)])
    }
    lmlist
}

# ��������
# ÿ��stategy�ֱ�ع�
lmlist <- run4f(quote(date <= "2015-07-01" & index.name == i), f.wreg.idx)
lmlist <- run4f(quote(date >= "2015-09-01" & index.name == i), f.wreg.idx)
# ������strategy
lmlist <- run4f(quote(date <= "2015-07-01" & index.name %in% i.idx), f.wreg.idx)
lmlist <- run4f(quote(date >= "2015-09-01" & index.name %in% i.idx), f.wreg.idx)

# �������棨robustness��
# ÿ��stategy�ֱ�ع�
lmlist <- run4f(quote(date <= "2015-07-01" & index.name == i), f.dreg.idx)
lmlist <- run4f(quote(date >= "2015-09-01" & index.name == i), f.dreg.idx)
# ������strategy
lmlist <- run4f(quote(date <= "2015-07-01"), f.dreg.idx)
lmlist <- run4f(quote(date >= "2015-09-01"), f.dreg.idx)

# ���htmlreg
file <- file.path(getwd(), "results", "4factor_index.html")
stars <- c(0.01, 0.05, 0.1)
htmlreg(lmlist, file = file, digits = 3, caption = '', stars = stars)

rm(lmlist, file)

# ����/��ָ��������лع飬 with dummy ----
ld(f.dret.idx)
ld(f.wreg.idx)

# �������� four factor�ع�ĺ�����ע������������dummy��
i.idx <- c("��Ʊ���", "��Ʊ��ͷ", "��Ʊ�г�����", "�����", "��������")
#i.idx <- c("�����", "��������", "��������")

# run4f ��ʾ����strategy
run4f <- function(cond.i, data, index.name = i.idx) {
    # Ϊ���ݼ���ǳ�is.ban
    data <- copy(data)[, ":="(is.ban = ifelse(date <= "2015-07-01", 0, ifelse(date >= "2015-09-01", 1, NA))), keyby = index.name][order(index.name, date)] %>% na.omit()
    lmlist <- list()
    for (i in index.name) {
        lmlist[[i]] <- lm(I(ret.idx * 100) ~ I(rm_rf * 100) + I(smb * 100) + I(hml * 100) + I(umd * 100) + is.ban, data = data[eval(cond.i)])
    }
    lmlist
}

# ʹ��������
# ����strategy
lmlist <- run4f(quote(index.name == i), f.wreg.idx)
# ������strategy
lmlist <- run4f(quote(index.name %in% i.idx), f.wreg.idx)

# ʹ��������
# ����strategy
lmlist <- run4f(quote(index.name == i), f.dreg.idx)
# ������strategy
lmlist <- run4f.all(quote(index.name %in% i.idx), f.dreg.idx)

# ���htmlreg
file <- file.path(getwd(), "results", "4factor_index.html")
stars <- c(0.01, 0.05, 0.1)
htmlreg(lmlist, file = file, digits = 3, caption = '', stars = stars)

# Conditional model�� no dummy ----
ld(f.wreg.cond)
ld(liq.wk.ps.4d)
ld(liq.wk.ps.stk.4d)
ld(liq.wk.ps.stk.5d)

# ���ڻع�ķ���
#i.idx <- c("��Ʊ��ͷ", "�����", "��������", "��������")
i.idx <- c("��Ʊ���", "��Ʊ��ͷ", "��Ʊ�г�����", "�����", "��������")

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

# ���ֲ�ͬ��strategy
lmlist <- runcond(quote(date <= "2015-07-01" & index.name == i))
lmlist <- runcond(quote(date >= "2015-09-01" & index.name == i))
# ������strategy
lmlist <- runcond(quote(date <= "2015-07-01" & index.name %in% i.idx))
lmlist <- runcond(quote(date >= "2015-09-01" & index.name %in% i.idx))

# �������htmlreg���
file <- file.path(getwd(), "results", "conditional_model.html")
stars <- c(0.01, 0.05, 0.1)
htmlreg(lmlist, file = file, digits = 3, caption = '', stars = stars)
#rm(file, lmlist, runcond)

# conditional model, with dummy ----
ld(f.wreg.cond)
ld(liq.wk.ps.4d)

reg <- copy(f.wreg.cond)[, ":="(is.ban = ifelse(date <= "2015-07-01", 0, ifelse(date >= "2015-09-01", 1, NA))), keyby = index.name][order(index.name, date)] %>% na.omit()
reg <- reg[liq.wk.ps.4d[order(year, week), .(year, week, liq, liq.rs, liq.rs.dm = liq.rs - fitted(ets(liq.rs, model = "ANN")) %>% as.vector())], on = .(year, week), nomatch = 0]

# ���ڻع�ķ���
i.idx <- c("��Ʊ���", "��Ʊ��ͷ", "��Ʊ�г�����", "�����", "��������")

runcond <- function(cond.i) {
    lm.list <- list()
    for (i in i.idx) {
        lm.list[[i]] <- lm(I(ret.idx * 100) ~ I(rm_rf * 100) + I(smb * 100) + I(hml * 100) + I(umd * 100) + I(whs300ret * c(NA, liq.rs.dm[-length(liq.rs.dm)]) * 100) + I(whs300ret * c(NA, whs300ret[-length(whs300ret)]) * 100) + I(whs300ret * c(NA, wrv[-length(wrv)]) * 10000) + is.ban, reg[eval(cond.i)])
    }
    lm.list
}

# ���ֲ�ͬ��strategy
lmlist <- runcond(quote(index.name == i))
# ������strategy
lmlist <- runcond(quote(index.name %in% i.idx))

# �������htmlreg���
file <- file.path(getwd(), "results", "conditional_model.html")
stars <- c(0.01, 0.05, 0.1)
htmlreg(lmlist, file = file, digits = 3, caption = '', stars = stars)