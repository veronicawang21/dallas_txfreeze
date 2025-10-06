rm(list = ls())

library(tidyverse)
library(geepack)
library(lme4)
library(ggrepel)
setwd("C:\\Users\\veron\\OneDrive - Harvard University\\smartsurfaces\\Dallas_911calls\\tx_freeze\\")

df_freeze <- readRDS("df_all_freeze.rds") %>%
  mutate(m_d = make_date(year=2000, month=month(date), day=mday(date)),
         treat=as.factor(ifelse(yr=="2021", "2021", "Control years")),
         treat=relevel(treat, ref="Control years"),
         post=ifelse(m_d>=as.Date("2000-02-09"), "Great TX Freeze", "pre-Great TX Freeze"),
         post=factor(post, levels=c("pre-Great TX Freeze", "Great TX Freeze"))) %>%
  filter(yr>2015,
         m_d<=as.Date("2000-02-20"),
         m_d>=as.Date("2000-01-28")) %>%
  select(date, yr, m_d, doy, post, tmax, tmin, treat, call, tot, fire, amb)

# defining cold snap
## plot tmax and tmin

pdf("fig1.pdf", height=5, width=9)
df_freeze %>%
  group_by(treat, m_d) %>%
  summarise(tmax_avg=mean(tmax), tmin_avg=mean(tmin)) %>%
  select(m_d, treat, tmax_avg, tmin_avg) %>%
  pivot_longer(cols = c("tmax_avg", "tmin_avg"),
               names_to=c("t"),
               values_to = c("Temperature, C")
  ) %>%
  mutate(g=interaction(treat, t),
  t=recode(t, 
    "tmax_avg"="Maximum",
    "tmin_avg"="Minimum"
  )) %>%
  mutate(label = if_else(m_d == max(m_d), g, NA_character_)) %>%
  ggplot(aes(x=m_d, y= `Temperature, C`)) +
  annotate("rect", xmin=as.Date("2000-02-09"), xmax=as.Date("2000-02-14"), ymin=-Inf, ymax=Inf, 
           fill = "grey", alpha = 0.2) +
  annotate("rect", xmin=as.Date("2000-02-14"), xmax=as.Date("2000-02-20"), ymin=-Inf, ymax=Inf, 
           fill = "grey", alpha = 0.5) +
  geom_hline(yintercept = 0, color="grey", linetype="dashed") +
  geom_line(aes(group=g, color=treat, linetype=t)) + 
  labs(color="Group", linetype="Temperature") +
  theme_bw() +
  theme(axis.title.x = element_blank())
dev.off()


# testing parallel trends assumption

## 1) visual test/DiD

pdf("fig2.pdf", height=9, width=9)
df_freeze %>%
  pivot_longer(cols = c("call", "tot", "fire", "amb"),
               names_to=c("out"),
               values_to = c("num")) %>%
  group_by(out, treat, m_d) %>%
  mutate(num_mean=mean(num),
         out=factor(out, levels=c("call", "tot", "fire", "amb")),
         out=recode(out,
                    "call"="Calls",
                    "tot"="All vehicles",
                    "fire"="Fire trucks only",
                    "amb"="Ambulances only")) %>%
  ggplot(aes(x=m_d, y=num, group=as.factor(yr), color=treat)) +
  geom_rect(xmin = as.Date("2000-02-14"), xmax = Inf, ymin = -Inf, ymax = Inf,
            fill="grey90", color=NA, alpha=0.05) +
  geom_line(linetype="dashed") +
  geom_smooth(aes(x=m_d, y=num_mean), method = "lm", se=FALSE) +
  facet_grid(out~post, scales = "free") +
  labs(y="Number of dispatches", color="Group") +
  theme_test() +
  theme(axis.title.x = element_blank())
dev.off()

## 2) statistical test

df_pre <- df_freeze %>%
  filter(m_d<as.Date("2000-02-09")) %>%
  mutate(per=doy-27,
         yr=as.factor(yr)) %>%
  select(yr, per, call, tot, fire, amb, treat)

mod <- geeglm(call ~ as.factor(treat)*per, data=df_pre, id=yr, corstr = "independence")
summary(mod)
mod <- geeglm(tot ~ as.factor(treat)*per, data=df_pre, id=yr, corstr = "independence")
summary(mod)
mod <- geeglm(fire ~ as.factor(treat)*per, data=df_pre, id=yr, corstr = "independence")
summary(mod)
mod <- geeglm(amb ~ as.factor(treat)*per, data=df_pre, id=yr, corstr = "independence")
summary(mod)

## DiD

df_did <- df_freeze %>%
  mutate(post_sub=ifelse(m_d>as.Date("2000-02-13"), "Great TX Freeze cold spell", 
                         ifelse(m_d>=as.Date("2000-02-09") & m_d<=as.Date("2000-02-13"), "Great TX Freeze cold front",
                                "pre-Great TX Freeze")),
         post_sub=factor(post_sub, levels=c("pre-Great TX Freeze", "Great TX Freeze cold front", "Great TX Freeze cold spell"))) %>%
  mutate(per=doy-27,
         yr=as.factor(yr),
         yr=relevel(yr, ref="2021")) %>%
  select(yr, per, treat, post, post_sub, call, tot, fire, amb)

res <- function(modd){
  co <- summary(modd)$coefficients[5:6,]
  co %>%
    mutate(lo=Estimate-1.96*Std.err, hi=Estimate+1.96*Std.err) %>%
    select(Estimate, lo, hi) %>% print
}

mod_did <- geeglm(call ~ as.factor(treat)*post_sub, data=df_did, id=yr, corstr = "independence")
res(mod_did)

mod_did <- geeglm(tot ~ as.factor(treat)*post_sub, data=df_did, id=yr, corstr = "independence")
res(mod_did)

mod_did <- geeglm(fire ~ as.factor(treat)*post_sub, data=df_did, id=yr, corstr = "independence")
res(mod_did)

mod_did <- geeglm(amb ~ as.factor(treat)*post_sub, data=df_did, id=yr, corstr = "independence")
res(mod_did)