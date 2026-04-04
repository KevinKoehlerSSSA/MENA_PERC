# ---------------------------------------------------------
# Albrecht, Bishara, Koehler. Costly Commitments in 
# Authoritarian Regime Formation: Evidence from Tunisia 
# ---------------------------------------------------------
# VERSION: 3 April 2026
# ---------------------------------------------------------
# This code replicates the analysis and figures in the 
# published version of the article. Original data are 
# available from 
# https://kevinkoehlersssa.github.io/MENA_PERC/data/;
# we also use Afrobarometer data (round 9); Figure 1
# uses descriptive data from various waves of the 
# Afrobarometer and Arab Barometer as well as from 
# various Emrhod polls
# ---------------------------------------------------------

# ---------------------------------------------------------
# 0. Packages and Functions
# ---------------------------------------------------------

library(tidyverse)   
library(patchwork)   
library(haven)       
library(broom)       
library(stargazer)   
library(scales)      

reverseCode <- function(x, min = 1, max = 5){
  
  # Written by Parker Tichko, May 2020
  # Email: my first name DOT my last name @ gmail.com
  
  if(min(x, na.rm = TRUE) < min | max(x, na.rm = TRUE) > max){
    warning("Warning: input is outside the range of the scale.")
  }
  
  sort(min:max, decreasing = TRUE)[x+(1-min)]
  
}

# ---------------------------------------------------------
# 1. Figure 2
# ---------------------------------------------------------
# Sources: 

# Emrhod refers to various polls conducted by the
# Tunisian polling firm Emrhod and published by various 
# news outlets, for example here: https://news.gnet.tn/Emrhod-Consulting-82%25-des-Tunisiens-sont-satisfaits-du-rendement-de-Kais-Saied

# The Arab Barometer (Wave 7) is available online at 
# https://www.arabbarometer.org/
# The question used in Panel A is QTUN1

# The Afrobarometer (Round 9) is available at 
# https://www.afrobarometer.org/survey-resource/tunisia-round-9-data-2023/
# The questions used in Panel A are
# Q37A (trust)
# Q47A (approval)
# Q96 (vote intention)

# Own survey refers to our July 2022 survey and is availble at
# https://kevinkoehlersssa.github.io/MENA_PERC/data/

# Panel B reproduces Figure 4 published in 
# Koehler, K. (2023). Breakdown by disengagement: 
# Tunisia’s transition from representative democracy. 
# Political Research Exchange, 5(1). 
# https://doi.org/10.1080/2474736X.2023.2279778

# ---------------------------------------------------------

theme_set(
  theme_classic(base_family = "Times New Roman")
)

# ---------------------------------------------------------
# 1.1 Panel A (2021–2022)
# ---------------------------------------------------------

saied_indicators <- tribble(
  ~date,         ~indicator,                                       ~value, ~source,
  as.Date("2021-06-15"), "Approval rating (pre–July 25)",             38,   "Emrhod, Jun 2021",
  as.Date("2021-08-01"), "Approval rating (post–July 25)",            82,   "Emrhod, Aug 2021",
  as.Date("2021-10-25"), "Agreed with suspension of parliament",      90,   "Arab Barometer, W7, Oct–Nov 2021",
  as.Date("2022-02-20"), "Trusted Kais Saied",                        75,   "Afrobarometer, W9, Feb–Mar 2022",
  as.Date("2022-02-20"), "Approved Saied's performance",              90,   "Afrobarometer, W9, Feb–Mar 2022",
  as.Date("2022-02-20"), "Would vote for Saied (if election held)",   47,   "Afrobarometer, W9, Feb–Mar 2022",
  as.Date("2022-06-15"), "July 25 is a threat (direct question)",     27,   "Own survey, Jun 2022",
  as.Date("2022-06-15"), "Return to 2014 constitution",               36,   "Own survey, Jun 2022",
  as.Date("2022-06-15"), "July 25 weakened democracy (list exp.)",    40,   "Own survey, Jun 2022"
) %>%
  mutate(indicator = factor(indicator, levels = rev(unique(indicator))))

saied_indicators <- saied_indicators %>%
  mutate(
    indicator = factor(indicator, levels = rev(unique(indicator))),
    source_label = paste0("(", source, ")")
  )


pA_lollipop <- ggplot(saied_indicators, aes(x = value, y = indicator)) +
  
  geom_segment(aes(x = 0, xend = value, yend = indicator),
               linewidth = 0.8,
               color = "black") +
  
  geom_point(size = 2.8, color = "black") +
  
  geom_text(aes(label = paste0(value, "%")),
            nudge_x = 6,
            size = 3.5,
            family = "Times New Roman") +
  
  geom_text(aes(x = 0, label = source_label),
            hjust = 0,
            vjust = 1.5,
            size = 2,
            color = "grey40",
            family = "Times New Roman") +
  
  scale_x_continuous(
    limits = c(-10, 105),
    breaks = seq(0, 100, 20)
  ) +
  
  labs(
    title = "A. Public attitudes toward Saied and July 25 (2021–2022)",
    x = "Percent",
    y = NULL
  ) +
  
  theme(
    plot.title = element_text(face = "bold", family = "Times New Roman"),
    axis.text = element_text(family = "Times New Roman"),
    axis.title = element_text(family = "Times New Roman")
  )
pA_lollipop

# ---------------------------------------------------------
# 1.2 Panel B (2013–2021)
# ---------------------------------------------------------

institutions <- tribble(
  ~year, ~institution,           ~value,
  2011,  "Government",            66.61,
  2013,  "Government",            41.69,
  2016,  "Government",            35.67,
  2018,  "Government",            21.69,
  2020,  "Government",            17.98,
  2021,  "Government",            35.16,
  
  2013,  "Parliament",            33.39,
  2016,  "Parliament",            22.16,
  2018,  "Parliament",            15.05,
  2021,  "Parliament",             8.86,
  
  2013,  "Political Parties",      9.45,
  2015,  "Political Parties",     21.03,
  2016,  "Political Parties",     13.49,
  2018,  "Political Parties",     10.26,
  2021,  "Political Parties",     12.76,
  
  2013,  "Democracy",             74.78,
  2015,  "Democracy",             69.57,
  2016,  "Democracy",             52.33,
  2018,  "Democracy",             70.68,
  2020,  "Democracy",             61.67,
  2021,  "Democracy",             68.51
)

pB <- ggplot(institutions,
             aes(x = year,
                 y = value,
                 color = institution)) +
  
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  
  geom_text(aes(label = round(value, 1)),
            nudge_y = 5,
            size = 3,
            family = "Times New Roman",
            show.legend = FALSE) +
  
  geom_hline(yintercept = 50,
             linetype = "dotted") +
  
  scale_x_continuous(breaks = seq(2011, 2022, 1)) +
  scale_y_continuous(limits = c(0, 100),
                     breaks = seq(0, 100, 20)) +
  
  scale_color_manual(values = c(
    "Democracy" = "black",
    "Government" = "grey30",
    "Parliament" = "grey50",
    "Political Parties" = "grey70"
  )) +
  
  labs(
    title = "B. Declining confidence in representative institutions (2011–2021)",
    x = NULL,
    y = "Percent favorable",
    color = NULL
  ) +
  
  theme_classic(base_family = "Times New Roman") +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

pB

# ---------------------------------------------------------
# 1.3 Combine panels
# ---------------------------------------------------------

# Choose either pA (timeline points) OR pA_lollipop (clean list of indicators).
fig <- (pA_lollipop / pB) +
  plot_annotation(
    caption = "Note: Indicators in Panel B from various waves of the Arab and Afrobarometers."
  )

fig

ggsave("figure1.png", dpi=600, width = 8, height = 6)

# ---------------------------------------------------------
# 2. Figure 3 (and Table A1)
# ---------------------------------------------------------

# ---------------------------------------------------------
# 2.1 2022 Tunisia Survey
# ---------------------------------------------------------

data <- read_sav("/Users/kevin/Dropbox/Tunisia Survey 2022/Survey data 280622.sav")

data <- data %>%
  mutate(
    Q34A = as.numeric(Q34A),
    Q34B = as.numeric(Q34B)
  )

data <- data %>%
  mutate(
    ## treatment + items (keep for filtering)
    treated = case_when(
      !is.na(Q34A) ~ 1,
      !is.na(Q34B) ~ 0
    ),
    items = case_when(
      !is.na(Q34A) ~ Q34A,
      !is.na(Q34B) ~ Q34B
    ),
    items = ifelse(items <= 5, items, NA_real_),
    
    ## controls
    female = case_when(
      Q4 == 1 ~ 0,
      Q4 == 2 ~ 1,
      TRUE ~ NA_real_
    ),
    edu = ifelse(Q6 < 98, Q6, NA_real_),
    age = ifelse(Q1 < 998, Q1, NA_real_),
    income = ifelse(Q10 < 9, Q10, NA_real_),
    
    ## trust items (numeric + cleaned)
    mps = ifelse(Q39 < 98, Q39, NA_real_),
    people = ifelse(Q40 < 98, Q40, NA_real_),
    officials = ifelse(Q41 < 98, Q41, NA_real_),
    
    ## reverse scales (1–5)
    mps_r = 6 - mps,
    people_r = 6 - people,
    officials_r = 6 - officials,
    
    ## populism components
    ae = (mps_r + officials_r) / 2,
    crd = people_r,
    
    ## rescale to [0,1]
    ae = rescale(ae),
    crd = rescale(crd),
    
    ## Goertz measure
    pop_goertz = pmin(ae, crd),
    
    ## dependent variables
    const_binary = case_when(
      Q36 %in% c(1, 2) ~ 1,
      Q36 %in% c(3, 4, 5) ~ 0,
      TRUE ~ NA_real_
    ),
    july25 = case_when(
      Q35 == 1 ~ 0,
      Q35 == 2 ~ 1,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(
    !is.na(treated),
    !is.na(items)
  )

# ---------------------------------------------------------
# 2.2 Models
# ---------------------------------------------------------

t.test(pop_goertz~const_binary, data=data)
t.test(pop_goertz~july25, data=data)

m1 <- glm(const_binary~
            pop_goertz +
            edu +
            income +
            age +
            female,
          data=data)
summary(m1)

m2 <- glm(july25~
            pop_goertz +
            edu +
            income +
            age +
            female,
          data=data)
summary(m2)

stargazer(m1,m2,
          type="latex",
          out="a1.tex")

# ---------------------------------------------------------
# 2.3 Figure 3
# ---------------------------------------------------------

cplot <- multiplot(m1,m2,
                   coefficients = "pop_goertz",
                   by="Model")$data
cplot$Coefficient <- c("Populism scale", "Populism scale")
cplot$Model <- c("Return to 2014 constitution", "25 July threatens democracy")

cplot$Outcome <- dplyr::case_when(
  cplot$Model == "25 July threatens democracy" ~ "July 25 threatens democracy",
  cplot$Model == "Return to 2014 constitution" ~ "Return to 2014 Constitution",
  TRUE ~ cplot$Model
)

cplot$Outcome <- factor(
  cplot$Outcome,
  levels = c("July 25 threatens democracy", "Return to 2014 Constitution")
)

plot <- ggplot(cplot, aes(x = Value, y = Outcome)) +
  geom_point(size = 2, shape = 16) +
  
  geom_errorbar(aes(xmin = LowInner, xmax = HighInner), width = 0, linewidth = 1) +
  geom_errorbar(aes(xmin = LowOuter, xmax = HighOuter), width = 0, linewidth = 0.5) +
  
  ## point labels
  geom_text(
    aes(label = sprintf("%.2f", Value)),
    vjust = -1.5,   
    size = 3,
    family = "Times New Roman"
  ) +
  
  geom_vline(xintercept = 0, linetype = "dotted") +
  coord_cartesian(xlim = c(-0.5, 0.5)) +
  theme_classic() +
  labs(x = "Estimated effect of populism", y = NULL) +
  theme(
    axis.text = element_text(color = "black", family = "Times New Roman", size = 10),
    axis.title.x = element_text(family = "Times New Roman", size = 14)
  )

plot
ggsave("figure3.jpg",
       width = 6,
       height = 2,
       dpi=600)

# ---------------------------------------------------------
# 3. Afrobarometer
# ---------------------------------------------------------

ab9 <- read_spss("/Users/kevin/Dropbox/Tunisia Transition Paper/afrobarometer_tunisia_round9-2023.sav")

ab9 <- ab9 %>%
  mutate(saied=case_when(
    Q96==1592~1,
    Q96!=1592 & Q96<9998~0,
    TRUE~NA_real_
  ),
  saied_all=case_when(
    Q96==1592~1,
    Q96!=1592 & Q96<9998~0,
    Q96==9998~0,
    TRUE~NA_real_
  ),
  rural=ifelse(URBRUR==2,1,0),
  female = ifelse(Q101==2,1,0),
  party_id=ifelse(Q89A<8,Q89A,NA_real_),
  edu=ifelse(Q94<10,Q94,NA_real_),
  age=ifelse(Q1<900,Q1,NA_real_),
  freedom_org=case_when(Q15==1~1,
                        Q15==2~2,
                        Q15==5~3,
                        Q15==3~4,
                        Q15==4~5,
                        TRUE~NA_real_),
  censor_media=case_when(Q16==1~1,
                         Q16==2~2,
                         Q16==5~3,
                         Q16==3~4,
                         Q16==4~5,
                         TRUE~NA_real_),
  media_harms=case_when(Q17==1~1,
                        Q17==2~2,
                        Q17==5~3,
                        Q17==3~4,
                        Q17==4~5,
                        TRUE~NA_real_),
  gov_acc=case_when(Q18==1~1,
                    Q18==2~2,
                    Q18==5~3,
                    Q18==3~4,
                    Q18==4~5,
                    TRUE~NA_real_),
  gov_employee=case_when(Q19A==1~1,
                         Q19A==2~2,
                         Q19A==5~3,
                         Q19A==3~4,
                         Q19A==4~5,
                         TRUE~NA_real_),
  mps_free=case_when(Q19B==1~1,
                     Q19B==2~2,
                     Q19B==5~3,
                     Q19B==3~4,
                     Q19B==4~5,
                     TRUE~NA_real_),
  single_party=case_when(Q22A==1~1,
                         Q22A==2~2,
                         Q22A==5~3,
                         Q22A==3~4,
                         Q22A==4~5,
                         TRUE~NA_real_),
  single_party_b=ifelse(single_party>3,1,0),
  military_rule=case_when(Q22B==1~1,
                          Q22B==2~2,
                          Q22B==5~3,
                          Q22B==3~4,
                          Q22B==4~5,
                          TRUE~NA_real_),
  abolish_parliament=case_when(Q22C==1~1,
                               Q22C==2~2,
                               Q22C==5~3,
                               Q22C==3~4,
                               Q22C==4~5,
                               TRUE~NA_real_),
  prefer_democracy=case_when(
    Q23==3~2,
    Q23==2~0,
    Q23==1~1,
    TRUE~NA_real_
  ),
  no_elections=case_when(Q24==1~1,
                         Q24==2~2,
                         Q24==5~3,
                         Q24==3~4,
                         Q24==4~5,
                         TRUE~NA_real_),
  multipartyism=case_when(Q25==1~1,
                          Q25==2~2,
                          Q25==5~3,
                          Q25==3~4,
                          Q25==4~5,
                          TRUE~NA_real_),
  parties_division=case_when(
    multipartyism==1~5,
    multipartyism==2~4,
    multipartyism==3~3,
    multipartyism==4~2,
    multipartyism==5~1,
  ),
  dominant_party=case_when(Q26==1~1,
                           Q26==2~2,
                           Q26==5~3,
                           Q26==3~4,
                           Q26==4~5,
                           TRUE~NA_real_),
  exec_over_leg1=case_when(Q27A==1~1,
                           Q27A==2~2,
                           Q27A==5~3,
                           Q27A==3~4,
                           Q27A==4~5,
                           TRUE~NA_real_),
  exec_over_leg2=case_when(Q27B==1~1,
                           Q27B==2~2,
                           Q27B==5~3,
                           Q27B==3~4,
                           Q27B==4~5,
                           TRUE~NA_real_),
  opposition_cooperate=case_when(Q27C==1~1,
                                 Q27C==2~2,
                                 Q27C==5~3,
                                 Q27C==3~4,
                                 Q27C==4~5,
                                 TRUE~NA_real_),
  pres_bound_by_law=case_when(Q28==1~1,
                              Q28==2~2,
                              Q28==5~3,
                              Q28==3~4,
                              Q28==4~5,
                              TRUE~NA_real_),
  no_term_limits=case_when(Q29A==1~1,
                           Q29A==2~2,
                           Q29A==5~3,
                           Q29A==3~4,
                           Q29A==4~5,
                           TRUE~NA_real_),
  military_rule_leg=case_when(Q29B==1~1,
                              Q29B==2~2,
                              Q29B==5~3,
                              Q29B==3~4,
                              Q29B==4~5,
                              TRUE~NA_real_),
  tun_democracy=ifelse(Q30<8,Q30,NA_real_),
  dem_sat=ifelse(Q31<9,Q31,NA_real_),
  more_dem_now=ifelse(Q32A<9,Q32A,NA_real_),
  more_dem_future=ifelse(Q32B<9,Q32B,NA_real_),
  mps_rep=ifelse(Q12A<9,Q12A,NA_real_),
  trust_saied=case_when(
    Q37A<2~0,
    Q37A>=2 & Q37A<9~1,
    TRUE~NA_real_),
  approve_saied=case_when(
    Q47A<2~0,
    Q47A>=2 & Q47A<9~1,
    TRUE~NA_real_),
  tun_dem_b=ifelse(tun_democracy>2,1,0),
  parties_bad=ifelse(parties_division>2,1,0)
  )

# ---------------------------------------------------------
# 3.1 Factor Analysis
# ---------------------------------------------------------

vars <- c(
  "freedom_org", "censor_media", "media_harms", "gov_acc",
  "gov_employee", "mps_free", "single_party", "military_rule",
  "abolish_parliament", "prefer_democracy", "no_elections",
  "parties_division", "dominant_party", "exec_over_leg1",
  "exec_over_leg2", "opposition_cooperate", "pres_bound_by_law",
  "no_term_limits"
)

prep_data <- function(data, vars) {
  data %>%
    select(all_of(vars)) %>%
    scale() %>%
    na.omit()
}

run_fa <- function(temp, n_factors = 3, cut = 0.2) {
  
  cor_matrix <- cor(temp)
  
  # Eigenvalues
  ev <- eigen(cor_matrix)$values
  print(ev)
  plot(ev, type = "b")
  
  # Factor analysis
  fa <- factanal(
    covmat = cor_matrix,
    factors = n_factors,
    rotation = "promax"
  )
  
  print(fa, digits = 4, cut = cut)
  
  return(fa)
}

select_vars <- function(fa, threshold = 0.3) {
  
  loadings <- as.matrix(fa$loadings)
  
  selected <- rownames(loadings)[
    apply(abs(loadings) >= threshold, 1, any)
  ]
  
  return(selected)
}

temp <- prep_data(ab9, vars)
fa1 <- run_fa(temp, cut = 0.2)

vars_selected <- select_vars(fa1, threshold = 0.3)

temp2 <- prep_data(ab9, vars_selected)
fa_final <- run_fa(temp2, cut = 0.2)

# ---------------------------------------------------------
# 3.2 Scales
# ---------------------------------------------------------

ab9 <- ab9 %>%
  mutate(
    ar1 = scale(no_term_limits),
    ar2 = scale(exec_over_leg1),
    ar3 = scale(exec_over_leg2),
    aut1 = scale(military_rule),
    aut2 = scale(abolish_parliament),
    aut3 = scale(single_party),
    rights1 = scale(freedom_org),
    rights2 = scale(gov_acc),
    rights3 = scale(gov_employee))
ab9$ar <- rowMeans(ab9[, c('ar1', 'ar2', 'ar3')], na.rm = TRUE)
ab9$aut <- rowMeans(ab9[, c('aut1', 'aut2', "aut3")], na.rm = TRUE)
ab9$rights <- rowMeans(ab9[, c('rights1', 'rights2', 'rights3')], na.rm = TRUE)

# ---------------------------------------------------------
# 3.3 Models
# ---------------------------------------------------------

m1 <- glm(saied~
            rights +
            aut +
            ar +
            tun_democracy +
            edu +
            age +
            female +
            rural,
          data=ab9,
          family="binomial")
summary(m1)
c1 <- tidy(m1)
c1$term <- c("(Intercept)",
             "Liberal",
             "Authoritarian",
             "Hyper-presidentialist",
             "Tunisia is democratic",
             "Education",
             "Age",
             "Female",
             "Rural")
c1$model <- "Vote intention"
c1$order <- as.numeric(row.names(c1))
c1 <- c1 %>%
  mutate(lower90=estimate-(std.error*1.645),
         upper90=estimate+(std.error*1.645),
         lower95=estimate-(std.error*1.96),
         upper95=estimate+(std.error*1.96),
         lower99=estimate-(std.error*2.576),
         upper99=estimate+(std.error*2.576)
  )

m2 <- glm(trust_saied~
            rights +
            aut +
            ar +
            tun_democracy +
            edu +
            age +
            female +
            rural,
          data=ab9,
          family="binomial")
summary(m2)
c2 <- tidy(m2)
c2$term <- c("(Intercept)",
             "Liberal",
             "Authoritarian",
             "Hyper-presidentialist",
             "Tunisia is democratic",
             "Education",
             "Age",
             "Female",
             "Rural")
c2$model <- "Trust"
c2$order <- as.numeric(row.names(c2))
c2 <- c2 %>%
  mutate(lower90=estimate-(std.error*1.645),
         upper90=estimate+(std.error*1.645),
         lower95=estimate-(std.error*1.96),
         upper95=estimate+(std.error*1.96),
         lower99=estimate-(std.error*2.576),
         upper99=estimate+(std.error*2.576)
  )

m3 <- glm(approve_saied~
            rights +
            aut +
            ar +
            tun_democracy +
            edu +
            age +
            female +
            rural,
          data=ab9,
          family="binomial")
summary(m3)
c3 <- tidy(m3)
c3$term <- c("(Intercept)",
             "Liberal",
             "Authoritarian",
             "Hyper-presidentialist",
             "Tunisia is democratic",
             "Education",
             "Age",
             "Female",
             "Rural")
c3$model <- "Approval"
c3$order <- as.numeric(row.names(c3))
c3 <- c3 %>%
  mutate(lower90=estimate-(std.error*1.645),
         upper90=estimate+(std.error*1.645),
         lower95=estimate-(std.error*1.96),
         upper95=estimate+(std.error*1.96),
         lower99=estimate-(std.error*2.576),
         upper99=estimate+(std.error*2.576)
  )

coeffs <- rbind(c1, c2, c3) %>%
  filter(term!="(Intercept)")

# ---------------------------------------------------------
# 3.4 Figure 4
# ---------------------------------------------------------

coef_plot <- ggplot(coeffs,
                    aes(x = estimate,
                        y = reorder(term, -order),
                        shape = model)) +   # replace color with shape
  
  geom_vline(xintercept = 0, linetype = "dotted") +
  
  geom_point(position = position_dodge(width = 0.6),
             size = 2,
             color = "black") +
  
  # 90% CI (thick)
  geom_errorbar(aes(xmin = lower90, xmax = upper90),
                position = position_dodge(width = 0.6),
                width = 0,
                linewidth = 1,
                color = "black") +
  
  # 95% CI (medium)
  geom_errorbar(aes(xmin = lower95, xmax = upper95),
                position = position_dodge(width = 0.6),
                width = 0,
                linewidth = 0.6,
                color = "black") +
  
  # 99% CI (thin)
  geom_errorbar(aes(xmin = lower99, xmax = upper99),
                position = position_dodge(width = 0.6),
                width = 0,
                linewidth = 0.3,
                color = "black") +
  
  scale_shape_manual(values = c(16, 17, 15, 18)) +  # distinct shapes
  
  theme_classic() +
  ylab(NULL) +
  xlab("Estimate") +
  
  theme(
    text = element_text(family = "Times New Roman", color = "black"),
    legend.title = element_blank()
  )

coef_plot
ggsave("figure4.jpg", 
       dpi=600, 
       width=6,
       height=5)

# ---------------------------------------------------------
# 3.5 Table A5
# ---------------------------------------------------------

stargazer(m1,m2,m3,
          type = "latex",
          out = "a5.tex")

# ---------------------------------------------------------
# 3.5 Party Models
# ---------------------------------------------------------

m4 <- glm(parties_bad~
            rights +
            aut +
            ar +
            tun_democracy +
            edu +
            age +
            female +
            rural,
          data=ab9,
          family="binomial")
summary(m4)

c4 <- tidy(m4)
c4$term <- c("(Intercept)",
             "Liberal",
             "Authoritarian",
             "Hyper-presidentialist",
             "Tunisia is democratic",
             "Education",
             "Age",
             "Female",
             "Rural")
c4$model <- "Parties are divisive"
c4$order <- as.numeric(row.names(c4))
c4 <- c4 %>%
  mutate(lower90=estimate-(std.error*1.645),
         upper90=estimate+(std.error*1.645),
         lower95=estimate-(std.error*1.96),
         upper95=estimate+(std.error*1.96),
         lower99=estimate-(std.error*2.576),
         upper99=estimate+(std.error*2.576)
  )

m5 <- glm(party_id~
            rights +
            aut +
            ar +
            tun_democracy +
            edu +
            age +
            female +
            rural,
          data=ab9,
          family="binomial")
summary(m5)

c5 <- tidy(m5)
c5$term <- c("(Intercept)",
             "Liberal",
             "Authoritarian",
             "Hyper-presidentialist",
             "Tunisia is democratic",
             "Education",
             "Age",
             "Female",
             "Rural")
c5$model <- "Identifies with party"
c5$order <- as.numeric(row.names(c5))
c5 <- c5 %>%
  mutate(lower90=estimate-(std.error*1.645),
         upper90=estimate+(std.error*1.645),
         lower95=estimate-(std.error*1.96),
         upper95=estimate+(std.error*1.96),
         lower99=estimate-(std.error*2.576),
         upper99=estimate+(std.error*2.576)
  )

coeffs <- rbind(c4, c5) %>%
  filter(term!="(Intercept)")

# ---------------------------------------------------------
# 3.6 Figure 5
# ---------------------------------------------------------

coef_plot <- ggplot(coeffs,
                    aes(x = estimate,
                        y = reorder(term, -order),
                        shape = model)) +   # replace color → shape
  
  geom_vline(xintercept = 0, linetype = "dotted") +
  
  geom_point(position = position_dodge(width = 0.7),
             size = 2,
             color = "black") +
  
  geom_errorbar(aes(xmin = lower90, xmax = upper90),
                position = position_dodge(width = 0.7),
                width = 0,
                linewidth = 1,
                color = "black") +
  
  geom_errorbar(aes(xmin = lower95, xmax = upper95),
                position = position_dodge(width = 0.7),
                width = 0,
                linewidth = 0.5,
                color = "black") +
  
  geom_errorbar(aes(xmin = lower99, xmax = upper99),
                position = position_dodge(width = 0.7),
                width = 0,
                linewidth = 0.2,
                color = "black") +
  
  scale_shape_manual(values = c(16, 17, 15, 18)) +  # distinct shapes
  
  theme_classic() +
  ylab(NULL) +
  xlab("Estimate") +
  
  guides(shape = guide_legend(title = NULL, reverse = TRUE)) +
  
  theme(
    text = element_text(family = "Times New Roman", color = "black")
  )

coef_plot
ggsave("figure5.jpg", 
       dpi=600, 
       width=6,
       height=5)

# ---------------------------------------------------------
# 3.7 Table A6
# ---------------------------------------------------------

stargazer(m4,m5,
          type = "latex",
          out = "a6.tex")
