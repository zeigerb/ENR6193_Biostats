################################################################################
# GLMM Mini-Workshop (30 min): Tick Movement Assay — ANSWER KEY (R script)
# Troy Koser
#
# This script mirrors the student “YOUR CODE HERE” prompts from the Rmd and
# provides working solutions + brief instructor notes.
#
# NOTE:
# - Your Rmd mixes GLM output (m_glm) with later GLMM objects (m_glmm_ri/m_glmm_rs)
# - In the later sections, some chunks reference `m_glmm` which isn’t defined.
#   Here, I assume:
#     - m_glm      = the simple logistic regression (ignores grouping)
#     - m_glmm_ri  = GLMM random intercepts model
#     - m_glmm_rs  = GLMM random slopes model
################################################################################

## 0) Packages ---------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(lme4)
  library(glmmTMB)
  library(broom.mixed)
  library(DHARMa)
})

set.seed(123)

## 1) Simulate the dataset (same as Rmd) -------------------------------------

# Five physical arenas
arenas <- c("Arena1", "Arena2", "Arena3", "Arena4", "Arena5")

trial_plan <- tibble(
  treatment = c("Control", "RepA", "RepB", "RepC", "RepD"),
  n_trials  = c(3, 6, 6, 6, 6)
)

trials <- trial_plan %>%
  uncount(n_trials, .id = "trial_num") %>%
  mutate(
    trial_id = paste0(treatment, "_", trial_num),
    arena    = sample(arenas, n(), replace = TRUE),
    temp_c   = runif(n(), 18, 28)
  )

# Arena-specific bias toward the repellent zone
arena_effect <- c(
  Arena1 =  0.0,
  Arena2 =  0.9,
  Arena3 = -0.3,
  Arena4 =  0.6,
  Arena5 = -0.2
)

beta_temp <- 0.12

# Treatment effects (relative to control) on the logit scale
beta_trt <- c(
  Control =  0.0,
  RepA    = -0.7,
  RepB    = -1.2,
  RepC    = -0.4,
  RepD    = -0.9
)

intercept <- qlogis(0.55)

tick_dat <- trials %>%
  slice(rep(1:n(), each = 15)) %>%
  group_by(trial_id) %>%
  mutate(tick_id = row_number()) %>%
  ungroup() %>%
  mutate(
    eta = intercept +
      beta_trt[treatment] +
      beta_temp * (temp_c - 23) +
      arena_effect[arena],
    p_toward = plogis(eta),
    toward   = rbinom(n(), size = 1, prob = p_toward)
  )

glimpse(tick_dat)


################################################################################
# STUDENT PROMPT #1:
# "Write code to answer:
#  1) How many trials are there?
#  2) How many ticks total?
#  3) Confirm that each trial has 15 ticks (show a small table)."
################################################################################

# 1) How many trials?
n_trials <- tick_dat %>% distinct(trial_id) %>% nrow() #could use unique(), countif, lots of options
n_trials

# 2) How many ticks total?
n_ticks <- nrow(tick_dat)
n_ticks

# 3) Confirm each trial has 15 ticks
ticks_per_trial <- tick_dat %>%
  count(trial_id, name = "n_ticks") %>%
  arrange(desc(n_ticks))

head(ticks_per_trial, 10)
ticks_per_trial %>% count(n_ticks)
# Expect all n_ticks == 15


## 2) Trial-level summary (same as Rmd) --------------------------------------

trial_sum <- tick_dat %>%
  group_by(trial_id, treatment, arena, temp_c) %>%
  summarise(
    n_ticks = n(),
    n_toward = sum(toward),
    prop_toward = mean(toward),
    .groups = "drop"
  )

head(trial_sum)


################################################################################
# STUDENT PROMPT #2 (Section 4):
# "Write code to confirm that:
#  1) toward is binary (only 0 and 1)
#  2) prop_toward is in [0,1]
#  3) n_toward is integer count from 0 to 15"
################################################################################

# 1) toward is binary
sort(unique(tick_dat$toward))
# Should be 0, 1

# 2) prop_toward bounded [0,1]
range(trial_sum$prop_toward)

# 3) n_toward integer count 0..15
range(trial_sum$n_toward)
all(trial_sum$n_toward == as.integer(trial_sum$n_toward))
# Also check max equals 15
max(trial_sum$n_toward)


################################################################################
# STUDENT PROMPT #3 (Section 6: factors + centering):
# "Write code to:
#  1) Check what form the treatment and arena variables are currently set as
#  2) Convert treatment to factor with Control reference
#  3) Convert arena to factor
#  4) Print the levels of both
#  Extra challenge: try to do this with piping!"
################################################################################

# 1) Check current classes
tick_dat %>% summarise(
  class_treatment = class(treatment)[1],
  class_arena     = class(arena)[1]
)

# 2-4) Convert to factors + print levels (piping version)
tick_mod_dat <- tick_dat %>%
  mutate(
    treatment = factor(treatment, levels = c("Control","RepA","RepB","RepC","RepD")),
    arena     = factor(arena),
    temp_c_center = temp_c - mean(temp_c)
  )

levels(tick_mod_dat$treatment)
levels(tick_mod_dat$arena)


################################################################################
# STUDENT PROMPT #4 (Centering vs scaling):
# "Write code to:
#  1) Create temp_c_center
#  2) Create temp_c_scaled
#  3) Print mean and sd of both
#  4) Make a quick histogram of each"
################################################################################

# 1-2) Add both centered and scaled temperature
tick_mod_dat <- tick_mod_dat %>%
  mutate(
    temp_c_scaled = as.numeric(scale(temp_c))  # z-score
  )

# 3) Means and SDs
tick_mod_dat %>%
  summarise(
    mean_center = mean(temp_c_center),
    sd_center   = sd(temp_c_center),
    mean_scaled = mean(temp_c_scaled),
    sd_scaled   = sd(temp_c_scaled)
  )
# Expect mean_center ~ 0; mean_scaled ~ 0; sd_scaled ~ 1

# 4) Histograms (base R)
hist(tick_mod_dat$temp_c_center, breaks = 20, main = "Centered temperature", xlab = "temp_c_center")
hist(tick_mod_dat$temp_c_scaled, breaks = 20, main = "Scaled temperature", xlab = "temp_c_scaled")


################################################################################
# Fit GLM (same structure as Rmd) ---------------------------------------------
################################################################################

m_glm <- glm(
  toward ~ treatment + temp_c_center,
  family = binomial(link = "logit"),
  data = tick_mod_dat
)

summary(m_glm)


################################################################################
# STUDENT PROMPT #5 (Independence is a lie):
# "Write code to show:
#  1) How many ticks per trial
#  2) How many trials per arena
#  3) A quick table of counts by arena × treatment"
################################################################################

# 1) ticks per trial
tick_mod_dat %>%
  count(trial_id, name = "ticks_per_trial") %>%
  count(ticks_per_trial)

# 2) trials per arena
tick_mod_dat %>%
  distinct(trial_id, arena, treatment) %>%
  count(arena, name = "trials_per_arena") %>%
  arrange(desc(trials_per_arena))

# 3) arena × treatment table (trials)
tick_mod_dat %>%
  distinct(trial_id, arena, treatment) %>%
  count(arena, treatment) %>%
  pivot_wider(names_from = treatment, values_from = n, values_fill = 0)


################################################################################
# Fit GLMMs (random intercepts + random slopes) -------------------------------
################################################################################

# Random intercepts model: arena + trial
m_glmm_ri <- glmer(
  toward ~ treatment + temp_c_center +
    (1 | arena) + (1 | trial_id),
  family = binomial(link = "logit"),
  data = tick_mod_dat
)

summary(m_glmm_ri)

# Random slopes by arena (temp effect varies by arena)
m_glmm_rs <- glmer(
  toward ~ treatment + temp_c_center +
    (1 + temp_c_center | arena) + (1 | trial_id),
  family = binomial(link = "logit"),
  data = tick_mod_dat
)

summary(m_glmm_rs)

# Comparison checks (as in Rmd)
isSingular(m_glmm_ri)
isSingular(m_glmm_rs)

VarCorr(m_glmm_ri)
VarCorr(m_glmm_rs)

anova(m_glmm_ri, m_glmm_rs, test = "Chisq")
AIC(m_glmm_ri, m_glmm_rs)


################################################################################
# STUDENT PROMPT #6 (Convergence/singularity/VarCorr/ranef):
# "Write code to:
#  1) Check singularity for both models
#  2) Extract VarCorr() and identify which variance component is ~0
#  3) Print random effects for arena and interpret which arena is 'highest'"
################################################################################

# 1) singularity
isSingular(m_glmm_ri)
isSingular(m_glmm_rs)

# 2) variance components
vc_ri <- VarCorr(m_glmm_ri)
vc_rs <- VarCorr(m_glmm_rs)

vc_ri
vc_rs

# Identify near-zero variance components (rough guide)
# For random intercept-only model:
as.data.frame(vc_ri) %>% arrange(vcov)

# For random slopes model:
as.data.frame(vc_rs) %>% arrange(vcov)

# 3) random effects for arena (random intercept deviations)
re_arena_ri <- ranef(m_glmm_ri)$arena
re_arena_ri

# Which arena has the highest random intercept (largest positive deviation)?
re_arena_ri %>%
  as.data.frame() %>%
  rownames_to_column("arena") %>%
  arrange(desc(`(Intercept)`)) %>%
  slice(1:5)

# Optional: interpret
# Arena with highest random intercept = highest baseline tendency to move toward repellent (all else equal)


################################################################################
# Residual diagnostics (DHARMa) -----------------------------------------------
################################################################################

# In your Rmd, some chunks refer to `m_glmm` (undefined). We'll use m_glmm_ri here:
res <- simulateResiduals(m_glmm_ri)
plot(res)
testDispersion(res)
testZeroInflation(res)


################################################################################
# Odds ratios → words (fixef table) -------------------------------------------
################################################################################

# In your Rmd, fixef_tab is computed from `m_glmm` but you likely mean a model.
# For teaching interpretation you can do this for the GLM OR the GLMM.
# Here I'll do it for the GLM to match your provided summary output.

fixef_tab_glm <- broom::tidy(m_glm, conf.int = TRUE) %>%
  mutate(
    odds_ratio = exp(estimate),
    lo = exp(conf.low),
    hi = exp(conf.high)
  )

fixef_tab_glm


################################################################################
# STUDENT PROMPT #7:
# "Pull OR for RepB, convert to % change in odds, write one sentence comment"
################################################################################

# 1) Pull OR for RepB
or_repB <- fixef_tab_glm %>%
  filter(term == "treatmentRepB") %>%
  pull(odds_ratio)

or_repB

# 2) Percent change in odds
pct_change_odds_repB <- (or_repB - 1) * 100
pct_change_odds_repB

# 3) One sentence interpretation (comment)
# RepB: holding temperature constant, the odds of moving toward are reduced by about
# (1 - OR)*100 percent vs control.  (Here: ~83% lower odds)


################################################################################
# Predicted probabilities (as in Rmd, but GLM doesn't use re.form) -------------
################################################################################

newdat <- expand_grid(
  treatment = levels(tick_mod_dat$treatment),
  temp_c_center = 0
) %>%
  mutate(
    logit = predict(m_glm, newdata = ., type = "link"),
    prob  = plogis(logit)
  )

newdat

ggplot(newdat, aes(treatment, prob)) +
  geom_point(size = 3) +
  ylim(0, 1) +
  theme_bw()


################################################################################
# STUDENT PROMPT #8:
# "Identify strongest repellent, compute absolute and relative diff vs control"
################################################################################

p_control <- newdat %>% filter(treatment == "Control") %>% pull(prob)

strongest <- newdat %>%
  filter(treatment != "Control") %>%
  arrange(prob) %>%
  slice(1)

strongest

abs_diff <- p_control - strongest$prob
rel_reduction_pct <- (abs_diff / p_control) * 100

abs_diff
rel_reduction_pct

# Interpretation (comment):
# Strongest repellent = lowest predicted probability of moving toward (at mean temp).
# Absolute reduction vs control = abs_diff; relative reduction = rel_reduction_pct %.


################################################################################
# Optional: glmmTMB equivalents for your "glmmTMB gurly" arc ------------------
################################################################################

# Random intercepts in glmmTMB (same formula syntax for random effects)
m_tmb_ri <- glmmTMB(
  toward ~ treatment + temp_c_center + (1 | arena) + (1 | trial_id),
  family = binomial(link = "logit"),
  data = tick_mod_dat
)

summary(m_tmb_ri)

# Random slopes by arena in glmmTMB
m_tmb_rs <- glmmTMB(
  toward ~ treatment + temp_c_center + (1 + temp_c_center | arena) + (1 | trial_id),
  family = binomial(link = "logit"),
  data = tick_mod_dat
)

summary(m_tmb_rs)

# Convergence-ish checks in glmmTMB:

AIC(m_tmb_ri, m_tmb_rs)

