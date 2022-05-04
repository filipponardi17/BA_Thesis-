setwd("DIRECTORY")

W7 <- readRDS("EVS_WVS_Joint_v2_0.rds")
lst = read.table("country_code.txt")

W7$cntry_name = NA
n = dim(lst)[1]
for (i in 1:n){
  W7$cntry_name[W7$cntry == lst[i,3]] = lst[i,1]
}

W7[W7 < 0] = NA
table(W7$cntry_name)


# Costruisco un survey più piccolo

cntry_name = W7$cntry_name
W7R <- data.frame(cntry_name)

# 1.Select Variables of Interest

# E181_EVS5 What party would you vote for?
library(labelled)
W7R$E181_EVS5 = W7$E181_EVS5
class(W7R$E181_EVS5)

# A001 - Important in life: Family
# A002 - Important in life: Friends
# A003 - Important in life: Leisure time
# A004 - Important in life: Politics
# A005 - Important in life: Work
# A006 - Important in life: Religion

W7R$imp_fml = as.numeric(W7$A001)
W7R$imp_frnd = as.numeric(W7$A002)
W7R$imp_leis = as.numeric(W7$A003)
W7R$imp_polt = as.numeric(W7$A004)
W7R$imp_wrk = as.numeric(W7$A005)
W7R$imp_rlg = as.numeric(W7$A006)



# A008 - Feeling of happiness
# A170 - Satisfaction with your life
# A173 - How much freedom of choice and control
# E012 - Willingness to fight for country


W7R$hap_feel = as.numeric(W7$A008)
W7R$life_sat = as.numeric(W7$A170)
W7R$frdm_cntl = as.numeric(W7$A173)
W7R$fgt_cntr = as.numeric(W7$E012)

# E023 - Interest in politics
# E035 - Income equality
# E036 - Private vs state ownership of business
# E037 - Government responsibility
# E039 - Competition good or harmful

W7R$plc_int = as.numeric(W7$E023)
W7R$plc_eql = as.numeric(W7$E035)
W7R$plc_prpt = as.numeric(W7$E036)
W7R$plc_rspn = as.numeric(W7$E037)
W7R$plc_cmpt = as.numeric(W7$E039)

# E224 - Democracy: Governments tax the rich and subsidize the poor.
# E225 - Democracy: Religious authorities interpret the laws
# E226 - Democracy: People choose their leaders in free elections
# E227 - Democracy: People receive state aid for unemployment.
# E228 - Democracy: The army takes over when government is incompetent.
# E229 - Democracy: Civil rights protect people's liberty against oppression
# E233 - Democracy: Women have the same rights as men.
# E233A - Democracy: The state makes people's incomes equal
# E233B - Democracy: People obey their rulers
# E235 - Importance of democracy
# E236 - Democraticness in own country (1 = not democratic, 10 = fully democratic)

W7R$dmc_tax = as.numeric(W7$E224)
W7R$dmc_rlg = as.numeric(W7$E225)
W7R$dmc_ppl = as.numeric(W7$E226)
W7R$dmc_aid = as.numeric(W7$E227)
W7R$dmc_coup = as.numeric(W7$E228)
W7R$dmc_rght = as.numeric(W7$E229)
W7R$dmc_wmn = as.numeric(W7$E233)
W7R$dmc_eql = as.numeric(W7$E233A)
W7R$dmc_obey = as.numeric(W7$E233B)
W7R$dmc_imp = as.numeric(W7$E235)
W7R$dmc_cnt = as.numeric(W7$E236)

# E265_01 - How often in country's elections: Votes are counted fairly (1 = very often, 4 = not at all often)
# E265_02 - How often in country's elections: Opposition candidates are prevented from running
# E265_03 - How often in country's elections: TV news favors the governing party
# E265_04 - How often in country's elections: Voters are bribed
# E265_05 - How often in country's elections: Journalists provide fair coverage of elections
# E265_06 - How often in country's elections: Election officials are fair
# E265_07 - How often in country's elections: Rich people buy elections
# E265_08 - How often in country's elections: Voters are threatened with violence at the polls

W7R$el_fair = as.numeric(W7$E265_01)
W7R$el_opp = as.numeric(W7$E265_02)
W7R$el_tv = as.numeric(W7$E265_03)
W7R$el_brb = as.numeric(W7$E265_04)
W7R$el_jrn = as.numeric(W7$E265_05)
W7R$el_off = as.numeric(W7$E265_06)
W7R$el_rch = as.numeric(W7$E265_07)
W7R$el_thr = as.numeric(W7$E265_08)


# 1 E069_01 Confidence: Churches (1 = a great deal, 4 = none at all)
# 2 E069_02 Confidence: Armed Forces
# 3 E069_04 Confidence: The Press
# 4 E069_05 Confidence: Labour Unions
# 5 E069_06 Confidence: The Police
# 6 E069_07 Confidence: Parliament
# 7 E069_08 Confidence: The Civil Services
# * E069_18A Confidence: Major regional organization (combined from country-specific)
# 14 E069_18 Confidence: The European Union
# 8 E069_11 Confidence: The Government
# 9 E069_12 Confidence: The Political Parties
# 10 E069_13 Confidence: Major Companies
# 11 E069_14 Confidence: The Environmental Protection Movement
# 12 E069_17 Confidence: Justice System/Courts
# 13 E069_20 Confidence: The United Nations

W7R$c_chr = as.numeric(W7$E069_01)
W7R$c_arm = as.numeric(W7$E069_02)
W7R$c_prs = as.numeric(W7$E069_04)
W7R$c_uns = as.numeric(W7$E069_05)
W7R$c_plc = as.numeric(W7$E069_06)
W7R$c_prl = as.numeric(W7$E069_07)
W7R$c_cvl = as.numeric(W7$E069_08)
#W7R$c_eu2 = as.numeric(W7$E069_18A
W7R$c_eu1 = as.numeric(W7$E069_18)
W7R$c_gvr = as.numeric(W7$E069_11)
W7R$c_prt = as.numeric(W7$E069_12)
W7R$c_cmp = as.numeric(W7$E069_13)
W7R$c_env = as.numeric(W7$E069_14)
W7R$c_jst = as.numeric(W7$E069_17)
W7R$c_onu = as.numeric(W7$E069_20)

#eu0 = merge_two_column(c_eu1, c_eu2)
#c_eu0 = eu0$v


# E111_01 Satisfaction with the political system (1 = satisfied, 10 = unsatisfied)
# E114 - Political system: Having a strong leader (1 = Very Good, 4 = Very Bad)
# E115 - Political system: Having experts make decisions
# E116 - Political system: Having the army rule
# E117 - Political system: Having a democratic political system


summary(W7$E111_01)
W7R$ps_sat = as.numeric(W7$E111_01)
summary(W7R$ps_sat)

W7R$ps_lead = as.numeric(W7$E114)
W7R$ps_exp = as.numeric(W7$E115)
W7R$ps_arm = as.numeric(W7$E116)
W7R$ps_dmc = as.numeric(W7$E117)


# RESPONDENT CAN CHOOSE MAXIMUM 5
# A027 Important child qualities: good manners (1 = mentioned, 0 = not_mentioned)
# A029 Important child qualities: independence **
# A030 Important child qualities: hard work *
# A032 Important child qualities: feeling of responsibility
# A034 Important child qualities: imagination **
# A035 Important child qualities: tolerance and respect for other people 
# A038 Important child qualities: thrift, saving money and things
# A039 Important child qualities: determination perseverance
# A040 Important child qualities: religious faith *
# A041 Important child qualities: unselfishness
# A042 Important child qualities: obedience *

# * Libertarian Values, ** Authoritarian Values

W7R$cq_mnr = as.numeric(W7$A027)
W7R$cq_ind = as.numeric(W7$A029)
W7R$cq_hwr = as.numeric(W7$A030)
W7R$cq_rsp = as.numeric(W7$A032)
W7R$cq_img = as.numeric(W7$A034)
W7R$cq_tlr = as.numeric(W7$A035)
W7R$cq_thr = as.numeric(W7$A038)
W7R$cq_prs = as.numeric(W7$A039)
W7R$cq_rlg = as.numeric(W7$A040)
W7R$cq_uns = as.numeric(W7$A041)
W7R$cq_obd = as.numeric(W7$A042)


# A165 - Most people can be trusted
# D001_B - Trust: Your family (B)
# G007_18_B - Trust: Your neighborhood (B)
# G007_33_B - Trust: People you know personally (B)
# G007_34_B - Trust: People you meet for the first time (B)
# G007_35_B - Trust: People of another religion (B)
# G007_36_B - Trust: People of another nationality (B)

W7R$trs_most = as.numeric(W7$A165)
W7R$trs_faml = as.numeric(W7$D001_B)
W7R$trs_ngh = as.numeric(W7$G007_18_B)
W7R$trs_knw = as.numeric(W7$G007_33_B)
W7R$trs_met = as.numeric(W7$G007_34_B)
W7R$trs_rlg = as.numeric(W7$G007_35_B)
W7R$trs_ntn = as.numeric(W7$G007_36_B)


# D059 - Men make better political leaders than women do
# D060 - University is more important for a boy than for a girl
# D061 - Pre-school child suffers with working mother
# D078 - Men make better business executives than women do

W7R$men_lead = as.numeric(W7$D059)
W7R$men_uni = as.numeric(W7$D060)
W7R$men_chwm = as.numeric(W7$D061)
W7R$men_bsn = as.numeric(W7$D078)


# Y002 - Post-Materialist index 4-item

W7R$post_mat = as.numeric(W7$Y002)


# B008 - Protecting environment vs. Economic growth

W7R$env_vs_grw = as.numeric(W7$B008)
i1 = which(W7R$env_vs_grw == 3)
W7R$env_vs_grw[i1] = 1.5


# C001_01 - Jobs scarce: Men should have more right to a job than women (5-point scale)
# C002_01 - Jobs scarce: Employers should give priority to (nation) people than immigrants

W7R$job_men = as.numeric(W7$C001_01)
W7R$job_ntn = as.numeric(W7$C002_01)


# D081 - Homosexual couples are as good parents as other couples
# D026_03 - Duty towards society to have children
# D026_05 - It is childs duty to take care of ill parent

W7R$hom_prn = as.numeric(W7$D081)
W7R$dut_chl = as.numeric(W7$D026_03)
W7R$dut_ill = as.numeric(W7$D026_05)




# E015 - Future changes: Less importance placed on work (1 = Good, 3 = bad)
# E018 - Future changes: Greater respect for authority (1 = Good, 3 = Bad)

W7R$fc_work = as.numeric(W7$E015)
W7R$fc_auth = as.numeric(W7$E018)


#tapply(W7$E015, W7$cntry_name, FUN=mean, na.rm = T)
#tapply(W7$E018, W7$cntry_name, FUN=mean, na.rm = T)

# F114A - Justifiable: Claiming government benefits to which you are not entitled
# F115 - Justifiable: Avoiding a fare on public transport
# F116 - Justifiable: Cheating on taxes
# F117 - Justifiable: Someone accepting a bribe
# F118 - Justifiable: Homosexuality (approximation of survival/self-expression scale)
# F119 - Justifiable: Prostitution
# F120 - Justifiable: Abortion (approximation of religious/secular scale)
# F121 - Justifiable: Divorce
# F122 - Justifiable: Euthanasia
# F123 - Justifiable: Suicide
# F132 - Justifiable: Having casual sex
# E290 - Justifiable: Political violence
# F144_02 - Justifiable: Death penalty

W7R$js_bnft = as.numeric(W7$F114A)
W7R$js_fare = as.numeric(W7$F115)
W7R$js_tax = as.numeric(W7$F116)
W7R$js_brib = as.numeric(W7$F117)
W7R$js_hom = as.numeric(W7$F118)
W7R$js_prst = as.numeric(W7$F119)
W7R$js_abr = as.numeric(W7$F120)
W7R$js_div = as.numeric(W7$F121)
W7R$js_euth = as.numeric(W7$F122)
W7R$js_suic = as.numeric(W7$F123)
W7R$js_sex = as.numeric(W7$F132)
W7R$js_vlnc = as.numeric(W7$E290)
W7R$js_dthp = as.numeric(W7$F144_02)

# G052 - Evaluate the impact of immigrants on the development of [your country]
# G062 - How close you feel: Continent; e.g. Europe, Asia etc
# G063 - How close you feel: World
# G255 - How close you feel: Your village, town or city
# G256 - How close do you feel: to your county, region, district
# G257 - How close do you feel: to country

W7R$imp_imm = as.numeric(W7$G052)
W7R$cls_eu = as.numeric(W7$G062)
W7R$cls_wrl = as.numeric(W7$G063)
W7R$cls_vlg = as.numeric(W7$G255)
W7R$cls_rgn = as.numeric(W7$G256)
W7R$cls_cnt = as.numeric(W7$G257)


# H009 - Government has the right: Keep people under video surveillance in public areas
# H010 - Government has the right: Monitor all e-mails and any other information exchanged on the Internet
# H011 - Government has the right: Collect information about anyone living in [COUNTRY] without their knowledge

W7R$gvr_vds = as.numeric(W7$H009)
W7R$gvr_eml = as.numeric(W7$H010)
W7R$gvr_inf = as.numeric(W7$H011)


# E033 - Self positioning in political scale (1 = left, 10 = right)

W7R$pol_sp = W7$E033
W7R$pol_rad = (W7R$pol_sp - 5)^2

#pol_sp = tapply(W7$E033, W7$cntry_name, FUN=mean, na.rm = T)
#cor_test(pol_sp)

# X001 - Sex
# X003R - Age recoded (6 intervals)
# G027A - Respondent immigrant / born in country
# X011 - How many children do you have
# X02R - Highest educational level attained
# X028 - Employment status Respondent
# size_5c - Size of town where interview was conducted (5 categories)
# F034 - Religious person
# G006 - How proud of nationality

W7R$sex = as.numeric(W7$X001)
W7R$age = as.numeric(W7$X003R)
W7R$nativ = as.numeric(W7$G027A)
W7R$n_child = as.numeric(W7$X011)
W7R$edu_lvl = as.numeric(W7$X025R)
W7R$incm_lvl = as.numeric(W7$X047E_EVS5)
W7R$city_siz = as.numeric(W7$size_5c)
W7R$rel = as.numeric(W7$F034)
W7R$prd = as.numeric(W7$G006)

#W7R$emp_sts = W7$X028
#class(W7R$emp_sts)

#  E181_EVS5: Which political party appeals to you most
W7R$E181_EVS5 = W7$E181_EVS5

save(W7R, file = "DIRECTORY/WVSurvey_reduced_with_ps.rda")


