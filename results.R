source('make_wide.R')

# A total of 195 countries had TB mortality estimates for 2015 by both WHO and IHME. 
table(df$have_both)

# Among those countries, WHO estimated 1,768,482 total number of deaths attributable to tuberculosis, whereas IHME estimated 1,322,916 deaths, resulting in a difference of 445,567 deaths (24% reduced mortality if taking WHO as a reference, or 33.7% increased mortality if IHME is the reference). 

sum(df$i_both_all_tbtotal_nd)
sum(df$w_both_all_tbtotal_nd)

sum(df$i_both_all_tbtotal_nd) -
sum(df$w_both_all_tbtotal_nd)

(sum(df$i_both_all_tbtotal_nd) -
    sum(df$w_both_all_tbtotal_nd)) / 
  sum(df$i_both_all_tbtotal_nd)

(sum(df$i_both_all_tbtotal_nd) -
    sum(df$w_both_all_tbtotal_nd)) / 
  sum(df$w_both_all_tbtotal_nd)

#This difference in TB mortality was higher in people living with HIV (211,604 by IHME vs 389,042 by WHO), where WHO estimates 84% higher number of deaths attributable to TB than WHO. 
sum(df$i_both_all_htb_nd)
sum(df$w_both_all_htb_nd)

# The relative difference in number of deaths was especially higher for the paediatric population, where WHO estimates almost three times higher number of deaths than IHME (209,837 vs 69,659 number of deaths by WHO and IHME respectively). 

sum(df$i_both_014_tb_nd + df$i_both_014_htb_nd)
sum(df$w_both_014_tb_nd + df$w_both_014_htb_nd)


# Among adult TB deaths, there were not large differences in the sex-specific mortality estimates (table 1).

# There were 84 countries (43.1%), in which WHO estimated higher number of deaths attributable to TB than IHME. 
table(df$w_both_all_tbtotal_nd > df$i_both_all_tbtotal_nd)
prop.table(table(df$w_both_all_tbtotal_nd > df$i_both_all_tbtotal_nd))


# Those countries with larger absolute differences in total number of TB deaths were (by decreasing magnitude of the difference): Nigeria (216621), Bangladesh (49863), Tanzania (38272), South Africa (29108), Mozambique (28909),  Indonesia, (26121),  Democratic Republic of Congo, (26010),  India (20696), North Korea(13218),  and Angola(9910). 
df %>%
  mutate(x = w_both_all_tbtotal_nd - i_both_all_tbtotal_nd) %>%
  arrange(desc(x)) %>%
  mutate(x = round(x)) %>%
  dplyr::select(country, x) %>%
  head(10)


# The countries in which IHME estimated higher number of deaths than WHO were: Ethiopia (22650), China (13538), Zimbabwe(11082), Philippines (9436), Nepal (5477), Uganda (5081), Burkina Faso, (4837),  Niger(3758), Viet Nam (3252),  and Senegal (3147),  (figure 2). 
df %>%
  mutate(x = i_both_all_tbtotal_nd - w_both_all_tbtotal_nd) %>%
  arrange(desc(x)) %>%
  mutate(x = round(x)) %>%
  dplyr::select(country, x) %>%
  head(10)

# Map 1 shows how the largest differences in terms of absolute number of deaths are concentrated in few countries. In fact, the correlation of TB mortality estimates between both institutions is very good for most countries and regions (figure 2) Can we statistically appraise this?. 
cor(df$i_both_all_tbtotal_nd, df$w_both_all_tbtotal_nd)


t.test(df$i_both_all_tbtotal_nd,
       df$w_both_all_tbtotal_nd, 
       paired=TRUE)

#For the African region, for those countries with lower mortality burden, IHME estimates higher mortality and viceversa. Can we statistically appraise this?
x <- df %>% filter(who_region == 'AFR')
plot(x$w_both_all_tbtotal_nd,
     x$i_both_all_tbtotal_nd)

# After standardizing the absolute difference by the total number of reported deaths, thus taking into account the country-specific TB burden, the countries where WHO estimated higher mortality than IHME (by decreasing magnitude of the adjusted standardized difference) are: Libya (100), Nigeria (37.0), Iceland (19.6), Congo (18.9), Afghanistan (13.7), Timor Leste(9.9), Tanzania (9.0), Angola (8.2), Sudan (7.6),  and Mozambique (6.8) ranking changed (figure 2). 

df %>%
  mutate(x = adjusted_stand_dif) %>%
  arrange(desc(x)) %>%
  mutate(x = round(x, 1)) %>%
  dplyr::select(country, x) %>%
  head(10)

#Likewise, the most important differences in which IHME estimated higher mortality are Eritrea (-13.4), Burkina Faso(-7.5), Togo (-6.1), Benin (-5.9), Burundi (-5.6), Nepal, (-5.0),  Rwanda(-4.9), South Sudan(-4.8), Senegal (-4.5),  and Botswana(-4.5) (figure 3). 

df %>%
  mutate(x = adjusted_stand_dif) %>%
  arrange(x) %>%
  mutate(x = round(x, 1)) %>%
  dplyr::select(country, x) %>%
  head(10)

# Twenty-three countries did not have an adjusted standardized difference since reported number of deaths were not available. 
table(is.na(df$adjusted_stand_dif))

# Standardization of the absolute difference in TB number of deaths by incident number of TB cases (as a proxy of burden of tuberculosis) yields similar country rankings. In most countries of South East Asia, WHO estimates higher number of deaths than IHME, although the magnitude of this difference (as depicted by adjusted standardized difference) is not large.
x <- df %>% filter(who_region == 'SEA')
table(x$w_both_all_tbtotal_nd > x$i_both_all_tbtotal_nd)

# As for the absolute differences we can see that the most dramatic differences in TB mortality estimates are concentrated in few countries (Map 2). In the online supplementary material, Map 3 shows all indicators by country of this descriptive analysis.


# When looking at the association between the adjusted standardized difference between IHME and WHO mortality estimates and different potential drivers of this difference we see poor correlation with reported HIV prevalence among TB cases (r= 0.03, 95%CI) , 

#MDR/RR prevalence (r= -0.05, 95%CI)  

#and case fatality rate (r= -0.19, 95%CI). 

#There is a moderate correlation with case detection rate (as estimated by WHO), (r= -0.32, 95%CI), which disappears when using CDR based on IHME number of incident cases (r= 0.06, 95%CI: ) (figure 2). Those countries which have had a national prevalence survey have a higher adjusted standardized difference than those without a prevalence survey (3.5 vs 0.88 respectively). In other words, in those countries for which prevalence survey is available, WHO tents to estimate higher number of deaths attributable to TB, although this difference is not statistically significant (p=0.2).
