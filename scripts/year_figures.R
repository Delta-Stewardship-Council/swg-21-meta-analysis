# year figures
summary_pub_year <- read_csv("data_clean/summary_pub_year.csv")
year_dat <- read_csv("data_clean/year_dat.csv")
content_coding_results_decomposed <- read_csv("data_clean/content_coding_results_decomposed.csv")

# publication year
tiff("year_published.tiff", units = "in", width = 8, height = 11, res = 300)

hist(summary_pub_year$pub_year, main = "", xlab = "Year Published", ylab = "Number of Articles", breaks = 25)

dev.off()

num_by_yr <- summary_pub_year %>%
  group_by(pub_year) %>%
  summarise_each(funs(n_distinct))

mean(num_by_yr$...1)
sd(num_by_yr$...1)
sum(num_by_yr$...1)

recent <- subset(num_by_yr, pub_year >= 2012)
sum(recent$...1)

# publication year, study year, connectivity type
new_dat <- merge(content_coding_results_decomposed[,c(2:3)], year_df_pub_corrected, by = "ID", all.y = TRUE)

pch <- c(0,1,2,6)
connectivity_type <- c("1", "2", "3", "0")

pch_ct <- data.frame(pch, connectivity_type)
new_dat_2 <- merge(new_dat, pch_ct, by = "connectivity_type")

tiff("year_published_vs_year_studied.tiff", units = "in", width = 11, height = 8, res = 300)

plot(jitter(new_dat_2$pub_year, amount = 2), jitter(new_dat_2$year, amount = 2), pch = new_dat_2$pch, cex = 1.5, xlab = "Publication Year", ylab = "Study Year")
legend("topleft", c("longitudinal", "lateral", "hyporheic", "none"),
       pch = c(0,1,2,6))

dev.off()

length(unique(new_dat_2$ID))

year_by_year <- new_dat[,c(1,4)] %>%
  group_by(ID) %>%
  summarise_each(funs(n_distinct))

mean(year_by_year$year)
sd(year_by_year$year)
