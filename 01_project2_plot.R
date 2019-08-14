
###plot2 



###histogram
g <- ggplot(dat, aes(x = fake_cis))
g <- g + geom_histogram(binwidth = 1, colour="black", fill="skyblue")
g <- g + ggtitle("Histogram of CIS") +  labs(x="CIS")
plot(g)


g <- ggplot(dat, aes(x = fake_age))
g <- g + geom_histogram(binwidth = 1.5, colour="black", fill="skyblue")
g <- g + ggtitle("Histogram of Age") +  labs(x="Age")
plot(g)

g <- ggplot(dat, aes(x = fake_los))
g <- g + geom_histogram(binwidth = 3, colour="black", fill="skyblue")
g <- g + ggtitle("Histogram of length of stay") +  labs(x="Length of stay")
plot(g)

###admission type

#bargraph of admission type
g <- ggplot(dat, aes(x = factor(fake_admission)))
g <- g + geom_bar()
g <- g + ggtitle("The bar graph of Admission Type") +  labs(x="Admission Type")
plot(g)

g <- ggplot(dat_group, aes(x = fake_type))
g <- g + geom_bar()
g <- g + ggtitle("The bar graph of Admission Type") +  labs(x="Admission Type")
plot(g)


#1
g <- ggplot(dat_group, aes(x = fake_admission2, fill=factor(fake_wait)))
g <- g + geom_bar(position = "fill") + scale_y_continuous(labels = percent)
g <- g + ggtitle("The waiting list type ratio by Admission Type") + labs(x="Admission Type", y="Percentage")
g <- g + scale_fill_nejm(name="Waiting List Type", labels=c("True waiting list", "Planned repeat waiting list", "Not on waiting list", "Not known")) 
plot(g)



###length of stay 
g <- ggplot(dat_group, aes(x = fake_stay, fill=fake_ipdc))
g <- g + geom_bar(position = "fill") + scale_y_continuous(labels = percent)
g <- g + ggtitle("The day case and inpatient ratio by length of stay") + labs(x="Length of stay", y="Percentage")
g <- g + scale_fill_nejm(name="Day Case / Inpatient", labels=c("Day case", "Inpatient")) 
plot(g)

g <- ggplot(dat_group, aes(x = fake_stay, fill=fake_period))
g <- g + geom_bar(position = "fill") + scale_y_continuous(labels = percent)
g <- g + ggtitle("The age ratio by length of stay") + labs(x="Length of stay", y="Percentage")
g <- g + scale_fill_discrete(name="Age") 
plot(g)

##test
g <- ggplot(dat_group, aes(x = fake_stay, fill=factor(fake_spec)))
g <- g + geom_bar(position = "fill") + scale_y_continuous(labels = percent)
g <- g + ggtitle("The day case ratio by Admission Type") + labs(x="Admission Type", y="Percentage")
g <- g + scale_fill_discrete(name="Age") 
plot(g)


###other
g <- ggplot(dat_group, aes(x = fake_period, fill=factor(fake_marital)))
g <- g + geom_bar(position = "fill") + scale_y_continuous(labels = percent)
g <- g + ggtitle("The marital ratio by age") + labs(x="Age", y="Percentage")
g <- g + scale_fill_nejm(name="Marital", labels=c("Never Married (Single)", "Married (includes seperated)", "Widowed", "Other", "Not Known" )) 
plot(g)

g <- ggplot(dat_group, aes(x = fake_ipdc, fill=factor(fake_wait)))
g <- g + geom_bar(position = "fill") + scale_y_continuous(labels = percent)
g <- g + ggtitle("The day case ratio by Admission Type") + labs(x="Day case / Inpatient", y="Percentage")
g <- g + scale_fill_nejm(name="Waiting List", labels=c("True waiting list", "Planned repeat waiting list", "Not on waiting list", "Not known")) 
plot(g)





####

g <- ggplot(dat_group, aes(x = fake_age, fill = factor(fake_spec)))
g <- g + geom_histogram(position = "identity", alpha = 0.8, binwidth = 1.5)
g <- g + scale_fill_npg() + scale_color_npg()
plot(g)






g <- ggplot(dat_group, aes(x = fake_age, y=fake_cis))
g <- g + geom_point(aes(colour=fake_admission2))
g <- g + scale_color_brewer(name="Admission Type", palette = "Set1")
g <- g +  labs(x="age",y="CIS")
plot(g)


g <- ggplot(dat_group, aes(x = fake_age, y=fake_cis))
g <- g + geom_point(aes(colour=fake_los))
g <- g + labs(x="age",y="CIS", colour="Length of stay")
plot(g)





dat_corr <- dat_group[,c(4:6,8,9,11,12,21,22,23 ,19
)]
dat_corr <- dat_group[,c(4:6,8:12,14,19,21,22,23 
)]
dat_corr$fake_ipdc <- as.factor(dat_corr$fake_ipdc)
dat_corr$fake_spec <- as.factor(dat_corr$fake_spec)
dat_corr$fake_sigfac <- as.factor(dat_corr$fake_sigfac)


dat_corr[] <- lapply(dat_corr, as.integer)

dcor <- cor(dat_corr)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor(dat_corr), method="color",  
         order="AOE", addgrid.col = "darkgray",
         addCoef.col = "black", col=col(200),
         tl.col="black", tl.srt=45 #Text label color and rotation
)
