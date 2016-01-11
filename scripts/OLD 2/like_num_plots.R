	# Set up x values and at
x <- c(1, 2)

	# Set up response percentages for non switchers
nonsw_e_ml_num <- c(92.86, 90.41 )
nonsw_e_cs_num <- c(7.14, 9.59)
nonsw_s_ml_num <- c(89.45, 86.18)
nonsw_s_cs_num <- c(10.55, 10.52)

	# Set up response percentages for switchers
sw_e_ml_num <- c(78.13, 66.67)
sw_e_cs_num <- c(21.88, 33.33)
sw_s_ml_num <- c(65.63, 71.25)
sw_s_cs_num <- c(34.48, 28.75)

	# Set up response percentages for super switchers
supsw_e_ml_num <- c(67.22, 51.81)
supsw_e_cs_num <- c(32.78, 48.19)
supsw_s_ml_num <- c(52.92, 54.03)
supsw_s_cs_num <- c(47.08, 45.97)

	# Make plot for non switchers
pdf("nonsw_num.pdf")

par(xpd=T, mar=par()$mar+c(0,0,0,15))
	
plot(x, nonsw_e_ml_num, type="o", lwd= 4, main="Non-switchers Responses\n by Languge and Stimulus Context", xaxt="n", xlab="Stimulus context", ylab="Percentage of responses", ylim=c(0, 100),cex.main=1.7,cex.lab=1.5,cex.axis=1.5)
lines(x, nonsw_s_ml_num, type="o", lwd = 4, lty = 2)
lines(x, nonsw_e_cs_num, type="o", lwd = 4, lty = 3)
lines(x, nonsw_s_cs_num, type="o", lwd = 4, lty = 4)
axis(1, Task, labels=c("Monolingual", "Code-switch"), cex.lab=1.5,cex.axis=1.5)
legend(2, 75,legend=c("English stim. ML response", "Spanish stim. ML response", "English stim. CS response", "Spanish stim. CS response"), lwd = 4, lty=c(1, 2, 3, 4), cex=1.3, bty="n")

dev.off()

	# Make plot for switchers
pdf("sw_num.pdf")

par(xpd=T, mar=par()$mar+c(0,0,0,15))
	
plot(x, sw_e_ml_num, type="o", lwd= 4, main="Switchers Responses\n by Languge and Stimulus Context", xaxt="n", xlab="Stimulus context", ylab="Percentage of responses", ylim=c(0, 100),cex.main=1.7,cex.lab=1.5,cex.axis=1.5)
lines(x, sw_s_ml_num, type="o", lwd = 4, lty = 2)
lines(x, sw_e_cs_num, type="o", lwd = 4, lty = 3)
lines(x, sw_s_cs_num, type="o", lwd = 4, lty = 4)
axis(1, Task, labels=c("Monolingual", "Code-switch"), cex.lab=1.5,cex.axis=1.5)
legend(2, 75,legend=c("English stim. ML response", "Spanish stim. ML response", "English stim. CS response", "Spanish stim. CS response"), lwd = 4, lty=c(1, 2, 3, 4), cex=1.3, bty="n")

dev.off()

	# Make plot for super switchers
pdf("supsw_num.pdf")

par(xpd=T, mar=par()$mar+c(0,0,0,15))
	
plot(x, supsw_e_ml_num, type="o", lwd= 4, main="Super switchers Responses\n by Languge and Stimulus Context", xaxt="n", xlab="Stimulus context", ylab="Percentage of responses", ylim=c(0, 100),cex.main=1.7,cex.lab=1.5,cex.axis=1.5)
lines(x, supsw_s_ml_num, type="o", lwd = 4, lty = 2)
lines(x, supsw_e_cs_num, type="o", lwd = 4, lty = 3)
lines(x, supsw_s_cs_num, type="o", lwd = 4, lty = 4)
axis(1, Task, labels=c("Monolingual", "Code-switch"), cex.lab=1.5,cex.axis=1.5)
legend(2, 75,legend=c("English stim. ML response", "Spanish stim. ML response", "English stim. CS response", "Spanish stim. CS response"), lwd = 4, lty=c(1, 2, 3, 4), cex=1.3, bty="n")

dev.off()




