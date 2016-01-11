	# Set up x values and at
x <- c(1, 2)

	# Set up response percentages for non switchers
nonsw_e_ml_rt <- c(2.908664,  2.902818)
nonsw_e_cs_rt <- c(3.072663, 3.098088)
nonsw_s_ml_rt <- c(2.869306, 2.926153)
nonsw_s_cs_rt <- c(3.118498, 3.105229)

	# Set up response percentages for switchers
sw_e_ml_rt <- c(3.11223, 3.09827)
sw_e_cs_rt <- c(3.16504, 3.1357)
sw_s_ml_rt <- c(3.09872, 3.12128)
sw_s_cs_rt <- c(3.25717, 3.20042)

	# Set up response percentages for super switchers
supsw_e_ml_rt <- c(3.180645, 3.151601)
supsw_e_cs_rt <- c(3.174556, 3.139753)
supsw_s_ml_rt <- c(3.144695, 3.187654)
supsw_s_cs_rt <- c(3.243675, 3.240222)

	# Make plot for non switchers
pdf("nonsw_rt.pdf")

par(xpd=T, mar=par()$mar+c(0,0,0,15))
	
plot(x, nonsw_e_ml_rt, type="o", lwd= 4, main="Non-switchers Reaction Times\n by Languge and Stimulus Context", xaxt="n", xlab="Stimulus context", ylab="Log transformed reaction times", ylim=c(2.7, 3.15),cex.main=1.7,cex.lab=1.5,cex.axis=1.5)
lines(x, nonsw_s_ml_rt, type="o", lwd = 4, lty = 2)
lines(x, nonsw_e_cs_rt, type="o", lwd = 4, lty = 3)
lines(x, nonsw_s_cs_rt, type="o", lwd = 4, lty = 4)
axis(1, Task, labels=c("Monolingual", "Code-switch"), cex.lab=1.5,cex.axis=1.5)
legend(2, 3,legend=c("English stim. ML response", "Spanish stim. ML response", "English stim. CS response", "Spanish stim. CS response"), lwd = 4, lty=c(1, 2, 3, 4), cex=1.3, bty="n")

dev.off()

	# Make plot for switchers
pdf("sw_rt.pdf")

par(xpd=T, mar=par()$mar+c(0,0,0,15))
	
plot(x, sw_e_ml_rt, type="o", lwd= 4, main="Switchers Reaction Times\n by Languge and Stimulus Context", xaxt="n", xlab="Stimulus context", ylab="Log transformed reaction times", ylim=c(3, 3.3),cex.main=1.7,cex.lab=1.5,cex.axis=1.5)
lines(x, sw_s_ml_rt, type="o", lwd = 4, lty = 2)
lines(x, sw_e_cs_rt, type="o", lwd = 4, lty = 3)
lines(x, sw_s_cs_rt, type="o", lwd = 4, lty = 4)
axis(1, Task, labels=c("Monolingual", "Code-switch"), cex.lab=1.5,cex.axis=1.5)
legend(2, 3.2,legend=c("English stim. ML response", "Spanish stim. ML response", "English stim. CS response", "Spanish stim. CS response"), lwd = 4, lty=c(1, 2, 3, 4), cex=1.3, bty="n")

dev.off()

	# Make plot for super switchers
pdf("supsw_rt.pdf")

par(xpd=T, mar=par()$mar+c(0,0,0,15))
	
plot(x, supsw_e_ml_rt, type="o", lwd= 4, main="Super switchers Reaction Times\n by Languge and Stimulus Context", xaxt="n", xlab="Stimulus context", ylab="Log transformed reaction times", ylim=c(3.08, 3.26),cex.main=1.7,cex.lab=1.5,cex.axis=1.5)
lines(x, supsw_s_ml_rt, type="o", lwd = 4, lty = 2)
lines(x, supsw_e_cs_rt, type="o", lwd = 4, lty = 3)
lines(x, supsw_s_cs_rt, type="o", lwd = 4, lty = 4)
axis(1, Task, labels=c("Monolingual", "Code-switch"), cex.lab=1.5,cex.axis=1.5)
legend(2, 3.2,legend=c("English stim. ML response", "Spanish stim. ML response", "English stim. CS response", "Spanish stim. CS response"), lwd = 4, lty=c(1, 2, 3, 4), cex=1.3, bty="n")

dev.off()




