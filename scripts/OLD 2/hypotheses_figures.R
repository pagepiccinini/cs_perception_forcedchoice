	# Question 1

sep_hyp = c(50, 50)
merged_hyp = c(25, 75)

	pdf("../Figures/Hypotheses/sep_num_hypothesis.pdf")
barplot(sep_hyp, xaxt="n", main="Complete Switch", xlab="Stimuli context", ylab="Percent code-switch response", ylim=c(0, 100), cex.main=1.7,cex.lab=1.5,cex.axis=1.5, col=c("black", "black"), density=c(200,25))
axis(1, at=c(0.75, 1.9), labels=c("",""), cex.lab=1.5,cex.axis=1.5)
#legend("topright",legend=c("E like E", "E like S"), fill=c("black", "black"),  density=c(200,25), cex=1.5, bty="n")
	dev.off()
	
	pdf("../Figures/Hypotheses/merged_num_hypothesis.pdf")
barplot(merged_hyp, xaxt="n", main="Activation and Supression", xlab="Stimuli context", ylab="Percent code-switch response", ylim=c(0, 100), cex.main=1.7,cex.lab=1.5,cex.axis=1.5, col=c("black", "black"), density=c(200,25))
axis(1, at=c(0.75, 1.9), labels=c("",""), cex.lab=1.5,cex.axis=1.5)
#legend("topright",legend=c("E like E", "Code-switching"), fill=c("black", "black"),  density=c(200,25), cex=1.5, bty="n")
	dev.off()
	
	# Question 2

sep_eng_rt	= c(5, 20) 
sep_sp_rt	= c(10, 15)

merged_eng_rt	= c(5, 10) 
merged_sp_rt	= c(10, 20)
	
	pdf("../Figures/Hypotheses/separate_rt_hypothesis.pdf")
plot(sep_eng_rt, type="o", ylim=c(2, 23), col="blue", cex.main=1.7,cex.lab=1.5,cex.axis=1.5, lwd=4, main="Complete Switch", xlab="Stimuli context", ylab="Reaction times in milliseconds", xaxt="n")
axis(1, at=c(1, 2), labels=c("ML", "CS"), cex.lab=1.5,cex.axis=1.5)
points(sep_sp_rt, type="o", col="darkgreen", lwd=4)
legend("topleft",legend=c("begins in dominant language", "begins in non-dominant language"), fill=c("blue", "darkgreen"), cex=1.5, bty="n")
	dev.off()
	
	pdf("../Figures/Hypotheses/merged_rt_hypothesis.pdf")
plot(merged_eng_rt, type="o", ylim=c(2, 23), col="blue", cex.main=1.7,cex.lab=1.5,cex.axis=1.5, lwd=4, main="Activation and Supression", xlab="Stimuli context", ylab="Reaction times in milliseconds", xaxt="n")
axis(1, at=c(1, 2), labels=c("ML", "CS"), cex.lab=1.5,cex.axis=1.5)
points(merged_sp_rt, type="o", col="darkgreen", lwd=4)
legend("topleft",legend=c("begins in dominant language", "begins in non-dominant language"), fill=c("blue", "darkgreen"), cex=1.5, bty="n")
	dev.off()
	
	