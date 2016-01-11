# Raw Data

eng_num_fig = c(14.65, 21.12)
sp_num_fig = c(21.65, 22.74)

eng_num_fig2 = c(0, 0)
sp_num_fig2 = c(0, 0)

	pdf("num_english.pdf")
barplot(c(eng_num_fig, sp_num_fig2), xaxt="n", xlab="Stimuli context", ylab="Percent code-switch response", ylim=c(0, 30), cex.main=1.7,cex.lab=1.5,cex.axis=1.5, col=c("blue", "blue", "darkgreen", "darkgreen"), density=c(200,25,200,25))
axis(1, at=c(0.75, 1.9), labels=c("E like (E)", "E like (S)"), cex.lab=1.5,cex.axis=1.5)
legend(2.8,15,legend=c("begins in English"), fill=c("blue", "blue"), density=c(25), cex=1.5, bty="n")
legend(2.6,15,legend=c(""), fill=c("blue", "blue"), density=c(200), cex=1.5, bty="n")
	dev.off()
	
	pdf("num_spanish.pdf")
barplot(c(eng_num_fig2, sp_num_fig), xaxt="n", xlab="Stimuli context", ylab="Percent code-switch response", ylim=c(0, 30), cex.main=1.7,cex.lab=1.5,cex.axis=1.5, col=c("blue", "blue", "darkgreen", "darkgreen"), density=c(200,25,200,25))
axis(1, at=c(3.1, 4.3), labels=c("S like (S)", "S like (E)"), cex.lab=1.5,cex.axis=1.5)
legend(0.2,15,legend=c("begins in Spanish"), fill=c("darkgreen"), density=c(25), cex=1.5, bty="n")
legend(0.0,15,legend=c(""), fill=c("darkgreen"), density=c(200), cex=1.5, bty="n")
	dev.off()
	
	pdf("num_both.pdf")
barplot(c(eng_num_fig, sp_num_fig), xaxt="n", xlab="Stimuli context", ylab="Percent code-switch response", ylim=c(0, 30), cex.main=1.7,cex.lab=1.5,cex.axis=1.5, col=c("blue", "blue", "darkgreen", "darkgreen"), density=c(200,25,200,25))
axis(1, at=c(0.75, 1.9, 3.1, 4.3), labels=c("E like (E)", "E like (S)", "S like (S)", "S like (E)"), cex.lab=1.5,cex.axis=1.5)
legend("topright",legend=c("begins in English","begins in Spanish"), fill=c("blue", "darkgreen"), density=c(25), cex=1.5, bty="n")
legend(2.4,30,legend=c("",""), fill=c("blue", "darkgreen"), density=c(200), cex=1.5, bty="n")
	dev.off()
	

# RT - Correct

eng_rt	= c(982.9852, 1128.652) 
sp_rt	= c(900.8718, 1341.22)

	pdf("rt_c_english.pdf")
plot(eng_rt, type="o", ylim=c(900, 1350), col="blue", cex.main=1.7,cex.lab=1.5,cex.axis=1.5, lwd=4, xlab="Stimuli context", ylab="Reaction times in milliseconds", xaxt="n")
axis(1, at=c(1, 2), labels=c("ML", "CS"), cex.lab=1.5,cex.axis=1.5)
legend("topleft",legend=c("begins in English", "begins in Spanish"), fill=c("blue", "darkgreen"), cex=1.5, bty="n")
	dev.off()
	
	pdf("rt_c_spanish.pdf")
plot(sp_rt, type="o", ylim=c(900, 1350), col="darkgreen", cex.main=1.7,cex.lab=1.5,cex.axis=1.5, lwd=4, xlab="Stimuli context", ylab="Reaction times in milliseconds", xaxt="n")
axis(1, at=c(1, 2), labels=c("ML", "CS"), cex.lab=1.5,cex.axis=1.5)
legend("topleft",legend=c("begins in English", "begins in Spanish"), fill=c("blue", "darkgreen"), cex=1.5, bty="n")
	dev.off()

	pdf("rt_c_both.pdf")
plot(eng_rt, type="o", ylim=c(900, 1350), col="blue", cex.main=1.7,cex.lab=1.5,cex.axis=1.5, lwd=4, xlab="Stimuli context", ylab="Reaction times in milliseconds", xaxt="n")
axis(1, at=c(1, 2), labels=c("ML", "CS"), cex.lab=1.5,cex.axis=1.5)
points(sp_rt, type="o", col="darkgreen", lwd=4)
legend("topleft",legend=c("begins in English", "begins in Spanish"), fill=c("blue", "darkgreen"), cex=1.5, bty="n")
	dev.off()
	
	
# RT - ML Response

eng_mlr		= c(987.8705, 964.6372)
sp_mlr		= c(909.7415, 1022.768)

	pdf("rt_mlr_both.pdf")
plot(eng_mlr, type="o", ylim=c(900, 1050), col="blue", cex.main=1.7,cex.lab=1.5,cex.axis=1.5, lwd=4, xlab="Stimuli context", ylab="Reaction times in milliseconds", xaxt="n")
axis(1, at=c(1, 2), labels=c("ML", "CS"), cex.lab=1.5,cex.axis=1.5)
points(sp_mlr, type="o", col="darkgreen", lwd=4)
legend("topleft",legend=c("begins in English", "begins in Spanish"), fill=c("blue", "darkgreen"), cex=1.5, bty="n")
	dev.off()
	

# RT - CS Response

eng_csr		= c(1313.711, 1268.259)
sp_csr		= c(1576.78, 1495.378)

	pdf("rt_csr_english.pdf")
plot(eng_csr, type="o", ylim=c(1200, 1600), col="blue", cex.main=1.7,cex.lab=1.5,cex.axis=1.5, lwd=4, xlab="Stimuli context", ylab="Reaction times in milliseconds", xaxt="n")
axis(1, at=c(1, 2), labels=c("ML", "CS"), cex.lab=1.5,cex.axis=1.5)
legend("topright",legend=c("English", "Spanish"), fill=c("blue", "darkgreen"), cex=1.5, bty="n")
	dev.off()
	
	pdf("rt_csr_spanish.pdf")
plot(sp_csr, type="o", ylim=c(1200, 1600), col="darkgreen", cex.main=1.7,cex.lab=1.5,cex.axis=1.5, lwd=4, xlab="Stimuli context", ylab="Reaction times in milliseconds", xaxt="n")
axis(1, at=c(1, 2), labels=c("ML", "CS"), cex.lab=1.5,cex.axis=1.5)
legend("topright",legend=c("English", "Spanish"), fill=c("blue", "darkgreen"), cex=1.5, bty="n")
	dev.off()

	pdf("rt_csr_both.pdf")
plot(eng_csr, type="o", ylim=c(1200, 1600), col="blue", cex.main=1.7,cex.lab=1.5,cex.axis=1.5, lwd=4, xlab="Stimuli context", ylab="Reaction times in milliseconds", xaxt="n")
axis(1, at=c(1, 2), labels=c("ML", "CS"), cex.lab=1.5,cex.axis=1.5)
points(sp_csr, type="o", col="darkgreen", lwd=4)
legend("topright",legend=c("begins in English", "begins in Spanish"), fill=c("blue", "darkgreen"), cex=1.5, bty="n")
	dev.off()
	

	
	
	
	
	
	
	
	
	
	