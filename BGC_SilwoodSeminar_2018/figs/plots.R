

pdf(file='school.pdf', width=5.5/2.54, height=4/2.54, pointsize=10,
    family='Times')
par(mar=c(2, 2.2, 0.4, 0.4), mgp=c(0.5, 0.2, 0), tcl=0.2, las=1)

k = 8.617e-5
temps = seq(10, 45, 0.1) + 273.15
rates = exp(-0.6/k * (1/temps - 1/283.15)) / (1 + exp(2/k * (1/315.15 - 1/temps)))

plot(temps-273.15, rates, type='l', lwd=2, xaxt='n', yaxt='n',
     xlab='Temperature', ylab=expression(italic(P)*','~italic(R)), col='royalblue2')
axis(2, at=rates[81], labels=expression(italic(B)[0]), tick=FALSE)
text(23, 4, expression(italic(E)))
arrows(x0=25, y0=4.1, x1=30, y1=5.6, length=0.07)

text(44, 4.5, expression(italic(E)[D]))
arrows(x0=40, y0=6, x1=43, y1=5, length=0.07)

text(21, 1.2, expression(italic(T)['ref']))
segments(x0=18, y0=-1, x1=18, y1=rates[81], lty='11')

lines(temps-273.15, rates, lwd=2, col='royalblue2')

dev.off()



pdf(file='school_simple.pdf', width=5.5/2.54, height=4/2.54, pointsize=10,
    family='Times')
par(mar=c(1, 0.4, 0.4, 0.4), mgp=c(0, 0.2, 0), tcl=0.2, las=1)

k = 8.617e-5
temps = seq(10, 45, 0.1) + 273.15
rates = exp(-0.6/k * (1/temps - 1/283.15)) / (1 + exp(2/k * (1/315.15 - 1/temps)))

plot(temps-273.15, rates, type='l', lwd=4, xaxt='n', yaxt='n',
     xlab='Temperature', ylab='', bty='n', col='white', col.lab='white')
box(col=rgb(0, 0, 0, 0.4), col='white')
text(23, 4, expression(italic(E)), col='white')
segments(x0=temps[which.max(rates)]-273.15, y0=-1,
         x1=temps[which.max(rates)]-273.15, y1=max(rates), lty='11', col='white')
lines(temps-273.15, rates, lwd=4, col='white')

text(temps[which.max(rates)]-273.15+3, 1.5, expression(italic(T)[pk]), col='white')

dev.off()




pdf(file='ba.pdf', width=11/2.54, height=4/2.54, pointsize=10,
    family='Times')
par(mar=c(0.1, 1.2, 0.4, 0.4), mgp=c(0.5, 0.2, 0), tcl=0.2)
split.screen(c(1, 2))
par(oma=c(1.8, 1.8, 0, 0))

k = 8.617e-5
temps = seq(10, 45, 0.1) + 273.15
rates1 = exp(-0.6/k * (1/temps - 1/283.15)) 
rates2 = exp(0.6/k * (1/temps - 1/283.15)) 

screen(1)
plot(temps-273.15, rates1, type='l', lwd=2, xaxt='n', yaxt='n',
     xlab='', ylab='', col='royalblue2')
axis(2, at=rates1[151], labels=expression(epsilon[0]), tick=FALSE)

text(30, 7, expression(italic(E)[epsilon]))
arrows(x0=32, y0=7.5, x1=38, y1=11, length=0.07)

text(28, 1.6, expression(italic(T)['ref']))
segments(x0=25, y0=-1, x1=25, y1=rates1[151], lty='11')
segments(x0=0, y0=rates1[151], x1=25, y1=rates1[151], lty='11')

lines(temps-273.15, rates1, lwd=2, col='royalblue2')
mtext(expression(italic(E)[epsilon]>0), side=3, line=-1.5)


screen(2)
plot(temps-273.15, rates2, type='l', lwd=2, xaxt='n', yaxt='n',
     xlab='', ylab='', col='royalblue2')

text(22, 0.5, expression(italic(E)[epsilon]))
arrows(x0=24, y0=0.4, x1=30, y1=0.26, length=0.07)

text(28, 1.6, expression(italic(T)['ref']))

lines(temps-273.15, rates2, lwd=2, col='royalblue2')
mtext(expression(italic(E)[epsilon]<0), side=3, line=-1.5)


mtext(expression(epsilon), side=2, line=-0.5, outer=TRUE)
mtext('Temperature', side=1, line=0, outer=TRUE)

close.screen(all=TRUE)
dev.off()



c1 = -0.001
c2 = -0.010
b1 = -300 * 2 * c1
b2 = -300 * 2 * c2
a1 = 10 - b1 * 300 - c1 * 300^2
a2 = 10 - b2 * 300 - c2 * 300^2

sp1 = a1 + b1 * temps + c1 * temps^2
sp1 = sp1 / max(sp1)
sp2 = a2 + b2 * temps + c2 * temps^2
sp2 = sp2 / max(sp2)

eps = exp(-0.02/k * (1/temps - 1/283.15)) 
eps = eps/max(eps)

spe1 = sp1 * eps
spe1 = spe1 / max(spe1)
spe2 = sp2 * eps
spe2 = spe2 / max(spe2)


pdf(file='delta_tpk_ex.pdf', width=10/2.54, height=5/2.54, pointsize=10,
    family='Times')
par(mar=c(0.1, 1.2, 0.4, 0.4), mgp=c(0.5, 0.2, 0), tcl=0.2)
split.screen(c(1, 2))
par(oma=c(1.8, 1.8, 0, 0))

screen(1)
plot(temps-273.15, sp1, type='l', lwd=2, xaxt='n', yaxt='n', ylim=c(0.5, 1.5), 
     xlab='', ylab='', col='royalblue4', lty='41')


screen(2)

plot(temps-273.15, sp2, type='l', lwd=2, xaxt='n', yaxt='n', ylim=c(0.7, 1.1), 
     xlab='', ylab='', col='royalblue2', lty='41')

mtext(expression(''%prop%'Net flux'~ italic(F)*', Growth'~italic(r)), side=2,
      outer=TRUE, line=-1)
mtext('Temperature', side=1, outer=TRUE)

close.screen(all=TRUE)
dev.off()




pdf(file='delta_tpk_ex2.pdf', width=10/2.54, height=5/2.54, pointsize=10,
    family='Times')
par(mar=c(0.1, 1.2, 0.4, 0.4), mgp=c(0.5, 0.2, 0), tcl=0.2)
split.screen(c(1, 2))
par(oma=c(1.8, 1.8, 0, 0))

screen(1)
plot(temps-273.15, sp1, type='l', lwd=2, xaxt='n', yaxt='n', ylim=c(0.5, 1.5), 
     xlab='', ylab='', col='royalblue4', lty='41')
lines(temps-273.15, spe1, lwd=2, col='royalblue4')

segments(x0=temps[which.max(sp1)]-273.15, y0=0, 
         x1=temps[which.max(sp1)]-273.15, y1=max(sp1), lty='11', col='royalblue4')

segments(x0=temps[which.max(spe1)]-273.15, y0=0, 
         x1=temps[which.max(spe1)]-273.15, y1=max(spe1), col='royalblue4')

arrows(x0=temps[which.max(sp1)]-273.15, y0=0.6,
       x1=temps[which.max(spe1)]-273.15 - 1, y1=0.6, lwd=1.5, col='red', length=0.07)


screen(2)

plot(temps-273.15, sp2, type='l', lwd=2, xaxt='n', yaxt='n', ylim=c(0.7, 1.1), 
     xlab='', ylab='', col='royalblue2', lty='41')
lines(temps-273.15, spe2, lwd=2, col='royalblue2')

segments(x0=temps[which.max(sp2)]-273.15, y0=0, 
         x1=temps[which.max(sp2)]-273.15, y1=max(sp1), lty='11', col='royalblue2')

segments(x0=temps[which.max(spe2)]-273.15, y0=0, 
         x1=temps[which.max(spe2)]-273.15, y1=max(spe2), col='royalblue2')

mtext(expression(''%prop%'Net flux'~ italic(F)*', Growth'~italic(r)), side=2,
      outer=TRUE, line=-1)
mtext('Temperature', side=1, outer=TRUE)

close.screen(all=TRUE)
dev.off()




pdf(file='nuts.pdf', width=5.5/2.54, height=4/2.54, pointsize=10,
    family='Times')
par(mar=c(2, 2.2, 0.4, 0.4), mgp=c(1.2, 0.2, 0), tcl=0.2, las=1)

S = seq(0.1, 10, 0.01)
ks = 2
vn = S / (ks + S)

plot(S, vn, type='l', lwd=2, xaxt='n', yaxt='n', col='royalblue2',
     xlab=expression('Nutrient concentration'~italic(S)), 
     ylab=expression(italic(V)[n]))
axis(1, at=2, labels=expression(italic(K[S])), tick=FALSE)

segments(x0=2, y0=-1, x1=2, y1=vn[191], lty='11')
segments(x0=-1, y0=vn[191], x1=2, y1=vn[191], lty='11')

lines(S, vn, lwd=2, col='royalblue2')

dev.off()

