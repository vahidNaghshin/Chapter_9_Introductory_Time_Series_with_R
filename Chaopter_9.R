t <- 1:128
c1 <- cos((2*pi/128)*t)
s1 <- sin((2*pi/128)*t)
c2 <- cos((4*pi/128)*t)
var(c1)
var(s1)
var(c2)
cor(c1,s1)
cor(c1,c2)
cor(s1,c2)


layout(1:2)
set.seed(1)
x <- rexp(2048)
spectrum(x-mean(x), log = c("no"))
spectrum(x-mean(x), span = 65, log = c("no"))
x.spec <- spectrum (x-mean(x), span = 65, log = c("no"))
spx <- x.spec$freq / 1
spy <- 2 * x.spec$spec
plot (spx, spy, xlab = "Hz", ylab = "variance/Hz", type = "l")

set.seed(1)
x <- w <- rexp(1024)
for (t in 2:1024) x[t]<- 0.9 * x[t-1] + w[t]
layout(1:3)
plot(as.ts(x))
acf(x)
spectrum(x, span = 51, log = c("no"))


t <- 0:7
s1 <- sin(pi *t*0.5)
s3 <- sin(0.75*pi*t)
s5 <- sin(5*pi*t/8)
X1 <- fft(s1)
X3 <- fft(s3)
X5 <- fft(s5)
plot(abs(X1), type='l')
plot(abs(X3), type='l')
plot(abs(X5), type='l')




t <- 0:31
s1 <- sin(11*pi*t/32)
plot(abs(fft(s1)), type='l')

alpha <- 0.1
t1 <- 0:2
t2 <- 3:31
s1 <- (1-cos(pi*(t1+0.5)/(alpha*length(t))))*sin(11*pi*t1/32)
s3 <- (1-cos(pi*(length(t2)-t2-0.5)/(alpha*length(t))))*sin(11*pi*t2/32)
plot(abs(fft(c(s1,s3))), type='l')

w <- seq(0, 2*pi, length.out = 1000)
a <-1
b <- 1
gamma.w <- a*w^(-5)*exp(-b*w^(-4))
plot(w,gamma.w, main='Peirson-Moskowitz spectrum',type="l", pch=10, col="red", xlab="w", ylab="gamma(w)")
a<-b<-2
gamma.w <- a*w^(-5)*exp(-b*w^(-4))
lines(w,gamma.w, type="l", pch=10, col="green")
legend(4, 0.3, legend=c("a=b=1", "a=b=2"),
       col=c("red", "green"), lty=1, cex=0.8)
