library(XML)
library(beepr)

url <- "http://www.basketball-reference.com/contracts/players.html"
thepage <- readLines(url)
tab <- readHTMLTable(url, stringsAsFactors = FALSE)[[1]]
# data cleaning salary, player remove
tab <- tab[-grep("Salary", tab$Player),]
tab <- tab[-grep("Player", tab$Player),]

allplayer.url <- vector(mode = "list", length = length(tab$Player))
for (i in 1:length(tab$Player)){
player.url <- thepage[grep(tab$Player[i] ,thepage)]
mypattern.start <- '<a href=\"'
mypattern.end <- '\">'
s <- regexpr(mypattern.start, player.url)
e <- regexpr(mypattern.end, player.url)
allplayer.url[[i]] <- unique(paste("http://www.basketball-reference.com",substring(player.url, s + attributes(s)$match.length, e - 1), sep = ""))
}

allplayer.url <- unlist(allplayer.url)
allplayer.url <- allplayer.url[grep("html", allplayer.url)]

# bigtable <- data.frame(mode = "list", stringsAsFactors = FALSE)
bigtable <- data.frame(matrix(nrow = 585, ncol = 32), stringsAsFactors = FALSE)
total <- length(allplayer.url)

# create progress bar
pb <- txtProgressBar(min = 0, max = total, style = 3)
for (i in 1:total){
    ability <- readHTMLTable(allplayer.url[i], 
                             stringsAsFactors = FALSE)[[1]]
    season <- dim(ability)[1]
    if (season == 1 & length(ability) < 8) {
        bigtable[i, ] <- cbind(tab[i, c(1,2,4)], t(rep(NA, 29)))
    } else{
        bigtable[i, ] <- cbind(tab[i, c(1,2,4)], ability[season, -1])
    }
    Sys.sleep(0.1)
    # update progress bar
    setTxtProgressBar(pb, i)
}
beep()
close(pb)

names(bigtable) <- names(cbind(tab[1, c(1,2,4)], ability[season, -1]))
bigtable.NBA <- bigtable[bigtable$Lg == "NBA",]
index <- bigtable.NBA$Pos == ""
bigtable.NBA <- bigtable.NBA[!is.na(bigtable.NBA$`2015-16`) & !index,]

# Dollar to numeric
bigtable.NBA$`2015-16` <- substr(bigtable.NBA$`2015-16`, 2, nchar(bigtable.NBA$`2015-16`))
bigtable.NBA$`2015-16` <- as.numeric(gsub('\\,', '', as.character(bigtable.NBA$`2015-16`)))


# change all to numeric using below two line
bigtable.NBA <- as.matrix(bigtable.NBA)
bigtable.NBA <- as.data.frame(bigtable.NBA)

bigtable.NBA[,-c(2,5,6,7)] <- lapply(bigtable.NBA[,-c(2,5,6,7)], function(x) as.numeric(as.character(x)))
 
bigtable.NBA <- transform(bigtable.NBA, Player = as.character(Player))
bigtable.NBA <- transform(bigtable.NBA, Tm = as.factor(Tm))
bigtable.NBA <- transform(bigtable.NBA, Pos = as.factor(Pos))
bigtable.NBA[is.na(bigtable.NBA)] <- 0
names(bigtable.NBA) <- names(cbind(tab[1, c(1,2,4)], ability[season, -1]))

# sapply(bigtable.NBA, class)
# salary <- bigtable.NBA$X2015.16 / 1000
# hist(salary, xlab = "Salary (Unit: $1000)", main = "Histogram of NBA players' salary")

# Write CSV in R
# write.csv(bigtable.NBA, file = "bigtableNBA.csv", row.names = FALSE, na = "")

bigtable.NBAPG <- bigtable.NBA[bigtable.NBA$Pos == "PG",]
bigtable.NBASG <- bigtable.NBA[bigtable.NBA$Pos == "SG",]
bigtable.NBASF <- bigtable.NBA[bigtable.NBA$Pos == "SF",]
bigtable.NBAPF <- bigtable.NBA[bigtable.NBA$Pos == "PF",]
bigtable.NBAC  <- bigtable.NBA[bigtable.NBA$Pos == "C",]


#write.csv(bigtable.NBAPG, file = "bigtableNBAPG.csv", row.names = FALSE, na = "")
#write.csv(bigtable.NBASG, file = "bigtableNBASG.csv", row.names = FALSE, na = "")
#write.csv(bigtable.NBASF, file = "bigtableNBASF.csv", row.names = FALSE, na = "")
#write.csv(bigtable.NBAPF, file = "bigtableNBAPF.csv", row.names = FALSE, na = "")
#write.csv(bigtable.NBAC, file = "bigtableNBAC.csv", row.names = FALSE, na = "")


# for bigtable.NBAPG
# View(dset)

dset <- bigtable.NBAC
xdata <- dset[-grep(names(bigtable.NBA)[3], names(dset))]
ydata <- dset[ grep(names(bigtable.NBAC)[3], names(dset))]

xdata <- xdata[-c(1:2,4:6)]
xdata <- scale(xdata)
ydata <- scale(ydata)
xdata <- cbind(rep(1,nrow(xdata)), xdata)
colnames(xdata)[1] <- "Constant" 

train.x <- xdata
train.y <- ydata


# beta matrix calculate
#================================================
soft_th <- function(z, r){
    sign(z) * max( abs(z) - r, 0 )
}

lambda <- 1 - seq(0.01, 1, length = 100)

beta_matrix <- matrix(0, nrow = 27, ncol = length(lambda))

total <- length(lambda)
# create progress bar
pb <- txtProgressBar(min = 0, max = total, style = 3)
beta_new <- rep(0, 27)
beta_hat <- rep(0, 27)
# beta_hat <- c(mean(train.y), rep(0, 26))

for (i in 1:length(lambda)){
    lamb <- lambda[i]
    repeat{
        for (j in 1:27){
            y_hat <- train.x %*% beta_hat
            s <- soft_th( mean(train.x[,j] * (train.y - y_hat)) + beta_hat[j] , lamb)
            beta_hat[j] <- s
        }
        
         epsilon <- max(abs(beta_new - beta_hat))
        
        if (epsilon < 2.22e-10){
            break
        }
        
        beta_new <- beta_hat
        
    }
    beta_matrix[ , i] <- beta_hat
    Sys.sleep(0.1)
    # update progress bar
    setTxtProgressBar(pb, i)
}
beep()
close(pb)

vr.num <- rep(0, length(lambda))
for (i in 1:length(lambda)){
    vr.num[i] <- length(which(beta_matrix[,i] != 0))
}
vr.num

col <- which(vr.num == 7)[1]
lambda[col]
index <- which(beta_matrix[, col] != 0)
beta_matrix[index, col]
fit.x <- train.x[,index]
colnames(fit.x)
b <- max(beta_matrix[index, col]) + 0.01

# Table
a <- rbind(colnames(fit.x), round(beta_matrix[index, col], 4))
library(xtable)
xtable(a)

endl <- which(vr.num == 0)
plot(lambda, rep(0,100), col = "black", xlim = rev(c(0, lambda[endl[length(endl)]] + 0.01)), ylab = "Coefficients", ylim = c(-b, b), type = 'l')
par(new = TRUE)
for(i in 1:27){
    lines(lambda,beta_matrix[i,], col = i , xlim = rev(range(lambda)))
}

# Prediction ====================================
train.x <- xdata[-test,]
train.y <- ydata[-test,]

valid.x <- xdata[test, ]
valid.y <- ydata[test, ]

col <- which(vr.num == 7)[1]
index <- which(beta_matrix[, col] != 0)
beta_hat <- beta_matrix[index, col]
fit.x <- train.x[,index]
y_hat <- fit.x %*% beta_hat

colnames(fit.x)
formula <- as.formula(paste("y ~", paste((colnames(fit.x)[-1]), collapse = "+")))

colnames(dset)[3] <- "y"
scale(dset)
reg <- lm(formula, dset)
reg$coefficients
summary(reg)


for (i in 50:100){
    index <- which(beta_matrix[, i] != 0)
    lassobeta_hat <- beta_matrix[index, i]
    fit.x <- train.x[, index]
    y_hat <- fit.x %*% lassobeta_hat
    predict[i] <- 1 - sum((train.y - y_hat)^2) / sum((train.y - mean(train.y))^2)
}
which.max(predict)

