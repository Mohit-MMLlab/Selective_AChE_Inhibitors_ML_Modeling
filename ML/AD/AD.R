# Applicability Domain (AD) Analysis
# - Leverage method (Williams plot)

library(dplyr)
library(ggplot2)
library(FNN)       

# =========================
# 1. Load Data
# =========================
train <- read.csv("train.csv")
test  <- read.csv("test.csv")

# 30 selected features
features <- c("ExtFP673","ExtFP773","ExtFP853","ExtFP246","ExtFP486","ExtFP766","ExtFP461",
              "ExtFP511","ExtFP619","ExtFP765","ExtFP492","ExtFP597","ExtFP658","PubchemFP638",
              "ExtFP630","ExtFP315","ExtFP812","ExtFP510","ExtFP922","ExtFP163","ExtFP241",
              "PubchemFP13","ExtFP65","ExtFP248","ExtFP602","ExtFP701","ExtFP279","ExtFP949",
              "KRFP2113","ExtFP531")

X_train <- as.matrix(train[, features])
X_test  <- as.matrix(test[, features])

# =========================
# 2. Leverage Approach (Williams plot)
# =========================
H <- X_train %*% solve(t(X_train) %*% X_train) %*% t(X_train)
leverage_train <- diag(H)
crit_leverage <- 3 * ncol(X_train) / nrow(X_train)

# Test leverage (projection on training space)
test_leverage <- diag(X_test %*% solve(t(X_train) %*% X_train) %*% t(X_test))

train$Leverage <- leverage_train
test$Leverage  <- test_leverage


# Flag outside AD
test$AD_Flag <- ifelse(test$Leverage > crit_leverage,
                       "Outside", "Inside")


# Williams Plot
ggplot() +
  geom_point(aes(x=1:nrow(train), y=train$Leverage), color="blue", alpha=0.6) +
  geom_point(aes(x=(nrow(train)+1):(nrow(train)+nrow(test)), y=test$Leverage), 
             color="red", alpha=0.6) +
  geom_hline(yintercept=crit_leverage, linetype="dashed", color="black") +
  labs(title="Williams Plot (Leverage Method)", 
       x="Molecule Index (Train + Test)", 
       y="Leverage")


# =========================
# 5. Save results
# =========================
write.csv(test[, c("Smiles","Class","Leverage","AD_Flag")],
          "Applicability_Domain_Results.csv", row.names = FALSE)

cat("✅ AD analysis complete. Results saved to Applicability_Domain_Results.csv\n")



library(ggplot2)

# Add Set labels
train$Set <- "Train"
test$Set  <- "Test"

# Combine into one dataframe
plotdata <- rbind(
  data.frame(Leverage = train$Leverage, Set = "Train"),
  data.frame(Leverage = test$Leverage, Set = "Test")
)

# Randomize order
set.seed(123)  # for reproducibility
plotdata <- plotdata[sample(nrow(plotdata)), ]

# Assign new mixed index
plotdata$Index <- 1:nrow(plotdata)

# Plot
ggplot(plotdata, aes(x = Index, y = Leverage, color = Set)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_hline(yintercept = crit_leverage, linetype = "dashed", color = "red") +
  labs(title = "Williams Plot (Train vs Test)",
       x = "No. of Molecules",
       y = "Leverage") +
    theme()


