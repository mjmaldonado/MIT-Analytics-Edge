# Read in the data
nba = read.csv('NBA_train.csv')

# Let's take a look at the structure of the data
str(nba)

table(nba$W, nba$Playoffs)

# Let's a new variable to look at the points difference between the home and away team
nba$PTSdiff = nba$PTS - nba$oppPTS

# Take a look a the new relationship
plot(nba$PTSdiff, nba$W)

# It looks like a good candidate for linear regression!  Great!
WinsReg = lm( W ~ PTSdiff, data = nba )

summary(WinsReg)

# We can see that there is a high R squared value here, that's great!
# Equation for wins =
# 41 + 0.0326*PTSdiff

# What is we want 42 or more wins?
# 41 + 0.0326*PTSdiff >= 42
# Solve for PTSdiff
# PTSdiff = 30.67 which means a team needs to score ~31 or more points than
# their opponent to have a good shot of making it to the playoffs

# Now let's make a prediction for PTS
PointsReg = lm( PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + TOV + STL + BLK, data = nba )
summary(PointsReg)

# Take a look a the sum of squared residuals
SSE = sum( PointsReg$residuals^2 )
SSE

# The SSE isn't that easy to interpret, but the Root Mean Square Error is, so let's compute this
RMSE = sqrt( SSE / nrow(nba) )
RMSE

# How does this compare to the mean PTS scored in a season?
mean( nba$PTS ) #8370.24

# So our RMSE looks pretty good, but there is still room for improvement!
# Why is this?  Well, not all of the variables in our model were significant predictors
PointsReg2 = lm( PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + STL + BLK, data = nba )
summary(PointsReg2)

PointsReg3 = lm( PTS ~ X2PA + X3PA + FTA + AST + ORB + STL + BLK, data = nba )
summary(PointsReg3)

PointsReg4 = lm( PTS ~ X2PA + X3PA + FTA + AST + ORB + STL, data = nba )
summary(PointsReg4)

SSE = sum( PointsReg4$residuals^2 )
RMSE = sqrt( SSE / nrow(nba) )
RMSE

# Althought we've increased the RMSE slightly, the simpler model is prefered

# Now let's make some predictions!
nba_test = read.csv('NBA_test.csv')
PointsPredition = predict(PointsReg4, newdata = nba_test)

# How well did we do?  Let's compute the out-of-sample R squared
SSE_OOS = sum( (PointsPredition - nba_test$PTS)^2 )
SST_OOS = sum( (mean(nba$PTS) - nba_test$PTS)^2 )
R2_OOS = 1 - SSE_OOS / SST_OOS
R2_OOS

RMSE = sqrt( SSE_OOS / nrow(nba_test) )
RMSE
