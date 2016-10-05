library(data.table)

setwd("H:/Pain Management Data/")

test2 <- read.csv("./test2.csv", stringsAsFactors=FALSE)
test2 <- test2[order(test2$Patient.CSN.ID, test2$Recorded.Time.Value),]

test2.1 <- test2


##### Pre-intervention assessment (by patient and SHIFT)


#### Possible case 1 -- Eligible assess time is in the previous row

## Example as below

# Pain Score Time    | Pain Intervention Time
# ------------------ | ----------------------
# 2016/01/01 8:00 AM | Null
# Null               | 2016/01/01 8:30 AM (subject)
# 2016/01/01 9:00 AM | Null

## diff time from the previous row

setDT(test2.1)[,diff.pre1:=round((Pain.Intervention.Time-shift(Pain.Score.Time,1,type="lag"))*24*60,digits=0),by=CSN...Shift]
test2.1$diff.pre1[test2.1$diff.pre1 < 0] <- NA  # eligible diff time must be >= 0

## EXCEPTION: The only pre-intervention assess is for the previous intervention

## Example as below

# Pain Score Time    | Pain Intervention Time
# ------------------ | ----------------------
# 2016/01/01 8:00 AM | 2016/01/01 8:00 AM
# Null               | 2016/01/01 8:30 AM (subject)
# 2016/01/01 9:00 AM | Null

# clean the exception

test2.1 <- test2.1[,diff.pre1.i:=as.numeric(shift(Pain.Intervention.Time,1,type="lag"))>0,by=CSN...Shift]
test2.1$diff.pre1.i[is.na(test2.1$Pain.Intervention.Time)] <- NA
test2.1$diff.pre1[test2.1$diff.pre1.i == "TRUE"] <- NA


#### Possible case 2 -- Eligible assess time is in the same row

## Example as below

# Pain Score Time    | Pain Intervention Time
# ------------------ | ----------------------
# 2016/01/01 8:00 AM | 2016/01/01 8:00 AM
# 2016/01/01 8:30 AM | 2016/01/01 8:30 AM (subject)
# 2016/01/01 9:00 AM | Null

# diff time from the same row

test2.1 <- test2.1[,diff.pre2:=round((Pain.Intervention.Time-Pain.Score.Time)*24*60,digits=0),by=CSN...Shift]
test2.1$diff.pre2[test2.1$diff.pre2 < 0] <- NA


#### Possible case 3 -- Eligible assess time is in the next row (could be due to entry error)

## Example as below

# Pain Score Time    | Pain Intervention Time
# ------------------ | ----------------------
# 2016/01/01 8:00 AM | 2016/01/01 8:00 AM
# Null               | 2016/01/01 8:30 AM (subject)
# 2016/01/01 8:30 AM | Null

# diff time from the next row

test2.1 <- test2.1[,diff.pre3:=round((Pain.Intervention.Time-shift(Pain.Score.Time,1,type="lead"))*24*60,digits=0),by=CSN...Shift]
test2.1$diff.pre3[test2.1$diff.pre3 < 0] <- NA

#### compliance -- If any of the eligible diff time exists then yes

test2.1$pre.compliance.itv <- as.numeric(!is.na(test2.1$diff.pre1)|!is.na(test2.1$diff.pre2)|!is.na(test2.1$diff.pre3))
test2.1$pre.compliance.itv[is.na(test2.1$pre.compliance.itv)] <- 0



##### Post-intervention assessment (by patient)


#### Possible case 1 -- Eligible assess time is in the next row

## Example as below

# Pain Score Time    | Pain Intervention Time
# ------------------ | ----------------------
# 2016/01/01 8:00 AM | 2016/01/01 8:00 AM
# 2016/01/01 8:30 AM | 2016/01/01 8:30 AM (subject)
# 2016/01/01 9:00 AM | Null

# diff time from the next row

test2.1 <- test2.1[,diff.post1:=round((shift(Pain.Score.Time,1,type="lead")-Pain.Intervention.Time)*24*60,digits=0),by=Patient.CSN.ID]
test2.1$diff.post1[test2.1$diff.post1 <= 0] <- NA  # eligible diff time must be > 0


#### Possible case 2 -- Eligible assess time is in the 2nd next row (could be due to entry error)

## Example as below

# Pain Score Time    | Pain Intervention Time
# ------------------ | ----------------------
# 2016/01/01 8:00 AM | 2016/01/01 8:00 AM
# Null               | 2016/01/01 8:30 AM (subject)
# 2016/01/01 8:30 AM | Null
# 2016/01/01 9:00 AM | Null

# diff time from the 2nd next row

test2.1 <- test2.1[,diff.post2:=round((shift(Pain.Score.Time,2,type="lead")-Pain.Intervention.Time)*24*60,digits=0),by=Patient.CSN.ID]
test2.1$diff.post2[test2.1$diff.post2 <= 0] <- NA

#### compliance -- If any of the eligible diff time <= 60 then yes

test2.1$post.compliance <- as.numeric(test2.1$diff.post1 <= 60 | test2.1$diff.post2 <= 60)
test2.1$post.compliance[is.na(test2.1$post.compliance)] <- 0


#### keep only CSN & interventions because our subjects are interventions

test2.1 <- subset(test2.1, !is.na(test2.1$Pain.Intervention.Time))

#### quick look at the percentage of compliance

prop.table(table(test2.1$pre.compliance.itv))
prop.table(table(test2.1$post.compliance))

#### export data

#write.csv(test2.1, "test.csv",row.names = FALSE)
