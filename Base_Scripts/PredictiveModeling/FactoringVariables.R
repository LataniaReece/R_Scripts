set.seed(2)
nums <- sample(1:4, replace = T, size = 100)
check <- data.frame(nums = nums)
check$words <- NA

table(check$nums)
check$words[nums == 1] <- 'Apple'
check$words[nums == 2] <- 'Bananas'
check$words[nums == 3] <- 'Cherries'
check$words[nums == 4] <- 'Grapes'
table(check$words)

check$words <- factor(check$words, 
                      levels = c('Cherries', 'Fruit', 'Apple', 'Bananas'))
str(check)

head(check)
levels(check$words)
