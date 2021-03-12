#Reading the csv file
data=read.csv("C:/Users/Deepali Attavar/Downloads/Diamonds.csv")
data

#Some of the Operations done
class(data)
class(data$ID)
class(data$Carat)
class(data$Color)
class(data$Clarity)
class(data$PricePerCt)

#vector
price=c(data$PricePerCt)
#accessing element in vectors
a=price[10]
a
a=price[2:5]
a
a=price[price>15000]
a

#operations on vector
head(price,10)
sort(head(price,10))
sort(head(price,10),decreasing=TRUE)
#is something a vector 
is.vector(price)
is.matrix(price)
is.array(price)

#array
b=array(head(price,10),dim=c(2,5))
b
is.array(b)
length(b)
#acessing in array
b[,1] 
b[2,] 
b[2,1]
#finding the dimension, length of rows, columns and array
dim(b)
nrow(b)
ncol(b)
#finding the sum of the column and row of the array
apply(b, c(1), sum)
apply(b, 2 , sum)

#matrix
x=matrix(head(price,12),nrow=4,ncol=3,byrow=TRUE)
x
#finding the sum of columns and rows 
colSums(x)
rowSums(x)
#naming the rows and columns 
colnames(x)=c("COL1","COL2","COL3")
rownames(x)=c("ROW1","ROW2","ROW3","ROW4")
x

#some more operations
max(data$PricePerCt)
is.element(60,data$Depth)
x=chartr('V','A',data$Clarity[1])
x
x=casefold(data$Clarity[1])
setequal(data$PricePerCt,data$TotalPrice)
identical(data$PricePerCt,data$TotalPrice)

#chapter 2-date
as.Date('2020-11-30')
as.Date('2020/12/1')
as.Date('12/1/2020',format='%m/%d/%Y')
as.Date('May 26, 2001',format='%B %d, %Y')
as.Date('24NOV20',format='%d%b%y')

#accessing today's date
today=Sys.Date()
format(today, format="%B %d %Y")

#chpater 3&4-conditional statements&loops
# a) to find the number of diamonds of the best grade(ie. D) in the dataset
j=0
for(i in data$Color)
{
  if(i=='D'||i=='E'||i=='F')
  {
    j=j+1
  }
}
print(paste(j," number of colorless diamonds are available in the dataset"))

# b)to find the cheapest best grade diamond

for(k in 1:nrow(data))
{
  i=data[k,"Color"]
  j=data[k,"TotalPrice"]
  id=data[k,"ID"]
  if(j<2000&& i=="D")
  {
    print(paste("no.",id,"is cheap as its price is ",j,"and is has the best color->",i))
  }
}

# c)different colors of diamond

d=vector()
e=vector()
f=vector()
g=vector()
h=vector()
I=vector()
for(i in data$Color)
{
  if(i=='D')
  {
    d=append(d,i)
  }
  if(i=='E')
  {
    e=append(e,i)
  }
  if(i=='F')
  {
    f=append(f,i)
  }
  if(i=='G')
  {
    g=append(g,i)
  }
  if(i=='H')
  {
    h=append(g,i)
  }
  if(i=='I')
  {
    I=append(I,i)
  }
}
print(paste("no. of Color D:",length(d), "E:" ,length(e), "F:",length(f),"G:",length(g),"H:",length(h),"I:",length(I)))



#grep
a=grep("S",head(data$Clarity))
b=grep("S",head(data$Clarity),value=TRUE)
c=grepl("S",head(data$Clarity))
d=grep("V*",head(data$Clarity),value=TRUE)
e=grep("V+",head(data$Clarity),value=TRUE)
f=grep("V{2}",head(data$Clarity),value=TRUE)
g=grep("V{2,}",head(data$Clarity),value=TRUE)
print(a)
print(b)
print(c)
print(d)
print(e)
print(f)
print(g)

# Sorting elements in a Vector using Recursion.
v=head(data$Carat)
sort_vect<-function(vect)
  if(length(vect)<=1){
    return(vect)
  }else{
    initial_element<-vect[1]
    prev_element<-vect[-1]
    greater<-prev_element[prev_element>initial_element]
    smaller<-prev_element[prev_element<=initial_element]
    greater<-sort_vect(greater)
    smaller<-sort_vect(smaller)
    return(c(smaller,initial_element,greater))
    
  }
print(v)
print(sort_vect(v))

data <- read.csv("C:/diamonds.csv")
print(data)
#all function
v1=data$PricePerCt
v2=data$TotalPrice
x=all(v1 %in% v2)
print(x)
y1=c(61.7,61.5,62.3)
y2=data$Depth
x=all(y1 %in% y2)
print(x)
#Any function
v1=data$PricePerCt
v2=data$TotalPrice
x=any(v1 %in% v2)
print(x)
#apply function
a=data$PricePerCt
b=data$Carat
df=data.frame(a,b)
print(df)
total=apply(df,2,sum)
print(total)
low=lapply(head(data$Clarity),tolower)
print(low)

#Which function

a1=which(data$Color=='E')
print(a1)
a2=which(data$Clarity=='VS1')
print(a2)

#match function

b1=head(data$PricePerCt)
b2=head(data$TotalPrice)
m=match(b1,b2)
print(m)
print(b1 %in% b2)

#rank,order,sort

r=rank(head(data$Depth))
s=sort(head(data$Depth))
o=order(head(data$Depth))
print(r)
print(s)
print(o)

#sub and gsub

p=head(data$Clarity)
q=sub("V","R",p)
print(p)
print(q)
p1=head(data$Clarity)
q1=gsub("V","R",p1)
print(p1)
print(q1)

#aggregate

dataframe=data.frame(head(data$Carat),head(data$Color),head(data$PricePerCt))
print(dataframe)
agg=aggregate(head(data$Color),list(head(data$PricePerCt)),max)
print(agg)

data <- read.csv("C:/diamonds.csv")
print(data)
# Relation between carat and pricepercarat
df=data[c('Carat','PricePerCt')]
print(df)
plot(df$Carat,df$PricePerCt,
     ylab = "price",
     xlab = "Carat",
     main="Variation of price of diamonds")

#Relation between depth and total price

df1=data[c('Depth','TotalPrice')]
print(df1)
plot(df1$Depth,df1$TotalPrice,
     ylab = "TotalPrice",
     xlab = "Depth",
     main="Depth Vs Price")
#histogram 
#total price
hist(df1$TotalPrice)
hist(df1$TotalPrice,main="Distribution of Price",
     xlab="Total Price")

#depth
hist(data$Depth)
hist(data$Depth,main="Distribution of Depth",
     xlab="depth")
#Price per carat

hist(data$PricePerCt)
hist(data$PricePerCt,main="Distribution of Pricepercarat",
     xlab="ppc")
#Carat

hist(data$Carat)
hist(data$Carat,main="Distribution of carat",
     xlab="Carat")

#boxplot for numerical variables

boxplot(data$Carat)
boxplot(data$Depth)
boxplot(data$PricePerCt)
boxplot(data$TotalPrice)

#barchart for Categorical data

x=table(data$Color)
print(x)
barplot(x,
        main="Colors",
        xlab="Color",
        ylab="Count",
        border="red",
        col="blue",
        density=50
)
y=table(data$Clarity)
print(y)
barplot(y,
        main="Clarity",
        xlab="Clarity",
        ylab="Count",
        border="blue",
        col="red",
        density=50
)

#Representation using a pie chart

x=table(data$Color)
pie(x,main="Pie chart for Colour",)
y=table(data$Clarity)

pie(y,main = "pie chart for Clarity",col = rainbow(length(y)))

#stacked barplot
stk=table(data$Color, data$Clarity)
barplot(stk,main="diamond distribution by color and clarity",
        xlab="Clarity",col=c("darkblue","red"),
        legend = rownames(stk)
        
)

#line plot
plot(head(data$Carat),
     type="o",col="blue",
     main="frquency of Carat",
     xlab="carat",ylab="count")
plot(head(data$Depth),
     main="frequency of Depth",
     type="o",col="blue"
     ,xlab="Depth",ylab="count")
plot(head(data$PricePerCt),
     type="o",col="blue",
     main="PRICE PER CARAT"
     ,xlab="PRICE",ylab="count")

#pie chart
library(ggplot2)
data=read.csv("C:/Users/Manyatha/Desktop/R/Diamonds.csv")
data
ggplot(data, aes(x="", fill=Color))+
  geom_bar(width = 1)+coord_polar(theta="y")+
  labs(title="Colour of Diamonds")+
  theme(plot.title = element_text(hjust = 0.5),  
        legend.title = element_text(hjust = 0.5, face="bold", size = 10))


library(dplyr)
x <- data.frame(a=data$Color,b=1:length("Color"))
x <- x %>% 
  group_by(a) %>% #group by one or more variables
  count() %>% 
  ungroup() %>% #removes grouping
  mutate(per=`n`/sum(`n`)) %>% percentage
arrange(desc(a))
x$label <- scales::percent(x$per)
ggplot(data=x)+
  geom_bar(aes(x="", y=per, fill=a), stat="identity", width = 1)+
  coord_polar("y", start=0)+
  theme_void()+
  geom_text(aes(x=1, y = cumsum(per) - per/2, label=label))+
  labs(title="Clarity of Diamonds")+
  theme(plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(hjust = 0.5, face="bold", size = 10))

#Line chart
ggplot(data, aes(x=Carat, y=TotalPrice)) +
  geom_line(linetype="solid", color="blue", size=1.2)+
  labs(title="Carat v/s Total price")

#Bar plot
ggplot(data,aes(Color))+
  geom_bar(aes(fill = factor(Color)))+
  theme_minimal()+
  labs(title = "Bar plot to show the frequency of colors of diamonds")

#Scatter plot
ggplot(data, aes(x = Clarity, y = PricePerCt)) +
  geom_point(aes(color = factor(Clarity)))+
  labs(title = " Clarity v/s Price per carat of diamonds ")

#Mean
mean(data$TotalPrice)
mean(data$Depth)
mean(data$Carat)

#median
median(data$TotalPrice)
median(data$Depth)
median(data$Carat)

#variance
var(data$TotalPrice)
var(data$Depth)
var(data$Carat)

#Summary of the dataset
summary(data)

#Box plot
boxplot(data$Depth, horizontal=FALSE, varwidth=TRUE,
        notch=FALSE, range=0.5,
        outline=TRUE, boxwex=0.28, border=c("blue"), col=c("gold"),xlab="Depth",
        main="Boxplot to visualize depth")

#Box plot for color v/s carat
boxplot(data$Carat ~data$Color,data,notch=F,
        col=c('sienna','greenyellow','orange','gold','ivory2','plum','forestgreen'),
        main="Boxplots to show color v/s carat",
        xlab="color",ylab="carat")

#Histogram for price per carat of diamonds
hist(data$PricePerCt,col=c("lightgreen","pink"),
     border = "maroon",
     xlab = "Price per ct",
     main="Price per carat of diamonds",
     xlim=c(0,18000),ylim = c(0,140),cex.main=1.3,labels = T )

#Histogram for carat of diamonds
hist(data$Carat,col=c("gold","skyblue"),
     border = "green4",
     xlab = "Carat",
     main="Carat of diamonds",
     xlim=c(0,3.5),ylim = c(0,200),labels = T)

#Factor
#To Apply the factor function on the color column:
# Test if color is a factor
print(is.factor(data$Color))

#Apply the factor function on color
fact_data=factor(data$Color)

# Print the color column so see the levels
print(fact_data)

print(is.factor(fact_data))

#To Apply the factor function on the Clarity column:

# Test if clarity column is a factor
print(is.factor(data$Clarity))

#Apply the factor function on clarity column
fact_data=factor(data$Clarity)

# Print the clarity column so see the levels
print(fact_data)

print(is.factor(fact_data))

#dnorm()
#Let us consider a sequence of numbers from the column Price per carat of diamonds ranging from 800 to 12,000 which is incremented by 2000 with mean 'm' and std. deviation 's'.
m=mean(data$PricePerCt)
s=sd(data$PricePerCt)
x=seq(800,12000,length=2000)
y=dnorm(x,mean=m,sd=s)
plot(x,y,type="l",lwd=2,col="red",main="Normal Distribution Curve")
head(y,10)

#pnorm()
m=mean(data$PricePerCt)
s=sd(data$PricePerCt)
x=seq(800,16000,length=2000)
y=pnorm(x,mean=m,sd=s)
plot(x,y,type="l",lwd=3,col="green4",main="CDF")
head(y,10)

#qnorm()
#To compute the values of depth for 10th,60th,95th and 99th quantiles respectively having the mean 'm' and std.deviation 's'.
m=mean(data$Depth)
s=sd(data$Depth)
qnorm(c(0.10,0.60,0.95,0.99),mean=m,sd=s)

#rnorm()
#Generate 150 random numbers which are normally distributed for the depth column. We draw a histogram to show the distribution of the generated numbers
m=mean(data$Depth)
s=sd(data$Depth)
y=rnorm(150,mean=m,sd=s)
hist(y,col=terrain.colors(6),main = "Normal Distribution",ylim=c(0,60),
     border="blue",xlab = "Depth",labels = T)
tail(y,10)

#Hypothesis testing 
#Suppose the diamond merchant claims that the mean price per carat of diamonds is more than $3250. In a sample of 30 diamonds, it was found that their average was $3472.4. Assume the population standard deviation is $2895.4. At 0.05 significance level, can we reject the claim by the diamond merchant?

xbar = 3472.4  # sample mean
mu0 = 3250   # hypothesized value
sigma = 2895.4  # population standard deviation
n = 30 # sample size
z = (xbar-mu0)/(sigma/sqrt(n))
z
alpha = 0.05

#using Z test statistic approach

z.alpha = qnorm(1-alpha)
-z.alpha # critical value

if(z < -z.alpha){
  print("Rejected")
} else {
  print("Accepted")
}

#using p value approach

pval = pnorm(z)
pval

if(pval < alpha){
  print("Rejected")
} else {
  print("Accepted")
}


#Using qqnorm
x=data$PricePerCt
m=mean(x)
s=sd(x)
x1=rnorm(500,m,s)
qqnorm(x1,xlim=c(-2,2))
qqline(x1,col="red",lwd=2)




