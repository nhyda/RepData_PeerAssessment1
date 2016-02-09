# Reproducible Research: Peer Assessment 1
Feb 5th 2016  



## Loading and preprocessing the data


```r
df<-read.csv("activity.csv")
df=mutate(df,date=as.Date(date, format = '%Y-%m-%d'))
knitr::kable(head(df),align="l")
```



steps   date         interval 
------  -----------  ---------
NA      2012-10-01   0        
NA      2012-10-01   5        
NA      2012-10-01   10       
NA      2012-10-01   15       
NA      2012-10-01   20       
NA      2012-10-01   25       

## What is total number of steps taken per day?

```r
df.day_sum=df %>% group_by(date) %>% 
  summarize(total_step=sum(steps,na.rm=T),mean_step=mean(steps,na.rm=T),median_step=median(steps,na.rm=T),num_na=sum(is.na(steps))) #%>%
  #filter(num_na>0)
knitr::kable(head(df.day_sum))
```



date          total_step   mean_step   median_step   num_na
-----------  -----------  ----------  ------------  -------
2012-10-01             0          NA            NA      288
2012-10-02           126     0.43750             0        0
2012-10-03         11352    39.41667             0        0
2012-10-04         12116    42.06944             0        0
2012-10-05         13294    46.15972             0        0
2012-10-06         15420    53.54167             0        0

```r
ggplot(df.day_sum,aes(total_step))+
  geom_histogram(bins = 10)+
  theme_bw()
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

### mean and median of the total number steps per day

```r
knitr::kable(summarise(df.day_sum,mean=mean(total_step),median=median(total_step)),align="l")
```



mean      median 
--------  -------
9354.23   10395  

## What is the average daily activity pattern?

```r
df.time_sum=df %>% group_by(interval) %>% 
  summarize(mean_step=mean(steps,na.rm=T),median_step=median(steps,na.rm=T))
knitr::kable(head(df.time_sum))
```



 interval   mean_step   median_step
---------  ----------  ------------
        0   1.7169811             0
        5   0.3396226             0
       10   0.1320755             0
       15   0.1509434             0
       20   0.0754717             0
       25   2.0943396             0

```r
ggplot(df.time_sum,aes(interval,y=mean_step))+
  geom_point()+
  geom_line()+
  theme_bw()
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

###time interval that has maxium step

```r
knitr::kable(filter(df.time_sum,mean_step==max(df.time_sum$mean_step)))
```



 interval   mean_step   median_step
---------  ----------  ------------
      835    206.1698            19

## Imputing missing values
###total number of missing value

```r
#df.sum_na=data.frame(num_na=sum(is.na(df$steps)),total_num=dim(df)[1])
knitr::kable(summarise(df,num_missing=sum(is.na(steps)),num_total=length(steps)),align="l")
```



num_missing   num_total 
------------  ----------
2304          17568     

###impute missing value using the median of that interval across all days

```r
df.imput=df
for (i in unique(df.imput$interval) ){
  value=filter(df.time_sum,interval==i)$median_step[1]
  df.imput=mutate(df.imput,steps=ifelse(interval==i & is.na(steps),value,steps))
  
#  idx=which(df.imput$interval==i & is.na(df.imput$steps))
#  print(i,value,idx)
#  df.imput[idx,]$steps=value
}
df.imput.day_sum=df.imput %>% group_by(date) %>% 
  summarize(total_step=sum(steps,na.rm=T),mean_step=mean(steps),median_step=median(steps),num_na=sum(is.na(steps))) #%>%
  #filter(num_na>0)
knitr::kable(head(df.imput.day_sum))
```



date          total_step   mean_step   median_step   num_na
-----------  -----------  ----------  ------------  -------
2012-10-01          1141    3.961806             0        0
2012-10-02           126    0.437500             0        0
2012-10-03         11352   39.416667             0        0
2012-10-04         12116   42.069444             0        0
2012-10-05         13294   46.159722             0        0
2012-10-06         15420   53.541667             0        0

```r
ggplot(df.imput.day_sum,aes(total_step))+
  geom_histogram(bins=10)+
  theme_bw()
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

### mean and median of the total number steps per day

```r
knitr::kable(summarise(df.imput.day_sum,mean=mean(total_step),median=median(total_step)),align="l")
```



mean       median 
---------  -------
9503.869   10395  

###plot difference in mean value for each day
draw lines between changes before and after NA imputation

```r
df.comp.day_sum=merge(df.day_sum,df.imput.day_sum,by="date") %>%
  select(date,before_impute=total_step.x,after_impute=total_step.y)
  
  
df.comp.day_sum.rmna=gather(df.comp.day_sum,key=impute,value=steps,-date) 
#df.comp.day_sum.rmna=mutate(df.comp.day_sum.rmna,mod_steps=ifelse(is.na(steps),-1,steps))

ggplot(df.comp.day_sum.rmna,aes(date,steps,group=date))+
  geom_point(aes(color=impute,size=2,alpha=0.5)) +
  geom_line()
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

###table show dates have changed mean steps

```r
knitr::kable(filter(df.comp.day_sum,before_impute!=after_impute | is.na(before_impute)))
```



date          before_impute   after_impute
-----------  --------------  -------------
2012-10-01                0           1141
2012-10-08                0           1141
2012-11-01                0           1141
2012-11-04                0           1141
2012-11-09                0           1141
2012-11-10                0           1141
2012-11-14                0           1141
2012-11-30                0           1141

## Are there differences in activity patterns between weekdays and weekends?

```r
df.weekday=mutate(df,weekday=ifelse(weekdays(df$date) %in% c("Sunday","Saturday"),"WeekEnd","WeekDays"))
df.weekday_sum=df.weekday %>% group_by(weekday,interval) %>% 
  summarize(mean_step=mean(steps,na.rm=T),median_step=median(steps,na.rm=T))
ggplot(df.weekday_sum,aes(interval,mean_step)) + 
  facet_wrap(~weekday,nrow=2) +
  geom_line() +
  theme_bw()
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png) 

##for double check the result
something doesn't seem right that all 8 dates that have NA value imputed to the same total

```r
df.wide=spread(df,date,steps)
knitr::kable(df.wide[1:5,1:10])
```



 interval   2012-10-01   2012-10-02   2012-10-03   2012-10-04   2012-10-05   2012-10-06   2012-10-07   2012-10-08   2012-10-09
---------  -----------  -----------  -----------  -----------  -----------  -----------  -----------  -----------  -----------
        0           NA            0            0           47            0            0            0           NA            0
        5           NA            0            0            0            0            0            0           NA            0
       10           NA            0            0            0            0            0            0           NA            0
       15           NA            0            0            0            0            0            0           NA            0
       20           NA            0            0            0            0            0            0           NA            0


