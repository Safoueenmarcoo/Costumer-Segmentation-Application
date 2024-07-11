options(warn=-1)
library("dplyr")
library("lubridate")


options(warn=0)
set.seed(5)

if (length(matching_files)!=0){
  dataframe<<-readRDS(paste0(directory,"saved_model.rds"))$dataframe
}


customers_number<<-length(unique(dataframe$customerid))
products_number<<-length(unique(dataframe$stockcode))


create_data<<-function(dff,variable){
library("dplyr")
  colnames(dff)=tolower(colnames(dff))
  colnames(dff)[which(names(dff) == "customer id")]="customerid"
  colnames(dff)[which(names(dff) == "invoicedate")]="date"
recency_costumers=dff%>%
  group_by(dff[[variable]])%>%
  summarize(maxpurchase=mean(date))%>%
  mutate(recency=as.numeric(round(difftime(max(maxpurchase),maxpurchase,units="days"))))%>%
  select(-maxpurchase)
colnames(recency_costumers)=c(variable,"recency")

c=sort(kmeans(recency_costumers$recency,centers=4)$centers,decreasing=T)
classes=kmeans(recency_costumers$recency,centers=c)$cluster-1
recency_costumers=cbind(recency_costumers,classes)

freq_costumers=dff%>%
  group_by(dff[[variable]])%>%
  summarize(frequency=n())

c=sort(kmeans(freq_costumers$frequency,centers=4)$centers)
classes=kmeans(freq_costumers$frequency,centers=c)$cluster-1
freq_costumers=cbind(freq_costumers,classes)

revenue_costumers=dff%>%
  mutate(revenue=quantity*price)%>%
  group_by(dff[[variable]])%>%
  summarize(spent=sum(revenue))

c=sort(kmeans(revenue_costumers$spent,centers=4)$centers)
classes=kmeans(revenue_costumers$spent,centers=c)$cluster-1

colnames(revenue_costumers)=c(variable,"spent")
revenue_costumers=cbind(revenue_costumers,classes)
revenue_costumers<<-revenue_costumers

clusters_data=cbind(recency_costumers,freq_costumers$frequency,freq_costumers$classes,revenue_costumers$spent,revenue_costumers$classes)

if (variable=="customerid"){
  if (!(which(names(dff) == "country")!=0)){
    

clusters_data=merge(unique(dff["customerid"]),clusters_data,by="customerid")

colnames(clusters_data)=c("customerid","recency","recency_score","frequency","frequency_score","spent","revenue_score")

clusters_data=data.frame(clusters_data)
}else{
  clusters_data=merge(unique(dff[c("customerid","country")]),clusters_data,by="customerid")
  
  colnames(clusters_data)=c("customerid","country","recency","recency_score","frequency","frequency_score","spent","revenue_score")
  
  clusters_data=data.frame(clusters_data)
  clusters_data$country=as.factor(clusters_data$country)
}} else{
  colnames(clusters_data)=c("stockcode","recency","recency_score","frequency","frequency_score","spent","revenue_score")
  clusters_data=data.frame(clusters_data)
}
country_coeff<<-1

clusters_data=clusters_data%>%mutate(score=recency_coeff * recency_score + frequency_coeff * frequency_score + revenue_coeff * revenue_score)
return (clusters_data)
}

clusters_data=create_data(dataframe,variable)

clusters_data=clusters_data%>%mutate(state=as.factor(ifelse(clusters_data$score<round(max(score)/3),"low",ifelse(clusters_data$score<2*round(max(score)/3),"medium","high"))))

x=sample(c(1:nrow(clusters_data)),round(nrow(clusters_data)*0.75))

df_train=clusters_data[x,]
df_test=clusters_data[-x,]

if (which(names(dataframe) == "country")!=0 && variable=="customerid"){
  m=lm(score~recency+frequency+spent+country,data=df_train)
  m$xlevels[["country"]] = union(m$xlevels[["country"]], levels(df_test$country))
  
}else{
  m=lm(score~recency+frequency+spent,data=df_train)}


tt=cbind(df_test,pred=predict(m,df_test))
tt$pred_state=as.factor(ifelse(tt$pred<round(max(tt$score)/3),"low",ifelse(tt$pred<2*round(max(tt$score)/3),"medium","high")))


d=data.frame(matrix(nrow=1,ncol=2))
colnames(d)=c("value","accuracy")
for (i in 0:250){
  k=1+i/10
  tt$pred_state=as.factor(ifelse(tt$pred<k,"low",ifelse(tt$pred<2*round(max(tt$score)/3),"medium","high")))
  t=table(tt$state,tt$pred_state)
  acc=sum(diag(t))/sum(t)
  d[i+1,1]=k
  d[i+1,2]=acc
}
max_low=sample_n(d[d$accuracy==max(d$accuracy),],1)$value
tt$pred_state=as.factor(ifelse(tt$pred<max_low,"low",ifelse(tt$pred<6,"medium","high")))




d=data.frame(matrix(nrow=1,ncol=2))
colnames(d)=c("value","accuracy")
for (i in 0:250){
  k=5+i/10
  tt$pred_state=as.factor(ifelse(tt$pred<max_low,"low",ifelse(tt$pred<k,"medium","high")))
  t=table(tt$state,tt$pred_state)
  acc=sum(diag(t))/sum(t)
  d[i+1,1]=k
  d[i+1,2]=acc
}
max_high=sample_n(d[d$accuracy==max(d$accuracy),],1)$value
tt$pred_state=as.factor(ifelse(tt$pred<max_low,"low",ifelse(tt$pred<max_high,"medium","high")))

t=table(tt$state,tt$pred_state)
accuracy<<-round(sum(diag(t))/sum(t)*100,2)
directory<<-"data/"
search_pattern<<-"saved_model"
matching_files <<- list.files(directory, pattern = search_pattern, full.names = TRUE)
if(length(matching_files)==0){
saveRDS(list(m=m,
             variable=variable,
             dataframe=dataframe,
             recency_coeff=recency_coeff,
             frequency_coeff=frequency_coeff,
             revenue_coeff=revenue_coeff,
             country_coeff=country_coeff,
             customers_number=customers_number,
             products_number=products_number,
             revenue_costumers=revenue_costumers,
             clusters_data=clusters_data,
             accuracy=accuracy,
             added_rows=0,
             added_customers=0,
             added_products=0
             ),paste0(directory,"saved_model.rds"))
}else{
  model_load$m=m
  model_load$variable=variable
  model_load$recency_coeff=recency_coeff
  model_load$frequency_coeff=frequency_coeff
  model_load$revenue_coeff=revenue_coeff
  model_load$country_coeff=country_coeff
  model_load$revenue_costumers=revenue_costumers
  model_load$clusters_data=clusters_data
  model_load$accuracy=accuracy
  if(is.null(model_load$added_rows)){
  model_load$added_rows=added_rows
  model_load$added_customers=added_customers
  model_load$added_products=added_products
  saveRDS(model_load,paste0(directory,"saved_model.rds"))} 
  saveRDS(model_load,paste0(directory,"saved_model.rds"))
}


tkdestroy(model_win)



