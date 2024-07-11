library(stringr)
directory<<-"data/"
search_pattern<<-"saved_model"
matching_files <<- list.files(directory, pattern = search_pattern, full.names = TRUE)

model_created<<-length(matching_files)!=0
inserted_data<<-length(matching_files)!=0
if (length(matching_files)!=0){
  model_load<<-readRDS(paste0(directory,"saved_model.rds"))
}
update_data=function(df){
  colnames(df)=tolower(colnames(df))
  colnames(df)[which(names(df) == "customer id")]="customerid"
  colnames(df)[which(names(df) == "invoicedate")]="date"
  df<-df[df$quantity>0 & df$price>0 & !is.na(df$customerid),]
  return(df)
  }

insert=function(){
  data_file=gsub(",", "",toString(tkgetOpenFile()))
  s=strsplit(data_file, ".",1)[[1]]
  str=s[length(s)]
  if (length(str)==0){
    tkconfigure(statut,text="no data imported!")
    return (0)
  }
  
  if (str=="csv"){
    library(readr)
    dataframe<<-read_csv(data_file)
    dataframe<<-update_data(dataframe)
    inserted_data<<-T
    tkconfigure(statut,text="data imported successfully!")
  } else if (str=="xlsx" || str=="xls"){
    library("readxl")
    dataframe<<-read_excel(data_file)
    dataframe<<-update_data(dataframe)
    inserted_data<<-T
    tkconfigure(statut,text="data imported successfully!")
  } else {
    tkmessageBox(title="File type Error", message="Invalid file type, please choose csv or xlsx file type!")
  }
}

data_verify=function(){
  if (inserted_data==F){
    tkmessageBox(title="No data imported", message="No data has been imported, please import data than create model!")
    return(0)
  }
  if (length(matching_files)!=0){
    df2<-readRDS(paste0(directory,"saved_model.rds"))$dataframe
    dataframe<<-df2}
  
  dataframe<<-update_data(dataframe)
  req_cols=data.frame(col=c("stockcode","quantity","date","price","customerid"))
  cols=data.frame(col=tolower(colnames(dataframe)))
  common_columns <- setdiff(req_cols[["col"]], cols[["col"]])
  if (length(common_columns)==0){
    if (!inherits(dataframe[["date"]], "POSIXct") && class(dataframe$date)!="Date"){
      tkmessageBox(title="Column type Error", message="Invalid date column type, please change column type to Date type!")
      return(F)
    }
    else if (typeof(dataframe$quantity)!="double"){
      tkmessageBox(title="Column type Error", message="Invalid quantity column type, please change column type to double type!")
      return(F)      
    }
     else if (typeof(dataframe$price)!="double"){
      tkmessageBox(title="Column type Error", message="Invalid price column type, please change column type to double type!")
      return(F)      
     }
    else return (T)
  } else{
    ch=common_columns[1]
    for (i in common_columns[-1]){
      ch=paste(ch,i,sep=", ")}
    tkmessageBox(title="Missing columns", message=paste("Columns: ",ch,"are missing!, please verify their existence or column names!"))
    return (F)
  }
  
}
selection <- function() {
  recency_coeff<<-as.integer(toString(tkget(recency_entry)))
  frequency_coeff<<-as.integer(toString(tkget(frequency_entry)))
  revenue_coeff<<-as.integer(toString(tkget(revenue_entry)))
  if(tclvalue(tkget(recency_entry))==""){
    recency_coeff<<-1
  }
  if (tclvalue(tkget(frequency_entry))==""){
    frequency_coeff<<-1
  }
  if (tclvalue(tkget(revenue_entry))==""){
    revenue_coeff<<-1
  }
  if(is.na(recency_coeff) || is.na(frequency_coeff) || is.na(revenue_coeff)){
    tkmessageBox(title="Coefficient type Error", message="Invalid coefficient type, please enter integer type!")
    return (0)
  } else if (recency_coeff<0 || frequency_coeff<0 || revenue_coeff<0){
    tkmessageBox(title="Coefficient value Error", message="Invalid coefficient value, please enter positive integer value!")
    return(0)
  }
  selected_value <- tclvalue(radio_var)
  if(selected_value==1){
    variable<<-"customerid"
  }
  else{
    variable<<-"stockcode"
  }
  if (data_verify()==T){
    source("app.R")
    tkmessageBox(title="Model created successfully", message=paste("The model has been created with accuracy around",accuracy,"%"))
    model_created<<-T}
}

model=function(){
  matching_files <<- list.files(directory, pattern = search_pattern, full.names = TRUE)
  if(length(matching_files)!=0){
    m<<-readRDS(paste0(directory,"saved_model.rds"))$m}
  else if (inserted_data==F){
    tkmessageBox(title="No data imported", message="No data has been imported, please import data than create model!")
    return(0)
  }

  model_win<<-tktoplevel()
  tkwm.iconbitmap(model_win,"icon.ico")
  tkwm.title(model_win, 'Choose model type')
  tkwm.geometry(model_win,"400x400")
  tkwm.resizable(model_win, T, T)
  radio_var <<- tclVar(1)  
  frame=tkframe(model_win)
  tkpack(frame,expand=T)
  
  recency_label=ttklabel(frame,text="Recency coefficient: ")
  recency_entry<<-ttkentry(frame)  
  frequency_label=ttklabel(frame,text="Frequency coefficient: ")
  frequency_entry<<-ttkentry(frame)
  revenue_label=ttklabel(frame,text="Revenue coefficient: ")
  revenue_entry<<-ttkentry(frame)
  
  tkgrid(recency_label, row = 2, column = 0, pady = 10)
  tkgrid(frequency_label, row = 3, column = 0, pady = 10)
  tkgrid(revenue_label, row = 4, column = 0, pady = 10)
  tkgrid(recency_entry, row = 2, column = 1, pady = 10)
  tkgrid(frequency_entry, row = 3, column = 1, pady = 10)
  tkgrid(revenue_entry, row = 4, column = 1, pady = 10)

  tkbind(recency_entry,"<Return>",selection)
  tkbind(frequency_entry,"<Return>",selection)
  tkbind(revenue_entry,"<Return>",selection)
  
  radio_button1 <<- ttkradiobutton(frame, text = "Costumer segmentation", variable = radio_var, value = 1)
  radio_button2 <<- ttkradiobutton(frame, text = "Product segmentation", variable = radio_var, value = 2)
  tkgrid(radio_button1,row=5,column=0,columnspan=2, pady = 10)
  tkgrid(radio_button2,row=6,column=0,columnspan=2, pady = 10)

  
  
  model_button <- ttkbutton(frame, text = "Create model", command = selection)
  tkgrid(model_button,row=7,column=0,columnspan=2,pady=20,ipady=5)

  frm2 <- tkframe(model_win, bg = "white", relief = "solid", bd = 1)
  tkpack(frm2, side = "bottom", fill = "x")
  
  statut2 <<- tklabel(frm2, text = "", bg = "white", font = 15)
  tkpack(statut2, side = "right", fill = "x",expand=T)
  
  tkbind(radio_button1, "<Enter>", butoon_hover_radio1)
  tkbind(radio_button1, "<Leave>", butoon_hover_leave_radio1)
  
  tkbind(radio_button2, "<Enter>", butoon_hover_radio2)
  tkbind(radio_button2, "<Leave>", butoon_hover_leave_radio2)
  
  tkbind(model_button, "<Enter>", butoon_hover_model_button)
  tkbind(model_button, "<Leave>", butoon_hover_leave_model_button)
  
  tkbind(radio_button1,"<Return>",selection)
  tkbind(radio_button2,"<Return>",selection)
}

remove_model=function(){
  matching_files <- list.files(directory, pattern = search_pattern, full.names = TRUE)
  if(length(matching_files)==0){
    tkmessageBox(title="No model created", message="Create the model first please!")
  } else {
    unlink(paste0(directory,"saved_model.rds"))
    tkmessageBox(title="Model deleted", message="The model has been deleted, make sure to create new model!")
    tkdestroy(delwin)
    rm(dataframe)
    rm(df2)
    rm(model_load)
    added_rows<<-0
    added_customers<<-0
    added_products<<-0
    model_created<<-F
    inserted_data<<-F
  }
}
closee=function(){
  tkdestroy(delwin)
}
delete=function(){
  if(model_created){
  delwin<<-tktoplevel()
  tkwm.iconbitmap(delwin,"icon.ico")
  tkwm.title(delwin, 'Delete model')
  tkwm.resizable(delwin, F, F)
  tkwm.geometry(delwin, "300x170")
  framee=tkframe(delwin)
  tkpack(framee,expand=T)
  message=tklabel(framee,text="are you sure you want to delete the saved model?")
  tkgrid(message,row=1,column=1,columnspan=2,pady=40)
  cancel=ttkbutton(framee, text = "Cancel", command = closee)
  confirm=ttkbutton(framee, text = "Confirm", command = remove_model)
  tkgrid(cancel,row=2,column=2)
  tkgrid(confirm,row=2,column=1)
  }else{
    tkmessageBox(title="No data imported", message="No data has been imported, please import data than create model!")
  }
}
data_verify2=function(df){
  req_cols=data.frame(col=c("stockcode","quantity","date","price","customerid"))
  cols=data.frame(col=tolower(colnames(df)))
  common_columns <- setdiff(req_cols[["col"]], cols[["col"]])
  if (length(common_columns)==0){
    if (!inherits(df[["date"]], "POSIXct") && class(df$date)!="Date"){
      tkmessageBox(title="Column type Error", message="Invalid date column type, please change column type to Date type!")
      return(F)
    }
    else if (typeof(df$quantity)!="double"){
      tkmessageBox(title="Column type Error", message="Invalid quantity column type, please change column type to double type!")
      return(F)      
    }
    else if (typeof(df$price)!="double"){
      tkmessageBox(title="Column type Error", message="Invalid price column type, please change column type to double type!")
      return(F)      
    }
    else return (T)
  } else{
    ch=common_columns[1]
    for (i in common_columns[-1]){
      ch=paste(ch,i,sep=", ")}
    tkmessageBox(title="Missing columns", message=paste("Columns: ",ch,"are missing!, please verify their existence or column names!"))
    return (F)
  }
}
affect=function(){
  if (inserted_data==F){
    tkmessageBox(title="No data imported", message="No data has been imported, please import data than create model!")
    return (0)
  }
  if(model_created==F){
    tkmessageBox(title="No model created", message="Create the model first please!")
    return (0)    
  }
  data_file2=gsub(",", "",toString(tkgetOpenFile()))
  s=strsplit(data_file2, ".",1)[[1]]
  str=s[length(s)]
  if (length(str)==0){
    tkconfigure(statut,text="no data imported!")
    return(0)
  }
  if (str=="csv"){
    library("readr")
    df2<<-read_csv(data_file2)
    df2<<-update_data(df2)
    tkconfigure(statut,text="data imported successfully!")
  } else if (str=="xlsx" || str=="xls"){
    library("readxl")
    df2<<-read_excel(data_file2,sheet=1)
    df2<<-update_data(df2)
    tkconfigure(statut,text="data imported successfully!")}
  if(data_verify2(df2)){
    model_load<-readRDS(paste0(directory,"saved_model.rds"))
    added_rows<-model_load$added_rows+nrow(df2)
    model_load$dataframe=rbind(model_load$dataframe,df2)
    added_customers<-model_load$added_customers+length(setdiff(unique(df2$customerid),unique(dataframe$customerid)))
    added_products<-model_load$added_products+length(setdiff(unique(df2$stockcode),unique(dataframe$stockcode)))
    
    model_load$added_rows=added_rows
    model_load$added_customers=added_customers
    model_load$added_products=added_products
    model_load$customers_number=model_load$customers_number+added_customers
    model_load$products_number=model_load$products_number+added_products
    
    saveRDS(model_load,paste0(directory,"saved_model.rds"))
    model_load<<-readRDS(paste0(directory,"saved_model.rds"))
    source("C:/Users/Safoueen/Desktop/application/app.R")
    model_created<<-T
    tkmessageBox(title="Data updated", message="Data and model have been updated successfully!")
  }
}
categories=function(){
  if (inserted_data==F){
    tkmessageBox(title="No data imported", message="No data has been imported, please import data than create model!")
    return (0)
  }
  if(model_created==F){
    tkmessageBox(title="No model created", message="Create the model first please!")
    return (0)    
  }
  categories_win<<-tktoplevel()
  tkwm.iconbitmap(categories_win,"icon.ico")
  tkwm.title(categories_win, 'Categories')
  tkwm.resizable(categories_win, T, T)
  model_load<<-readRDS(paste0(directory,"saved_model.rds"))
  data=model_load$clusters_data
  data_low=data[data$state=="low",]
  txt=ifelse(model_load$variable=="customerid","customers","products")
  low_message=paste0("\nLow category:\n\nMean of recency: ",round(mean(data_low[["recency"]])),
                     "\nMean of frequency: ",round(mean(data_low[["frequency"]])),
                     "\nMean of spending: ",round(mean(data_low[["spent"]]),2),
                     "\nnumber of ",txt,": ",nrow(data_low),
                     "\nRange of scores: ",min(data_low$score)," - ",max(data_low$score)
                     )
  
  data_medium=data[data$state=="medium",]
  medium_message=paste0("Medium category:\n\nMean of recency: ",round(mean(data_medium[["recency"]]),2),
                     "\nMean of frequency: ",round(mean(data_medium[["frequency"]])),
                     "\nMean of spending: ",round(mean(data_medium[["spent"]]),2),
                     "\nnumber of ",txt,": ",nrow(data_medium),
                     "\nRange of scores: ",min(data_medium$score)," - ",max(data_medium$score)
  )
  
  data_high=data[data$state=="high",]
  high_message=paste0("High category:\n\nMean of recency: ",round(mean(data_high[["recency"]])),
                     "\nMean of frequency: ",round(mean(data_high[["frequency"]])),
                     "\nMean of spending: ",round(mean(data_high[["spent"]]),2),
                     "\nnumber of ",txt,": ",nrow(data_high),
                     "\nRange of scores: ",min(data_high$score)," - ",max(data_high$score)
  )
  
  
  closefct=function(){tkdestroy(categories_win)}
  data_label=tklabel(categories_win,text=low_message,font=tkfont.create(family = "Helvetica", size = 12), justify = "left")
  tkgrid(data_label,row=1,padx=60,pady=10)
  data_label2=tklabel(categories_win,text=medium_message,font=tkfont.create(family = "Helvetica", size = 12), justify = "left")
  tkgrid(data_label2,row=2,padx=10,pady=10)
  data_label3=tklabel(categories_win,text=high_message,font=tkfont.create(family = "Helvetica", size = 12), justify = "left")
  tkgrid(data_label3,row=3,padx=10,pady=10)
  close=ttkbutton(categories_win,text="Close",command=closefct)
  tkgrid(close,row=4,padx=10,pady=40)
  
}
dash=function(){
  if (inserted_data==F){
    tkmessageBox(title="No data imported", message="No data has been imported, please import data than create model!")
    return (0)
  }
  if(model_created==F){
    tkmessageBox(title="No model created", message="Create the model first please!")
    return (0)    
  }
  dash_win<<-tktoplevel()
  tkwm.iconbitmap(dash_win,"icon.ico")
  tkwm.title(dash_win, 'Dashboard')
  tkwm.resizable(dash_win, T, T)
  s=strsplit(toString(model_load$m$call),split="~",1)[[1]]
  s=strsplit(s[2],split=",",1)[[1]]
  s=strsplit(s[1],split="+ ",1)
  s=sapply(s, str_trim)
  k=""

  model_load<<-readRDS(paste0(directory,"saved_model.rds"))
  coefs=c( model_load$recency_coeff,model_load$frequency_coeff,model_load$revenue_coeff,model_load$country_coeff)
  for (i in 1:length(s)){k=paste(k,"-",s[i],"(",coefs[i],")","\n\t\t")}
  
  txt=ifelse(model_load$variable=="customerid","spending Costumer code","bought product code")
  message_text<<-paste0("Data:\n\nNumber of transactions: ",nrow(model_load$dataframe),
                        "\nNumber of customers: ",model_load$customers_number,
                        "\nNumber of products: ",model_load$products_number,
                        "\nMinimum product price: ",min(model_load$dataframe$price),
                        "\nMaximum product price: ",max(model_load$dataframe$price),
                        "\nMost ",txt,": ",toString(model_load$revenue_costumers[model_load$revenue_costumers$spent==max(model_load$revenue_costumers$spent),model_load$variable]),
                        "\nLeast ",txt,": ",toString(model_load$revenue_costumers[model_load$revenue_costumers$spent==min(model_load$revenue_costumers$spent),model_load$variable])
                        )
  message_text2<<-paste0("Model:\n\nModel type: ",ifelse(model_load$variable=="customerid","Customer segmentation\n","Product segmentation\n"),
                         "Model accuracy: ",model_load$accuracy," %\n",
                         "Used caracteristics:  ",k,"\n",
                         "Added data: ",model_load$added_rows,"\n",
                         "Added customers: ",model_load$added_customers,"\n",
                         "Added products: ",model_load$added_products
                         )
  closefct=function(){tkdestroy(dash_win)}
  data_label=tklabel(dash_win,text=message_text,font=tkfont.create(family = "Helvetica", size = 12), justify = "left")
  tkgrid(data_label,row=1,padx=10,pady=10)
  data_label2=tklabel(dash_win,text=message_text2,font=tkfont.create(family = "Helvetica", size = 12), justify = "left")
  tkgrid(data_label2,row=2,padx=10,pady=10)
  close=ttkbutton(dash_win,text="Close",command=closefct)
  tkgrid(close,row=3,padx=10,pady=10)
}

verify_user=function(){
  con <- dbConnect(SQLite(), dbname = "C:/Users/Safoueen/Desktop/application/users.db")
  select1=paste0("select count(*) from users where username=='",toString(tkget(user)),"'")
  select2=paste0("select * from users where username=='",username_logged,"'")
  if (dbGetQuery(con,select1)[[1]]!=0){
    tkmessageBox(title="Username existed", message="This username is already existed!")
  } else if ((dbGetQuery(con,select2)[[2]]!=toString(tkget(password_entry)))){
    tkmessageBox(title="Wrong password", message="You entred wrong password!")
  }else{
    user_add=paste0("insert into users values('",toString(tkget(user)),"','",toString(tkget(userpassword)),"')")
    dbExecute(con, user_add)
    tkmessageBox(title="User added", message="User added successfully!")
    tkdestroy(add_user)
  }
}


add=function(){
  add_user<<-tktoplevel()
  tkwm.iconbitmap(add_user,"icon.ico")
  tkwm.title(add_user,'Add new user')
  tkwm.resizable(add_user,T,T)
  tkwm.geometry(add_user,"300x300")
  user_frame=tkframe(add_user)
  tkpack(user_frame,expand=T)
  username=ttklabel(user_frame,text="Username: ")
  user<<-ttkentry(user_frame,width=30)  
  user_label=ttklabel(user_frame,text="User Password: ")
  userpassword<<-ttkentry(user_frame,width=30,show="•")
  password=ttklabel(user_frame,text="Your Password: ")
  password_entry<<-ttkentry(user_frame,width=30,show="•")
  change=ttkbutton(user_frame,text="Add user!",command=verify_user,width=20)
  tkgrid(username, row = 2, column = 0, pady = 10)
  tkgrid(user_label, row = 3, column = 0, pady = 10)
  tkgrid(password, row = 4, column = 0, pady = 10)
  tkgrid(user, row = 2, column = 1, pady = 10)
  tkgrid(userpassword, row = 3, column = 1, pady = 10)
  tkgrid(password_entry, row = 4, column = 1, pady = 10)
  tkgrid(change,sticky="s",columnspan=2)
  tkbind(user,"<Return>",verify_user)
  tkbind(password,"<Return>",verify_user)
  tkbind(password_entry,"<Return>",verify_user)
}
pass=function(){
  passw<<-tktoplevel()
  tkwm.iconbitmap(passw,"icon.ico")
  tkwm.title(passw,'Change password')
  tkwm.resizable(passw,T,T)
  tkwm.geometry(passw,"300x300")
  passwd=tkframe(passw)
  tkpack(passwd,expand=T)
  password_label_old=ttklabel(passwd,text="Old Password: ")
  password_entry_old<<-ttkentry(passwd,width=30,show="•")
  password_label_new=ttklabel(passwd,text="New Password: ")
  password_entry_new<<-ttkentry(passwd,width=30,show="•")
  change=ttkbutton(passwd,text="Change password!",command=change_password,width=20)
  tkgrid(password_label_old, row = 2, column = 0, pady = 10)
  tkgrid(password_label_new, row = 3, column = 0, pady = 10)
  tkgrid(password_entry_old, row = 2, column = 1, pady = 10)
  tkgrid(password_entry_new, row = 3, column = 1, pady = 10)
  tkgrid(change,sticky="s",columnspan=2)
  tkbind(password_entry_old,"<Return>",change_password)
  tkbind(password_entry_new,"<Return>",change_password)
}

change_password=function(){
  password=toString(tkget(password_entry_old))
  con <- dbConnect(SQLite(), dbname = "C:/Users/Safoueen/Desktop/application/users.db")
  select=paste0("select * from users where username=='",username_logged,"'")
  if(dbGetQuery(con,select)[[2]]==password){
    new=toString(tkget(password_entry_new))
    update=paste0("update users set password='",new,"' where username=='",username_logged,"'")
    dbExecute(con,update)
    tkdelete(password_entry_old,0,"end")
    tkdelete(password_entry_new,0,"end")
    tkmessageBox(title="Password Changed!", message="Password changed successfully!")
    tkdestroy(passw)
  } else {
    tkmessageBox(title="Password Error", message="Invalid entry, re-verify your password!")
  }
}

logout=function(){
  tkdestroy(logged)
  source("C:/Users/Safoueen/Desktop/application/register in R.R")
}


butoon_hover_login <- function(event) {
  tkconfigure(statut,text="Click to login")
}

butoon_hover_leave_login <- function(event) {
 
  tkconfigure(statut,text="")
}

butoon_hover_quit <- function(event) {
  tkconfigure(statut,text="Click to quit")
}

butoon_hover_leave_quit <- function(event) {
  tkconfigure(statut,text="")
}

butoon_hover_data <- function(event) {
  txt="This is the first part of the segmentaion,\nYou need to insert your data but it must follow those requirements:\n- The data must have those columns:
  \t- stockcode: \n\t- quantity \n\t- date \n\t- price \n\t- customerid\n\t- country (optional)\n- The date column must be Date type\n- Quantity and price columns must be integer type."
  tkconfigure(info,text=txt,justify = "left")
  tkconfigure(statut,text="Click to insert data")
}

butoon_hover_leave_data <- function(event) {
  tkconfigure(info,text="Please hover on any other button for more informations \nabout its the fonctionality",justify="center")
  tkconfigure(statut,text="")
}

butoon_hover_model <- function(event) {
  txt="Here where you could create the model, but you need\nto import the data first.\n\nTo create the model, you need to add some serveral informations:\n
  \t- Recency coefficient: the weight of the recency of purchases
  \t- Frequency coefficient: the weight of the frequency\n\tof purchasing
  \t- Revenue coefficient: the weight of the spending
  \t- Segmentation type: There is two type of segmentation:
  \t\t- Customer's segmentation
  \t\t- Product's segmentation\nNB:\nThe model is saved and you do not need to re-create new model\nuntil you delete the actual one."
  tkconfigure(info,text=txt,justify = "left")
  tkconfigure(statut,text="Click to create the model")
}

butoon_hover_leave_model <- function(event) {
  tkconfigure(info,text="Please hover on any other button for more informations \nabout its the fonctionality",justify="center")
  tkconfigure(statut,text="")
}

butoon_hover_delete <- function(event) {
  txt="You can delete the created model and all the saved data will be lost\nand you need to re-import data and re-create new model."
  tkconfigure(info,text=txt,justify = "left")
  tkconfigure(statut,text="Click to delete the model")
}

butoon_hover_leave_delete <- function(event) {
  tkconfigure(info,text="Please hover on any other button for more informations \nabout its the fonctionality",justify="center")
  tkconfigure(statut,text="")
}

butoon_hover_affect <- function(event) {
  txt="You could add new data and new customers to the actual data\nand actual model and it has the same requirements as importing data"
  tkconfigure(info,text=txt,justify = "left")
  tkconfigure(statut,text="Click to affect new clients to a category")
}

butoon_hover_leave_affect <- function(event) {
  tkconfigure(info,text="Please hover on any other button for more informations \nabout its the fonctionality",justify="center")
  tkconfigure(statut,text="")
}

butoon_hover_cat <- function(event) {
  txt="You could check some informations about the distribution of categories\nof customers / products.
But it works only after importing the data and creating the model."
  tkconfigure(info,text=txt,justify = "left")
  tkconfigure(statut,text="Click to show the client's categories")
}

butoon_hover_leave_cat <- function(event) {
  tkconfigure(info,text="Please hover on any other button for more informations \nabout its the fonctionality",justify="center")
  tkconfigure(statut,text="")
}

butoon_hover_dashboard <- function(event) {
  txt="This section gives you informations about the Initial data and the created
model.
But it works only after importing the data and creating the model."
  tkconfigure(info,text=txt,justify = "left")
  tkconfigure(statut,text="Click to show the dashboard")
}

butoon_hover_leave_dashboard <- function(event) {
  tkconfigure(info,text="Please hover on any other button for more informations \nabout its the fonctionality",justify="center")
  tkconfigure(statut,text="")
}

butoon_hover_adduser <- function(event) {
  txt="You could also add new users to the application and give them
the access.
This could be done is this section."
  tkconfigure(info,text=txt,justify = "left")
  tkconfigure(statut,text="Click to add new user of the application")
}

butoon_hover_leave_adduser <- function(event) {
  tkconfigure(info,text="Please hover on any other button for more informations \nabout its the fonctionality",justify="center")
  tkconfigure(statut,text="")
}

butoon_hover_password <- function(event) {
  txt="Passwords can be updated anytime, your old password must
be provided."
  tkconfigure(info,text=txt,justify = "left")
  tkconfigure(statut,text="Click to change password")
}

butoon_hover_leave_password <- function(event) {
  tkconfigure(info,text="Please hover on any other button for more informations \nabout its the fonctionality",justify="center")
  tkconfigure(statut,text="")
}

butoon_hover_logout <- function(event) {
  txt="Here you can log out of your session and redict you back to the
login page."
  tkconfigure(info,text=txt,justify = "left")
  tkconfigure(statut,text="Click to logout")
}

butoon_hover_leave_logout <- function(event) {
  tkconfigure(info,text="Please hover on any other button for more informations \nabout its the fonctionality",justify="center")
  tkconfigure(statut,text="")
}

butoon_hover_radio1 <- function(event) {
  tkconfigure(statut2,text="Click to choose customer segmentation")
}

butoon_hover_leave_radio1 <- function(event) {
  tkconfigure(info,text="Please hover on any other button for more informations \nabout its the fonctionality",justify="center")
  tkconfigure(statut2,text="")
}

butoon_hover_radio2 <- function(event) {
  tkconfigure(statut2,text="Click to choose product segmentation")
}

butoon_hover_leave_radio2 <- function(event) {
  tkconfigure(info,text="Please hover on any other button for more informations \nabout its the fonctionality",justify="center")
  tkconfigure(statut2,text="")
}

butoon_hover_model_button <- function(event) {
  tkconfigure(statut2,text="Click to confirm your choice")
}

butoon_hover_leave_model_button <- function(event) {
  tkconfigure(info,text="Please hover on any other button for more informations \nabout its the fonctionality",justify="center")
  tkconfigure(statut2,text="")
}