list.of.packages <- c("tcltk", "RSQLite","stringr", "readr","readxl","dplyr", "lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(tcltk)
library(RSQLite)
source("functions.R")

win <<- tktoplevel()
tkwm.title(win,'Segmentation application')
tkwm.iconbitmap(win,"icon.ico")
tkwm.geometry(win, "600x700")
tkwm.resizable(win, F,F)
tkconfigure(win, bg = "white")


quit = function() {	tkdestroy(win) }
logged_in=function(){
source("logged.R")
}


validd = function(){
  username_logged<<-toString(tkget(entry1))
  password=toString(tkget(entry2))
  con <- dbConnect(SQLite(), dbname = "users.db")
  select1=paste0("select count(*) from users where username=='",username_logged,"'")
  select2=paste0("select * from users where username=='",username_logged,"'")
  if (dbGetQuery(con,select1)[[1]]==0){
    tkmessageBox(title="Login Error", message="Username not found, re-verify your informations!")
  }else if(dbGetQuery(con,select2)[[2]]!=password){
    tkmessageBox(title="Login Error", message="Wrong password, re-verify your informations!")
  }else{
    logged_in()}
}

frm2 <- tkframe(win, bg = "white", relief = "solid", bd = 1)
tkpack(frm2, side = "bottom", fill = "x")

statut <- tklabel(frm2, text = "", bg = "white", font = 15)
tkpack(statut, side = "right", fill = "x",expand=T)

title <- tklabel(win, text = "Welcome to segmentation application\nPlease login",font=tkfont.create(family = "Helvetica", size = 19), bg = "white")
tkpack(title, side = "top",pady=50)

frm <- tkframe(win, bg = "white")
tkpack(frm, expand = TRUE)

Rprof()


age <- tklabel(frm, text = "Username: ", font = 10, bg = "white")
psw <- tklabel(frm, text = "Password: ", font = 10, bg = "white")


tkgrid(age, row = 2, column = 0, pady = 10)
tkgrid(psw, row = 3, column = 0, pady = 10)


entry1 <- ttkentry(frm, width = 20, font = 5)
entry2 <- ttkentry(frm, width = 20, font = 5,show="•")


tkgrid(entry1, row = 2, column = 1, columnspan = 2)
tkgrid(entry2, row = 3, column = 1, columnspan = 2)


valid <- ttkbutton(frm, text = "Login!", command = validd, width = 12)
tkgrid(valid, row = 7, column = 1, columnspan = 1)

quit <- ttkbutton(frm, text = "Quit", command = quit, width = 12)
tkgrid(quit, row = 7, column = 2, columnspan = 1)


tkbind(entry1,"<Return>",validd)
tkbind(entry2,"<Return>",validd)

tkbind(valid, "<Enter>", butoon_hover_login)
tkbind(valid, "<Leave>", butoon_hover_leave_login)

tkbind(quit, "<Enter>", butoon_hover_quit)
tkbind(quit, "<Leave>", butoon_hover_leave_quit)
