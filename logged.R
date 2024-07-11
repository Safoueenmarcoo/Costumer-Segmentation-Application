library(tcltk)
source("functions.R")
tkdestroy(win)
logged <<- tktoplevel()
tkwm.title(logged, paste('Costumer Segmentation Program :',username_logged))
tkwm.iconbitmap(logged,"icon.ico")
tkwm.resizable(logged, F, F)
tkwm.geometry(logged, "650x600")


frm2 <- tkframe(logged, bg = "white", relief = "solid", bd = 1)
tkpack(frm2, side = "bottom", fill = "x")

statut <- tklabel(frm2, text = "", bg = "white", font = 15)
tkpack(statut, side = "right", fill = "x",expand=T)

frm <- tkframe(logged)
tkpack(frm,side="left")

data <- ttkbutton(frm, width = 25, text = "Import data", command = insert)
model <- ttkbutton(frm, width = 25, text = "Create model", command = model)
delete <- ttkbutton(frm, width = 25, text = "Delete model", command = delete)
affect <- ttkbutton(frm, width = 25, text = "Affect new clients/Products", command = affect)
categories <- ttkbutton(frm, width = 25, text = "Client categories", command = categories)
dashboard <- ttkbutton(frm, width = 25, text = "Show dashboard", command = dash)
adduser <- ttkbutton(frm, width = 25, text = "Add new user", command = add)
password <- ttkbutton(frm, width = 25, text = "Change password", command = pass)
logout <- ttkbutton(frm, width = 25, text = "Log Out", command = logout)

frm3 <- tkframe(logged, relief = "solid", bd = 1)
tkpack(frm3, side = "left", fill = "both",padx=20,pady=20)


txt1="Welcome to my Customer Segmentation Program\n\n\nPlease hover on any button for more informations\n about the application"

info <- tklabel(frm3, text = txt1, bg = "white", font = tkfont.create(family = "e", size = 10),width=80)
tkpack(info, side = "right", fill = "both",expand=T)


x=10
y=10


tkgrid(data,padx=x,pady=y,ipady=5)
tkgrid(model,padx=x,pady=y,ipady=5)
tkgrid(delete,padx=x,pady=y,ipady=5)
tkgrid(affect,padx=x,pady=y,ipady=5)
tkgrid(categories,padx=x,pady=y,ipady=5)
tkgrid(dashboard,padx=x,pady=y,ipady=5)
tkgrid(adduser,padx=x,pady=y,ipady=5)
tkgrid(password,padx=x,pady=y,ipady=5)
tkgrid(logout,padx=x,pady=y,ipady=5)




tkbind(data, "<Enter>", butoon_hover_data)
tkbind(data, "<Leave>", butoon_hover_leave_data)

tkbind(model, "<Enter>", butoon_hover_model)
tkbind(model, "<Leave>", butoon_hover_leave_model)

tkbind(delete, "<Enter>", butoon_hover_delete)
tkbind(delete, "<Leave>", butoon_hover_leave_delete)

tkbind(affect, "<Enter>", butoon_hover_affect)
tkbind(affect, "<Leave>", butoon_hover_leave_affect)

tkbind(categories, "<Enter>", butoon_hover_cat)
tkbind(categories, "<Leave>", butoon_hover_leave_cat)

tkbind(dashboard, "<Enter>", butoon_hover_dashboard)
tkbind(dashboard, "<Leave>", butoon_hover_leave_dashboard)

tkbind(adduser, "<Enter>", butoon_hover_adduser)
tkbind(adduser, "<Leave>", butoon_hover_leave_adduser)

tkbind(password, "<Enter>", butoon_hover_password)
tkbind(password, "<Leave>", butoon_hover_leave_password)

tkbind(logout, "<Enter>", butoon_hover_logout)
tkbind(logout, "<Leave>", butoon_hover_leave_logout)

