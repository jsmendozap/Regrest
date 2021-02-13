#!/usr/bin/env Rscript
if("RGtk2" %in% installed.packages() == T){
  library(RGtk2)
}else{
  install.packages("RGtk2", depen=T)
  library(RGtk2)
}

principal <- gtkWindow(type = "toplevel", show = F)

horizontal <- gtkHBox(T, 10)

vertical1 <- gtkVBox(T, 2)
tipos <- c("Regresión Simple", "Regresión Múltiple", "Regresión Exponencial", "Regresión Polinómica")
grupo <- NULL
for (tipo in tipos){
  mod <- gtkRadioButton(grupo, tipo)
  vertical1$add(mod)
  grupo <- c(grupo, mod)
}

boton <- gtkButton("Iniciar")
reg <- function(widget){
  modelo <<- lm(datos[,gtkComboBoxGetActive(y)+1] ~ datos[,gtkComboBoxGetActive(x)+1])
  print(summary(modelo))}
gSignalConnect(boton, "clicked", reg)

vertical1$packStart(boton, fill = F)
horizontal$packStart(vertical1)

vertical2 <- gtkVBox(F, 5)
buscar <- function(widget){
  archivo <- gtkFileChooserDialog(title = "Selecciona un archivo", parent = NULL , action = "open", "gtk-ok",
                       GtkResponseType ["ok"], "gtk-cancel", GtkResponseType["cancel"])
  if (archivo$run() == GtkResponseType["ok"]){
    datos <<- read.csv2(archivo$getFilename(), sep = ";")
    nombres <<- colnames(datos)
    archivo$destroy()}
  else if (archivo$run() == GtkResponseType["cancel"]){archivo$destroy()}
  
  for(nombre in nombres){x$appendText(nombre)}
  x$setActive(0)
  
  for(nombre in nombres){y$appendText(nombre)}
  y$setActive(0)
  }

seleccion <- gtkButton("Seleccionar archivo")
gSignalConnect(seleccion, "clicked", buscar)
vertical2$packStart(seleccion, fill = F)

ejes <- gtkHBox(T, 5)
vertical2$packStart(ejes, fill = F)

sel <- gtkVBox(T, 4)
ejes$packEnd(sel, fill = F)

ej <- gtkVBox(T, 4)
ejes$packStart(ej, fill = F)

ejex <- gtkLabel("V. Dependiente:")
ej$packStart(ejex)

ejey <- gtkLabel("V. Independiente:")
ej$packStart(ejey)

limpiar <- gtkButton("Limpiar")
gSignalConnect(limpiar, "clicked", function(widget){for(i in 0:(length(nombres)-1)){x$removeText(0); y$removeText(0)}})
ej$packStart(limpiar)

agregar <- gtkButton("Añadir variable")
gSignalConnect(agregar, "clicked", function(widget){for(i in 0:(length(nombres)-1)){x$removeText(0)}})
sel$packEnd(agregar)

x <- gtkComboBoxNewText()
x$show()
sel$packStart(x, fill = F)

y <- gtkComboBoxNewText()
y$show()
sel$packStart(y, fill = F)

horizontal$packStart(vertical2)

principal$add(horizontal)

principal$setDefaultSize(300, 50)
principal$set(title = "Regrest")
principal["visible"] <- T
principal["border-width"] <- 7
principal$show()
