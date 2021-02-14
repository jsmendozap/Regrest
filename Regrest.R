#!/usr/bin/env Rscript
if("RGtk2" %in% installed.packages() == T){
  library(RGtk2)
}else{
  install.packages("RGtk2", dependencies = T)
  library(RGtk2)
}

principal <- gtkWindow(type = "toplevel", show = F)

horizontal <- gtkHBox(T, 10)

vertical1 <- gtkVBox(F, 2)

regsim <- gtkRadioButton(NULL, "Regresión Simple")
vertical1$packStart(regsim)

regmul <- gtkRadioButton(c(NULL, regsim), "Regresión Múltiple")
vertical1$packStart(regmul)

regexp <- gtkRadioButton(c(NULL, regsim, regmul), "Regresión Exponencial")
vertical1$packStart(regexp)

regpol <- gtkRadioButton(c(NULL, regsim, regmul, regexp), "Regresión Polinómica")
vertical1$packStart(regpol)

gr <- gtkHBox(F, 0)
vertical1$add(gr)

eti <- gtkLabel("Grado Polinomio:")
eti$xpad <- 5
gr$packStart(eti, fill = F, expand = F)

grado <- gtkEntry(5)
grado$`width-request` <- 45
gtkEntrySetAlignment(grado, 0)
gr$packStart(grado, fill = F, expand = F)

boton <- gtkButton("Modelar")
reg <- function(widget){
  if (regsim$active == T){
    fm <<- as.formula(paste(colnames(datos)[gtkComboBoxGetActive(y)+1], "~", colnames(datos)[gtkComboBoxGetActive(x)+1]))
  }else if(regmul$active == T){
    total <- NULL
    for (variable in variables){
      total <- paste(total, "+", variable, sep = " ")
    }
    total <- substr(total, 4, nchar(total))
    fm <<- as.formula(paste(colnames(datos)[gtkComboBoxGetActive(y)+1], "~", total))
    variables <<- NULL
  }else if(regexp$active == T){
    fm <<- as.formula(paste(paste("log(",colnames(datos)[gtkComboBoxGetActive(y)+1], ")"), "~", paste("log(",colnames(datos)[gtkComboBoxGetActive(x)+1]), ")"))
  }else if(regpol$active == T){
    fm <<- as.formula(paste(colnames(datos)[gtkComboBoxGetActive(y)+1], "~", "poly(", colnames(datos)[gtkComboBoxGetActive(x)+1], ",", as.numeric(gtkEntryGetText(grado)),", raw = T)", sep = ""))
  }
  modelo <<- lm(fm, data = datos)
  modelo$call$formula <- fm
  print(summary(modelo))
}
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

variables <- NULL
var <- function(widget){
  variables <<- c(variables, colnames(datos)[gtkComboBoxGetActive(x)+1])
}

agregar <- gtkButton("Añadir variable")
gSignalConnect(agregar, "clicked", var)
sel$packEnd(agregar)

x <- gtkComboBoxNewText()
x$show()
sel$packStart(x, fill = F)

y <- gtkComboBoxNewText()
y$show()
sel$packStart(y, fill = F)

horizontal$packStart(vertical2)

principal$add(horizontal)

principal$set(title = "Regrest")
principal["visible"] <- T
principal["border-width"] <- 7
principal$show()
