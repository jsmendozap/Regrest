#!/usr/bin/env Rscript

if("easypackages" %in% installed.packages() == F){install.packages("easypackages")}
if("RGtk2" %in% installed.packages() == F){install.packages("RGtk2", dependencies = T)}
library(easypackages)
library(RGtk2)

paquetes <- c("ggplot2", "cairoDevice")
packages(paquetes)

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

limpiar <- gtkButton("Limpiar")
gSignalConnect(limpiar, "clicked", function(widget){for(i in 0:(length(nombres)-1)){x$removeText(0); y$removeText(0)}})
vertical2$packEnd(limpiar)

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

save <- function(widget){
  dialog <- gtkFileChooserDialog("Enter a name for the file", NULL, "save", "gtk-cancel",
                                 GtkResponseType["cancel"], "gtk-save", GtkResponseType["accept"])
  if (dialog$run() == GtkResponseType["accept"]){
  dev.copy(jpeg, paste(dialog$getFilename(), ".jpeg"), width = 400, height = 400)
  dev.off()
  dialog$destroy()
  }else if(dialog$run() == GtkResponseType["cancel"]){dialog$destroy()}
}

graficar <- function(widget){
  graf <- gtkWindow(show = F)
  division <- gtkVBox(F, 5)
  graf$add(division)
  
  graphics <- gtkDrawingArea()
  asCairoDevice(graphics)
  
  division$packStart(graphics, fill = T, expand = T)
  
  # Fila 1 - Titulo y seleccion
  opciones <- gtkHBox(F, 0)
  division$packStart(opciones, fill = F, expand = F)
  opciones$`height-request` <- 20
  
  tit <- gtkLabel("Titulo:")
  tit$xpad <- 5
  opciones$packStart(tit, fill = F, expand = F)
  
  titulo <- gtkEntry()
  titulo$`width-request` <- 110
  opciones$packStart(titulo, fill = F, expand = F)
  
  resaltar <- gtkLabel("Resaltar:")
  resaltar$xpad <- 7
  opciones$packStart(resaltar, fill = F, expand = F)
  
  color <- gtkComboBoxNewText()
  for(nombre in nombres){color$appendText(nombre)}
  color$setActive(0)
  color$show()
  opciones$packStart(color, fill = F, expand = F)
  
  aplicar <- gtkButton("Aplicar")
  gSignalConnect(aplicar, "clicked", save)
  opciones$packStart(aplicar, fill = F)
  
  # Fila 2 - Etiqueta x e y
  opciones2 <- gtkHBox(F, 0)
  division$packStart(opciones2, fill = F, expand = F)
  opciones2$`height-request` <- 20
  
  etiqueta <- gtkLabel("Etiqueta x:")
  etiqueta$xpad <- 5
  opciones2$packStart(etiqueta, fill = F, expand = F)
  
  etiquetax <- gtkEntry()
  etiquetax$`width-request` <- 90
  opciones2$packStart(etiquetax, fill = F, expand = F)
  
  etiquet <- gtkLabel("Etiqueta y:")
  etiquet$xpad <- 5
  opciones2$packStart(etiquet, fill = F, expand = F)
  
  etiquetay <- gtkEntry()
  etiquetay$`width-request` <- 90
  opciones2$packStart(etiquetay, fill = F, expand = F)
  
  guardar <- gtkButton("Guardar")
  gSignalConnect(guardar, "clicked", save)
  opciones2$packStart(guardar, fill = F)
    
  graf$setDefaultSize(400,400)
  graf["border-width"] <- 7
  graf$showAll()
  
  plot(x = datos[,gtkComboBoxGetActive(x)+1],
       y = datos[,gtkComboBoxGetActive(y)+1],
       xlab = names(datos)[gtkComboBoxGetActive(x)+1],
       ylab = names(datos)[gtkComboBoxGetActive(y)+1],
       pch = 21, cex = 1, bg = "blue", font.lab = 2,
       family = "serif", col.lab = "#41B83F", bty = "L", fg = "#7DFF7D",
       main = "Gráfico de dispersión", col.main = "#1FAB1D")
}

grafico <- gtkButton("Graficar")
gSignalConnect(grafico, "clicked", graficar)
ej$packStart(grafico)

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
