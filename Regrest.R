if("easypackages" %in% installed.packages() == F){install.packages("easypackages")}
if("RGtk2" %in% installed.packages() == F){install.packages("RGtk2", dependencies = T)}
library(easypackages)
library(RGtk2)

paquetes <- c("cairoDevice", "randomcoloR")
packages(paquetes)

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

buscar <- function(widget){
  if(.Platform$OS.type == "windows"){
    datos <<- read.csv2(file = choose.files(), sep = ifelse(nchar(gtkEntryGetText(separador)) == 0, ";", gtkEntryGetText(separador)))
  }else{
    archivo <- gtkFileChooserDialog(title = "Selecciona un archivo", parent = NULL , action = "open", "gtk-ok",
                                    GtkResponseType ["ok"], "gtk-cancel", GtkResponseType["cancel"])
    if (archivo$run() == GtkResponseType["ok"]){
      datos <<- read.csv2(archivo$getFilename(), sep = ifelse(nchar(gtkEntryGetText(separador)) == 0, ";", gtkEntryGetText(separador)))
      archivo$destroy()}
    else if (archivo$run() == GtkResponseType["cancel"]){archivo$destroy()} 
  }
  
  nombres <<- colnames(datos)
  for(nombre in nombres){x$appendText(nombre)}
  x$setActive(0)
  
  for(nombre in nombres){y$appendText(nombre)}
  y$setActive(0)
}

save <- function(widget){
  if(.Platform$OS.type == "windows"){
    dev.copy(jpeg, paste(choose.dir(), "\\grafico.jpeg", sep = ""))
    dev.off()
  }else{
    dialog <- gtkFileChooserDialog("Enter a name for the file", NULL, "save", "gtk-cancel",
                                   GtkResponseType["cancel"], "gtk-save", GtkResponseType["accept"])
    if (dialog$run() == GtkResponseType["accept"]){
      dev.copy(jpeg, paste(dialog$getFilename(), ".jpeg"), width = 400, height = 400)
      dev.off()
      dialog$destroy()
    }else if(dialog$run() == GtkResponseType["cancel"]){dialog$destroy()}
  }
}

g <- function(etx, ety, titul, color = NULL, back = "blue"){
  options(warn = -1)
  plot(x = datos[,gtkComboBoxGetActive(x)+1],
       y = datos[,gtkComboBoxGetActive(y)+1],
       xlab = etx, ylab = ety,
       pch = 21, cex = 1, bg = back, font.lab = 2, col = color,
       family = "serif", col.lab = "#41B83F", bty = "L", fg = "#7DFF7D",
       main = titul, col.main = "#1FAB1D")
  options(warn = 0)
}

graficar <- function(widget){
  graf <- gtkWindow(show = F)
  division <- gtkVBox(F, 5)
  graf$add(division)
  
  graphics <- gtkDrawingArea()
  asCairoDevice(graphics)
  
  division$packStart(graphics, fill = T, expand = T)
  
  aplibot <- function(widget){
    title <- ifelse(nchar(gtkEntryGetText(titulo)) == 0,
                    "Gráfico de dispersión", gtkEntryGetText(titulo))
    labx <- ifelse(nchar(gtkEntryGetText(etiquetax)) == 0,
                   names(datos)[gtkComboBoxGetActive(x)+1],
                   gtkEntryGetText(etiquetax))
    laby <- ifelse(nchar(gtkEntryGetText(etiquetay)) == 0,
                   names(datos)[gtkComboBoxGetActive(y)+1],
                   gtkEntryGetText(etiquetay))
    col <- gtkComboBoxGetActive(color)
    if(col != 0){
      paleta <- randomColor(count = length(levels(datos[,gtkComboBoxGetActive(color)])), 
                             luminosity = "dark")
      grup <- paleta[datos[,gtkComboBoxGetActive(color)]]
    }
    ifelse(col == 0, g(labx, laby, title), g(labx, laby, title, grup, grup))
    if(col != 0){legend("bottomright", legend = levels(datos[,gtkComboBoxGetActive(color)]), lwd = 3,
           col = paleta, bty = "n")}
  }

  # Fila 1 - Titulo y seleccion
  opciones <- gtkHBox(F, 0)
  division$packStart(opciones, fill = F, expand = F)
  ifelse(.Platform$OS.type == "windows", opciones$`height-request` <- 20, opciones$`height-request` <- 35)
  
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
  color$appendText("Ninguno")
  for(nombre in nombres){color$appendText(nombre)}
  color$setActive(0)
  color$show()
  opciones$packStart(color, fill = F, expand = F)
  
  aplicar <- gtkButton("Aplicar")
  gSignalConnect(aplicar, "clicked", aplibot)
  opciones$packStart(aplicar, fill = F)
  
  # Fila 2 - Etiqueta x e y
  opciones2 <- gtkHBox(F, 0)
  division$packStart(opciones2, fill = F, expand = F)
  ifelse(.Platform$OS.type == "windows", opciones2$`height-request` <- 20, opciones2$`height-request` <- 30)
  
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
  
  etx <- names(datos)[gtkComboBoxGetActive(x)+1]
  ety <- names(datos)[gtkComboBoxGetActive(y)+1]
  titul <- "Gráfico de dispersión"
  g(etx, ety, titul)
}

nuevo <- function(widget){
  for(i in 0:(length(nombres)-1)){x$removeText(0); y$removeText(0)}
  
  nombres <<- colnames(datos)
  for(nombre in nombres){x$appendText(nombre)}
  x$setActive(0)
  
  for(nombre in nombres){y$appendText(nombre)}
  y$setActive(0)
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

options(warn = -1)
grado <- gtkEntry(5)
grado$`width-request` <- 45
gtkEntrySetAlignment(grado, 0)
gr$packStart(grado, fill = F, expand = F)
options(warn = 0)

boton <- gtkButton("Modelar")
gSignalConnect(boton, "clicked", reg)

vertical1$packStart(boton, fill = F)
horizontal$packStart(vertical1)

vertical2 <- gtkVBox(F, 5)

cargar <- gtkHBox(F, 3)
vertical2$packStart(cargar)

seleccion <- gtkButton("Seleccionar archivo")
gSignalConnect(seleccion, "clicked", buscar)
cargar$packEnd(seleccion, fill = F)

sep <- gtkLabel("Separador:")
cargar$packStart(sep)

options(warn = -1)
separador <- gtkEntry(1)
separador$`width-request` <- 20
cargar$packStart(separador)
options(warn = 0)

act <- gtkHBox(T, 5)
vertical2$packEnd(act)

limpiar <- gtkButton("Limpiar")
gSignalConnect(limpiar, "clicked", function(widget){for(i in 0:(length(nombres)-1)){x$removeText(0); y$removeText(0)}})
act$packEnd(limpiar)

actualizar <- gtkButton("Actualizar")
gSignalConnect(actualizar, "clicked", nuevo)
act$packStart(actualizar)

ejes <- gtkHBox(T, 5)
vertical2$packStart(ejes, fill = F)

sel <- gtkVBox(T, 4)
ejes$packEnd(sel, fill = F)

ej <- gtkVBox(T, 4)
ejes$packStart(ej, fill = F)

ejex <- gtkLabel("V. Independiente:")
ej$packStart(ejex)

ejey <- gtkLabel("V. Dependiente:")
ej$packStart(ejey)

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
