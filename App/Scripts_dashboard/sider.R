##############
##   Sider   #
##############

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Presentación",tabName = "p1", icon = icon("chalkboard")),
    menuItem("Descriptivo",tabName = "p2", icon = icon("chalkboard")),
    menuItem("Muestra MUM",tabName = "p3", icon = icon("chalkboard")),
    menuItem("Muestra LES",tabName = "p4", icon = icon("chalkboard")),
    menuItem("Muestra Atributos",tabName = "p5", icon = icon("chalkboard")),
    menuItem("Evaluación",tabName = "p6", icon = icon("ruler"))
    
  )
)