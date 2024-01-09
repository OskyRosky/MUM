##############
##   Sider   #
##############

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Presentación",tabName = "p1", icon = icon("chalkboard")),
    menuItem("Descriptivo",tabName = "p2", icon = icon("chalkboard")),
    menuItem("Muestra",tabName = "p3", icon = icon("chalkboard")),
    menuItem("Evaluación",tabName = "p4", icon = icon("ruler"))
    
  )
)