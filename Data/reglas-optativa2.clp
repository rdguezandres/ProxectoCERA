(deftemplate Playa
   (slot provincia)
   (slot codigo-provincia)
   (slot concello)
   (slot codigo-concello)
   (slot nombre)
   (slot lugar-parroquia)
   (slot longitud)
   (slot tipo)
   (slot tipo-arena)
   (slot coordenadas)
)

(deftemplate Usuario
   (slot username)
   (slot provincia)
)

(defrule Bienvenida
   "Regla para mostrar un mensaje de bienvenida"
   =>
   (printout t "Bienvenido al Sistema de Recomendación Basado en Reglas (SBR)." crlf)
   (printout t "------------------------------" crlf)
)

(defrule Registro
   "Regla para preguntar el nombre de usuario"
   =>
   (printout t "¿Cuál es tu nombre de usuario? ")
   (bind ?respuesta (read))
   (assert (Usuario (username ?respuesta)))
)

(defrule Despedida
   "Regla para mostrar un mensaje de despedida"
   =>
   (printout t "------------------------------" crlf)
   (printout t "Gracias por usar el SBR. Hasta pronto!" crlf)
)
