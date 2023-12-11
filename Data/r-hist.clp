(deftemplate Playa
   (slot provincia (default nil))
   (slot concello (default nil))
   (slot nombre (default nil))
   (slot lugar-parroquia (default nil))
   (slot longitud (default nil))
   (slot tipo (default nil))
   (slot tipo-arena (default nil))
)

(deftemplate PlayaHistorial
   (slot playa_nombre (default nil))
   (slot respuesta (allowed-symbols p c l t ar tar lar par ltar ptar plar pltar plt lt pt pl car ctar clar cltar clt cl ct nil) (default nil))
   (slot fin (allowed-symbols si no nil) (default nil))
   (multislot playa-r (default nil))
)

(defrule Final
    ?historial <- (PlayaHistorial (fin si))
    =>
    (printout t "Gracias por usar nuestro sistema de recomendación de playas" crlf)
    (retract ?historial)
    (halt)
)

(defrule Preguntar
   "Regla para recomendar similares"
   (declare (salience 10))
   ?historial <- (PlayaHistorial (playa_nombre ?p) (respuesta nil))
   ?playa <- (Playa (nombre ?p) (provincia ?provincia) (concello ?concello) (lugar-parroquia ?lugar))
   =>
   (printout t "Recientemente has visitado " ?p " en " ?lugar "( " ?concello ", " ?provincia ")" crlf)
   (printout t "Buscaremos playas similares. Qué características deseas que comparta? " crlf 
                "- Provincia (p)" crlf
                "- Concello (c)" crlf
                "- Longitud (l)" crlf
                "- Tipo (t)" crlf
                "- Tipo de arena (ar)" crlf
                " SE DESEXAS VARIAS OPCIONS, ESCRÍBEAS XUNTAS NA ORDE OFRECIDA, por exemplo 'pcltar' ou 'ct'" crlf
                " --> ")
   (bind ?respuesta (read))
   (modify ?historial (respuesta ?respuesta))
)

;--------------------------------
; REGLAS BÁSICAS

(defrule CompartirProvincia
    ?historial <- (PlayaHistorial (playa_nombre ?p) (respuesta p) (fin nil) (playa-r $?playas))
    ?playa <- (Playa (nombre ?p) (provincia ?provincia))
    ?playa_recomendada <- (Playa (nombre ?p2) (provincia ?provincia) (concello ?concello) (lugar-parroquia ?lugar) (longitud ?longitud) (tipo ?tipo) (tipo-arena ?arena))
    (test (neq ?p ?p2))
    (test (not (member$ ?playa_recomendada ?playas)))
    =>
    (printout t "Te recomendamos la playa " ?p2 " en " ?lugar "( " ?concello ", " ?provincia ")" crlf)
    (modify ?historial (playa-r ?playas ?playa_recomendada))
)

(defrule CompartirLonxitude
    ?historial <- (PlayaHistorial (playa_nombre ?p) (respuesta l) (fin nil) (playa-r $?playas))
    ?playa <- (Playa (nombre ?p) (longitud ?longitud))
    ?playa_recomendada <- (Playa (nombre ?p2) (provincia ?provincia) (concello ?concello) (lugar-parroquia ?lugar) (longitud ?longitud) (tipo ?tipo) (tipo-arena ?arena))
    (test (neq ?p ?p2))
    (test (not (member$ ?playa_recomendada ?playas)))
    =>
    (printout t "Te recomendamos la playa " ?p2 " en " ?lugar "( " ?concello ", " ?provincia ")" crlf)
    (modify ?historial (playa-r ?playas ?playa_recomendada))
)

(defrule CompartirTipo
    ?historial <- (PlayaHistorial (playa_nombre ?p) (respuesta t) (fin nil) (playa-r $?playas))
    ?playa <- (Playa (nombre ?p) (tipo ?tipo))
    ?playa_recomendada <- (Playa (nombre ?p2) (provincia ?provincia) (concello ?concello) (lugar-parroquia ?lugar) (longitud ?longitud) (tipo ?tipo) (tipo-arena ?arena))
    (test (neq ?p ?p2))
    (test (not (member$ ?playa_recomendada ?playas)))
    =>
    (printout t "Te recomendamos la playa " ?p2 " en " ?lugar "( " ?concello ", " ?provincia ")" crlf)
    (modify ?historial (playa-r ?playas ?playa_recomendada))
)

(defrule CompartirTipoArena
    ?historial <- (PlayaHistorial (playa_nombre ?p) (respuesta ar) (fin nil) (playa-r $?playas))
    ?playa <- (Playa (nombre ?p) (tipo-arena ?arena))
    ?playa_recomendada <- (Playa (nombre ?p2) (provincia ?provincia) (concello ?concello) (lugar-parroquia ?lugar) (longitud ?longitud) (tipo ?tipo) (tipo-arena ?arena))
    (test (neq ?p ?p2))
    (test (not (member$ ?playa_recomendada ?playas)))
    =>
    (printout t "Te recomendamos la playa " ?p2 " en " ?lugar "( " ?concello ", " ?provincia ")" crlf)
    (modify ?historial (playa-r ?playas ?playa_recomendada))
)

(defrule CompartirConcello
    ?historial <- (PlayaHistorial (playa_nombre ?p) (respuesta c) (fin nil) (playa-r $?playas))
    ?playa <- (Playa (nombre ?p) (concello ?concello))
    ?playa_recomendada <- (Playa (nombre ?p2) (provincia ?provincia) (lugar-parroquia ?lugar) (concello ?concello) (longitud ?longitud) (tipo ?tipo) (tipo-arena ?arena))
    (test (not (member$ ?playa_recomendada ?playas)))
    (test (neq ?p ?p2))
    =>
    (printout t "Te recomendamos la playa " ?p2 " en " ?lugar "( " ?concello ", " ?provincia ")" crlf) 
    (modify ?historial (playa-r $?playas ?playa_recomendada))
)

(defrule CompartirTodo
    ?historial <- (PlayaHistorial (playa_nombre ?p) (respuesta pltar) (fin nil) (playa-r $?playas))
    ?playa <- (Playa (nombre ?p) (provincia ?provincia) (longitud ?longitud) (tipo ?tipo) (tipo-arena ?arena))
    ?playa_recomendada <- (Playa (nombre ?p2) (provincia ?provincia) (concello ?concello) (lugar-parroquia ?lugar) (longitud ?longitud) (tipo ?tipo) (tipo-arena ?arena))
    (test (neq ?p ?p2))
    (test (not (member$ ?playa_recomendada ?playas)))
    =>
    (printout t "Te recomendamos la playa " ?p2 " en " ?lugar "( " ?concello ", " ?provincia ")" crlf)
    (modify ?historial (playa-r ?playas ?playa_recomendada))
)

;-----------------------------------------

(defrule CompartirTAR
    "Regla para mismo tipo de arena y tipo de playa"
    ?historial <- (PlayaHistorial (playa_nombre ?p) (respuesta tar) (fin nil) (playa-r $?playas))
    ?playa <- (Playa (nombre ?p) (tipo ?tipo) (tipo-arena ?arena))
    ?playa_recomendada <- (Playa (nombre ?p2) (provincia ?provincia) (concello ?concello) (lugar-parroquia ?lugar) (longitud ?longitud) (tipo ?tipo) (tipo-arena ?arena))
    (test (neq ?p ?p2))
    (test (not (member$ ?playa_recomendada ?playas)))
    =>
    (printout t "Te recomendamos la playa " ?p2 " en " ?lugar "( " ?concello ", " ?provincia ")" crlf)
    (modify ?historial (playa-r ?playas ?playa_recomendada))
)

(defrule CompartirPAR
    "Regla para misma provincia y tipo de arena"
    ?historial <- (PlayaHistorial (playa_nombre ?p) (respuesta par) (fin nil) (playa-r $?playas))
    ?playa <- (Playa (nombre ?p) (provincia ?provincia) (tipo-arena ?arena))
    ?playa_recomendada <- (Playa (nombre ?p2) (provincia ?provincia) (concello ?concello) (lugar-parroquia ?lugar) (longitud ?longitud) (tipo ?tipo) (tipo-arena ?arena))
    (test (neq ?p ?p2))
    (test (not (member$ ?playa_recomendada ?playas)))
    (test (neq ?p ?p2))
    =>
    (printout t "Te recomendamos la playa " ?p2 " en " ?lugar "( " ?concello ", " ?provincia ")" crlf) 
    (modify ?historial (playa-r ?playas ?playa_recomendada))
)

(defrule CompartirLAR
    "Regla para misma longitud y tipo de arena"
    ?historial <- (PlayaHistorial (playa_nombre ?p) (respuesta lar) (fin nil) (playa-r $?playas))
    ?playa <- (Playa (nombre ?p) (longitud ?longitud) (tipo-arena ?arena))
    ?playa_recomendada <- (Playa (nombre ?p2) (provincia ?provincia) (concello ?concello) (lugar-parroquia ?lugar) (longitud ?longitud) (tipo ?tipo) (tipo-arena ?arena))
    (test (neq ?p ?p2))
    (test (not (member$ ?playa_recomendada ?playas)))
    =>
    (printout t "Te recomendamos la playa " ?p2 " en " ?lugar "( " ?concello ", " ?provincia ")" crlf)
    (modify ?historial (playa-r ?playas ?playa_recomendada))
)

(defrule CompartirLTAR
    "Regla para mismo tipo de playa, longitud y tipo de arena"
    ?historial <- (PlayaHistorial (playa_nombre ?p) (respuesta ltar) (fin nil) (playa-r $?playas))
    ?playa <- (Playa (nombre ?p) (tipo ?tipo) (longitud ?longitud) (tipo-arena ?arena))
    ?playa_recomendada <- (Playa (nombre ?p2) (provincia ?provincia) (concello ?concello) (lugar-parroquia ?lugar) (longitud ?longitud) (tipo ?tipo) (tipo-arena ?arena))
    (test (neq ?p ?p2))
    (test (not (member$ ?playa_recomendada ?playas)))
    =>
    (printout t "Te recomendamos la playa " ?p2 " en " ?lugar "( " ?concello ", " ?provincia ")" crlf)
    (modify ?historial (playa-r ?playas ?playa_recomendada))
)

(defrule CompartirPLAR
    "Regla para misma provincia, longitud y tipo de arena"
    ?historial <- (PlayaHistorial (playa_nombre ?p) (respuesta plar) (fin nil) (playa-r $?playas))
    ?playa <- (Playa (nombre ?p) (provincia ?provincia) (longitud ?longitud) (tipo-arena ?arena))
    ?playa_recomendada <- (Playa (nombre ?p2) (provincia ?provincia) (concello ?concello) (lugar-parroquia ?lugar) (longitud ?longitud) (tipo ?tipo) (tipo-arena ?arena))
    (test (neq ?p ?p2))
    (test (not (member$ ?playa_recomendada ?playas)))
    =>
    (printout t "Te recomendamos la playa " ?p2 " en " ?lugar "( " ?concello ", " ?provincia ")" crlf)
    (modify ?historial (playa-r ?playas ?playa_recomendada))
)

(defrule CompartirPTAR
    "Regla para misma provincia, tipo de playa y tipo de arena"
    ?historial <- (PlayaHistorial (playa_nombre ?p) (respuesta ptar) (fin nil) (playa-r $?playas))
    ?playa <- (Playa (nombre ?p) (provincia ?provincia) (tipo ?tipo) (tipo-arena ?arena))
    ?playa_recomendada <- (Playa (nombre ?p2) (provincia ?provincia) (concello ?concello) (lugar-parroquia ?lugar) (longitud ?longitud) (tipo ?tipo) (tipo-arena ?arena))
    (test (neq ?p ?p2))
    (test (not (member$ ?playa_recomendada ?playas)))
    =>
    (printout t "Te recomendamos la playa " ?p2 " en " ?lugar "( " ?concello ", " ?provincia ")" crlf)
    (modify ?historial (playa-r $?playas ?playa_recomendada))
)


(defrule CompartirPL
    ?historial <- (PlayaHistorial (playa_nombre ?p) (respuesta pl) (fin nil) (playa-r $?playas))
    ?playa <- (Playa (nombre ?p) (provincia ?provincia) (longitud ?longitud))
    ?playa_recomendada <- (Playa (nombre ?p2) (provincia ?provincia) (concello ?concello) (lugar-parroquia ?lugar) (longitud ?longitud) (tipo ?tipo) (tipo-arena ?arena))
    (test (neq ?p ?p2))
    (test (not (member$ ?playa_recomendada ?playas)))
    =>
    (printout t "Te recomendamos la playa " ?p2 " en " ?lugar "( " ?concello ", " ?provincia ")" crlf)
    (modify ?historial (playa-r ?playas ?playa_recomendada))
)

(defrule CompartirLT
    "Regla para mismo tipo de arena y longitud"
    ?historial <- (PlayaHistorial (playa_nombre ?p) (respuesta lt) (fin nil) (playa-r $?playas))
    ?playa <- (Playa (nombre ?p) (tipo ?tipo) (longitud ?longitud))
    ?playa_recomendada <- (Playa (nombre ?p2) (provincia ?provincia) (concello ?concello) (lugar-parroquia ?lugar) (longitud ?longitud) (tipo ?tipo) (tipo-arena ?arena))
    (test (neq ?p ?p2))
    (test (not (member$ ?playa_recomendada ?playas)))
    =>
    (printout t "Te recomendamos la playa " ?p2 " en " ?lugar "( " ?concello ", " ?provincia ")" crlf)
    (modify ?historial (playa-r ?playas ?playa_recomendada))
)

(defrule CompartirPT
    "Regla para misma provincia y tipo"
    ?historial <- (PlayaHistorial (playa_nombre ?p) (respuesta pt) (fin nil) (playa-r $?playas))
    ?playa <- (Playa (nombre ?p) (provincia ?provincia) (tipo ?tipo))
    ?playa_recomendada <- (Playa (nombre ?p2) (provincia ?provincia) (concello ?concello) (lugar-parroquia ?lugar) (longitud ?longitud) (tipo ?tipo) (tipo-arena ?arena))
    (test (neq ?p ?p2))
    (test (not (member$ ?playa_recomendada ?playas)))
    =>
    (printout t "Te recomendamos la playa " ?p2 " en " ?lugar "( " ?concello ", " ?provincia ")" crlf)
    (modify ?historial (playa-r ?playas ?playa_recomendada))
)

(defrule CompartirPLT
    "Regla para mismo tipo de playa, longitud y tipo de arena"
    ?historial <- (PlayaHistorial (playa_nombre ?p) (respuesta plt) (fin nil) (playa-r $?playas))
    ?playa <- (Playa (nombre ?p) (provincia ?provincia) (tipo ?tipo) (longitud ?longitud))
    ?playa_recomendada <- (Playa (nombre ?p2) (provincia ?provincia) (concello ?concello) (lugar-parroquia ?lugar) (longitud ?longitud) (tipo ?tipo) (tipo-arena ?arena))
    (test (neq ?p ?p2))
    (test (not (member$ ?playa_recomendada ?playas)))
    =>
    (printout t "Te recomendamos la playa " ?p2 " en " ?lugar "( " ?concello ", " ?provincia ")" crlf)
    (modify ?historial (playa-r ?playas ?playa_recomendada))
)

(defrule CompartirCAR
    "Regla para mismo concello y tipo de arena"
    ?historial <- (PlayaHistorial (playa_nombre ?p) (respuesta car) (fin nil) (playa-r $?playas))
    ?playa <- (Playa (nombre ?p) (concello ?concello) (tipo-arena ?arena))
    ?playa_recomendada <- (Playa (nombre ?p2) (provincia ?provincia) (concello ?concello) (lugar-parroquia ?lugar) (longitud ?longitud) (tipo ?tipo) (tipo-arena ?arena))
    (test (neq ?p ?p2))
    (test (not (member$ ?playa_recomendada ?playas)))
    =>
    (printout t "Te recomendamos la playa " ?p2 " en " ?lugar "( " ?concello ", " ?provincia ")" crlf)
    (modify ?historial (playa-r $?playas ?playa_recomendada))
)

(defrule CompartirCTAR
    "Regla para mismo concello, tipo de playa y tipo de arena"
    ?historial <- (PlayaHistorial (playa_nombre ?p) (respuesta ctar) (fin nil) (playa-r $?playas))
    ?playa <- (Playa (nombre ?p) (concello ?concello) (tipo ?tipo) (tipo-arena ?arena))
    ?playa_recomendada <- (Playa (nombre ?p2) (provincia ?provincia) (concello ?concello) (lugar-parroquia ?lugar) (longitud ?longitud) (tipo ?tipo) (tipo-arena ?arena))
    (test (neq ?p ?p2))
    (test (not (member$ ?playa_recomendada ?playas)))
    (test (neq ?p ?p2))
    =>
    (printout t "Te recomendamos la playa " ?p2 " en " ?lugar "( " ?concello ", " ?provincia ")" crlf) 
    (modify ?historial (playa-r $?playas ?playa_recomendada))
)

(defrule CompartirCLAR
    "Regla para mismo concello, longitud y tipo de arena"
    ?historial <- (PlayaHistorial (playa_nombre ?p) (respuesta clar) (fin nil) (playa-r $?playas))
    ?playa <- (Playa (nombre ?p) (concello ?concello) (longitud ?longitud) (tipo-arena ?arena))
    ?playa_recomendada <- (Playa (nombre ?p2) (provincia ?provincia) (concello ?concello) (lugar-parroquia ?lugar) (longitud ?longitud) (tipo ?tipo) (tipo-arena ?arena))
    (test (neq ?p ?p2))
    (test (not (member$ ?playa_recomendada ?playas))) 
    =>
    (printout t "Te recomendamos la playa " ?p2 " en " ?lugar "( " ?concello ", " ?provincia ")" crlf) 
    (modify ?historial (playa-r $?playas ?playa_recomendada))
)

(defrule CompartirCLTAR
    "Regla para mismo concello, longitud, tipo de playa y tipo de arena"
    ?historial <- (PlayaHistorial (playa_nombre ?p) (respuesta cltar) (fin nil) (playa-r $?playas))
    ?playa <- (Playa (nombre ?p) (concello ?concello) (longitud ?longitud) (tipo ?tipo) (tipo-arena ?arena))
    ?playa_recomendada <- (Playa (nombre ?p2) (provincia ?provincia) (concello ?concello) (lugar-parroquia ?lugar) (longitud ?longitud) (tipo ?tipo) (tipo-arena ?arena))
    (test (neq ?p ?p2))
    (test (not (member$ ?playa_recomendada ?playas))) 
    =>
    (printout t "Te recomendamos la playa " ?p2 " en " ?lugar "( " ?concello ", " ?provincia ")" crlf) 
    (modify ?historial (playa-r $?playas ?playa_recomendada))
)

(defrule CompartirCLT
    "Regla para mismo concello, longitud y tipo de playa"
    ?historial <- (PlayaHistorial (playa_nombre ?p) (respuesta clt) (fin nil) (playa-r $?playas))
    ?playa <- (Playa (nombre ?p) (concello ?concello) (longitud ?longitud) (tipo ?tipo))
    ?playa_recomendada <- (Playa (nombre ?p2) (provincia ?provincia) (concello ?concello) (lugar-parroquia ?lugar) (longitud ?longitud) (tipo ?tipo) (tipo-arena ?arena))
    (test (neq ?p ?p2))
    (test (not (member$ ?playa_recomendada ?playas))) 
    =>
    (printout t "Te recomendamos la playa " ?p2 " en " ?lugar "( " ?concello ", " ?provincia ")" crlf) 
    (modify ?historial (playa-r $?playas ?playa_recomendada))
)

(defrule CompartirCT
    "Regla para mismo concello y tipo de playa"
    ?historial <- (PlayaHistorial (playa_nombre ?p) (respuesta ct) (fin nil) (playa-r $?playas))
    ?playa <- (Playa (nombre ?p) (concello ?concello) (tipo ?tipo))
    ?playa_recomendada <- (Playa (nombre ?p2) (provincia ?provincia) (concello ?concello) (lugar-parroquia ?lugar) (longitud ?longitud) (tipo ?tipo) (tipo-arena ?arena))
    (test (neq ?p ?p2))
    (test (not (member$ ?playa_recomendada ?playas))) 
    =>
    (printout t "Te recomendamos la playa " ?p2 " en " ?lugar "( " ?concello ", " ?provincia ")" crlf) 
    (modify ?historial (playa-r $?playas ?playa_recomendada))
)

(defrule CompartirCL
    "Regla para mismo concello y longitud"
    ?historial <- (PlayaHistorial (playa_nombre ?p) (respuesta cl) (fin nil) (playa-r $?playas))
    ?playa <- (Playa (nombre ?p) (concello ?concello) (longitud ?longitud))
    ?playa_recomendada <- (Playa (nombre ?p2) (provincia ?provincia) (concello ?concello) (lugar-parroquia ?lugar) (longitud ?longitud) (tipo ?tipo) (tipo-arena ?arena))
    (test (neq ?p ?p2))
    (test (not (member$ ?playa_recomendada ?playas))) 
    =>
    (printout t "Te recomendamos la playa " ?p2 " en " ?lugar "( " ?concello ", " ?provincia ")" crlf) 
    (modify ?historial (playa-r $?playas ?playa_recomendada))
)