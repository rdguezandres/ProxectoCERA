(deftemplate Playa
   (slot provincia (default nil))
   (slot concello (default nil))
   (slot nombre (default nil))
   (slot lugar-parroquia (default nil))
   (slot longitud (default nil))
   (slot tipo (default nil))
   (slot tipo-arena (default nil))
)

(deftemplate Preferencias
   (slot provincia (allowed-symbols lugo corunha ourense pontevedra nil) (default nil))
   (slot longitud (allowed-symbols corta media larga nil) (default nil))
   (slot tipo (allowed-symbols resguardada abierta nil) (default nil))
   (slot tipo-arena (allowed-symbols cantos fina grosa nil) (default nil))
   (multislot playa-r (default nil))
)

(defrule PreguntarProvincia
   "Regla para preguntar la provincia al usuario"
   (declare (salience 10))
   ?x <- (Preferencias (provincia nil) (playa-r nil))
   =>
   (printout t "¿En qué provincia te gustaría encontrar una playa? ")
   (bind ?respuesta (read))
   (modify ?x (provincia ?respuesta))
)

; -------------------------------------------------------------------------------
; PREGUNTAS

(defrule PreguntarTipoArena
   "Regla para preguntar sobre el tipo de arena"
   ?x <- (Preferencias (provincia ?provincia) (tipo-arena nil) (playa-r nil))
   =>
   (printout t ?provincia ", ¿te gustaría una playa con arena fina, gruesa o cantos rodados? ")
   (bind ?respuesta (read))
   (modify ?x (tipo-arena ?respuesta))
)

; -------------------------------------------------------------------------------
; REGLAS DE PREGUNTAS
; A CORUÑA

; A Coruña - Se *tipo area: fina*, pregunta por *tipo de praia*
(defrule PreguntarTipoPlayaACoruna_F
   "Regla para preguntar sobre el tipo de playa en A Coruña"
   ?x <- (Preferencias (provincia corunha) (tipo-arena fina) (tipo nil) (playa-r nil))
   =>
   (printout t "Arena fina en A Coruña, ¿prefieres una playa resguardada o abierta? ")
   (bind ?respuesta (read))
   (modify ?x (tipo ?respuesta))
)

; A Coruña - Se *tipo area: grosa*, pregunta por *tipo de praia*
(defrule PreguntarTipoPlayaACoruna_G
   "Regla para preguntar sobre el tipo de playa en A Coruña"
   ?x <- (Preferencias (provincia corunha) (tipo-arena grosa) (tipo nil) (playa-r nil))
   =>
   (printout t "Arena grosa en A Coruña, ¿prefieres una playa resguardada o abierta? ")
   (bind ?respuesta (read))
   (modify ?x (tipo ?respuesta))
)

; A Coruña - Se *tipo area: fina*, *tipo de praia: resgardada*, pregunta por *longitud*
(defrule PreguntarLongitudACoruna_F_R
   "Regla para preguntar sobre la longitud de la playa en A Coruña"
   ?x <- (Preferencias (provincia corunha) (tipo-arena fina) (tipo resguardada) (longitud nil) (playa-r nil))
   =>
   (printout t "Arena fina y resguardada en A Coruña, ¿prefieres una playa corta, mediana o larga en longitud? ")
   (bind ?respuesta (read))
   (modify ?x (longitud ?respuesta))
)

; LUGO
; Lugo - Preguntar por tipo de praia
(defrule PreguntarTipoPlayaLugo
   "Regla para preguntar sobre el tipo de playa en Lugo"
   ?x <- (Preferencias (provincia lugo) (tipo nil) (playa-r nil))
   =>
   (printout t "¿Prefieres una playa resguardada o abierta? ")
   (bind ?respuesta (read))
   (modify ?x (tipo ?respuesta) (tipo-arena fina))
)

; Lugo - Preguntar lonxitude
(defrule PreguntarLongitudLugo
   "Regla para preguntar sobre la longitud de la playa en Lugo"
   ?x <- (Preferencias (provincia lugo) (longitud nil) (playa-r nil))
   =>
   (printout t "¿Prefieres una playa corta, mediana o larga en longitud? ")
   (bind ?respuesta (read))
   (modify ?x (longitud ?respuesta))
)

; PONTEVEDRA
; Pontevedra - Preguntar tipo de area e despois tipo de praia
(defrule PreguntarTipoArenaPontevedra
   "Regla para preguntar sobre el tipo de arena en Pontevedra"
   ?x <- (Preferencias (provincia pontevedra) (tipo-arena nil) (playa-r nil))
   =>
   (printout t "¿Prefieres una playa con arena fina, gruesa o cantos rodados? ")
   (bind ?respuesta (read))
   (modify ?x (tipo-arena ?respuesta))
)

; Pontevedra - Con tipo de area respostada, pregunta por *tipo de praia*
(defrule PreguntarTipoPlayaPontevedra
   "Regla para preguntar sobre el tipo de playa en Pontevedra"
   ?x <- (Preferencias (provincia pontevedra) (tipo-arena fina|grosa) (tipo nil) (playa-r nil))
   =>
   (printout t "¿Prefieres una playa resguardada o abierta? ")
   (bind ?respuesta (read))
   (modify ?x (tipo ?respuesta))
)

; Pontevedra - Con *tipo de area: fina*, calquera tipo de praia, preguntar por *longitud*
(defrule PreguntarLongitudPontevedra_F
   "Regla para preguntar sobre la longitud en Pontevedra con tipo de arena fina"
   ?x <- (Preferencias (provincia pontevedra) (tipo-arena fina) (tipo ~nil) (longitud nil) (playa-r nil))
   =>
   (printout t "¿Prefieres una playa corta, mediana o larga en longitud? ")
   (bind ?respuesta (read))
   (modify ?x (longitud ?respuesta))
)

; -------------------------------------------------------------------------------
; RECOMENDACIONES
; A CORUÑA
(defrule RecomendarACoruna_CR
   "Regla para recomendar playa en A Coruña con arena de cantos rodados"
   ?p <- (Preferencias (provincia corunha) (tipo-arena cantos) (playa-r $?playas))
   ?x <- (Playa (nombre ?nombre) (lugar-parroquia ?lugar) (concello ?concello) (provincia "A Corunha") (tipo-arena "Cantos rodados"))
   (test (not (member$ ?x ?playas)))
   =>
   (printout t "Te recomendamos la playa " ?nombre " en " ?lugar "( " ?concello " )" crlf)
   (modify ?p (playa-r $?playas ?x))
)

(defrule RecomendarACoruna_F_A
   "Regla para recomendar playa en A Coruña con arena fina y abierta"
   ?p <- (Preferencias (provincia corunha) (tipo-arena fina) (tipo abierta) (playa-r $?playas))
   ?x <- (Playa (nombre ?nombre) (lugar-parroquia ?lugar) (concello ?concello) (provincia "A Corunha") (tipo-arena "Fina") (tipo "Praia aberta"))
   (test (not (member$ ?x ?playas)))
   =>
   (printout t "Te recomendamos la playa " ?nombre " en " ?lugar "( " ?concello " )" crlf)
   (modify ?p (playa-r $?playas ?x))
)

(defrule RecomendarACoruna_F_R_C
   "Regla para recomendar playa en A Coruña con arena fina, resguardada y corta"
   ?p <- (Preferencias (provincia corunha) (tipo-arena fina) (tipo resguardada) (longitud corta) (playa-r $?playas))
   ?x <- (Playa (nombre ?nombre) (lugar-parroquia ?lugar) (concello ?concello) (provincia "A Corunha") (tipo-arena "Fina") (tipo "Praia resgardada") (longitud "Corta"))
   (test (not (member$ ?x ?playas)))
   =>
   (printout t "Te recomendamos la playa " ?nombre " en " ?lugar "( " ?concello " )" crlf)
   (modify ?p (playa-r $?playas ?x))
)

(defrule RecomendarACoruna_F_R_M
   "Regla para recomendar playa en A Coruña con arena fina, resguardada y mediana"
   ?p <- (Preferencias (provincia corunha) (tipo-arena fina) (tipo resguardada) (longitud media) (playa-r $?playas))
   ?x <- (Playa (nombre ?nombre) (lugar-parroquia ?lugar) (concello ?concello) (provincia "A Corunha") (tipo-arena "Fina") (tipo "Praia resgardada") (longitud "Media"))
   (test (not (member$ ?x ?playas)))
   =>
   (printout t "Te recomendamos la playa " ?nombre " en " ?lugar "( " ?concello " )" crlf)
   (modify ?p (playa-r $?playas ?x))
)

(defrule RecomendarACoruna_F_R_L
   "Regla para recomendar playa en A Coruña con arena fina, resguardada y larga"
   ?p <- (Preferencias (provincia corunha) (tipo-arena fina) (tipo resguardada) (longitud larga) (playa-r $?playas))
   ?x <- (Playa (nombre ?nombre) (lugar-parroquia ?lugar) (concello ?concello) (provincia "A Corunha") (tipo-arena "Fina") (tipo "Praia resgardada") (longitud "Larga"))
   (test (not (member$ ?x ?playas)))
   =>
   (printout t "Te recomendamos la playa " ?nombre " en " ?lugar "( " ?concello " )" crlf)
   (modify ?p (playa-r $?playas ?x))
)

(defrule RecomendarACoruna_G_A
   "Regla para recomendar playa en A Coruña con arena gruesa, abierta "
   ?p <- (Preferencias (provincia corunha) (tipo-arena grosa) (tipo abierta) (playa-r $?playas))
   ?x <- (Playa (nombre ?nombre) (lugar-parroquia ?lugar) (concello ?concello) (provincia "A Corunha") (tipo-arena "Grosa") (tipo "Praia aberta"))
   (test (not (member$ ?x ?playas)))
   =>
   (printout t "Te recomendamos la playa " ?nombre " en " ?lugar "( " ?concello " )" crlf)
   (modify ?p (playa-r $?playas ?x))
)

(defrule RecomendarACoruna_G_R
   "Regla para recomendar playa en A Coruña con arena gruesa, resguardada "
   ?p <- (Preferencias (provincia corunha) (tipo-arena grosa) (tipo resguardada) (playa-r $?playas))
   ?x <- (Playa (nombre ?nombre) (lugar-parroquia ?lugar) (concello ?concello) (provincia "A Corunha") (tipo-arena "Grosa") (tipo "Praia resgardada"))
   (test (not (member$ ?x ?playas)))
   =>
   (printout t "Te recomendamos la playa " ?nombre " en " ?lugar "( " ?concello " )" crlf)
   (modify ?p (playa-r $?playas ?x))
)

; LUGO
(defrule RecomendarLugo_A_C
   "Regla para recomendar playa en Lugo del tipo abierta y longitud corta"
   ?p <- (Preferencias (provincia lugo) (tipo abierta) (longitud corta) (playa-r $?playas))
   ?x <- (Playa (nombre ?nombre) (lugar-parroquia ?lugar) (concello ?concello) (provincia "Lugo") (tipo "Praia aberta") (longitud "Corta"))
   (test (not (member$ ?x ?playas)))
   =>
   (printout t "Te recomendamos la playa " ?nombre " en " ?lugar "( " ?concello " )" crlf)
   (modify ?p (playa-r $?playas ?x))
)

(defrule RecomendarLugo_A_M
   "Regla para recomendar playa en Lugo del tipo abierta y longitud media"
   ?p <- (Preferencias (provincia lugo) (tipo abierta) (longitud media) (playa-r $?playas))
   ?x <- (Playa (nombre ?nombre) (lugar-parroquia ?lugar) (concello ?concello) (provincia "Lugo") (tipo "Praia aberta") (longitud "Media"))
   (test (not (member$ ?x ?playas)))
   =>
   (printout t "Te recomendamos la playa " ?nombre " en " ?lugar " ( " ?concello " )" crlf)
   (modify ?p (playa-r $?playas ?x))
)

(defrule RecomendarLugo_A_M
   "Regla para recomendar playa en Lugo del tipo abierta y longitud media"
   ?p <- (Preferencias (provincia lugo) (tipo abierta) (longitud larga) (playa-r $?playas))
   ?x <- (Playa (nombre ?nombre) (lugar-parroquia ?lugar) (concello ?concello) (provincia "Lugo") (tipo "Praia aberta") (longitud "Larga"))
   (test (not (member$ ?x ?playas)))
   =>
   (printout t "Te recomendamos la playa " ?nombre " en " ?lugar " ( " ?concello " )" crlf)
   (modify ?p (playa-r $?playas ?x))
)

(defrule RecomendarLugo_R_M
   "Regla para recomendar playa en Lugo del tipo resguardada y longitud media"
   ?p <- (Preferencias (provincia lugo) (tipo resguardada) (longitud media) (playa-r $?playas))
   ?x <- (Playa (nombre ?nombre) (lugar-parroquia ?lugar) (concello ?concello) (provincia "Lugo") (tipo "Praia resgardada") (longitud "Media"))
   (test (not (member$ ?x ?playas)))
   =>
   (printout t "Te recomendamos la playa " ?nombre " en " ?lugar " ( " ?concello " )" crlf)
   (modify ?p (playa-r $?playas ?x))
)

(defrule RecomendarLugo_R_L
   "Regla para recomendar playa en Lugo del tipo resguardada y longitud larga"
   ?p <- (Preferencias (provincia lugo) (tipo resguardada) (longitud larga) (playa-r $?playas))
   ?x <- (Playa (nombre ?nombre) (lugar-parroquia ?lugar) (concello ?concello) (provincia "Lugo") (tipo "Praia resgardada") (longitud "Larga"))
   (test (not (member$ ?x ?playas)))
   =>
   (printout t "Te recomendamos la playa " ?nombre " en " ?lugar " ( " ?concello " )" crlf)
   (modify ?p (playa-r $?playas ?x))
)

; PONTEVEDRA
; Area grosa
(defrule RecomendarPontevedra_G_A
   "Regla para recomendar playa en Pontevedra del tipo gruesa y abierta"
   ?p <- (Preferencias (provincia pontevedra) (tipo-arena grosa) (tipo abierta) (playa-r $?playas))
   ?x <- (Playa (nombre ?nombre) (lugar-parroquia ?lugar) (concello ?concello) (provincia "Pontevedra") (tipo-arena "Grosa") (tipo "Praia aberta"))
   (test (not (member$ ?x ?playas)))
   =>
   (printout t "Te recomendamos la playa " ?nombre " en " ?lugar " ( " ?concello " )" crlf)
   (modify ?p (playa-r $?playas ?x))
)

(defrule RecomendarPontevedra_G_R
   "Regla para recomendar playa en Pontevedra del tipo gruesa y resguardada"
   ?p <- (Preferencias (provincia pontevedra) (tipo-arena grosa) (tipo resguardada) (playa-r $?playas))
   ?x <- (Playa (nombre ?nombre) (lugar-parroquia ?lugar) (concello ?concello) (provincia "Pontevedra") (tipo-arena "Grosa") (tipo "Praia resgardada"))
   (test (not (member$ ?x ?playas)))
   =>
   (printout t "Te recomendamos la playa " ?nombre " en " ?lugar " ( " ?concello " )" crlf)
   (modify ?p (playa-r $?playas ?x))
)

; Area fina
; Area fina - Abierta
(defrule RecomendarPontevedra_F_A_C
   "Regla para recomendar playa en Pontevedra del tipo fina, abierta y corta"
   ?p <- (Preferencias (provincia pontevedra) (tipo-arena fina) (tipo abierta) (longitud corta) (playa-r $?playas))
   ?x <- (Playa (nombre ?nombre) (lugar-parroquia ?lugar) (concello ?concello) (provincia "Pontevedra") (tipo-arena "Fina") (tipo "Praia aberta") (longitud "Corta"))
   (test (not (member$ ?x ?playas)))
   =>
   (printout t "Te recomendamos la playa " ?nombre " en " ?lugar " ( " ?concello " )" crlf)
   (modify ?p (playa-r $?playas ?x))
)

(defrule RecomendarPontevedra_F_A_M
   "Regla para recomendar playa en Pontevedra del tipo fina, abierta y media"
   ?p <- (Preferencias (provincia pontevedra) (tipo-arena fina) (tipo abierta) (longitud media) (playa-r $?playas))
   ?x <- (Playa (nombre ?nombre) (lugar-parroquia ?lugar) (concello ?concello) (provincia "Pontevedra") (tipo-arena "Fina") (tipo "Praia aberta") (longitud "Media"))
   (test (not (member$ ?x ?playas)))
   =>
   (printout t "Te recomendamos la playa " ?nombre " en " ?lugar " ( " ?concello " )" crlf)
   (modify ?p (playa-r $?playas ?x))
)

(defrule RecomendarPontevedra_F_A_L
   "Regla para recomendar playa en Pontevedra del tipo fina, abierta y larga"
   ?p <- (Preferencias (provincia pontevedra) (tipo-arena fina) (tipo abierta) (longitud larga) (playa-r $?playas))
   ?x <- (Playa (nombre ?nombre) (lugar-parroquia ?lugar) (concello ?concello) (provincia "Pontevedra") (tipo-arena "Fina") (tipo "Praia aberta") (longitud "Larga"))
   (test (not (member$ ?x ?playas)))
   =>
   (printout t "Te recomendamos la playa " ?nombre " en " ?lugar " ( " ?concello " )" crlf)
   (modify ?p (playa-r $?playas ?x))
)

; Area fina - Resguardada
(defrule RecomendarPontevedra_F_R_C
   "Regla para recomendar playa en Pontevedra del tipo fina, resguardada y corta"
   ?p <- (Preferencias (provincia pontevedra) (tipo-arena fina) (tipo resguardada) (longitud corta) (playa-r $?playas))
   ?x <- (Playa (nombre ?nombre) (lugar-parroquia ?lugar) (concello ?concello) (provincia "Pontevedra") (tipo-arena "Fina") (tipo "Praia resgardada") (longitud "Corta"))
   (test (not (member$ ?x ?playas)))
   =>
   (printout t "Te recomendamos la playa " ?nombre " en " ?lugar " ( " ?concello " )" crlf)
   (modify ?p (playa-r $?playas ?x))
)

(defrule RecomendarPontevedra_F_R_M
   "Regla para recomendar playa en Pontevedra del tipo fina, resguardada y media"
   ?p <- (Preferencias (provincia pontevedra) (tipo-arena fina) (tipo resguardada) (longitud media) (playa-r $?playas))
   ?x <- (Playa (nombre ?nombre) (lugar-parroquia ?lugar) (concello ?concello) (provincia "Pontevedra") (tipo-arena "Fina") (tipo "Praia resgardada") (longitud "Media"))
   (test (not (member$ ?x ?playas)))
   =>
   (printout t "Te recomendamos la playa " ?nombre " en " ?lugar " ( " ?concello " )" crlf)
   (modify ?p (playa-r $?playas ?x))
)

(defrule RecomendarPontevedra_F_R_L
   "Regla para recomendar playa en Pontevedra del tipo fina, resguardada y larga"
   ?p <- (Preferencias (provincia pontevedra) (tipo-arena fina) (tipo resguardada) (longitud larga) (playa-r $?playas))
   ?x <- (Playa (nombre ?nombre) (lugar-parroquia ?lugar) (concello ?concello) (provincia "Pontevedra") (tipo-arena "Fina") (tipo "Praia resgardada") (longitud "Larga"))
   (test (not (member$ ?x ?playas)))
   =>
   (printout t "Te recomendamos la playa " ?nombre " en " ?lugar " ( " ?concello " )" crlf)
   (modify ?p (playa-r $?playas ?x))
)

; SIN RESULTADOS
(defrule SinResultados
   "Regla para mostrar mensaje de no hay resultados"
   ?p <- (Preferencias (provincia ?provincia) (tipo-arena ?arena) (tipo ?tipo) (longitud ?longitud) (playa-r nil))
   (not (Playa (provincia ?provincia) (tipo-arena ?arena) (tipo ?tipo) (longitud ?longitud)))
   =>
   (printout t "No hay resultados para tu búsqueda" crlf)
   (retract ?p)
)