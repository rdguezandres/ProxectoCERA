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


(deffacts Todas_Praias
(Playa (provincia "A Corunha") (concello "Arteixo") (nombre "A Salsa ") (lugar-parroquia "Repibelo") (longitud "Corta") (tipo "Praia resgardada") (tipo-arena "Cantos rodados"))
(Playa (provincia "A Corunha") (concello "Arteixo") (nombre "Area Grande ou Valcovo") (lugar-parroquia "Valcovo - Arteixo (Santiago)") (longitud "Media") (tipo "Praia aberta") (tipo-arena "Fina"))
(Playa (provincia "A Corunha") (concello "Arteixo") (nombre "Area Grande de Suevos") (lugar-parroquia "Suevos") (longitud "Corta") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "A Corunha") (concello "Arteixo") (nombre "Area Pequena - A Hucha") (lugar-parroquia "Arteixo - Arteixo (Santiago)") (longitud "Corta") (tipo "Praia aberta") (tipo-arena "Fina"))
(Playa (provincia "A Corunha") (concello "Arteixo") (nombre "Areal das Combouzas ") (lugar-parroquia "Barranhan - Barranhan (San Xian)") (longitud "Media") (tipo "Praia aberta") (tipo-arena "Fina"))
(Playa (provincia "A Corunha") (concello "Arteixo") (nombre "Areal de Barranhan") (lugar-parroquia "Barranhan - Barranhan (San Xian)") (longitud "Larga") (tipo "Praia aberta") (tipo-arena "Fina"))
(Playa (provincia "A Corunha") (concello "Arteixo") (nombre "Areal do Reiro") (lugar-parroquia "Arteixo") (longitud "Corta") (tipo "Praia aberta") (tipo-arena "Fina"))
(Playa (provincia "A Corunha") (concello "Arteixo") (nombre "Sabon") (lugar-parroquia "Sabon - Oseiro (San Tirso)") (longitud "Media") (tipo "Praia aberta") (tipo-arena "Fina"))
(Playa (provincia "A Corunha") (concello "Bergondo") (nombre "Gandario") (lugar-parroquia "Gandario - Ouces (San Xoan)") (longitud "Media") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "A Corunha") (concello "Camarinhas") (nombre "Arou") (lugar-parroquia "Arou - Camelle (O Espirito Santo)") (longitud "Media") (tipo "Praia aberta") (tipo-arena "Fina"))
(Playa (provincia "A Corunha") (concello "Carballo") (nombre "Baldaio-Sainhas") (lugar-parroquia "") (longitud "Larga") (tipo "Praia aberta") (tipo-arena "Fina"))
(Playa (provincia "A Corunha") (concello "Carballo") (nombre "Pedra do Sal") (lugar-parroquia "") (longitud "Larga") (tipo "Praia aberta") (tipo-arena "Fina"))
(Playa (provincia "A Corunha") (concello "Carballo") (nombre "Razo") (lugar-parroquia "Razo da Costa - Razo (San Martiï¿½o)") (longitud "Larga") (tipo "Praia aberta") (tipo-arena "Fina"))
(Playa (provincia "A Corunha") (concello "A Corunha") (nombre "As Lapas") (lugar-parroquia "Paseo Maritimo") (longitud "Corta") (tipo "Praia resgardada") (tipo-arena "Grosa"))
(Playa (provincia "A Corunha") (concello "A Corunha") (nombre "Orzan-Matadero") (lugar-parroquia "Orzan") (longitud "Media") (tipo "Praia aberta") (tipo-arena "Grosa"))
(Playa (provincia "A Corunha") (concello "A Corunha") (nombre "Oza") (lugar-parroquia "") (longitud "Corta") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "A Corunha") (concello "A Corunha") (nombre "Riazor") (lugar-parroquia "Paseo Maritimo") (longitud "Media") (tipo "Praia aberta") (tipo-arena "Grosa"))
(Playa (provincia "A Corunha") (concello "A Corunha") (nombre "San Amaro") (lugar-parroquia "Adormideras") (longitud "Media") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "A Corunha") (concello "Dumbria") (nombre "Ezaro") (lugar-parroquia "O Ezaro - O Ezaro (Santa Uxia)") (longitud "Media") (tipo "Praia aberta") (tipo-arena "Fina"))
(Playa (provincia "A Corunha") (concello "Ferrol") (nombre "Caranza") (lugar-parroquia "Avenida del Mar ") (longitud "Media") (tipo "Praia resgardada") (tipo-arena "Grosa"))
(Playa (provincia "A Corunha") (concello "Ferrol") (nombre "A Fragata") (lugar-parroquia "Covas (San Martinho)") (longitud "Corta") (tipo "Praia aberta") (tipo-arena "Fina"))
(Playa (provincia "A Corunha") (concello "Ferrol") (nombre "Doninhos") (lugar-parroquia "Doninhos - Doninhos (San Roman)") (longitud "Larga") (tipo "Praia aberta") (tipo-arena "Fina"))
(Playa (provincia "A Corunha") (concello "Ferrol") (nombre "Esmelle") (lugar-parroquia "Esmelle - Esmelle (San Xoan)") (longitud "Corta") (tipo "Praia aberta") (tipo-arena "Fina"))
(Playa (provincia "A Corunha") (concello "Ferrol") (nombre "San Xurxo") (lugar-parroquia "Vila da Area - San Xurxo da Marinha (San Xurxo)") (longitud "Larga") (tipo "Praia aberta") (tipo-arena "Fina"))
(Playa (provincia "A Corunha") (concello "A Laracha") (nombre "Caion-Salseiras") (lugar-parroquia "Caion - Caion (Santa Maria do Socorro)") (longitud "Media") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "A Corunha") (concello "Laxe") (nombre "Laxe") (lugar-parroquia "Laxe (Santa Maria)") (longitud "Larga") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "A Corunha") (concello "Minho") (nombre "Perbes") (lugar-parroquia "Minho") (longitud "Media") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "A Corunha") (concello "Oleiros") (nombre "Bastiagueiro") (lugar-parroquia "Perillo - Perillo (Santa Locaia)") (longitud "Media") (tipo "Praia aberta") (tipo-arena "Fina"))
(Playa (provincia "A Corunha") (concello "Oleiros") (nombre "Espinheiro") (lugar-parroquia "") (longitud "Media") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "A Corunha") (concello "Oleiros") (nombre "Mera") (lugar-parroquia "A Lagoa - Maianca (San Cosme)") (longitud "Corta") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "A Corunha") (concello "Oleiros") (nombre "Porto Naval") (lugar-parroquia "A Aguieira - Dorneda (San Martinho)") (longitud "Corta") (tipo "Praia resgardada") (tipo-arena "Cantos rodados"))
(Playa (provincia "A Corunha") (concello "Oleiros") (nombre "Santa Cristina") (lugar-parroquia "Perillo - Perillo (Santa Locaia)") (longitud "Larga") (tipo "Praia aberta") (tipo-arena "Fina"))
(Playa (provincia "A Corunha") (concello "Ponteceso") (nombre "Balares") (lugar-parroquia "Balares - Cospindo (San Tirso)") (longitud "Media") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "A Corunha") (concello "Ponteceso") (nombre "Ermida") (lugar-parroquia "Gondomil - Corme Aldea (Santo Adran)") (longitud "Corta") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "A Corunha") (concello "Ponteceso") (nombre "Osmo") (lugar-parroquia "O Porto de Corme - Corme Porto (Nosa Senhora dos Remedios)") (longitud "Corta") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "A Corunha") (concello "As Pontes") (nombre "Lago das Pontes") (lugar-parroquia "Vilavella (Santa Maria)") (longitud "Media") (tipo "Praia resgardada") (tipo-arena "Grosa"))
(Playa (provincia "A Corunha") (concello "Ribeira") (nombre "Coroso") (lugar-parroquia "Santa Uxia de Ribeira") (longitud "Media") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "Lugo") (concello "Barreiros") (nombre "A Pasada") (lugar-parroquia "San Miguel de Reinante") (longitud "Media") (tipo "Praia aberta") (tipo-arena "Fina"))
(Playa (provincia "Lugo") (concello "Barreiros") (nombre "Coto") (lugar-parroquia "San Cosme - San Cosme de Barreiros") (longitud "Media") (tipo "Praia aberta") (tipo-arena "Fina"))
(Playa (provincia "Lugo") (concello "Barreiros") (nombre "Fontela-Balea") (lugar-parroquia "") (longitud "Media") (tipo "Praia aberta") (tipo-arena "Fina"))
(Playa (provincia "Lugo") (concello "Burela") (nombre "A Marosa") (lugar-parroquia "Burela (Santa Maria)") (longitud "Media") (tipo "Praia aberta") (tipo-arena "Fina"))
(Playa (provincia "Lugo") (concello "Burela") (nombre "O Portelo") (lugar-parroquia "Burela (Santa Maria)") (longitud "Media") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "Lugo") (concello "Burela") (nombre "Ril") (lugar-parroquia "Burela (Santa Maria)") (longitud "Corta") (tipo "Praia aberta") (tipo-arena "Fina "))
(Playa (provincia "Lugo") (concello "Cervo") (nombre "O Torno") (lugar-parroquia "San Cibrao - Lieiro (Santa Maria)") (longitud "Media") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "Lugo") (concello "Foz") (nombre "A Rapadoira") (lugar-parroquia "Foz - Foz (Santiago)") (longitud "Media") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "Lugo") (concello "Foz") (nombre "Areoura") (lugar-parroquia "") (longitud "Corta") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "Lugo") (concello "Foz") (nombre "As Polas") (lugar-parroquia "Nois") (longitud "Media") (tipo "Praia aberta") (tipo-arena "Fina"))
(Playa (provincia "Lugo") (concello "Foz") (nombre "Llas") (lugar-parroquia "") (longitud "Media") (tipo "Praia aberta") (tipo-arena "Fina"))
(Playa (provincia "Lugo") (concello "Foz") (nombre "Peizas") (lugar-parroquia "") (longitud "Media") (tipo "Praia aberta") (tipo-arena "Fina"))
(Playa (provincia "Lugo") (concello "Ribadeo") (nombre "A Praia das Catedrais") (lugar-parroquia "") (longitud "Media") (tipo "Praia aberta") (tipo-arena "Fina"))
(Playa (provincia "Lugo") (concello "Ribadeo") (nombre "Os Castros-Illas") (lugar-parroquia "A Rochela - A Devesa (Santalla)") (longitud "Media") (tipo "Praia aberta") (tipo-arena "Fina"))
(Playa (provincia "Lugo") (concello "O Vicedo") (nombre "Abrela") (lugar-parroquia "Abrela - Suegos (Santa Maria)") (longitud "Media") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "Lugo") (concello "O Vicedo") (nombre "Xilloi") (lugar-parroquia "Xilloi - O Vicedo (Santo Estevo)") (longitud "Media") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "Lugo") (concello "Viveiro") (nombre "Area") (lugar-parroquia "Area - Faro (San Xiao)") (longitud "Larga") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "Lugo") (concello "Xove") (nombre "Esteiro") (lugar-parroquia "Ceranzos - Xuances (San Pedro)") (longitud "Media") (tipo "Praia aberta") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Baiona") (nombre "A Ribeira") (lugar-parroquia "Baiona (Santa Maria)") (longitud "Corta") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Baiona") (nombre "Barbeira") (lugar-parroquia "Baiona (Santa Maria)") (longitud "Media") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Baiona") (nombre "Cuncheira") (lugar-parroquia "Paseo de Pinzon - Baiona (Santa Maria)") (longitud "Corta") (tipo "Praia aberta") (tipo-arena "Grosa"))
(Playa (provincia "Pontevedra") (concello "Baiona") (nombre "Os Frades ") (lugar-parroquia "Baiona (Santa Maria)") (longitud "Corta") (tipo "Praia aberta") (tipo-arena "Grosa"))
(Playa (provincia "Pontevedra") (concello "Baiona") (nombre "Santa Marta") (lugar-parroquia "") (longitud "Media") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Bueu") (nombre "Area de Bon") (lugar-parroquia "Bon de Arriba - Beluso (Santa Maria)") (longitud "Media") (tipo "Praia aberta") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Bueu") (nombre "Banda de Rio") (lugar-parroquia "Bueu (San Martinho)") (longitud "Media") (tipo "Praia resgardada") (tipo-arena "Grosa"))
(Playa (provincia "Pontevedra") (concello "Bueu") (nombre "Lagos") (lugar-parroquia "Montemogos - Beluso (Santa Maria)") (longitud "Media") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Bueu") (nombre "Lapaman") (lugar-parroquia "") (longitud "Media") (tipo "Praia aberta") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Bueu") (nombre "Portomaior") (lugar-parroquia "Castrelo - Cela (Santa Maria)") (longitud "Media") (tipo "Praia aberta") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Cangas ") (nombre "Area Brava") (lugar-parroquia "Vilanova - O Hio (Santo Andre)") (longitud "Media") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Cangas ") (nombre "Area Milla") (lugar-parroquia "Balea - Darbo (Santa Maria)") (longitud "Corta") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Cangas ") (nombre "Limens") (lugar-parroquia "Limens  - O Hio (Santo Andre)") (longitud "Media") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Cangas ") (nombre "Menduinha") (lugar-parroquia "Menduinha - Aldan (San Cibran)") (longitud "Media") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Cangas ") (nombre "Nerga (Espazo Natural Cabo Home)") (lugar-parroquia "Nerga - O Hio (Santo Andre)") (longitud "Media") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Cangas ") (nombre "Rodeira") (lugar-parroquia "Coiro (San Salvador)") (longitud "Media") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "A Guarda") (nombre "Area Grande") (lugar-parroquia "A Guarda - A Guarda (Santa Maria)") (longitud "Media") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "A Guarda") (nombre "O Muinho") (lugar-parroquia "O Muinho - Camposancos (Santa Isabel)") (longitud "Larga") (tipo "Praia aberta") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Illa de Arousa") (nombre "Area de Secada") (lugar-parroquia "") (longitud "Media") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Illa de Arousa") (nombre "Camaxe") (lugar-parroquia " ") (longitud "Corta") (tipo "Praia resgardada") (tipo-arena "Grosa"))
(Playa (provincia "Pontevedra") (concello "Marin") (nombre "Aguete") (lugar-parroquia "Aguete - Seixo (Nosa Senhora do Carme)") (longitud "Media") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Marin") (nombre "Mogor") (lugar-parroquia "O Monte - Mogor (San Xurxo)") (longitud "Media") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Marin") (nombre "Portocelo") (lugar-parroquia "O Monte - Mogor (San Xurxo)") (longitud "Media") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Marin") (nombre "Santo do Mar-A Covinha") (lugar-parroquia "Casas - Ardan (Santa Maria)") (longitud "Media") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Moanha") (nombre "O Con") (lugar-parroquia "O Con - Tiran (San Xoan)") (longitud "Corta") (tipo "Praia aberta") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Poio") (nombre "Cabeceira") (lugar-parroquia "Campelo - Poio (San Xoan)") (longitud "Corta") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Sanxenxo") (nombre "A Lanzada") (lugar-parroquia "O Grove - Sanxenxo") (longitud "Larga") (tipo "Praia aberta") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Sanxenxo") (nombre "A Lapa") (lugar-parroquia "A Lanzada - Noalla (Santo Estevo)") (longitud "Media") (tipo "Praia aberta") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Sanxenxo") (nombre "Areas") (lugar-parroquia "Areas - Bordons (San Pedro)") (longitud "Media") (tipo "Praia aberta") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Sanxenxo") (nombre "Areas Gordas  ") (lugar-parroquia " ") (longitud "Media") (tipo "Praia aberta") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Sanxenxo") (nombre "Baltar") (lugar-parroquia "Baltar - Adina (Santa Maria)") (longitud "Media") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Sanxenxo") (nombre "Bascuas") (lugar-parroquia "Aios - Noalla (Santo Estevo)") (longitud "Media") (tipo "Praia aberta") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Sanxenxo") (nombre "Canelas") (lugar-parroquia "Canelas - Adina (Santa Maria)") (longitud "Media") (tipo "Praia aberta") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Sanxenxo") (nombre "Canelinhas") (lugar-parroquia "Portonovo - Adina (Santa Maria)") (longitud "Corta") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Sanxenxo") (nombre "De Agra") (lugar-parroquia "Areas - Dorron (San Xoan)") (longitud "Media") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Sanxenxo") (nombre "Espinheira") (lugar-parroquia "A Lanzada - Noalla (Santo Estevo)") (longitud "Corta") (tipo "Praia aberta") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Sanxenxo") (nombre "Foxos") (lugar-parroquia "A Lanzada - Noalla (Santo Estevo)") (longitud "Media") (tipo "Praia aberta") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Sanxenxo") (nombre "Major") (lugar-parroquia "") (longitud "Media") (tipo "Praia aberta") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Sanxenxo") (nombre "Montalvo") (lugar-parroquia "") (longitud "Media") (tipo "Praia aberta") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Sanxenxo") (nombre "Panadeira") (lugar-parroquia "Sanxenxo - Padrinhan (San Xenxo)") (longitud "Corta") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Sanxenxo") (nombre "Paxarinhas") (lugar-parroquia "Paxarinhas - Adina (Santa Maria)") (longitud "Corta") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Sanxenxo") (nombre "Pragueira") (lugar-parroquia " ") (longitud "Media") (tipo "Praia aberta") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Sanxenxo") (nombre "Silgar") (lugar-parroquia "Sanxenxo - Padrinhan (San Xenxo)") (longitud "Media") (tipo "Praia aberta") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Vigo") (nombre "A Punta") (lugar-parroquia "") (longitud "Corta") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Vigo") (nombre "Argazada") (lugar-parroquia "") (longitud "Media") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Vigo") (nombre "Canido") (lugar-parroquia "Canido - Oia (San Miguel)") (longitud "Media") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Vigo") (nombre "Carril ") (lugar-parroquia "A Igrexa - Alcabre (Santa Baia)") (longitud "Media") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Vigo") (nombre "Fontainha (A Sirenita)") (lugar-parroquia "Breadouro - Coruxo (San Salvador)") (longitud "Media") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Vigo") (nombre "O Vao") (lugar-parroquia "Fontela - Coruxo (San Salvador)") (longitud "Media") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Vigo") (nombre "Os Muinhos - Fortinhon") (lugar-parroquia "A Corveira - Saians (San Xurxo)") (longitud "Corta") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Vigo") (nombre "Rodas (Illas Cies)") (lugar-parroquia "Illas Cies") (longitud "Larga") (tipo "Praia aberta") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Vigo") (nombre "Samil") (lugar-parroquia "Samil - Navia (San Paio)") (longitud "Larga") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Vigo") (nombre "Santa Baia") (lugar-parroquia "") (longitud "Media") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Vigo") (nombre "Tombo do Gato") (lugar-parroquia "") (longitud "Corta") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Vigo") (nombre "O Adro") (lugar-parroquia "Vigo") (longitud "Media") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Vilagarcia de Arousa") (nombre "Bamio") (lugar-parroquia "O Campanario - Bamio (San Xens)") (longitud "Corta") (tipo "Praia resgardada") (tipo-arena "Fina"))
(Playa (provincia "Pontevedra") (concello "Vilagarcia de Arousa") (nombre "Compostela") (lugar-parroquia "Vilagarcia (Santa Baia)") (longitud "Larga") (tipo "Praia resgardada") (tipo-arena "Fina"))
)

(deffacts BH
   (PlayaHistorial (playa_nombre "Abrela") (respuesta nil) (fin nil) (playa-r))
)