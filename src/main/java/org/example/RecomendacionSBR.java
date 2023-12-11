package org.example;

import net.sf.clipsrules.jni.*;

import java.util.*;

public class RecomendacionSBR {

    private Environment clips;

    public RecomendacionSBR() {
        clips = new Environment();
    }


    public void limpiarDatos() throws CLIPSException {
        clips.clear();
    }

    public void ejecutarSistemaRecomendacion(List<Playa> playas) throws CLIPSException {
        clips.reset();
        //cargarDatosPlayas(playas);
        clips.assertString("(Preferencias (provincia pontevedra) (tipo-arena fina) (tipo resguardada) (longitud corta))");
        clips.run();
    }

    // Otros métodos según sea necesario
}

