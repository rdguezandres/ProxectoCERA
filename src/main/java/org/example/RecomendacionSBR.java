package org.example;

import net.sf.clipsrules.jni.*;

import java.util.*;

public class RecomendacionSBR {

    private Environment clips;

    public RecomendacionSBR() {
        clips = new Environment();
    }

    public void cargarReglas(String rutaReglas) throws CLIPSException {
        clips.load(rutaReglas);
    }

    public void cargarDatosPlayas(List<Playa> playas) throws CLIPSException {
        // Iterar sobre la lista de playas
        for (Playa playa : playas) {
            try {
                String coordenadas = playa.getCoordenadas1() + ", " + playa.getCoordenadas2();

                // Construir el comando CLIPS para insertar una nueva instancia de la clase Playa
                String assertCommand = String.format(
                        "(assert (Playa (provincia \"%s\") (codigo-provincia \"%s\") (concello \"%s\") " +
                                "(codigo-concello \"%s\") (nombre \"%s\") (lugar-parroquia \"%s\") (longitud \"%s\") " +
                                "(tipo \"%s\") (tipo-arena \"%s\") (coordenadas \"%s\")))",
                        playa.getProvincia(), playa.getCodigoProvincia(), playa.getConcello(),
                        playa.getCodigoConcello(), playa.getNombre(), playa.getLugarParroquia(),
                        playa.getLongitud(), playa.getTipo(), playa.getTipoArea(), coordenadas
                );

                // Ejecutar el comando CLIPS para insertar la instancia de Playa
                clips.eval(assertCommand);
                System.out.println("Playa insertada: " + playa.getNombre());

            } catch (Exception e) {
                System.out.println("Error: " + e.getMessage());
                System.out.println("Al insertar la playa: " + playa.getNombre());
            }
        }
    }

    public void ejecutarSistemaRecomendacion(List<Playa> playas) throws CLIPSException {
        clips.reset();
        cargarDatosPlayas(playas);
        clips.run();
    }

    // Otros métodos según sea necesario
}

