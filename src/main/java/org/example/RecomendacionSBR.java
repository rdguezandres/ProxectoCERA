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
                if (playa.getLongitud() != "") {
                    if (Integer.parseInt(playa.getLongitud()) <= 200){
                        playa.setLongitud("Corta");
                    }
                    else if (Integer.parseInt(playa.getLongitud()) > 200 && Integer.parseInt(playa.getLongitud()) <= 1000){
                        playa.setLongitud("Media");
                    }
                    else if (Integer.parseInt(playa.getLongitud()) > 1000){
                        playa.setLongitud("Larga");
                    }
                } else{
                    playa.setLongitud("Corta");
                }


                // Construir el comando CLIPS para insertar una nueva instancia de la clase Playa
                String assertCommand = String.format(
                        "(assert (Playa (provincia \"%s\") (codigo-provincia \"%s\") (concello \"%s\") " +
                                "(codigo-concello \"%s\") (nombre \"%s\") (lugar-parroquia \"%s\") (longitud \"%s\") " +
                                "(tipo \"%s\") (tipo-arena \"%s\") (coordenadas \"%s\")))",
                        playa.getProvincia(), playa.getCodigoProvincia(), playa.getConcello(),
                        playa.getCodigoConcello(), playa.getNombre(), playa.getLugarParroquia(),
                        playa.getLongitud(), playa.getTipo(), playa.getTipoArea(), coordenadas
                );

                String assertCommand2 = String.format(
                        "(Playa (provincia \"%s\") (concello \"%s\") (nombre \"%s\") (lugar-parroquia \"%s\") (longitud \"%s\") " +
                                "(tipo \"%s\") (tipo-arena \"%s\"))",
                        playa.getProvincia(), playa.getConcello(),playa.getNombre(), playa.getLugarParroquia(),
                        playa.getLongitud(), playa.getTipo(), playa.getTipoArea()
                );

                System.out.println(assertCommand2);

                // Ejecutar el comando CLIPS para insertar la instancia de Playa
                clips.eval(assertCommand);
                //System.out.println("Playa insertada: " + playa.getNombre());

            } catch (Exception e) {
                System.out.println("Error: " + e.getMessage());
                System.out.println("Al insertar la playa: " + playa.getNombre());
            }
        }
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

