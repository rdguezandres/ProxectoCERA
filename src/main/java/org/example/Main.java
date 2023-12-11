package org.example;
import com.opencsv.CSVReader;
import com.opencsv.CSVReaderBuilder;
import com.opencsv.exceptions.CsvException;
import net.sf.clipsrules.jni.*;

import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import net.sf.clipsrules.jni.*;
import java.util.*;

public class Main {
    private static Environment clips;

    public static void main(String[] args) {
        // Crear instancia de RecomendacionSBR
        clips = new Environment();

        try {
            // Cargar reglas desde un archivo CLIPS
            clips.load("Data/r-java.clp");

            // Leer datos de playas desde un archivo o fuente de datos
            List<Playa> playas = leerDatosPlayas();

            // Ejecutar el sistema de recomendación
            clips.reset();

            // Preparar datos de playas para insertar en CLIPS
            PrepararDatosPlayas(playas);

            // Preparar filtro
            String selecProvincia = "lugo";
            String selecTipoArena = "fina";
            String selecTipo = "abierta";
            String selecLongitud = "corta";
            clips.assertString(String.format("(Preferencias (provincia %s) (tipo-arena %s) (tipo %s) (longitud %s))",
                    selecProvincia, selecTipoArena, selecTipo, selecLongitud));
            clips.run();


        } catch (CLIPSException e) {
            e.printStackTrace();
        }
    }

    public static void PrepararDatosPlayas(List<Playa> playas) throws CLIPSException {
        // Iterar sobre la lista de playas
        for (Playa playa : playas) {
            try {
                String assertCommand = String.format(
                        "(Playa (provincia \"%s\") (concello \"%s\") (nombre \"%s\") (lugar-parroquia \"%s\") (longitud \"%s\") " +
                                "(tipo \"%s\") (tipo-arena \"%s\"))",
                        playa.getProvincia(), playa.getConcello(),playa.getNombre(), playa.getLugarParroquia(),
                        playa.getLongitud(), playa.getTipo(), playa.getTipoArea()
                );
                clips.assertString(assertCommand);

            } catch (Exception e) {
                System.out.println("Error: " + e.getMessage());
                System.out.println("Al insertar la playa: " + playa.getNombre());
            }
        }
    }

    private static List<Playa> leerDatosPlayas() {
        String filePath = "Data/DatosPraias_Definitivo.csv";

        List<Playa> playas = new ArrayList<>();

        // 1. Leer datos de CSV y crear instancias de Playa
        try (CSVReader csvReader = new CSVReaderBuilder(new FileReader(filePath)).build()) {
            // Leer todas las filas del CSV
            List<String[]> data = csvReader.readAll();
            int len = 52;

            // Iterar sobre las filas (omitir la primera fila con títulos)
            for (int i = 1; i < len; i++) {
                String col[] = data.get(i)[0].split(";");

                // Crear instancia de Playa y asignar valores
                Playa playa = new Playa(col);

                // Agregar la instancia de Playa a la lista
                playas.add(playa);
            }
        } catch (IOException | CsvException e) {
            e.printStackTrace();
        }
        return playas;
    }
}
